const test = require('node:test');
const assert = require('node:assert/strict');
const loadUi = require('./load-ui.cjs');

function page({ webgui = true, busy = false, supported = true, legacy = false } = {}) {
  const events = new Map();
  const hooks = new Map();
  const document = {
    addEventListener() {},
    createElement() {
      return { attributes: {}, children: [],
        setAttribute(k, v) { this.attributes[k] = v; },
        getAttribute(k) { return this.attributes[k] ?? null; },
        removeAttribute(k) { delete this.attributes[k]; },
        appendChild(n) { this.children.push(n); n.parentNode = this; },
        removeChild(n) { this.children.splice(this.children.indexOf(n), 1); },
        contains(n) { return this.children.includes(n); },
        focus() { document.activeElement = this; }
      };
    }
  };
  document.body = document.createElement();
  const input = document.createElement();
  document.body.appendChild(input);
  input.focus();
  const shell = {
    E_EVENTS: { Lock: 'lock', Unlock: 'unlock' },
    attachEvent(name, receiver, method) { hooks.set(name, () => receiver[method]()); },
    detachEvent(name) { hooks.delete(name); },
    bLocked() { return busy; }
  };
  // SAP removes g4h.$ before rendering the HTML viewer.
  const parent = { sap: { g4h: {}, its: {} }, UCF_System: {} };
  const facade = { oGetInternal(system) { assert.equal(system, parent.UCF_System); return shell; } };
  if (supported) {
    if (legacy) parent.mysap = { LS: facade };
    else parent.sap.its.LS = facade;
  }
  const context = loadUi({ document, parent,
    addEventListener(name, handler) { events.set(name, handler); },
    removeEventListener(name) { events.delete(name); }
  });
  context.gEnv.isWebGui = webgui;
  context.initializeWebGuiBusyLock();
  return { context, document, input, shell, hooks, events,
    overlay: document.body.children[1] };
}

test('locks on backend activity and unlocks without replacing the page', () => {
  const p = page();
  assert.equal(p.overlay.hidden, true);
  p.hooks.get('lock')();
  assert.equal(p.overlay.hidden, false);
  assert.equal(p.document.body.getAttribute('aria-busy'), 'true');
  assert.equal(p.document.activeElement, p.overlay);
  for (const name of ['click', 'keydown', 'submit', 'touchstart', 'touchmove', 'wheel']) {
    let prevented = false, stopped = false;
    p.events.get(name)({ preventDefault() { prevented = true; }, stopImmediatePropagation() { stopped = true; } });
    assert.ok(prevented && stopped, name);
  }
  p.hooks.get('lock')(); // repeated notification must not overwrite saved focus
  p.hooks.get('unlock')();
  assert.equal(p.overlay.hidden, true);
  assert.equal(p.document.activeElement, p.input);
  assert.equal(p.document.body.getAttribute('aria-busy'), null);
  p.events.get('click')({ preventDefault() { assert.fail('idle click blocked'); } });
});

test('initial busy state, page disposal and restored page do not leak subscriptions', () => {
  const p = page({ busy: true });
  assert.equal(p.overlay.hidden, false);
  p.context.initializeWebGuiBusyLock();
  assert.equal(p.document.body.children.length, 2);
  p.events.get('pagehide')();
  assert.equal(p.hooks.size, 0);
  assert.equal(p.document.body.children.length, 1);
  assert.equal(p.events.has('keydown'), false);
  p.events.get('pageshow')();
  assert.equal(p.hooks.size, 2);
});

test('unlock preserves backend-assigned focus and pre-existing aria-busy', () => {
  const p = page();
  p.document.body.setAttribute('aria-busy', 'false');
  p.hooks.get('lock')();
  const next = p.document.createElement();
  p.document.body.appendChild(next);
  next.focus();
  p.hooks.get('unlock')();
  assert.equal(p.document.activeElement, next);
  assert.equal(p.document.body.getAttribute('aria-busy'), 'false');
});

for (const options of [{ webgui: false }, { supported: false }]) {
  test(`unsupported environment remains interactive: ${JSON.stringify(options)}`, () => {
    const p = page(options);
    assert.equal(p.hooks.size, 0);
    assert.equal(p.overlay, undefined);
  });
}

test('cross-origin parent does not break initialization', () => {
  const p = page({ supported: false });
  Object.defineProperty(p.context, 'parent', { get() { throw Error('cross origin'); } });
  assert.doesNotThrow(() => p.context.initializeWebGuiBusyLock());
});

for (const legacy of [false, true]) {
  test(`initializes after the bootstrap object is deleted (legacy alias: ${legacy})`, () => {
    const p = page({ legacy });
    assert.equal(p.context.parent.sap.g4h.$, undefined);
    assert.ok(p.overlay);
    p.hooks.get('lock')();
    assert.equal(p.overlay.hidden, false);
    p.hooks.get('unlock')();
    assert.equal(p.overlay.hidden, true);
  });
}
