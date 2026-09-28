const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const loadUi = require("./load-ui.cjs");

const root = path.join(__dirname, "../..");
const jsSource = fs.readFileSync(path.join(root, "src/ui/zabapgit_js_common.w3mi.data.js"), "utf8");

function abapSources(dir) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const file = path.join(dir, entry.name);
    if (entry.isDirectory()) return abapSources(file);
    if (!entry.name.endsWith(".abap") || entry.name.includes(".testclasses.")) return [];
    return [{ file: path.relative(root, file), text: fs.readFileSync(file, "utf8") }];
  });
}

const abap = abapSources(path.join(root, "src"));

function escapeRegExp(text) {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

// The ways the ABAP side emits an element ID: a literal attribute, an ID parameter of an
// HTML helper, or a text input whose name doubles as its ID.
function idEmitters(id) {
  const value = escapeRegExp(id);
  return [
    new RegExp(`id="${value}"`),
    new RegExp(`\\biv_(?:div_)?id\\s*=\\s*['|\`]${value}['|\`]`, "i"),
    new RegExp(`render_text_input\\(\\s*iv_name\\s*=\\s*['|\`]${value}['|\`]`, "i")
  ];
}

function filesEmitting(patterns) {
  return abap.filter(({ text }) => patterns.some((pattern) => pattern.test(text))).map(({ file }) => file);
}

// Fixed IDs the script looks up. IDs passed in from ABAP (params.ids, CheckListWrapper, ...) are not listed.
function hardCodedIds() {
  const ids = new Set();
  for (const match of jsSource.matchAll(/getElementById\((?:[\w.]+\s*\|\|\s*)?"([^"]+)"\)/g)) ids.add(match[1]);
  for (const match of jsSource.matchAll(/querySelector(?:All)?\("#([\w-]+)/g)) ids.add(match[1]);
  return [...ids].sort();
}

test("finds the fixed element IDs in the script", () => {
  const ids = hardCodedIds();
  // Guards against the extraction silently matching nothing after a refactoring.
  for (const id of ["global_sapevent_form", "header", "jump", "debug-output", "hotkeys"]) {
    assert.ok(ids.includes(id), `${id} not extracted from common.js`);
  }
});

for (const id of hardCodedIds()) {
  test(`ABAP still renders element ID "${id}"`, () => {
    assert.notDeepEqual(filesEmitting(idEmitters(id)), [], `no ABAP source renders id "${id}"`);
  });
}

test("ABAP still renders the IDs of the patch page", () => {
  const context = loadUi();
  const stage = context.Patch.prototype.ID.STAGE;
  const patchLine = context.PatchLine.prototype.ID;

  assert.notDeepEqual(filesEmitting(idEmitters(stage)), [], `no ABAP source renders id "${stage}"`);
  assert.notDeepEqual(
    filesEmitting([new RegExp(`\\biv_id\\s*=\\s*\\|${escapeRegExp(patchLine)}_\\{`, "i")]),
    [],
    `no ABAP source renders ids "${patchLine}_..."`);
});

test("ABAP still labels the diff filter entry the check list handles specially", () => {
  const match = jsSource.match(/option === "([^"]+)"/);
  assert.ok(match, "special check list option not found in common.js");
  assert.notDeepEqual(filesEmitting([new RegExp(`iv_txt\\s*=\\s*'${escapeRegExp(match[1])}'`)]), [],
    `no ABAP source renders the filter entry "${match[1]}"`);
});
