CLASS zcl_abapgit_gui_page_sett_remo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_hotkeys.

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA ms_settings_snapshot TYPE types=>ty_remote_settings.
    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map .
    DATA mv_refresh_on_back TYPE abap_bool.
    DATA mv_offline_switch_saved_url TYPE string.
    DATA mo_popup_picklist TYPE REF TO zcl_abapgit_gui_picklist.

    METHODS get_remote_settings_from_repo
      IMPORTING
        io_repo            TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(rs_settings) TYPE types=>ty_remote_settings
      RAISING
        zcx_abapgit_exception.

    METHODS get_remote_settings_from_form
      IMPORTING
        io_form_data       TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rs_settings) TYPE types=>ty_remote_settings
      RAISING
        zcx_abapgit_exception.

    METHODS get_form_schema
      IMPORTING
        io_existing_form_data TYPE REF TO zcl_abapgit_string_map OPTIONAL
      RETURNING
        VALUE(ro_form)        TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

    METHODS initialize_form_data
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS check_protection
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        io_form_data             TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS save_settings
      RAISING
        zcx_abapgit_exception.

    METHODS choose_url
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS choose_branch
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

    METHODS choose_tag
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

    METHODS choose_pr
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

    METHODS choose_commit
      RETURNING
        VALUE(rv_commit) TYPE types=>ty_remote_settings-commit
      RAISING
        zcx_abapgit_exception.

    METHODS switch_online_offline
      RAISING
        zcx_abapgit_exception.

    METHODS switch_to_branch_tag
      IMPORTING
        !iv_name TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception.

    METHODS switch_to_commit
      IMPORTING
        !iv_revert TYPE abap_bool DEFAULT abap_false
        !iv_commit TYPE types=>ty_remote_settings-commit OPTIONAL
      RAISING
        zcx_abapgit_exception.

    METHODS switch_to_pull_req
      IMPORTING
        !iv_revert TYPE abap_bool DEFAULT abap_false
        !iv_pull   TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception.

    METHODS handle_picklist_state
      RAISING
        zcx_abapgit_exception.

    METHODS render_content
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_sett_remo IMPLEMENTATION.


  METHOD check_protection.

    IF mo_repo->is_offline( ) = abap_true.
      zcx_abapgit_exception=>raise( 'Unexpected switch for offline repo' ).
    ENDIF.
    IF mo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot switch. Repository is write-protected in local settings' ).
    ENDIF.

  ENDMETHOD.


  METHOD choose_branch.

    DATA lv_url         TYPE zif_abapgit_persistence=>ty_repo-url.
    DATA lv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name.
    DATA ls_branch      TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( const=>id-offline ) = abap_true.
        RETURN.
      ENDIF.

      lv_url         = mo_form_data->get( const=>id-url ).
      lv_branch_name = mo_form_data->get( const=>id-branch ).

      mo_popup_picklist = zcl_abapgit_popup_branch_list=>create(
        iv_show_new_option = abap_false
        iv_url             = lv_url
        iv_default_branch  = lv_branch_name
        )->create_picklist(
        )->set_id( const=>event-choose_branch
        )->set_in_page( ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_branch ).
        IF ls_branch IS NOT INITIAL.
          mo_form_data->set(
            iv_key = const=>id-branch
            iv_val = ls_branch-display_name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_commit.

    DATA:
      lv_url         TYPE string,
      lv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name,
      li_popups      TYPE REF TO zif_abapgit_popups.

    IF mo_form_data->get( const=>id-offline ) = abap_true.
      RETURN.
    ENDIF.

    lv_url = mo_form_data->get( const=>id-url ).
    lv_branch_name = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && mo_form_data->get( const=>id-branch ).

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).

    rv_commit = li_popups->commit_list_popup(
      iv_repo_url    = lv_url
      iv_branch_name = lv_branch_name )-sha1.

  ENDMETHOD.


  METHOD choose_pr.

    DATA ls_pull TYPE zif_abapgit_pr_enum_provider=>ty_pull_request.
    DATA lv_url TYPE types=>ty_remote_settings-url.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( const=>id-offline ) = abap_true.
        zcx_abapgit_exception=>raise( 'Not possible for offline repositories' ).
      ENDIF.

      lv_url = mo_form_data->get( const=>id-url ).
      mo_popup_picklist = zcl_abapgit_popup_pull_request=>create( lv_url
        )->create_picklist(
        )->set_id( const=>event-choose_pull_request
        )->set_in_page( abap_true ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_pull ).
        IF ls_pull IS NOT INITIAL.
          mo_form_data->set(
            iv_key = const=>id-pull_request
            iv_val = ls_pull-head_url && '@' && ls_pull-head_branch ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_tag.

    DATA ls_tag TYPE zif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_url TYPE types=>ty_remote_settings-url.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( const=>id-offline ) = abap_true.
        RETURN.
      ELSEIF mo_repo->is_offline( ) = abap_true.
        MESSAGE 'Please save conversion to online repository before choosing a tag' TYPE 'S'.
        RETURN.
      ENDIF.

      lv_url = mo_form_data->get( const=>id-url ).
      mo_popup_picklist = zcl_abapgit_popup_tag_list=>create( lv_url
        )->create_picklist(
        )->set_id( const=>event-choose_tag
        )->set_in_page( ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_tag ).
        IF ls_tag IS NOT INITIAL.
          mo_form_data->set(
            iv_key = const=>id-tag
            iv_val = ls_tag-display_name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_url.

    " todo, get url history from DB and show selection popup #3639
    rv_url = ''.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mo_repo              = io_repo.
    ms_settings_snapshot = get_remote_settings_from_repo( mo_repo ).
    mo_form              = get_form_schema( ).
    mo_form_data         = initialize_form_data( ).
    CREATE OBJECT mo_validation_log.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_remo.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Remote Settings'
      io_page_menu       = zcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = zif_abapgit_definitions=>c_action-repo_remote_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA:
      lv_button    TYPE string,
      lv_icon      TYPE string,
      lv_offline   TYPE abap_bool,
      lv_head_type TYPE types=>ty_head_type.

    IF io_existing_form_data IS BOUND AND io_existing_form_data->is_empty( ) = abap_false.
      lv_offline = io_existing_form_data->get( const=>id-offline ).
      IF lv_offline = abap_false.
        lv_head_type = io_existing_form_data->get( const=>id-head_type ).
      ENDIF.
    ELSE.
      lv_offline   = ms_settings_snapshot-offline.
      lv_head_type = ms_settings_snapshot-head_type.
    ENDIF.

    ro_form = zcl_abapgit_html_form=>create(
      iv_form_id   = 'repo-remote-settings-form'
      iv_help_page = 'https://docs.abapgit.org/settings-remote.html' ).

    IF lv_offline = abap_true.
      lv_button = 'Switch to Online'.
      lv_icon   = 'plug/darkgrey'.
    ELSE.
      lv_button = 'Switch to Offline'.
      lv_icon   = 'cloud-upload-alt/darkgrey'.
    ENDIF.

    ro_form->start_group(
      iv_name  = const=>id-general
      iv_label = 'General'
      iv_hint  = 'Change the general type and origin of the repository'
    )->text(
      iv_name        = const=>id-repo_type
      iv_label       = |Type of Repository: { zcl_abapgit_html=>icon( lv_icon ) }|
      iv_readonly    = abap_true
    )->hidden( const=>id-offline ).

    IF lv_offline = abap_false.

      ro_form->text(
        iv_name        = const=>id-url
        iv_condense    = abap_true
        iv_label       = 'Git Repository URL'
        iv_hint        = 'URL of original repository'
        iv_placeholder = 'https://github.com/...git' ).

      ro_form->start_group(
        iv_name  = const=>id-head_group
        iv_label = 'Head'
      )->radio(
        iv_label  = 'Type'
        iv_name   = const=>id-head_type
        iv_action = const=>event-change_head_type
      )->option(
        iv_label = 'Branch'
        iv_value = const=>head_types-branch
      )->option(
        iv_label = 'Tag'
        iv_value = const=>head_types-tag
      )->option(
        iv_label = 'Commit'
        iv_value = const=>head_types-commit
      )->option(
        iv_label = 'Pull Request'
        iv_value = const=>head_types-pull_request ).

      IF lv_head_type = const=>head_types-branch OR
         lv_head_type = const=>head_types-commit.
        ro_form->text(
          iv_name        = const=>id-branch
          iv_label       = 'Branch'
          iv_required    = abap_true
          iv_side_action = const=>event-choose_branch ).
      ENDIF.

      IF lv_head_type = const=>head_types-tag.
        ro_form->text(
          iv_name        = const=>id-tag
          iv_label       = 'Tag'
          iv_required    = abap_true
          iv_side_action = const=>event-choose_tag ).
      ENDIF.

      IF lv_head_type = const=>head_types-commit.
        ro_form->text(
          iv_name        = const=>id-commit
          iv_label       = 'Commit'
          iv_required    = abap_true
          iv_min         = 40
          iv_max         = 40
          iv_side_action = const=>event-choose_commit ).
      ENDIF.

      IF lv_head_type = const=>head_types-pull_request.
        ro_form->text(
          iv_name        = const=>id-pull_request
          iv_label       = 'Pull Request'
          iv_required    = abap_true
          iv_side_action = const=>event-choose_pull_request ).
      ENDIF.

    ENDIF.

    ro_form->command(
      iv_label    = 'Save Settings'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = const=>event-save
    )->command(
      iv_label  = lv_button
      iv_action = const=>event-switch
    )->command(
      iv_label  = 'Back'
      iv_action = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_remote_settings_from_form.

    rs_settings-offline = io_form_data->get( const=>id-offline ).

    IF rs_settings-offline = abap_false.
      rs_settings-url       = io_form_data->get( const=>id-url ).
      rs_settings-head_type = io_form_data->get( const=>id-head_type ).

      CASE rs_settings-head_type.
        WHEN const=>head_types-branch.
          rs_settings-branch = zif_abapgit_git_definitions=>c_git_branch-heads_prefix &&
            io_form_data->get( const=>id-branch ).
        WHEN const=>head_types-tag.
          rs_settings-tag = zif_abapgit_git_definitions=>c_git_branch-tags_prefix &&
            io_form_data->get( const=>id-tag ).
        WHEN const=>head_types-commit.
          rs_settings-branch = zif_abapgit_git_definitions=>c_git_branch-heads_prefix &&
            io_form_data->get( const=>id-branch ).
          rs_settings-commit = io_form_data->get( const=>id-commit ).
        WHEN const=>head_types-pull_request.
          rs_settings-pull_request = io_form_data->get( const=>id-pull_request ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get_remote_settings_from_repo.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
          lv_branch      TYPE types=>ty_remote_settings-branch.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      rs_settings-url = lo_repo_online->get_url( ).
      rs_settings-offline = abap_false.
      rs_settings-switched_origin = lo_repo_online->get_switched_origin( ).

      IF lo_repo_online->get_selected_commit( ) IS NOT INITIAL.
        rs_settings-commit = lo_repo_online->get_selected_commit( ).
        rs_settings-branch = lo_repo_online->get_selected_branch( ).
        rs_settings-head_type = const=>head_types-commit.
      ELSEIF lo_repo_online->get_switched_origin( ) IS NOT INITIAL.
        " get_switched_origin( ) returns the original repo url + HEAD concatenated with @
        " get_branch( ) returns the branch of the PR in the source repo
        " get_url( ) returns the source repo of the PR branch

        rs_settings-switched_origin = lo_repo_online->get_switched_origin( ).
        SPLIT rs_settings-switched_origin AT '@' INTO rs_settings-url rs_settings-branch.
        IF rs_settings-branch CP zif_abapgit_git_definitions=>c_git_branch-tags.
          rs_settings-tag = rs_settings-branch.
          CLEAR rs_settings-branch.
        ENDIF.

        lv_branch = lo_repo_online->get_selected_branch( ).
        REPLACE FIRST OCCURRENCE OF zif_abapgit_git_definitions=>c_git_branch-heads_prefix IN lv_branch WITH space.
        CONDENSE lv_branch.
        rs_settings-pull_request = |{ lo_repo_online->get_url( ) }@{ lv_branch }|.
        rs_settings-head_type = const=>head_types-pull_request.
      ELSE.
        rs_settings-branch = lo_repo_online->get_selected_branch( ).
        rs_settings-head_type = const=>head_types-branch.

        IF rs_settings-branch CP zif_abapgit_git_definitions=>c_git_branch-tags.
          rs_settings-head_type = const=>head_types-tag.
          rs_settings-tag = rs_settings-branch.
          CLEAR rs_settings-branch.
        ENDIF.
      ENDIF.

    ELSE.
      rs_settings-offline = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD handle_picklist_state.

    IF mo_popup_picklist IS BOUND AND
      ( mo_popup_picklist->is_fulfilled( ) = abap_true OR mo_popup_picklist->is_in_page( ) = abap_false ).
      " Picklist is either fullfilled OR
      " it was on its own page and user went back from it via F3/ESC and the picklist had no "graceful back" handler
      CASE mo_popup_picklist->id( ).
        WHEN const=>event-choose_pull_request.
          choose_pr( abap_true ).
        WHEN const=>event-choose_branch.
          choose_branch( abap_true ).
        WHEN const=>event-choose_tag.
          choose_tag( abap_true ).
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |Unexpected picklist id { mo_popup_picklist->id( ) }| ).
      ENDCASE.

      CLEAR mo_popup_picklist.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_form_data.

    DATA:
      lv_type TYPE string,
      lv_head TYPE string.

    CREATE OBJECT ro_form_data.

    IF ms_settings_snapshot-offline = abap_true.
      lv_type = const=>repo_type-offline.
    ELSE.
      lv_type = const=>repo_type-online.
    ENDIF.

    ro_form_data->set(
      iv_key = const=>id-offline
      iv_val = ms_settings_snapshot-offline ).
    ro_form_data->set(
      iv_key = const=>id-repo_type
      iv_val = lv_type ).

    IF ms_settings_snapshot-offline = abap_false.
      ro_form_data->set(
        iv_key = const=>id-url
        iv_val = ms_settings_snapshot-url ).

      ro_form_data->set(
        iv_key = const=>id-head_type
        iv_val = ms_settings_snapshot-head_type ).

      " When pull request is selected the previously selected branch/tag is also loaded to be able to switch back to it
      lv_head = zcl_abapgit_git_branch_list=>get_display_name( ms_settings_snapshot-branch ).
      ro_form_data->set(
        iv_key = const=>id-branch
        iv_val = lv_head ).

      lv_head = zcl_abapgit_git_branch_list=>get_display_name( ms_settings_snapshot-tag ).
      ro_form_data->set(
        iv_key = const=>id-tag
        iv_val = lv_head ).

      ro_form_data->set(
        iv_key = const=>id-commit
        iv_val = ms_settings_snapshot-commit ).

      ro_form_data->set(
        iv_key = const=>id-pull_request
        iv_val = ms_settings_snapshot-pull_request ).
    ENDIF.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo               = mo_repo
      iv_show_commit        = abap_false
      iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.


  METHOD save_settings.

    DATA:
      lo_repo_online  TYPE REF TO zcl_abapgit_repo_online,
      ls_settings_new TYPE types=>ty_remote_settings.

    ls_settings_new = get_remote_settings_from_form( mo_form_data ).

    " Switch online / offline
    IF ls_settings_new-offline <> ms_settings_snapshot-offline.
      " Remember key, switch, retrieve new instance (todo, refactor #2244)
      mo_repo->switch_repo_type( ls_settings_new-offline ).
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).
    ENDIF.

    IF mo_repo->is_offline( ) = abap_false.
      " Online: Save url
      lo_repo_online ?= mo_repo.
      lo_repo_online->set_url( ls_settings_new-url ).
    ENDIF.

    CASE ls_settings_new-head_type.
      WHEN const=>head_types-branch.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( ls_settings_new-branch ).
      WHEN const=>head_types-tag.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( ls_settings_new-tag ).
      WHEN const=>head_types-commit.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_commit = ls_settings_new-commit ).
      WHEN const=>head_types-pull_request.
        switch_to_commit( iv_revert = abap_true ).
        switch_to_pull_req( iv_pull = ls_settings_new-pull_request ).
    ENDCASE.

    IF mo_repo->is_offline( ) = abap_false AND ls_settings_new-head_type <> const=>head_types-pull_request.
      " Switching from PR to something else will reset the URL in repo->switch_origin( space )
      " -> set URL again
      lo_repo_online->set_url( ls_settings_new-url ).
    ENDIF.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mv_refresh_on_back = abap_true.
    ms_settings_snapshot = get_remote_settings_from_repo( mo_repo ).

  ENDMETHOD.


  METHOD switch_online_offline.

    DATA: lv_offline_new TYPE abap_bool,
          lv_url         TYPE types=>ty_remote_settings-url,
          lv_branch      TYPE types=>ty_remote_settings-branch.

    lv_offline_new = boolc( mo_form_data->get( const=>id-offline ) = abap_false ).
    mo_form_data->set(
      iv_key = const=>id-offline
      iv_val = lv_offline_new ).

    IF lv_offline_new = abap_true.
      lv_url = mo_form_data->get( const=>id-url ).
      mv_offline_switch_saved_url = lv_url.
      mo_form_data->set(
        iv_key = const=>id-url
        iv_val = '' ).
      mo_form_data->set(
        iv_key = const=>id-repo_type
        iv_val = const=>repo_type-offline ).
    ELSE.
      mo_form_data->set(
        iv_key = const=>id-repo_type
        iv_val = const=>repo_type-online ).
      IF mv_offline_switch_saved_url IS NOT INITIAL.
        mo_form_data->set(
          iv_key = const=>id-url
          iv_val = mv_offline_switch_saved_url ).
      ENDIF.

      lv_url = mo_form_data->get( const=>id-url ).
      IF mo_form_data->get( const=>id-head_type ) IS INITIAL.
        TRY.
            mo_form_data->set(
              iv_key = const=>id-head_type
              iv_val = const=>head_types-branch ).

            IF lv_url CP 'http*'.
              lv_branch = zcl_abapgit_git_factory=>get_git_transport( )->branches( lv_url )->get_head_symref( ).
              mo_form_data->set(
                iv_key = const=>id-branch
                iv_val = lv_branch ).
            ENDIF.
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD switch_to_branch_tag.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo_online.

    check_protection( ).
    lo_repo ?= mo_repo.
    lo_repo->select_branch( iv_name ).

  ENDMETHOD.


  METHOD switch_to_commit.

    DATA lo_repo TYPE REF TO zcl_abapgit_repo_online.

    check_protection( ).

    lo_repo ?= mo_repo.

    IF iv_revert = abap_true.
      lo_repo->select_commit( '' ).
    ELSE.
      lo_repo->select_commit( iv_commit ).
    ENDIF.

  ENDMETHOD.


  METHOD switch_to_pull_req.

    DATA:
      lo_repo   TYPE REF TO zcl_abapgit_repo_online,
      lv_url    TYPE types=>ty_remote_settings-url,
      lv_branch TYPE types=>ty_remote_settings-branch.

    check_protection( ).

    lo_repo ?= mo_repo.

    " Switching twice does not work so reset to original repo first
    lo_repo->switch_origin( '' ).

    IF iv_revert = abap_false.
      SPLIT iv_pull AT '@' INTO lv_url lv_branch.
      lo_repo->switch_origin(
        iv_url    = lv_url
        iv_branch = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && lv_branch ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_form.

    DATA: lo_form_validator TYPE REF TO lcl_form_validator.

    CREATE OBJECT lo_form_validator
      EXPORTING
        io_form      = mo_form
        io_form_data = io_form_data.

    ro_validation_log = lo_form_validator->validate( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_url    TYPE types=>ty_remote_settings-url,
      lv_commit TYPE types=>ty_remote_settings-commit.

    mo_form_data->merge( zcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        IF mv_refresh_on_back = abap_true.
          " Note this doesn't trigger if the tab is switched first
          mo_repo->refresh( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = initialize_form_data( ) ).

      WHEN const=>event-choose_url.
        lv_url = choose_url( ).

        IF lv_url IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = const=>id-url
            iv_val = lv_url ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN const=>event-change_head_type.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        mo_validation_log->clear( ).

      WHEN const=>event-choose_branch.
        choose_branch( ). " Unformly handle state below

      WHEN const=>event-choose_tag.
        choose_tag( ). " Unformly handle state below

      WHEN const=>event-choose_pull_request.
        choose_pr( ). " Unformly handle state below

      WHEN const=>event-choose_commit.
        lv_commit = choose_commit( ).

        IF lv_commit IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = const=>id-commit
            iv_val = lv_commit ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN const=>event-switch.
        switch_online_offline( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN const=>event-save.
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

    IF mo_popup_picklist IS BOUND. " Uniform popup state handling
      " This should happen only for a new popup because
      " on the first re-render main component event handling is blocked
      " and not called again until the popup distruction
      IF mo_popup_picklist->is_in_page( ) = abap_true.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      ELSE.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
        rs_handled-page  = zcl_abapgit_gui_page_hoc=>create(
          ii_child_component = mo_popup_picklist
          iv_show_as_modal   = abap_true ).
      ENDIF.
    ENDIF.

    " If staying on form, initialize it with current settings
    IF rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render AND mo_popup_picklist IS NOT BOUND.
      " Switching tabs must change the form layout
      mo_form = get_form_schema( mo_form_data ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions,
          lv_head_type     TYPE types=>ty_head_type,
          lv_offline       TYPE abap_bool.

    IF mo_form_data IS BOUND AND mo_form_data->is_empty( ) = abap_false.
      lv_offline = mo_form_data->get( const=>id-offline ).
      IF lv_offline = abap_false.
        lv_head_type = mo_form_data->get( const=>id-head_type ).
      ENDIF.
    ELSE.
      lv_offline = ms_settings_snapshot-offline.
      IF lv_offline = abap_false.
        lv_head_type = ms_settings_snapshot-head_type.
      ENDIF.
    ENDIF.

    ls_hotkey_action-ui_component = 'Remote'.

    ls_hotkey_action-description = 'Choose URL'.
    ls_hotkey_action-action      = const=>event-choose_url.
    ls_hotkey_action-hotkey      = 'u'.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    IF lv_head_type = const=>head_types-branch OR
       lv_head_type = const=>head_types-commit.
      ls_hotkey_action-description = 'Choose Branch'.
      ls_hotkey_action-action      = const=>event-choose_branch.
      ls_hotkey_action-hotkey      = 'b'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = const=>head_types-tag.
      ls_hotkey_action-description = 'Choose Tag'.
      ls_hotkey_action-action      = const=>event-choose_tag.
      ls_hotkey_action-hotkey      = 't'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = const=>head_types-commit.
      ls_hotkey_action-description = 'Choose Commit'.
      ls_hotkey_action-action      = const=>event-choose_commit.
      ls_hotkey_action-hotkey      = 'c'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = const=>head_types-pull_request.
      ls_hotkey_action-description = 'Choose Pull Request'.
      ls_hotkey_action-action      = const=>event-choose_pull_request.
      ls_hotkey_action-hotkey      = 'p'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_offline = abap_true.
      ls_hotkey_action-description = 'Switch to Online'.
      ls_hotkey_action-action      = const=>event-switch.
      ls_hotkey_action-hotkey      = 'o'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ELSE.
      ls_hotkey_action-description = 'Switch to Offline'.
      ls_hotkey_action-action      = const=>event-switch.
      ls_hotkey_action-hotkey      = 'o'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    handle_picklist_state( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->wrap(
      iv_tag     = 'div'
      iv_class   = 'repo' " It's OK because it's repo settings ... for now
      ii_content = render_content( ) ).

    IF mo_popup_picklist IS NOT BOUND OR mo_popup_picklist->is_in_page( ) = abap_false.
      register_handlers( ).
    ELSEIF mo_popup_picklist->is_in_page( ) = abap_true.
      " Block usual page events if the popup is an in-page popup
      ri_html->add( zcl_abapgit_gui_in_page_modal=>create( mo_popup_picklist ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
