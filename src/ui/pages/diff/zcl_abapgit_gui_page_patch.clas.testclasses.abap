CLASS ltcl_get_patch_data DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      get_patch_data_add FOR TESTING RAISING cx_static_check,
      get_patch_data_remove FOR TESTING RAISING cx_static_check,
      multi_digit_section FOR TESTING RAISING cx_static_check,
      filename_with_no_underscores FOR TESTING RAISING cx_static_check,
      invalid_patch_missing_file FOR TESTING RAISING cx_static_check,
      invalid_patch_missing_index FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_is_patch_line_possible DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mv_is_patch_line_possible TYPE abap_bool,
      ms_diff_line              TYPE zif_abapgit_definitions=>ty_diff.

    METHODS:
      initial_diff_line FOR TESTING RAISING cx_static_check,
      for_update_patch_shd_be_possbl FOR TESTING RAISING cx_static_check,
      for_insert_patch_shd_be_possbl FOR TESTING RAISING cx_static_check,
      for_delete_patch_shd_be_possbl FOR TESTING RAISING cx_static_check,

      given_diff_line
        IMPORTING
          is_diff_line TYPE zif_abapgit_definitions=>ty_diff OPTIONAL,

      when_is_patch_line_possible,

      then_patch_shd_be_possible,
      then_patch_shd_not_be_possible.

ENDCLASS.


CLASS ltcl_are_all_lines_patched DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      all_lines_patched FOR TESTING RAISING cx_static_check,
      no_lines_patched FOR TESTING RAISING cx_static_check,
      some_lines_patched FOR TESTING RAISING cx_static_check,
      empty_diff FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_render_patch_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      patch_not_possible FOR TESTING RAISING cx_static_check,
      patch_possible_not_patched FOR TESTING RAISING cx_static_check,
      patch_possible_patched FOR TESTING RAISING cx_static_check,
      patch_id_format FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltd_html_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_html.
    DATA mv_checkbox_called  TYPE abap_bool.
    DATA mv_checkbox_id      TYPE string.
    DATA mv_checkbox_checked TYPE abap_bool.
    DATA mv_add_a_called     TYPE abap_bool.
ENDCLASS.

CLASS ltd_html_double IMPLEMENTATION.
  METHOD zif_abapgit_html~add.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~add_checkbox.
    mv_checkbox_called  = abap_true.
    mv_checkbox_id      = iv_id.
    mv_checkbox_checked = iv_checked.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~set_title.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~render.
  ENDMETHOD.
  METHOD zif_abapgit_html~is_empty.
  ENDMETHOD.
  METHOD zif_abapgit_html~add_a.
    mv_add_a_called = abap_true.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~a.
  ENDMETHOD.
  METHOD zif_abapgit_html~icon.
  ENDMETHOD.
  METHOD zif_abapgit_html~add_icon.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~wrap.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~td.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~th.
    ri_self = me.
  ENDMETHOD.
  METHOD zif_abapgit_html~div.
    ri_self = me.
  ENDMETHOD.
ENDCLASS.


CLASS ltd_diff_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_diff.
    DATA mt_diff                   TYPE zif_abapgit_definitions=>ty_diffs_tt.
    DATA mv_set_patch_new_called   TYPE abap_bool.
    DATA mv_set_patch_new_line     TYPE i.
    DATA mv_set_patch_new_flag     TYPE abap_bool.
    DATA mv_set_patch_old_called   TYPE abap_bool.
    DATA mv_set_patch_old_line     TYPE i.
    DATA mv_set_patch_old_flag     TYPE abap_bool.
    DATA mv_set_patch_by_old_called TYPE abap_bool.
    DATA ms_set_patch_by_old_diff   TYPE zif_abapgit_definitions=>ty_diff.
    DATA mv_set_patch_by_old_flag   TYPE abap_bool.
    DATA mv_is_line_patched         TYPE abap_bool.
ENDCLASS.

CLASS ltd_diff_double IMPLEMENTATION.
  METHOD zif_abapgit_diff~get.
    rt_diff = mt_diff.
  ENDMETHOD.
  METHOD zif_abapgit_diff~create.
  ENDMETHOD.
  METHOD zif_abapgit_diff~stats.
  ENDMETHOD.
  METHOD zif_abapgit_diff~set_patch_new.
    mv_set_patch_new_called = abap_true.
    mv_set_patch_new_line   = iv_line_new.
    mv_set_patch_new_flag   = iv_patch_flag.
  ENDMETHOD.
  METHOD zif_abapgit_diff~set_patch_old.
    mv_set_patch_old_called = abap_true.
    mv_set_patch_old_line   = iv_line_old.
    mv_set_patch_old_flag   = iv_patch_flag.
  ENDMETHOD.
  METHOD zif_abapgit_diff~get_beacons.
  ENDMETHOD.
  METHOD zif_abapgit_diff~is_line_patched.
    rv_patched = mv_is_line_patched.
  ENDMETHOD.
  METHOD zif_abapgit_diff~set_patch_by_old_diff.
    mv_set_patch_by_old_called = abap_true.
    ms_set_patch_by_old_diff   = is_diff_old.
    mv_set_patch_by_old_flag   = iv_patch_flag.
  ENDMETHOD.
ENDCLASS.


CLASS ltd_stage_double DEFINITION FOR TESTING INHERITING FROM zcl_abapgit_stage.
  PUBLIC SECTION.
    DATA mv_add_called TYPE abap_bool.
    DATA mv_rm_called  TYPE abap_bool.
    DATA mv_add_lstate TYPE zif_abapgit_git_definitions=>ty_item_state.
    DATA mv_rm_lstate  TYPE zif_abapgit_git_definitions=>ty_item_state.
    METHODS add REDEFINITION.
    METHODS rm  REDEFINITION.
ENDCLASS.

CLASS ltd_stage_double IMPLEMENTATION.
  METHOD add.
    mv_add_called = abap_true.
    IF is_status IS SUPPLIED.
      mv_add_lstate = is_status-lstate.
    ENDIF.
  ENDMETHOD.
  METHOD rm.
    mv_rm_called = abap_true.
    IF is_status IS SUPPLIED.
      mv_rm_lstate = is_status-lstate.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_get_diff_line DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      valid_index FOR TESTING RAISING cx_static_check,
      invalid_index FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_apply_patch_to_diff DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      update_sets_patch_new FOR TESTING RAISING cx_static_check,
      insert_sets_patch_new FOR TESTING RAISING cx_static_check,
      delete_sets_patch_old FOR TESTING RAISING cx_static_check,
      unchanged_sets_nothing FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_restore_patch_flags DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      patched_line_is_restored FOR TESTING RAISING cx_static_check,
      unpatched_line_not_restored FOR TESTING RAISING cx_static_check,
      file_not_in_old_skipped FOR TESTING RAISING cx_static_check,
      unbound_old_diff_skipped FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_get_diff_object DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      file_found FOR TESTING RAISING cx_static_check,
      file_not_found FOR TESTING RAISING cx_static_check,
      correct_file_among_multiple FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_apply_patch_all_impl DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      single_entry_add FOR TESTING RAISING cx_static_check,
      single_entry_remove FOR TESTING RAISING cx_static_check,
      multiple_entries FOR TESTING RAISING cx_static_check,
      empty_patch_skipped FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_get_staging_lstate DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      deleted_when_d_and_all_patched   FOR TESTING RAISING cx_static_check,
      added_when_a_and_all_patched     FOR TESTING RAISING cx_static_check,
      modified_when_d_partial_patch    FOR TESTING RAISING cx_static_check,
      modified_when_a_partial_patch    FOR TESTING RAISING cx_static_check,
      modified_when_m_regardless       FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_render_patch_head DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      checkbox_has_patch_file_prefix FOR TESTING RAISING cx_static_check,
      checkbox_uses_normalized_path  FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_render_diff_head DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      empty_obj_skips_link    FOR TESTING RAISING cx_static_check,
      obj_with_data_adds_link FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_add_to_stage DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      nothing_patched_raises       FOR TESTING RAISING cx_static_check,
      unbound_diff_is_skipped      FOR TESTING RAISING cx_static_check,
      no_patch_flags_raises        FOR TESTING RAISING cx_static_check,
      deleted_all_patched_calls_rm FOR TESTING RAISING cx_static_check,
      added_all_patched_calls_add  FOR TESTING RAISING cx_static_check,
      modified_patched_calls_add   FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_render_patch DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      empty_filename_no_diff_lookup  FOR TESTING RAISING cx_static_check,
      patched_line_checkbox_checked  FOR TESTING RAISING cx_static_check,
      unpatched_line_not_checked     FOR TESTING RAISING cx_static_check,
      id_format_with_section_count   FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_get_hotkey_actions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      returns_three_hotkeys  FOR TESTING RAISING cx_static_check,
      stage_hotkey_is_s      FOR TESTING RAISING cx_static_check,
      refresh_local_hotkey_r FOR TESTING RAISING cx_static_check,
      refresh_all_hotkey_a   FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS zcl_abapgit_gui_page_patch DEFINITION LOCAL FRIENDS ltcl_get_patch_data
                                                          ltcl_is_patch_line_possible
                                                          ltcl_are_all_lines_patched
                                                          ltcl_render_patch_cell
                                                          ltcl_get_diff_line
                                                          ltcl_apply_patch_to_diff
                                                          ltcl_restore_patch_flags
                                                          ltcl_get_diff_object
                                                          ltcl_apply_patch_all_impl
                                                          ltcl_render_patch_head
                                                          ltcl_render_diff_head
                                                          ltcl_get_staging_lstate
                                                          ltcl_add_to_stage
                                                          ltcl_render_patch
                                                          ltcl_get_hotkey_actions.

CLASS ltcl_render_patch_cell IMPLEMENTATION.

  METHOD patch_not_possible.

    DATA lo_html TYPE REF TO ltd_html_double.
    CREATE OBJECT lo_html.

    zcl_abapgit_gui_page_patch=>render_patch_cell(
      ii_html        = lo_html
      iv_id          = |some_0_5|
      iv_is_possible = abap_false
      iv_patched     = abap_false ).

    cl_abap_unit_assert=>assert_false(
      act = lo_html->mv_checkbox_called
      msg = |No checkbox should be rendered when patch is not possible| ).

  ENDMETHOD.

  METHOD patch_possible_not_patched.

    DATA lo_html TYPE REF TO ltd_html_double.
    CREATE OBJECT lo_html.

    zcl_abapgit_gui_page_patch=>render_patch_cell(
      ii_html        = lo_html
      iv_id          = |myfile.abap_0_5|
      iv_is_possible = abap_true
      iv_patched     = abap_false ).

    cl_abap_unit_assert=>assert_true(
      act = lo_html->mv_checkbox_called
      msg = |Checkbox should be rendered when patch is possible| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_html->mv_checkbox_checked
      msg = |Checkbox should not be checked when not patched| ).

  ENDMETHOD.

  METHOD patch_possible_patched.

    DATA lo_html TYPE REF TO ltd_html_double.
    CREATE OBJECT lo_html.

    zcl_abapgit_gui_page_patch=>render_patch_cell(
      ii_html        = lo_html
      iv_id          = |myfile.abap_0_5|
      iv_is_possible = abap_true
      iv_patched     = abap_true ).

    cl_abap_unit_assert=>assert_true(
      act = lo_html->mv_checkbox_checked
      msg = |Checkbox should be checked when line is patched| ).

  ENDMETHOD.

  METHOD patch_id_format.

    DATA lo_html TYPE REF TO ltd_html_double.
    CREATE OBJECT lo_html.

    zcl_abapgit_gui_page_patch=>render_patch_cell(
      ii_html        = lo_html
      iv_id          = |myfile.abap_0_5|
      iv_is_possible = abap_true
      iv_patched     = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      exp = |patch_line_myfile.abap_0_5|
      act = lo_html->mv_checkbox_id
      msg = |Checkbox ID should have patch_line_ prefix| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_get_patch_data IMPLEMENTATION.

  METHOD get_patch_data_add.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string.

    zcl_abapgit_gui_page_patch=>get_patch_data(
      EXPORTING
        iv_patch      = |patch_line_zcl_test_git_add_p.clas.abap_0_19|
      IMPORTING
        ev_filename   = lv_file_name
        ev_line_index = lv_line_index ).

    cl_abap_unit_assert=>assert_equals(
      exp = |zcl_test_git_add_p.clas.abap|
      act = lv_file_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = |19|
      act = lv_line_index ).

  ENDMETHOD.

  METHOD get_patch_data_remove.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string.

    zcl_abapgit_gui_page_patch=>get_patch_data(
      EXPORTING
        iv_patch      = |patch_line_ztest_patch.prog.abap_0_39|
      IMPORTING
        ev_filename   = lv_file_name
        ev_line_index = lv_line_index ).

    cl_abap_unit_assert=>assert_equals(
      exp = |ztest_patch.prog.abap|
      act = lv_file_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = |39|
      act = lv_line_index ).

  ENDMETHOD.

  METHOD multi_digit_section.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string.

    zcl_abapgit_gui_page_patch=>get_patch_data(
      EXPORTING
        iv_patch      = |patch_line_zcl_some_class.clas.abap_12_99|
      IMPORTING
        ev_filename   = lv_file_name
        ev_line_index = lv_line_index ).

    cl_abap_unit_assert=>assert_equals(
      exp = |zcl_some_class.clas.abap|
      act = lv_file_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = |99|
      act = lv_line_index ).

  ENDMETHOD.

  METHOD filename_with_no_underscores.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string.

    zcl_abapgit_gui_page_patch=>get_patch_data(
      EXPORTING
        iv_patch      = |patch_line_zreport.prog.abap_0_5|
      IMPORTING
        ev_filename   = lv_file_name
        ev_line_index = lv_line_index ).

    cl_abap_unit_assert=>assert_equals(
      exp = |zreport.prog.abap|
      act = lv_file_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = |5|
      act = lv_line_index ).

  ENDMETHOD.

  METHOD invalid_patch_missing_file.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string,
          lx_error      TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_gui_page_patch=>get_patch_data(
          EXPORTING
            iv_patch      = |patch_39|
          IMPORTING
            ev_filename   = lv_file_name
            ev_line_index = lv_line_index ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Invalid patch|
          act = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD invalid_patch_missing_index.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string,
          lx_error      TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_gui_page_patch=>get_patch_data(
          EXPORTING
            iv_patch      = |patch_ztest_patch.prog.abap|
          IMPORTING
            ev_filename   = lv_file_name
            ev_line_index = lv_line_index ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Invalid patch|
          act = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.



CLASS ltcl_is_patch_line_possible IMPLEMENTATION.

  METHOD initial_diff_line.

    given_diff_line( ).
    when_is_patch_line_possible( ).
    then_patch_shd_not_be_possible( ).

  ENDMETHOD.


  METHOD for_update_patch_shd_be_possbl.

    DATA: ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-update.

    given_diff_line( ls_diff_line ).
    when_is_patch_line_possible( ).
    then_patch_shd_be_possible( ).

  ENDMETHOD.


  METHOD for_insert_patch_shd_be_possbl.

    DATA: ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-insert.

    given_diff_line( ls_diff_line ).
    when_is_patch_line_possible( ).
    then_patch_shd_be_possible( ).

  ENDMETHOD.


  METHOD for_delete_patch_shd_be_possbl.

    DATA: ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-delete.

    given_diff_line( ls_diff_line ).
    when_is_patch_line_possible( ).
    then_patch_shd_be_possible( ).

  ENDMETHOD.


  METHOD when_is_patch_line_possible.

    mv_is_patch_line_possible = zcl_abapgit_gui_page_patch=>is_patch_line_possible( ms_diff_line ).

  ENDMETHOD.


  METHOD then_patch_shd_be_possible.

    cl_abap_unit_assert=>assert_not_initial(
        act = mv_is_patch_line_possible
        msg = |Patch should be possible| ).

  ENDMETHOD.


  METHOD then_patch_shd_not_be_possible.

    cl_abap_unit_assert=>assert_initial(
        act = mv_is_patch_line_possible
        msg = |Patch should not be possible| ).

  ENDMETHOD.


  METHOD given_diff_line.

    ms_diff_line = is_diff_line.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_are_all_lines_patched IMPLEMENTATION.

  METHOD all_lines_patched.

    DATA: lt_diff  TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff  TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE abap_bool.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.
    INSERT ls_diff INTO TABLE lt_diff.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>are_all_lines_patched( lt_diff ).

    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = |All patched lines should return true| ).

  ENDMETHOD.

  METHOD no_lines_patched.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE abap_bool.

    ls_diff-patch_flag = abap_false.
    INSERT ls_diff INTO TABLE lt_diff.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>are_all_lines_patched( lt_diff ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = |No patched lines should return false| ).

  ENDMETHOD.

  METHOD some_lines_patched.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE abap_bool.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.
    ls_diff-patch_flag = abap_false.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>are_all_lines_patched( lt_diff ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = |Partially patched lines should return false| ).

  ENDMETHOD.

  METHOD empty_diff.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          lv_result TYPE abap_bool.

    lv_result = zcl_abapgit_gui_page_patch=>are_all_lines_patched( lt_diff ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = |Empty diff should return false| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_apply_patch_to_diff IMPLEMENTATION.

  METHOD update_sets_patch_new.

    DATA: lo_diff      TYPE REF TO ltd_diff_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    ls_diff_line-result  = zif_abapgit_definitions=>c_diff-update.
    ls_diff_line-new_num = 42.

    zcl_abapgit_gui_page_patch=>apply_patch_to_diff(
      io_diff       = lo_diff
      is_diff_line  = ls_diff_line
      iv_patch_flag = abap_true ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_new_called
      msg = |set_patch_new should be called for update| ).

    cl_abap_unit_assert=>assert_equals(
      exp = 42
      act = lo_diff->mv_set_patch_new_line
      msg = |new_num should be passed to set_patch_new| ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_new_flag
      msg = |patch_flag should be passed through| ).

  ENDMETHOD.

  METHOD insert_sets_patch_new.

    DATA: lo_diff      TYPE REF TO ltd_diff_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    ls_diff_line-result  = zif_abapgit_definitions=>c_diff-insert.
    ls_diff_line-new_num = 7.

    zcl_abapgit_gui_page_patch=>apply_patch_to_diff(
      io_diff       = lo_diff
      is_diff_line  = ls_diff_line
      iv_patch_flag = abap_true ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_new_called
      msg = |set_patch_new should be called for insert| ).

    cl_abap_unit_assert=>assert_equals(
      exp = 7
      act = lo_diff->mv_set_patch_new_line
      msg = |new_num should be passed to set_patch_new| ).

  ENDMETHOD.

  METHOD delete_sets_patch_old.

    DATA: lo_diff      TYPE REF TO ltd_diff_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    ls_diff_line-result  = zif_abapgit_definitions=>c_diff-delete.
    ls_diff_line-old_num = 15.

    zcl_abapgit_gui_page_patch=>apply_patch_to_diff(
      io_diff       = lo_diff
      is_diff_line  = ls_diff_line
      iv_patch_flag = abap_false ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_old_called
      msg = |set_patch_old should be called for delete| ).

    cl_abap_unit_assert=>assert_equals(
      exp = 15
      act = lo_diff->mv_set_patch_old_line
      msg = |old_num should be passed to set_patch_old| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff->mv_set_patch_old_flag
      msg = |patch_flag false should be passed through| ).

  ENDMETHOD.

  METHOD unchanged_sets_nothing.

    DATA: lo_diff      TYPE REF TO ltd_diff_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    ls_diff_line-result = zif_abapgit_definitions=>c_diff-unchanged.

    zcl_abapgit_gui_page_patch=>apply_patch_to_diff(
      io_diff       = lo_diff
      is_diff_line  = ls_diff_line
      iv_patch_flag = abap_true ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff->mv_set_patch_new_called
      msg = |set_patch_new should not be called for equal lines| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff->mv_set_patch_old_called
      msg = |set_patch_old should not be called for equal lines| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_restore_patch_flags IMPLEMENTATION.

  METHOD patched_line_is_restored.

    DATA: lt_new          TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lt_old          TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff    TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff_new     TYPE REF TO ltd_diff_double,
          lo_diff_old     TYPE REF TO ltd_diff_double,
          ls_diff_old     TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff_new.
    CREATE OBJECT lo_diff_old.

    ls_diff_old-result     = zif_abapgit_definitions=>c_diff-update.
    ls_diff_old-new_num    = 5.
    ls_diff_old-patch_flag = abap_true.
    INSERT ls_diff_old INTO TABLE lo_diff_old->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff_old.
    INSERT ls_file_diff INTO TABLE lt_old.

    ls_file_diff-o_diff = lo_diff_new.
    INSERT ls_file_diff INTO TABLE lt_new.

    zcl_abapgit_gui_page_patch=>restore_patch_flags_impl(
      it_diff_files     = lt_new
      it_diff_files_old = lt_old ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff_new->mv_set_patch_by_old_called
      msg = |set_patch_by_old_diff should be called for patched lines| ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff_new->mv_set_patch_by_old_flag
      msg = |patch_flag true should be passed through| ).

  ENDMETHOD.

  METHOD unpatched_line_not_restored.

    DATA: lt_new       TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lt_old       TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff_new  TYPE REF TO ltd_diff_double,
          lo_diff_old  TYPE REF TO ltd_diff_double,
          ls_diff_old  TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff_new.
    CREATE OBJECT lo_diff_old.

    ls_diff_old-result     = zif_abapgit_definitions=>c_diff-update.
    ls_diff_old-patch_flag = abap_false.
    INSERT ls_diff_old INTO TABLE lo_diff_old->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff_old.
    INSERT ls_file_diff INTO TABLE lt_old.

    ls_file_diff-o_diff = lo_diff_new.
    INSERT ls_file_diff INTO TABLE lt_new.

    zcl_abapgit_gui_page_patch=>restore_patch_flags_impl(
      it_diff_files     = lt_new
      it_diff_files_old = lt_old ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff_new->mv_set_patch_by_old_called
      msg = |set_patch_by_old_diff should not be called for unpatched lines| ).

  ENDMETHOD.

  METHOD file_not_in_old_skipped.

    DATA: lt_new       TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lt_old       TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff_new  TYPE REF TO ltd_diff_double,
          lo_diff_old  TYPE REF TO ltd_diff_double,
          ls_diff_old  TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff_new.
    CREATE OBJECT lo_diff_old.

    ls_diff_old-patch_flag = abap_true.
    INSERT ls_diff_old INTO TABLE lo_diff_old->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'zother.prog.abap'.
    ls_file_diff-o_diff   = lo_diff_old.
    INSERT ls_file_diff INTO TABLE lt_old.

    ls_file_diff-filename = 'znew.prog.abap'.
    ls_file_diff-o_diff   = lo_diff_new.
    INSERT ls_file_diff INTO TABLE lt_new.

    zcl_abapgit_gui_page_patch=>restore_patch_flags_impl(
      it_diff_files     = lt_new
      it_diff_files_old = lt_old ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff_new->mv_set_patch_by_old_called
      msg = |set_patch_by_old_diff should not be called for files not in old list| ).

  ENDMETHOD.

  METHOD unbound_old_diff_skipped.

    DATA: lt_new       TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lt_old       TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff_new  TYPE REF TO ltd_diff_double.

    CREATE OBJECT lo_diff_new.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    " o_diff intentionally left unbound (binary file simulation)
    INSERT ls_file_diff INTO TABLE lt_old.

    ls_file_diff-o_diff = lo_diff_new.
    INSERT ls_file_diff INTO TABLE lt_new.

    zcl_abapgit_gui_page_patch=>restore_patch_flags_impl(
      it_diff_files     = lt_new
      it_diff_files_old = lt_old ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff_new->mv_set_patch_by_old_called
      msg = |set_patch_by_old_diff should not be called when old diff is unbound| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_get_diff_object IMPLEMENTATION.

  METHOD file_found.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          lo_result    TYPE REF TO zif_abapgit_diff.

    CREATE OBJECT lo_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.clas.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    lo_result = zcl_abapgit_gui_page_patch=>get_diff_object_impl(
      it_diff_files = lt_files
      iv_filename   = '_src__ztest_clas_abap' ).

    cl_abap_unit_assert=>assert_bound(
      act = lo_result
      msg = |Diff object should be returned for a known filename| ).

    cl_abap_unit_assert=>assert_equals(
      exp = lo_diff
      act = lo_result
      msg = |Correct diff object should be returned| ).

  ENDMETHOD.

  METHOD file_not_found.

    DATA: lt_files TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lx_error TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_gui_page_patch=>get_diff_object_impl(
          it_diff_files = lt_files
          iv_filename   = 'unknown_file' ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Invalid filename unknown_file|
          act = lx_error->get_text( )
          msg = |Exception message should contain the invalid filename| ).
    ENDTRY.

  ENDMETHOD.

  METHOD correct_file_among_multiple.

    DATA: lt_files      TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff  TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff_a     TYPE REF TO ltd_diff_double,
          lo_diff_b     TYPE REF TO ltd_diff_double,
          lo_result     TYPE REF TO zif_abapgit_diff.

    CREATE OBJECT lo_diff_a.
    CREATE OBJECT lo_diff_b.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'zclass_a.clas.abap'.
    ls_file_diff-o_diff   = lo_diff_a.
    INSERT ls_file_diff INTO TABLE lt_files.

    ls_file_diff-filename = 'zclass_b.clas.abap'.
    ls_file_diff-o_diff   = lo_diff_b.
    INSERT ls_file_diff INTO TABLE lt_files.

    lo_result = zcl_abapgit_gui_page_patch=>get_diff_object_impl(
      it_diff_files = lt_files
      iv_filename   = '_src__zclass_b_clas_abap' ).

    cl_abap_unit_assert=>assert_equals(
      exp = lo_diff_b
      act = lo_result
      msg = |Should return diff_b, not diff_a| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_get_diff_line IMPLEMENTATION.

  METHOD valid_index.

    DATA: lo_diff   TYPE REF TO ltd_diff_double,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          ls_result TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.

    ls_diff-result = zif_abapgit_definitions=>c_diff-insert.
    ls_diff-new    = |line one|.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_diff-result = zif_abapgit_definitions=>c_diff-delete.
    ls_diff-new    = |line two|.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_result = zcl_abapgit_gui_page_patch=>get_diff_line(
      io_diff       = lo_diff
      iv_line_index = |2| ).

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_definitions=>c_diff-delete
      act = ls_result-result
      msg = |Should return the diff line at the given index| ).

  ENDMETHOD.

  METHOD invalid_index.

    DATA: lo_diff  TYPE REF TO ltd_diff_double,
          ls_diff  TYPE zif_abapgit_definitions=>ty_diff,
          lx_error TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT lo_diff.

    ls_diff-result = zif_abapgit_definitions=>c_diff-insert.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    TRY.
        zcl_abapgit_gui_page_patch=>get_diff_line(
          io_diff       = lo_diff
          iv_line_index = |99| ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Invalid line index 99|
          act = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_apply_patch_all_impl IMPLEMENTATION.

  METHOD single_entry_add.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.

    ls_diff-result  = zif_abapgit_definitions=>c_diff-update.
    ls_diff-new_num = 10.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    zcl_abapgit_gui_page_patch=>apply_patch_all_impl(
      it_diff_files = lt_files
      iv_patch      = |patch_line__src__ztest_prog_abap_0_1|
      iv_patch_flag = abap_true ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_new_called
      msg = |set_patch_new should be called for update line| ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_new_flag
      msg = |patch_flag true should be passed| ).

  ENDMETHOD.

  METHOD single_entry_remove.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.

    ls_diff-result  = zif_abapgit_definitions=>c_diff-delete.
    ls_diff-old_num = 5.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    zcl_abapgit_gui_page_patch=>apply_patch_all_impl(
      it_diff_files = lt_files
      iv_patch      = |patch_line__src__ztest_prog_abap_0_1|
      iv_patch_flag = abap_false ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff->mv_set_patch_old_called
      msg = |set_patch_old should be called for delete line| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff->mv_set_patch_old_flag
      msg = |patch_flag false should be passed| ).

  ENDMETHOD.

  METHOD multiple_entries.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff_a    TYPE REF TO ltd_diff_double,
          lo_diff_b    TYPE REF TO ltd_diff_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff_a.
    CREATE OBJECT lo_diff_b.

    ls_diff-result  = zif_abapgit_definitions=>c_diff-update.
    ls_diff-new_num = 10.
    INSERT ls_diff INTO TABLE lo_diff_a->mt_diff.
    INSERT ls_diff INTO TABLE lo_diff_b->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'zclass_a.clas.abap'.
    ls_file_diff-o_diff   = lo_diff_a.
    INSERT ls_file_diff INTO TABLE lt_files.

    ls_file_diff-filename = 'zclass_b.clas.abap'.
    ls_file_diff-o_diff   = lo_diff_b.
    INSERT ls_file_diff INTO TABLE lt_files.

    zcl_abapgit_gui_page_patch=>apply_patch_all_impl(
      it_diff_files = lt_files
      iv_patch      = |patch_line__src__zclass_a_clas_abap_0_1,patch_line__src__zclass_b_clas_abap_0_1|
      iv_patch_flag = abap_true ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff_a->mv_set_patch_new_called
      msg = |set_patch_new should be called for zclass_a| ).

    cl_abap_unit_assert=>assert_true(
      act = lo_diff_b->mv_set_patch_new_called
      msg = |set_patch_new should be called for zclass_b| ).

  ENDMETHOD.

  METHOD empty_patch_skipped.

    DATA: lt_files TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lo_diff  TYPE REF TO ltd_diff_double.

    CREATE OBJECT lo_diff.

    zcl_abapgit_gui_page_patch=>apply_patch_all_impl(
      it_diff_files = lt_files
      iv_patch      = ||
      iv_patch_flag = abap_true ).

    cl_abap_unit_assert=>assert_false(
      act = lo_diff->mv_set_patch_new_called
      msg = |No calls should happen for empty patch| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_render_patch_head IMPLEMENTATION.

  METHOD checkbox_has_patch_file_prefix.

    DATA: lo_html TYPE REF TO ltd_html_double,
          ls_diff TYPE zif_abapgit_gui_diff=>ty_file_diff.

    CREATE OBJECT lo_html.
    ls_diff-path     = '/src/'.
    ls_diff-filename = 'ztest.prog.abap'.

    zcl_abapgit_gui_page_patch=>render_patch_head(
      ii_html = lo_html
      is_diff = ls_diff ).

    cl_abap_unit_assert=>assert_true(
      act = lo_html->mv_checkbox_called
      msg = |Checkbox should be rendered| ).

    cl_abap_unit_assert=>assert_true(
      act = boolc( lo_html->mv_checkbox_id CP 'patch_file_*' )
      msg = |Checkbox ID should start with patch_file_| ).

  ENDMETHOD.

  METHOD checkbox_uses_normalized_path.

    DATA: lo_html TYPE REF TO ltd_html_double,
          ls_diff TYPE zif_abapgit_gui_diff=>ty_file_diff.

    CREATE OBJECT lo_html.
    ls_diff-path     = '/src/'.
    ls_diff-filename = 'ztest.prog.abap'.

    zcl_abapgit_gui_page_patch=>render_patch_head(
      ii_html = lo_html
      is_diff = ls_diff ).

    cl_abap_unit_assert=>assert_equals(
      exp = |patch_file__src__ztest_prog_abap|
      act = lo_html->mv_checkbox_id
      msg = |Checkbox ID should use normalized path and filename| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_get_staging_lstate IMPLEMENTATION.

  METHOD deleted_when_d_and_all_patched.

    DATA: lt_diff  TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff  TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE zif_abapgit_git_definitions=>ty_item_state.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>get_staging_lstate(
      iv_lstate = zif_abapgit_definitions=>c_state-deleted
      it_diff   = lt_diff ).

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_definitions=>c_state-deleted
      act = lv_result
      msg = |Deleted file with all lines patched should return deleted state| ).

  ENDMETHOD.

  METHOD added_when_a_and_all_patched.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE zif_abapgit_git_definitions=>ty_item_state.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>get_staging_lstate(
      iv_lstate = zif_abapgit_definitions=>c_state-added
      it_diff   = lt_diff ).

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_definitions=>c_state-added
      act = lv_result
      msg = |Added file with all lines patched should return added state| ).

  ENDMETHOD.

  METHOD modified_when_d_partial_patch.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE zif_abapgit_git_definitions=>ty_item_state.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.
    ls_diff-patch_flag = abap_false.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>get_staging_lstate(
      iv_lstate = zif_abapgit_definitions=>c_state-deleted
      it_diff   = lt_diff ).

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_definitions=>c_state-modified
      act = lv_result
      msg = |Deleted file with partial patch should return modified state| ).

  ENDMETHOD.

  METHOD modified_when_a_partial_patch.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE zif_abapgit_git_definitions=>ty_item_state.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.
    ls_diff-patch_flag = abap_false.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>get_staging_lstate(
      iv_lstate = zif_abapgit_definitions=>c_state-added
      it_diff   = lt_diff ).

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_definitions=>c_state-modified
      act = lv_result
      msg = |Added file with partial patch should return modified state| ).

  ENDMETHOD.

  METHOD modified_when_m_regardless.

    DATA: lt_diff   TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ls_diff   TYPE zif_abapgit_definitions=>ty_diff,
          lv_result TYPE zif_abapgit_git_definitions=>ty_item_state.

    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lt_diff.

    lv_result = zcl_abapgit_gui_page_patch=>get_staging_lstate(
      iv_lstate = zif_abapgit_definitions=>c_state-modified
      it_diff   = lt_diff ).

    cl_abap_unit_assert=>assert_equals(
      exp = zif_abapgit_definitions=>c_state-modified
      act = lv_result
      msg = |Modified file should always return modified state| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_render_diff_head IMPLEMENTATION.

  METHOD empty_obj_skips_link.

    DATA: lo_html TYPE REF TO ltd_html_double,
          ls_diff TYPE zif_abapgit_gui_diff=>ty_file_diff.

    CREATE OBJECT lo_html.

    zcl_abapgit_gui_page_patch=>render_diff_head_impl(
      ii_html = lo_html
      is_diff = ls_diff ).

    cl_abap_unit_assert=>assert_false(
      act = lo_html->mv_add_a_called
      msg = |No link should be rendered when obj_type is empty| ).

  ENDMETHOD.

  METHOD obj_with_data_adds_link.

    DATA: lo_html TYPE REF TO ltd_html_double,
          ls_diff TYPE zif_abapgit_gui_diff=>ty_file_diff.

    CREATE OBJECT lo_html.
    ls_diff-obj_type = 'PROG'.
    ls_diff-obj_name = 'ZTEST'.

    zcl_abapgit_gui_page_patch=>render_diff_head_impl(
      ii_html = lo_html
      is_diff = ls_diff ).

    cl_abap_unit_assert=>assert_true(
      act = lo_html->mv_add_a_called
      msg = |Link should be rendered when obj_type and obj_name are set| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_add_to_stage IMPLEMENTATION.

  METHOD nothing_patched_raises.

    DATA: lt_files TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          lo_stage TYPE REF TO ltd_stage_double,
          lx_error TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT lo_stage.

    TRY.
        zcl_abapgit_gui_page_patch=>add_to_stage_impl(
          it_diff_files = lt_files
          io_stage      = lo_stage ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Nothing added|
          act = lx_error->get_text( )
          msg = |Empty diff files should raise Nothing added| ).
    ENDTRY.

  ENDMETHOD.

  METHOD unbound_diff_is_skipped.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_stage     TYPE REF TO ltd_stage_double,
          lx_error     TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT lo_stage.
    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    INSERT ls_file_diff INTO TABLE lt_files.

    TRY.
        zcl_abapgit_gui_page_patch=>add_to_stage_impl(
          it_diff_files = lt_files
          io_stage      = lo_stage ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Nothing added|
          act = lx_error->get_text( )
          msg = |Unbound diff should be skipped and raise Nothing added| ).
    ENDTRY.

  ENDMETHOD.

  METHOD no_patch_flags_raises.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          lo_stage     TYPE REF TO ltd_stage_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff,
          lx_error     TYPE REF TO zcx_abapgit_exception.

    CREATE OBJECT lo_diff.
    CREATE OBJECT lo_stage.

    ls_diff-result     = zif_abapgit_definitions=>c_diff-update.
    ls_diff-patch_flag = abap_false.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    TRY.
        zcl_abapgit_gui_page_patch=>add_to_stage_impl(
          it_diff_files = lt_files
          io_stage      = lo_stage ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Nothing added|
          act = lx_error->get_text( )
          msg = |No patch flags set should raise Nothing added| ).
    ENDTRY.

  ENDMETHOD.

  METHOD deleted_all_patched_calls_rm.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          lo_stage     TYPE REF TO ltd_stage_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    CREATE OBJECT lo_stage.

    ls_diff-result     = zif_abapgit_definitions=>c_diff-delete.
    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-lstate   = zif_abapgit_definitions=>c_state-deleted.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    zcl_abapgit_gui_page_patch=>add_to_stage_impl(
      it_diff_files = lt_files
      io_stage      = lo_stage ).

    cl_abap_unit_assert=>assert_true(
      act = lo_stage->mv_rm_called
      msg = |rm should be called for deleted file with all lines patched| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_stage->mv_add_called
      msg = |add should not be called for deleted file| ).

  ENDMETHOD.

  METHOD added_all_patched_calls_add.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          lo_stage     TYPE REF TO ltd_stage_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    CREATE OBJECT lo_stage.

    ls_diff-result     = zif_abapgit_definitions=>c_diff-insert.
    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-lstate   = zif_abapgit_definitions=>c_state-added.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    zcl_abapgit_gui_page_patch=>add_to_stage_impl(
      it_diff_files = lt_files
      io_stage      = lo_stage ).

    cl_abap_unit_assert=>assert_true(
      act = lo_stage->mv_add_called
      msg = |add should be called for added file with all lines patched| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_stage->mv_rm_called
      msg = |rm should not be called for added file| ).

  ENDMETHOD.

  METHOD modified_patched_calls_add.

    DATA: lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double,
          lo_stage     TYPE REF TO ltd_stage_double,
          ls_diff      TYPE zif_abapgit_definitions=>ty_diff.

    CREATE OBJECT lo_diff.
    CREATE OBJECT lo_stage.

    ls_diff-result     = zif_abapgit_definitions=>c_diff-update.
    ls_diff-patch_flag = abap_true.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.
    ls_diff-patch_flag = abap_false.
    INSERT ls_diff INTO TABLE lo_diff->mt_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-lstate   = zif_abapgit_definitions=>c_state-modified.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    zcl_abapgit_gui_page_patch=>add_to_stage_impl(
      it_diff_files = lt_files
      io_stage      = lo_stage ).

    cl_abap_unit_assert=>assert_true(
      act = lo_stage->mv_add_called
      msg = |add should be called for partially patched modified file| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_stage->mv_rm_called
      msg = |rm should not be called for modified file| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_render_patch IMPLEMENTATION.

  METHOD empty_filename_no_diff_lookup.

    DATA: lo_html      TYPE REF TO ltd_html_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff,
          lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs.

    CREATE OBJECT lo_html.
    ls_diff_line-result = zif_abapgit_definitions=>c_diff-update.

    zcl_abapgit_gui_page_patch=>render_patch_impl(
      ii_html          = lo_html
      iv_filename      = ||
      is_diff_line     = ls_diff_line
      iv_index         = 1
      iv_section_count = 0
      it_diff_files    = lt_files ).

    cl_abap_unit_assert=>assert_true(
      act = lo_html->mv_checkbox_called
      msg = |Checkbox should be rendered for empty filename| ).

    cl_abap_unit_assert=>assert_false(
      act = lo_html->mv_checkbox_checked
      msg = |Should not be checked when filename is empty| ).

  ENDMETHOD.

  METHOD patched_line_checkbox_checked.

    DATA: lo_html      TYPE REF TO ltd_html_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff,
          lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double.

    CREATE OBJECT lo_html.
    CREATE OBJECT lo_diff.
    lo_diff->mv_is_line_patched = abap_true.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-insert.

    zcl_abapgit_gui_page_patch=>render_patch_impl(
      ii_html          = lo_html
      iv_filename      = '_src__ztest_prog_abap'
      is_diff_line     = ls_diff_line
      iv_index         = 3
      iv_section_count = 1
      it_diff_files    = lt_files ).

    cl_abap_unit_assert=>assert_true(
      act = lo_html->mv_checkbox_checked
      msg = |Checkbox should be checked when line is patched| ).

  ENDMETHOD.

  METHOD unpatched_line_not_checked.

    DATA: lo_html      TYPE REF TO ltd_html_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff,
          lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double.

    CREATE OBJECT lo_html.
    CREATE OBJECT lo_diff.
    lo_diff->mv_is_line_patched = abap_false.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-update.

    zcl_abapgit_gui_page_patch=>render_patch_impl(
      ii_html          = lo_html
      iv_filename      = '_src__ztest_prog_abap'
      is_diff_line     = ls_diff_line
      iv_index         = 5
      iv_section_count = 2
      it_diff_files    = lt_files ).

    cl_abap_unit_assert=>assert_false(
      act = lo_html->mv_checkbox_checked
      msg = |Should not be checked when line is not patched| ).

  ENDMETHOD.

  METHOD id_format_with_section_count.

    DATA: lo_html      TYPE REF TO ltd_html_double,
          ls_diff_line TYPE zif_abapgit_definitions=>ty_diff,
          lt_files     TYPE zif_abapgit_gui_diff=>ty_file_diffs,
          ls_file_diff TYPE zif_abapgit_gui_diff=>ty_file_diff,
          lo_diff      TYPE REF TO ltd_diff_double.

    CREATE OBJECT lo_html.
    CREATE OBJECT lo_diff.

    ls_file_diff-path     = '/src/'.
    ls_file_diff-filename = 'ztest.prog.abap'.
    ls_file_diff-o_diff   = lo_diff.
    INSERT ls_file_diff INTO TABLE lt_files.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-insert.

    zcl_abapgit_gui_page_patch=>render_patch_impl(
      ii_html          = lo_html
      iv_filename      = '_src__ztest_prog_abap'
      is_diff_line     = ls_diff_line
      iv_index         = 7
      iv_section_count = 3
      it_diff_files    = lt_files ).

    cl_abap_unit_assert=>assert_equals(
      exp = |patch_line__src__ztest_prog_abap_3_7|
      act = lo_html->mv_checkbox_id
      msg = |ID should include filename, section count, and index| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_get_hotkey_actions IMPLEMENTATION.

  METHOD returns_three_hotkeys.

    DATA lt_actions TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr.

    lt_actions = zcl_abapgit_gui_page_patch=>get_hotkey_actions_impl( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_actions )
      msg = |Should return exactly 3 hotkey actions| ).

  ENDMETHOD.

  METHOD stage_hotkey_is_s.

    DATA: lt_actions TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
          ls_action  LIKE LINE OF lt_actions.

    lt_actions = zcl_abapgit_gui_page_patch=>get_hotkey_actions_impl( ).

    READ TABLE lt_actions INTO ls_action
      WITH KEY action = 'stagePatch'.

    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      msg = |stagePatch action should exist| ).

    cl_abap_unit_assert=>assert_equals(
      exp = |s|
      act = ls_action-hotkey
      msg = |stagePatch hotkey should be s| ).

  ENDMETHOD.

  METHOD refresh_local_hotkey_r.

    DATA: lt_actions TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
          ls_action  LIKE LINE OF lt_actions.

    lt_actions = zcl_abapgit_gui_page_patch=>get_hotkey_actions_impl( ).

    READ TABLE lt_actions INTO ls_action
      WITH KEY action = 'refreshLocal'.

    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      msg = |refreshLocal action should exist| ).

    cl_abap_unit_assert=>assert_equals(
      exp = |r|
      act = ls_action-hotkey
      msg = |refreshLocal hotkey should be r| ).

  ENDMETHOD.

  METHOD refresh_all_hotkey_a.

    DATA: lt_actions TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
          ls_action  LIKE LINE OF lt_actions.

    lt_actions = zcl_abapgit_gui_page_patch=>get_hotkey_actions_impl( ).

    READ TABLE lt_actions INTO ls_action
      WITH KEY action = 'refreshAll'.

    cl_abap_unit_assert=>assert_subrc(
      exp = 0
      msg = |refreshAll action should exist| ).

    cl_abap_unit_assert=>assert_equals(
      exp = |a|
      act = ls_action-hotkey
      msg = |refreshAll hotkey should be a| ).

  ENDMETHOD.

ENDCLASS.
