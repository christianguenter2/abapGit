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
  ENDMETHOD.
  METHOD zif_abapgit_diff~set_patch_by_old_diff.
    mv_set_patch_by_old_called = abap_true.
    ms_set_patch_by_old_diff   = is_diff_old.
    mv_set_patch_by_old_flag   = iv_patch_flag.
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

CLASS zcl_abapgit_gui_page_patch DEFINITION LOCAL FRIENDS ltcl_get_patch_data
                                                          ltcl_is_patch_line_possible
                                                          ltcl_are_all_lines_patched
                                                          ltcl_render_patch_cell
                                                          ltcl_get_diff_line
                                                          ltcl_apply_patch_to_diff
                                                          ltcl_restore_patch_flags
                                                          ltcl_get_diff_object.

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

    lo_result = zcl_abapgit_gui_page_patch=>get_diff_object_from(
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
        zcl_abapgit_gui_page_patch=>get_diff_object_from(
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

    lo_result = zcl_abapgit_gui_page_patch=>get_diff_object_from(
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
