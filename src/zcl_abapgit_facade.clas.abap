CLASS zcl_abapgit_facade DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_staged,
        path     TYPE string,
        filename TYPE string,
      END OF ty_staged,
      tty_staged TYPE STANDARD TABLE OF ty_staged
                      WITH NON-UNIQUE DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING
          iv_key      TYPE zif_abapgit_persistence=>ty_value OPTIONAL
          iv_user     TYPE string OPTIONAL
          iv_password TYPE string OPTIONAL
        RAISING
          zcx_abapgit_exception,

      create_tag
        IMPORTING
          iv_tag_name       TYPE csequence
          iv_sha1           TYPE zif_abapgit_definitions=>ty_sha1 OPTIONAL
        RETURNING
          VALUE(rv_message) TYPE string
        RAISING
          zcx_abapgit_exception,

      create_commit
        IMPORTING
          iv_comment           TYPE csequence
          iv_committer_name    TYPE csequence
          iv_committer_email   TYPE csequence
          iv_remove            TYPE abap_bool DEFAULT abap_false
        EXPORTING
          et_staged            TYPE tty_staged
          et_removed           TYPE tty_staged
          ev_nothing_to_commit TYPE abap_bool
          et_code_inspection   TYPE scit_alvlist
          ev_push_not_allowed  TYPE abap_bool
        RAISING
          zcx_abapgit_exception,

      clone
        IMPORTING
          iv_url         TYPE string
          iv_package     TYPE devclass
        RETURNING
          VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo
        RAISING
          zcx_abapgit_exception,

      uninstall
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_key      TYPE zif_abapgit_persistence=>ty_value,
      mo_repo     TYPE REF TO zcl_abapgit_repo_online,
      mv_username TYPE string,
      mv_password TYPE string.

    METHODS:
      has_error
        IMPORTING
          it_code_inspection  TYPE scit_alvlist
        RETURNING
          VALUE(rv_has_error) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_facade IMPLEMENTATION.


  METHOD clone.

    DATA: lo_popup_provider TYPE REF TO zcl_abapgit_popup_provider.

    CREATE OBJECT lo_popup_provider.

    lo_popup_provider->set_url( iv_url ).
    lo_popup_provider->set_package( iv_package ).

    zcl_abapgit_ui_injector=>set_popups( lo_popup_provider ).

    IF  mv_username IS NOT INITIAL
    AND mv_password IS NOT INITIAL.

      zcl_abapgit_login_manager=>set( iv_uri      = iv_url
                                      iv_username = mv_username
                                      iv_password = mv_password ).

    ENDIF.

    TRY.
        ro_repo = zcl_abapgit_services_repo=>new_online( VALUE #( url = iv_url ) ).
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    mv_username = iv_user.
    mv_password = iv_password.

    IF iv_key IS NOT INITIAL.

      mv_key      = iv_key.
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_key ).

      IF  iv_user IS NOT INITIAL
      AND iv_password IS NOT INITIAL.

        zcl_abapgit_login_manager=>set( iv_uri      = mo_repo->get_url( )
                                        iv_username = iv_user
                                        iv_password = iv_password ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_commit.

    DATA: ls_comment TYPE zif_abapgit_definitions=>ty_comment,
          ls_files   TYPE zif_abapgit_definitions=>ty_stage_files,
          lo_stage   TYPE REF TO zcl_abapgit_stage,
          ls_staged  LIKE LINE OF et_staged,
          ls_removed LIKE LINE OF et_staged.

    FIELD-SYMBOLS: <ls_local>  LIKE LINE OF ls_files-local,
                   <ls_remote> TYPE zif_abapgit_definitions=>ty_file.

    CLEAR: et_code_inspection,
           et_removed,
           et_staged,
           ev_nothing_to_commit,
           ev_push_not_allowed.

    ls_files = zcl_abapgit_factory=>get_stage_logic( )->get( mo_repo ).
    IF lines( ls_files-local ) = 0 AND lines( ls_files-remote ) = 0.
      ev_nothing_to_commit = abap_true.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_stage.

    LOOP AT ls_files-local ASSIGNING <ls_local>.

      ls_staged-path     = <ls_local>-file-path.
      ls_staged-filename =  <ls_local>-file-filename.
      INSERT ls_staged INTO TABLE et_staged.

      lo_stage->add( iv_path     = <ls_local>-file-path
                     iv_filename = <ls_local>-file-filename
                     iv_data     = <ls_local>-file-data ).
    ENDLOOP.

    IF iv_remove = abap_true.

      LOOP AT ls_files-remote ASSIGNING <ls_remote>.

        ls_removed-path     = <ls_remote>-path.
        ls_removed-filename = <ls_remote>-filename.
        INSERT ls_removed INTO TABLE et_removed.

        lo_stage->rm( iv_path     = <ls_remote>-path
                      iv_filename = <ls_remote>-filename ).

      ENDLOOP.

    ENDIF.

    ls_comment-committer-name  = iv_committer_name.
    ls_comment-committer-email = iv_committer_email.
    ls_comment-comment         = iv_comment.

    DATA(lv_check_variant) = mo_repo->get_local_settings( )-code_inspector_check_variant.

    IF lv_check_variant IS NOT INITIAL.

      et_code_inspection = zcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) )->run( lv_check_variant ).

      IF mo_repo->get_local_settings( )-block_commit = abap_true
      AND has_error( et_code_inspection ) = abap_true.

        ev_push_not_allowed = abap_true.
        RETURN.

      ENDIF.

    ENDIF.

    mo_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.


  METHOD create_tag.

    DATA: lv_sha1 TYPE zif_abapgit_definitions=>ty_sha1,
          ls_tag  TYPE zif_abapgit_definitions=>ty_git_tag.

    lv_sha1 = iv_sha1.

    IF lv_sha1 IS INITIAL.
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( mv_key ).
      lv_sha1 = mo_repo->get_current_remote( ).
    ENDIF.

    ls_tag-name = zcl_abapgit_git_tag=>add_tag_prefix( iv_tag_name ).
    ls_tag-type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
    ls_tag-sha1 = lv_sha1.

    TRY.
        zcl_abapgit_git_porcelain=>create_tag( iv_url = mo_repo->get_url( )
                                               is_tag = ls_tag ).

      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

    rv_message = |Lightweight tag { zcl_abapgit_git_tag=>remove_tag_prefix( iv_tag_name ) } created| ##NO_TEXT.

  ENDMETHOD.


  METHOD has_error.

    READ TABLE it_code_inspection TRANSPORTING NO FIELDS
                                  WITH KEY kind = 'E'.
    rv_has_error = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD uninstall.

    DATA: lo_popup_provider TYPE REF TO zcl_abapgit_popup_provider.

    CREATE OBJECT lo_popup_provider.

    lo_popup_provider->set_popup_to_confirm_answer( '1' ).

    zcl_abapgit_ui_injector=>set_popups(  lo_popup_provider ).

    TRY.
        zcl_abapgit_services_repo=>purge( mv_key ).
      CATCH zcx_abapgit_cancel INTO DATA(error).
        zcx_abapgit_exception=>raise( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
