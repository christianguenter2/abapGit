*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_form_validator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_form      TYPE REF TO zcl_abapgit_html_form
          io_form_data TYPE REF TO zcl_abapgit_string_map
        RAISING
          zcx_abapgit_exception,

      validate
        RETURNING
          VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA:
      mo_form_data      TYPE REF TO zcl_abapgit_string_map,
      mo_validation_log TYPE REF TO zcl_abapgit_string_map.

    METHODS:
      validate_url
        IMPORTING
          iv_url TYPE string OPTIONAL
          iv_key TYPE string OPTIONAL
        RAISING
          zcx_abapgit_exception,

      validate_head_type_specific
        RAISING
          zcx_abapgit_exception,

      validate_branch
        IMPORTING
          iv_url    TYPE string OPTIONAL
          iv_branch TYPE string
          iv_key    TYPE string
        RAISING
          zcx_abapgit_exception,

      branch
        RAISING
          zcx_abapgit_exception,

      tag
        RAISING
          zcx_abapgit_exception,

      pull_request
        RAISING
          zcx_abapgit_exception,

      commit
        RAISING
          zcx_abapgit_exception,

      unknown_head_type
        RAISING
          zcx_abapgit_exception.

ENDCLASS.


CLASS lcl_form_validator IMPLEMENTATION.

  METHOD constructor.

    mo_form_data      = io_form_data.
    mo_validation_log = zcl_abapgit_html_form_utils=>create( io_form )->validate( mo_form_data ).

  ENDMETHOD.


  METHOD validate.

    ro_validation_log = mo_validation_log.

    IF mo_form_data->get( const=>id-offline ) = abap_true.
      RETURN.
    ENDIF.

    validate_url( ).
    validate_head_type_specific( ).

  ENDMETHOD.


  METHOD validate_url.

    DATA:
      lv_url   TYPE string,
      lv_key   TYPE string,
      lx_error TYPE REF TO zcx_abapgit_exception,
      lo_url   TYPE REF TO zcl_abapgit_git_url.

    lv_url = iv_url.
    IF lv_url IS INITIAL.
      lv_url = mo_form_data->get( const=>id-url ).
    ENDIF.

    lv_key = iv_key.
    IF lv_key IS INITIAL.
      lv_key = const=>id-url.
    ENDIF.

    IF lv_url NP 'http*'.
      mo_validation_log->set(
        iv_key = lv_key
        iv_val = 'Invalid URL' ).
    ELSE.
      TRY.
          zcl_abapgit_url=>name(
            iv_url      = lv_url
            iv_validate = abap_true ).

          " Provider-specific URL check
          CREATE OBJECT lo_url.
          lo_url->validate_url( lv_url ).
        CATCH zcx_abapgit_exception INTO lx_error.
          mo_validation_log->set(
            iv_key = lv_key
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD validate_head_type_specific.

    DATA:
      lv_head_type TYPE types=>ty_head_type.

    lv_head_type = mo_form_data->get( const=>id-head_type ).

    CASE lv_head_type.
      WHEN const=>head_types-branch.

        branch( ).

      WHEN const=>head_types-tag.

        tag( ).

      WHEN const=>head_types-pull_request.

        pull_request( ).

      WHEN const=>head_types-commit.

        commit( ).

      WHEN OTHERS.

        unknown_head_type( ).

    ENDCASE.

  ENDMETHOD.


  METHOD validate_branch.

    DATA:
      lx_error TYPE REF TO zcx_abapgit_exception,
      lv_url   TYPE string.

    lv_url = iv_url.

    IF lv_url IS INITIAL.
      lv_url = mo_form_data->get( const=>id-url ).
    ENDIF.

    TRY.
        zcl_abapgit_git_factory=>get_git_transport( )->branches( lv_url )->find_by_name( iv_branch ).

      CATCH zcx_abapgit_exception INTO lx_error.
        mo_validation_log->set(
          iv_key = iv_key
          iv_val = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD branch.

    DATA:
      lv_branch TYPE types=>ty_remote_settings-branch.

    lv_branch = condense( zif_abapgit_git_definitions=>c_git_branch-heads_prefix
                       && mo_form_data->get( const=>id-branch ) ).

    validate_branch(
        iv_branch = lv_branch
        iv_key    = const=>id-branch ).

  ENDMETHOD.


  METHOD tag.

    DATA:
      lv_branch TYPE types=>ty_remote_settings-branch.

    lv_branch = condense( zif_abapgit_git_definitions=>c_git_branch-tags_prefix
                       && mo_form_data->get( const=>id-tag ) ).

    validate_branch(
        iv_branch = lv_branch
        iv_key    = const=>id-tag ).

  ENDMETHOD.


  METHOD pull_request.

    DATA:
      lv_url          TYPE string,
      lv_branch       TYPE types=>ty_remote_settings-branch,
      lv_pull_request TYPE types=>ty_remote_settings-pull_request.

    lv_pull_request = mo_form_data->get( const=>id-pull_request ).

    SPLIT lv_pull_request AT '@' INTO lv_url lv_branch.
    IF lv_branch IS INITIAL.
      mo_validation_log->set(
        iv_key = const=>id-pull_request
        iv_val = |Invalid pull request, no branch detected| ).
      RETURN.
    ENDIF.

    lv_branch = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && lv_branch.

    validate_url(
        iv_url = lv_url
        iv_key = const=>id-pull_request ).

    validate_branch(
        iv_url    = lv_url
        iv_branch = lv_branch
        iv_key    = const=>id-pull_request ).

  ENDMETHOD.


  METHOD commit.

    DATA:
      lv_commit TYPE types=>ty_remote_settings-commit.

    lv_commit = mo_form_data->get( const=>id-commit ).

    " Cannot check for commit existence currently (needs API that doesn't rely on finding the first commit
    " in the branch), check format instead
    IF lv_commit CN '0123456789abcdef'.
      mo_validation_log->set(
        iv_key = const=>id-commit
        iv_val = 'Commit needs to be hexadecimal and in lowercase' ).
    ENDIF.

  ENDMETHOD.


  METHOD unknown_head_type.

    mo_validation_log->set(
      iv_key = const=>id-head_type
      iv_val = 'Unknown head type' ).

  ENDMETHOD.

ENDCLASS.
