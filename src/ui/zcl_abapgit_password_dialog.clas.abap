CLASS zcl_abapgit_password_dialog DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS popup
      IMPORTING
        !iv_repo_url TYPE string
      CHANGING
        !cv_user     TYPE string
        !cv_pass     TYPE string
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      is_called_from_prog_withscreen
        RETURNING
          VALUE(rv_is_called_from_abapgit) TYPE abap_bool
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_password_dialog IMPLEMENTATION.


  METHOD is_called_from_prog_withscreen.

    DATA: lt_abap_stack TYPE abap_callstack,
          lt_syst_stack TYPE sys_callst,
          lt_dynpros    TYPE STANDARD TABLE OF d020s.

    FIELD-SYMBOLS: <ls_abap_stack> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack    = lt_abap_stack
        et_callstack = lt_syst_stack.

    READ TABLE lt_abap_stack ASSIGNING <ls_abap_stack>
                             INDEX lines( lt_abap_stack ).

    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = <ls_abap_stack>-mainprogram
      TABLES
        dynpros   = lt_dynpros
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_dynpros TRANSPORTING NO FIELDS
                          WITH KEY dnum = '1002'.
    rv_is_called_from_abapgit = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD popup.


    IF is_called_from_prog_withscreen( ) = abap_true.

      PERFORM password_popup
        IN PROGRAM zabapgit
        USING iv_repo_url
        CHANGING cv_user cv_pass.

    ELSE.

      " API calls cannot call sreens in abapgit report

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = 'Z_ABAPGIT_GET_USERNAME_AND_PW'
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'HTTP 401, unauthorized' ).
      ENDIF.

      zcl_abapgit_login_manager=>clear( ).

      CALL FUNCTION 'Z_ABAPGIT_GET_USERNAME_AND_PW'
        EXPORTING
          iv_repo_url = iv_repo_url
        CHANGING
          cv_user     = cv_user
          cv_pass     = cv_pass.

      zcl_abapgit_login_manager=>set( iv_uri      = iv_repo_url
                                      iv_username = cv_user
                                      iv_password = cv_pass ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
