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

    DATA lv_gui_is_available TYPE abap_bool.

    lv_gui_is_available = zcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ).

    IF  lv_gui_is_available = abap_true
    AND is_called_from_prog_withscreen( ) = abap_true.
      PERFORM password_popup
        IN PROGRAM (sy-cprog)
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


      "Extract user credentials from the environment...
      "Class ZCL_ABAPGIT_DEFAULT_AUTH_INFO is part of https://github.com/abapGit/ADT_Backend.
      "It stores the credentials of a private repository as long as the session exists.
      "Usually this class should belong to abapGit core and a refactoring is recommended.
      "As a temporary solution - and to avoid a DYNPRO_SEND_IN_BACKGROUND dump - a generic
      "call of the getter methods for username and password is implemented by PR#2635.
      "TRY.
      "    CALL METHOD ('ZCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_USER')
      "      RECEIVING
      "        rv_user = cv_user.
      "  CATCH cx_root.
      "    RETURN.
      "ENDTRY.
      "TRY.
      "    CALL METHOD ('ZCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSWORD')
      "      RECEIVING
      "        rv_password = cv_pass.
      "  CATCH cx_root.
      "    "check if old version with typo in method name exists
      "    TRY.
      "        CALL METHOD ('ZCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSOWORD')
      "          RECEIVING
      "            rv_password = cv_pass.
      "      CATCH cx_root.
      "        RETURN.
      "    ENDTRY.
      "ENDTRY.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
