FUNCTION Z_ABAPGIT_GET_USERNAME_AND_PW.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_REPO_URL) TYPE  STRING
*"  CHANGING
*"     REFERENCE(CV_USER) TYPE  STRING
*"     REFERENCE(CV_PASS) TYPE  STRING
*"----------------------------------------------------------------------
  CLEAR: cv_pass.

  lcl_screen=>get_instance( )->init( ).

  gv_url  = iv_repo_url.
  gv_user = cv_user.

  CALL SCREEN 0100 STARTING AT 5 5 ENDING AT 60 8.
  IF lcl_screen=>get_instance( )->is_cancelled( ) = abap_true.
    CLEAR: cv_user, cv_pass.
  ELSE.
    cv_user = gv_user.
    cv_pass = gv_pass.
  ENDIF.

ENDFUNCTION.
