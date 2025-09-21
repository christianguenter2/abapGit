CLASS zcl_abapgit_pr_enum_gitlab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_user_and_repo TYPE string
        !ii_http_agent    TYPE REF TO zif_abapgit_http_agent
      RAISING
        zcx_abapgit_exception.
    INTERFACES:
      zif_abapgit_pr_enum_provider.

  PRIVATE SECTION.
    DATA:
      mv_user_and_repo TYPE string,
      mi_http_agent    TYPE REF TO zif_abapgit_http_agent.
    METHODS get_project_id
      RETURNING
        VALUE(rv_project_id) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS convert_list
      IMPORTING
        ii_json         TYPE REF TO zif_abapgit_ajson
      RETURNING
        VALUE(rt_pulls) TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests.

ENDCLASS.



CLASS zcl_abapgit_pr_enum_gitlab IMPLEMENTATION.

  METHOD constructor.

    mv_user_and_repo = iv_user_and_repo.
    mi_http_agent = ii_http_agent.

*    IF zcl_abapgit_login_manager=>get( mv_repo_url ) IS NOT INITIAL.
*      mi_http_agent->global_headers( )->set(
*        iv_key = 'Authorization'
*        iv_val = zcl_abapgit_login_manager=>get( mv_repo_url ) ).
*    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_pr_enum_provider~create_initial_branch.

  ENDMETHOD.


  METHOD zif_abapgit_pr_enum_provider~list_pull_requests.

    DATA:
      lx_ajson      TYPE REF TO zcx_abapgit_ajson_error,
      lv_project_id TYPE string,
      lv_url        TYPE string,
      li_response   TYPE REF TO zif_abapgit_http_response.

    lv_project_id = get_project_id( ).

    " https://gitlab.com/api/v4/projects/15406525/merge_requests

    lv_url = |https://gitlab.com/api/v4/projects/{ lv_project_id }/merge_requests?state=opened|.

    li_response = mi_http_agent->request( lv_url ).

    IF li_response->is_ok( ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        rt_pulls = convert_list( li_response->json( ) ).

      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.


  ENDMETHOD.


  METHOD get_project_id.

    DATA:
      lx_ajson    TYPE REF TO zcx_abapgit_ajson_error,
      lv_url      TYPE string,
      li_response TYPE REF TO zif_abapgit_http_response.

    lv_url = |https://gitlab.com/api/v4/projects/{ cl_http_utility=>escape_url( mv_user_and_repo ) }|.

    li_response = mi_http_agent->request( lv_url ).

    IF li_response->is_ok( ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        rv_project_id = li_response->json( )->get( '/id' ).

      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_list.

    DATA lt_items TYPE string_table.
    DATA lv_i TYPE string.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF rt_pulls.

    lt_items = ii_json->members( '/' ).

    LOOP AT lt_items INTO lv_i.
      APPEND INITIAL LINE TO rt_pulls ASSIGNING <ls_p>.
      <ls_p>-base_url        = ii_json->get( |/{ lv_i }/web_url| ).
      <ls_p>-number          = ii_json->get( |/{ lv_i }/iid| ).
      <ls_p>-title           = ii_json->get( |/{ lv_i }/title| ).
      <ls_p>-user            = ii_json->get( |/{ lv_i }/author/name| ).
      <ls_p>-head_url        = ii_json->get( |/{ lv_i }/head/repo/clone_url| ).
      <ls_p>-head_branch     = ii_json->get( |/{ lv_i }/source_branch| ).
      <ls_p>-created_at      = ii_json->get( |/{ lv_i }/created_at| ).
      <ls_p>-draft           = ii_json->get_boolean( |/{ lv_i }/draft| ).
*      <ls_p>-html_url        = ii_json->get( |/{ lv_i }/html_url| ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
