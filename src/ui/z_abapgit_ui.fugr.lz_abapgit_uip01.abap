*----------------------------------------------------------------------*
***INCLUDE LZ_ABAPGIT_UIP01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_screen
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen IMPLEMENTATION.

  METHOD get_instance.

    IF mo_instance IS NOT BOUND.
      CREATE OBJECT mo_instance.
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.

  METHOD is_cancelled.

    rv_is_cancelled = mv_cancelled.

  ENDMETHOD.

  METHOD pai.

    DATA(lv_okcode) = okcode.

    CLEAR: okcode.

    CASE lv_okcode.
      WHEN '&CANC'.
        mv_cancelled = abap_true.
      WHEN '&OK'.
    ENDCASE.

    SET SCREEN 0.

  ENDMETHOD.

  METHOD pbo.

    CLEAR: mv_cancelled.

    SET PF-STATUS 'MAIN_0100'.
    SET TITLEBAR 'MAIN_0100'.

  ENDMETHOD.


  METHOD init.

    CLEAR: gv_url,
           gv_user,
           gv_pass.

  ENDMETHOD.

ENDCLASS.
