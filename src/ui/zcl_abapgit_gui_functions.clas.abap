CLASS zcl_abapgit_gui_functions DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_functions.

ENDCLASS.



CLASS zcl_abapgit_gui_functions IMPLEMENTATION.

  METHOD zif_abapgit_gui_functions~gui_is_available.

    DATA: lv_test TYPE char01.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

    WRITE: lv_test.

  ENDMETHOD.

ENDCLASS.
