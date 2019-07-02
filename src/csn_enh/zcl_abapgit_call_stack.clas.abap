CLASS zcl_abapgit_call_stack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      is_called_from_abapgit
        RETURNING
          VALUE(rv_is_called_from_abapgit) TYPE abap_bool.

ENDCLASS.



CLASS zcl_abapgit_call_stack IMPLEMENTATION.

  METHOD is_called_from_abapgit.

    DATA: lt_callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = lt_callstack.

    rv_is_called_from_abapgit = boolc( line_exists( lt_callstack[ mainprogram = 'ZABAPGIT' ] ) ).

  ENDMETHOD.

ENDCLASS.
