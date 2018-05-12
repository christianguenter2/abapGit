*&---------------------------------------------------------------------*
*& Include          LZ_ABAPGIT_UID01
*&---------------------------------------------------------------------*

CLASS lcl_screen DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO lcl_screen.

    CLASS-DATA:
      okcode TYPE syucomm.

    METHODS:
      pbo,
      pai,
      is_cancelled
        RETURNING VALUE(rv_is_cancelled) TYPE abap_bool,
      init.

  PRIVATE SECTION.
    CLASS-DATA mo_instance TYPE REF TO lcl_screen.
    DATA: mv_cancelled TYPE abap_bool.

ENDCLASS.
