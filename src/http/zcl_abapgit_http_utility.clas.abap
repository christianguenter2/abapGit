CLASS zcl_abapgit_http_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      escape
        IMPORTING
          iv_text          TYPE csequence
        RETURNING
          VALUE(rv_result) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_http_utility IMPLEMENTATION.

  METHOD escape.

    " Some characters in object names cause problems when identifying the object later
    " -> we escape these characters here
    " cl_http_utility=>escape_url doesn't do dots but escapes slash which we use for namespaces
    " -> we escape just some selected characters

    rv_result = iv_text.

    REPLACE ALL OCCURRENCES OF `%` IN rv_result WITH '%25'.
    REPLACE ALL OCCURRENCES OF ` ` IN rv_result WITH '%20'.
    REPLACE ALL OCCURRENCES OF `#` IN rv_result WITH '%23'.
    REPLACE ALL OCCURRENCES OF `.` IN rv_result WITH '%2e'.
    REPLACE ALL OCCURRENCES OF `=` IN rv_result WITH '%3d'.
    REPLACE ALL OCCURRENCES OF `?` IN rv_result WITH '%3f'.
    REPLACE ALL OCCURRENCES OF `<` IN rv_result WITH '%3c'.
    REPLACE ALL OCCURRENCES OF `>` IN rv_result WITH '%3e'.

  ENDMETHOD.

ENDCLASS.
