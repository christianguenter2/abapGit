"Name: \TY:CL_PXN_INTERFACE\IN:IF_PXN_CONFIGURATION\ME:GET_INTERFACE_PATTERN\SE:END\EI
ENHANCEMENT 0 Z_ENH_ABAPGIT_CL_PXN_INTERFACE.

    IF pattern IS INITIAL
    AND zcl_abapgit_call_stack=>is_called_from_abapgit( ).
      pattern = '1'.
    ENDIF.

ENDENHANCEMENT.
