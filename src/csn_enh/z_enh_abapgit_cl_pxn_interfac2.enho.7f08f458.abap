"Name: \TY:CL_PXN_OPERATION\IN:IF_PXN_OPERATION_CONFIG\ME:GET_OPERATION_PATTERN\SE:END\EI
ENHANCEMENT 0 Z_ENH_ABAPGIT_CL_PXN_INTERFAC2.

  IF pattern IS INITIAL
  AND zcl_abapgit_call_stack=>is_called_from_abapgit( ).
    pattern = 'NORMAL'.
  ENDIF.

ENDENHANCEMENT.
