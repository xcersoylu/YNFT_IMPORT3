CLASS lhc_yi_nft_ddl_t001 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR yi_nft_ddl_t001 RESULT result.

ENDCLASS.

CLASS lhc_yi_nft_ddl_t001 IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.