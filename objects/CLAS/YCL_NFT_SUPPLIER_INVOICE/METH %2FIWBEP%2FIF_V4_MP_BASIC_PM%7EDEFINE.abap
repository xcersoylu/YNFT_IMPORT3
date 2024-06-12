  METHOD /iwbep/if_v4_mp_basic_pm~define.

    mo_model = io_model.
    mo_model->set_schema_namespace( 'API_SUPPLIERINVOICE_PROCESS_SRV' ).

    def_cancel_invoice_export_para( ).
    def_post_invoice_export_parame( ).
    def_release_invoice_export_par( ).
    def_a_suplr_invc_header_whld_2( ).
    def_a_suplr_invc_item_acct_a_2( ).
    def_a_suplr_invc_item_pur_or_2( ).
    def_a_suplr_invc_seld_inb_de_2( ).
    def_a_suplr_invc_seld_purg_d_2( ).
    def_a_suplr_invc_seld_srvc_e_2( ).
    def_a_suplr_invoice_addition_2( ).
    def_a_supplier_invoice_item__2( ).
    def_a_supplier_invoice_item__3( ).
    def_a_supplier_invoice_item__4( ).
    def_a_supplier_invoice_tax_typ( ).
    def_a_supplier_invoice_type( ).
    def_cancel( ).
    def_post( ).
    def_release( ).
    define_primitive_types( ).

  ENDMETHOD.