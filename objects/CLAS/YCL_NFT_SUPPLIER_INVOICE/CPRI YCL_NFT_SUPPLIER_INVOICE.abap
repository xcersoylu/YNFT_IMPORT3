  PRIVATE SECTION.

    "! <p class="shorttext synchronized">Model</p>
    DATA mo_model TYPE REF TO /iwbep/if_v4_pm_model.


    "! <p class="shorttext synchronized">Define CancelInvoiceExportParameters</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_cancel_invoice_export_para RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PostInvoiceExportParameters</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_post_invoice_export_parame RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define ReleaseInvoiceExportParameters</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_release_invoice_export_par RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvcHeaderWhldgTaxType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invc_header_whld_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvcItemAcctAssgmtType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invc_item_acct_a_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvcItemPurOrdRefType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invc_item_pur_or_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvcSeldInbDeliveryNoteType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invc_seld_inb_de_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvcSeldPurgDocumentType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invc_seld_purg_d_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvcSeldSrvcEntrShtLeanType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invc_seld_srvc_e_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SuplrInvoiceAdditionalDataType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_suplr_invoice_addition_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SupplierInvoiceItemAssetType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_supplier_invoice_item__2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SupplierInvoiceItemGLAcctType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_supplier_invoice_item__3 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SupplierInvoiceItemMaterialType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_supplier_invoice_item__4 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SupplierInvoiceTaxType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_supplier_invoice_tax_typ RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SupplierInvoiceType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_supplier_invoice_type RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define Cancel</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_cancel RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define Post</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_post RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define Release</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_release RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define all primitive types</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS define_primitive_types RAISING /iwbep/cx_gateway.
