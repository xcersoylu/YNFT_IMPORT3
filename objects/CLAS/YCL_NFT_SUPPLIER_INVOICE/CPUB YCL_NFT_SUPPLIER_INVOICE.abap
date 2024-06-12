"! <p class="shorttext synchronized">Consumption model for client proxy - generated</p>
"! This class has been generated based on the metadata with namespace
"! <em>API_SUPPLIERINVOICE_PROCESS_SRV</em>
CLASS ycl_nft_supplier_invoice DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_v4_abs_pm_model_prov
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized">Types for "OData Primitive Types"</p>
      BEGIN OF tys_types_for_prim_types,
        "! Used for primitive type DISCOUNT_DAYS_HAVE_TO_BE_S
        discount_days_have_to_be_s TYPE abap_bool,
        "! Used for primitive type FISCAL_YEAR
        fiscal_year                TYPE c LENGTH 4,
        "! Used for primitive type FISCAL_YEAR_2
        fiscal_year_2              TYPE c LENGTH 4,
        "! Used for primitive type FISCAL_YEAR_3
        fiscal_year_3              TYPE c LENGTH 4,
        "! Used for primitive type POSTING_DATE
        posting_date               TYPE timestamp,
        "! Used for primitive type REVERSAL_REASON
        reversal_reason            TYPE c LENGTH 2,
        "! Used for primitive type SUPPLIER_INVOICE
        supplier_invoice           TYPE c LENGTH 10,
        "! Used for primitive type SUPPLIER_INVOICE_2
        supplier_invoice_2         TYPE c LENGTH 10,
        "! Used for primitive type SUPPLIER_INVOICE_3
        supplier_invoice_3         TYPE c LENGTH 10,
      END OF tys_types_for_prim_types.

    TYPES:
      "! <p class="shorttext synchronized">CancelInvoiceExportParameters</p>
      BEGIN OF tys_cancel_invoice_export_para,
        "! ReverseDocument
        reverse_document TYPE c LENGTH 10,
        "! FiscalYear
        fiscal_year      TYPE c LENGTH 4,
        "! Success
        success          TYPE abap_bool,
      END OF tys_cancel_invoice_export_para,
      "! <p class="shorttext synchronized">List of CancelInvoiceExportParameters</p>
      tyt_cancel_invoice_export_para TYPE STANDARD TABLE OF tys_cancel_invoice_export_para WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">PostInvoiceExportParameters</p>
      BEGIN OF tys_post_invoice_export_parame,
        "! Success
        success TYPE abap_bool,
      END OF tys_post_invoice_export_parame,
      "! <p class="shorttext synchronized">List of PostInvoiceExportParameters</p>
      tyt_post_invoice_export_parame TYPE STANDARD TABLE OF tys_post_invoice_export_parame WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">ReleaseInvoiceExportParameters</p>
      BEGIN OF tys_release_invoice_export_par,
        "! Success
        success TYPE abap_bool,
      END OF tys_release_invoice_export_par,
      "! <p class="shorttext synchronized">List of ReleaseInvoiceExportParameters</p>
      tyt_release_invoice_export_par TYPE STANDARD TABLE OF tys_release_invoice_export_par WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function Cancel</p>
      "! <em>with the internal name</em> CANCEL
      BEGIN OF tys_parameters_1,
        "! PostingDate
        posting_date     TYPE timestamp,
        "! ReversalReason
        reversal_reason  TYPE c LENGTH 2,
        "! FiscalYear
        fiscal_year      TYPE c LENGTH 4,
        "! SupplierInvoice
        supplier_invoice TYPE c LENGTH 10,
      END OF tys_parameters_1,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_1</p>
      tyt_parameters_1 TYPE STANDARD TABLE OF tys_parameters_1 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function Post</p>
      "! <em>with the internal name</em> POST
      BEGIN OF tys_parameters_2,
        "! SupplierInvoice
        supplier_invoice TYPE c LENGTH 10,
        "! FiscalYear
        fiscal_year      TYPE c LENGTH 4,
      END OF tys_parameters_2,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_2</p>
      tyt_parameters_2 TYPE STANDARD TABLE OF tys_parameters_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function Release</p>
      "! <em>with the internal name</em> RELEASE
      BEGIN OF tys_parameters_3,
        "! SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! DiscountDaysHaveToBeShifted
        discount_days_have_to_be_s TYPE abap_bool,
      END OF tys_parameters_3,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_3</p>
      tyt_parameters_3 TYPE STANDARD TABLE OF tys_parameters_3 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvcHeaderWhldgTaxType</p>
      BEGIN OF tys_a_suplr_invc_header_whld_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> WithholdingTaxType
        withholding_tax_type       TYPE c LENGTH 2,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! WithholdingTaxCode
        withholding_tax_code       TYPE c LENGTH 2,
        "! WithholdingTaxBaseAmount
        withholding_tax_base_amoun TYPE p LENGTH 9 DECIMALS 3,
        "! ManuallyEnteredWhldgTaxAmount
        manually_entered_whldg_tax TYPE p LENGTH 9 DECIMALS 3,
      END OF tys_a_suplr_invc_header_whld_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvcHeaderWhldgTaxType</p>
      tyt_a_suplr_invc_header_whld_2 TYPE STANDARD TABLE OF tys_a_suplr_invc_header_whld_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvcItemAcctAssgmtType</p>
      BEGIN OF tys_a_suplr_invc_item_acct_a_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> SupplierInvoiceItem
        supplier_invoice_item      TYPE c LENGTH 6,
        "! <em>Key property</em> OrdinalNumber
        ordinal_number             TYPE c LENGTH 4,
        "! CostCenter
        cost_center                TYPE c LENGTH 10,
        "! ControllingArea
        controlling_area           TYPE c LENGTH 4,
        "! BusinessArea
        business_area              TYPE c LENGTH 4,
        "! ProfitCenter
        profit_center              TYPE c LENGTH 10,
        "! FunctionalArea
        functional_area            TYPE c LENGTH 16,
        "! GLAccount
        glaccount                  TYPE c LENGTH 10,
        "! SalesOrder
        sales_order                TYPE c LENGTH 10,
        "! SalesOrderItem
        sales_order_item           TYPE c LENGTH 6,
        "! CostObject
        cost_object                TYPE c LENGTH 12,
        "! CostCtrActivityType
        cost_ctr_activity_type     TYPE c LENGTH 6,
        "! BusinessProcess
        business_process           TYPE c LENGTH 12,
        "! WBSElement
        wbselement                 TYPE c LENGTH 24,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! SuplrInvcAcctAssignmentAmount
        suplr_invc_acct_assignment TYPE p LENGTH 8 DECIMALS 3,
        "! PurchaseOrderQuantityUnit
        purchase_order_quantity_un TYPE c LENGTH 3,
        "! PurchaseOrderQtyUnitSAPCode
        purchase_order_qty_unit_sa TYPE c LENGTH 3,
        "! PurchaseOrderQtyUnitISOCode
        purchase_order_qty_unit_is TYPE c LENGTH 3,
        "! Quantity
        quantity                   TYPE p LENGTH 7 DECIMALS 3,
        "! TaxCode
        tax_code                   TYPE c LENGTH 2,
        "! AccountAssignmentNumber
        account_assignment_number  TYPE c LENGTH 2,
        "! AccountAssignmentIsUnplanned
        account_assignment_is_unpl TYPE abap_bool,
        "! PersonnelNumber
        personnel_number           TYPE c LENGTH 8,
        "! WorkItem
        work_item                  TYPE c LENGTH 10,
        "! MasterFixedAsset
        master_fixed_asset         TYPE c LENGTH 12,
        "! FixedAsset
        fixed_asset                TYPE c LENGTH 4,
        "! DebitCreditCode
        debit_credit_code          TYPE c LENGTH 1,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! InternalOrder
        internal_order             TYPE c LENGTH 12,
        "! ProjectNetworkInternalID
        project_network_internal_i TYPE c LENGTH 10,
        "! NetworkActivityInternalID
        network_activity_internal  TYPE c LENGTH 10,
        "! ProjectNetwork
        project_network            TYPE c LENGTH 12,
        "! NetworkActivity
        network_activity           TYPE c LENGTH 4,
        "! CommitmentItem
        commitment_item            TYPE c LENGTH 24,
        "! FundsCenter
        funds_center               TYPE c LENGTH 16,
        "! Fund
        fund                       TYPE c LENGTH 10,
        "! GrantID
        grant_id                   TYPE c LENGTH 20,
        "! PartnerBusinessArea
        partner_business_area      TYPE c LENGTH 4,
        "! CompanyCode
        company_code               TYPE c LENGTH 4,
        "! SuplrInvcAccountAssignmentText
        suplr_invc_account_assignm TYPE c LENGTH 50,
        "! PurchaseOrderPriceUnit
        purchase_order_price_unit  TYPE c LENGTH 3,
        "! PurchaseOrderPriceUnitSAPCode
        purchase_order_price_uni_2 TYPE c LENGTH 3,
        "! PurchaseOrderPriceUnitISOCode
        purchase_order_price_uni_3 TYPE c LENGTH 3,
        "! QuantityInPurchaseOrderUnit
        quantity_in_purchase_order TYPE p LENGTH 7 DECIMALS 3,
        "! ProfitabilitySegment
        profitability_segment      TYPE c LENGTH 10,
        "! BudgetPeriod
        budget_period              TYPE c LENGTH 10,
        "! TaxCountry
        tax_country                TYPE c LENGTH 3,
        "! TaxDeterminationDate
        tax_determination_date     TYPE datn,
      END OF tys_a_suplr_invc_item_acct_a_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvcItemAcctAssgmtType</p>
      tyt_a_suplr_invc_item_acct_a_2 TYPE STANDARD TABLE OF tys_a_suplr_invc_item_acct_a_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvcItemPurOrdRefType</p>
      BEGIN OF tys_a_suplr_invc_item_pur_or_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> SupplierInvoiceItem
        supplier_invoice_item      TYPE c LENGTH 6,
        "! PurchaseOrder
        purchase_order             TYPE c LENGTH 10,
        "! PurchaseOrderItem
        purchase_order_item        TYPE c LENGTH 5,
        "! Plant
        plant                      TYPE c LENGTH 4,
        "! ReferenceDocument
        reference_document         TYPE c LENGTH 10,
        "! ReferenceDocumentFiscalYear
        reference_document_fiscal  TYPE c LENGTH 4,
        "! ReferenceDocumentItem
        reference_document_item    TYPE c LENGTH 4,
        "! IsSubsequentDebitCredit
        is_subsequent_debit_credit TYPE c LENGTH 1,
        "! TaxCode
        tax_code                   TYPE c LENGTH 2,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! SupplierInvoiceItemAmount
        supplier_invoice_item_amou TYPE p LENGTH 8 DECIMALS 3,
        "! PurchaseOrderQuantityUnit
        purchase_order_quantity_un TYPE c LENGTH 3,
        "! PurchaseOrderQtyUnitSAPCode
        purchase_order_qty_unit_sa TYPE c LENGTH 3,
        "! PurchaseOrderQtyUnitISOCode
        purchase_order_qty_unit_is TYPE c LENGTH 3,
        "! QuantityInPurchaseOrderUnit
        quantity_in_purchase_order TYPE p LENGTH 7 DECIMALS 3,
        "! PurchaseOrderPriceUnit
        purchase_order_price_unit  TYPE c LENGTH 3,
        "! PurchaseOrderPriceUnitSAPCode
        purchase_order_price_uni_2 TYPE c LENGTH 3,
        "! PurchaseOrderPriceUnitISOCode
        purchase_order_price_uni_3 TYPE c LENGTH 3,
        "! QtyInPurchaseOrderPriceUnit
        qty_in_purchase_order_pric TYPE p LENGTH 7 DECIMALS 3,
        "! SuplrInvcDeliveryCostCndnType
        suplr_invc_delivery_cost_c TYPE c LENGTH 4,
        "! SuplrInvcDeliveryCostCndnStep
        suplr_invc_delivery_cost_2 TYPE c LENGTH 3,
        "! SuplrInvcDeliveryCostCndnCount
        suplr_invc_delivery_cost_3 TYPE c LENGTH 3,
        "! SupplierInvoiceItemText
        supplier_invoice_item_text TYPE c LENGTH 50,
        "! FreightSupplier
        freight_supplier           TYPE c LENGTH 10,
        "! IsNotCashDiscountLiable
        is_not_cash_discount_liabl TYPE abap_bool,
        "! PurchasingDocumentItemCategory
        purchasing_document_item_c TYPE c LENGTH 1,
        "! ProductType
        product_type               TYPE c LENGTH 2,
        "! RetentionAmountInDocCurrency
        retention_amount_in_doc_cu TYPE p LENGTH 8 DECIMALS 3,
        "! RetentionPercentage
        retention_percentage       TYPE p LENGTH 3 DECIMALS 2,
        "! RetentionDueDate
        retention_due_date         TYPE datn,
        "! SuplrInvcItmIsNotRlvtForRtntn
        suplr_invc_itm_is_not_rlvt TYPE abap_bool,
        "! ServiceEntrySheet
        service_entry_sheet        TYPE c LENGTH 10,
        "! ServiceEntrySheetItem
        service_entry_sheet_item   TYPE c LENGTH 10,
        "! TaxCountry
        tax_country                TYPE c LENGTH 3,
        "! IsFinallyInvoiced
        is_finally_invoiced        TYPE abap_bool,
        "! TaxDeterminationDate
        tax_determination_date     TYPE datn,
        "! IN_HSNOrSACCode
        in_hsnor_saccode           TYPE c LENGTH 16,
        "! IN_CustomDutyAssessableValue
        in_custom_duty_assessable  TYPE p LENGTH 8 DECIMALS 3,
      END OF tys_a_suplr_invc_item_pur_or_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvcItemPurOrdRefType</p>
      tyt_a_suplr_invc_item_pur_or_2 TYPE STANDARD TABLE OF tys_a_suplr_invc_item_pur_or_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvcSeldInbDeliveryNoteType</p>
      BEGIN OF tys_a_suplr_invc_seld_inb_de_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice      TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year           TYPE c LENGTH 4,
        "! <em>Key property</em> InboundDeliveryNote
        inbound_delivery_note TYPE c LENGTH 16,
      END OF tys_a_suplr_invc_seld_inb_de_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvcSeldInbDeliveryNoteType</p>
      tyt_a_suplr_invc_seld_inb_de_2 TYPE STANDARD TABLE OF tys_a_suplr_invc_seld_inb_de_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvcSeldPurgDocumentType</p>
      BEGIN OF tys_a_suplr_invc_seld_purg_d_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice    TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year         TYPE c LENGTH 4,
        "! <em>Key property</em> PurchaseOrder
        purchase_order      TYPE c LENGTH 10,
        "! <em>Key property</em> PurchaseOrderItem
        purchase_order_item TYPE c LENGTH 5,
      END OF tys_a_suplr_invc_seld_purg_d_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvcSeldPurgDocumentType</p>
      tyt_a_suplr_invc_seld_purg_d_2 TYPE STANDARD TABLE OF tys_a_suplr_invc_seld_purg_d_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvcSeldSrvcEntrShtLeanType</p>
      BEGIN OF tys_a_suplr_invc_seld_srvc_e_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice         TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year              TYPE c LENGTH 4,
        "! <em>Key property</em> ServiceEntrySheet
        service_entry_sheet      TYPE c LENGTH 10,
        "! <em>Key property</em> ServiceEntrySheetItem
        service_entry_sheet_item TYPE c LENGTH 5,
      END OF tys_a_suplr_invc_seld_srvc_e_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvcSeldSrvcEntrShtLeanType</p>
      tyt_a_suplr_invc_seld_srvc_e_2 TYPE STANDARD TABLE OF tys_a_suplr_invc_seld_srvc_e_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SuplrInvoiceAdditionalDataType</p>
      BEGIN OF tys_a_suplr_invoice_addition_2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! InvoicingPartyName1
        invoicing_party_name_1     TYPE c LENGTH 35,
        "! InvoicingPartyName2
        invoicing_party_name_2     TYPE c LENGTH 35,
        "! InvoicingPartyName3
        invoicing_party_name_3     TYPE c LENGTH 35,
        "! InvoicingPartyName4
        invoicing_party_name_4     TYPE c LENGTH 35,
        "! PostalCode
        postal_code                TYPE c LENGTH 10,
        "! CityName
        city_name                  TYPE c LENGTH 35,
        "! Country
        country                    TYPE c LENGTH 3,
        "! StreetAddressName
        street_address_name        TYPE c LENGTH 35,
        "! POBox
        pobox                      TYPE c LENGTH 10,
        "! POBoxPostalCode
        pobox_postal_code          TYPE c LENGTH 10,
        "! PostOfficeBankAccount
        post_office_bank_account   TYPE c LENGTH 16,
        "! BankAccount
        bank_account               TYPE c LENGTH 18,
        "! Bank
        bank                       TYPE c LENGTH 15,
        "! BankCountry
        bank_country               TYPE c LENGTH 3,
        "! TaxID1
        tax_id_1                   TYPE c LENGTH 16,
        "! TaxID2
        tax_id_2                   TYPE c LENGTH 11,
        "! TaxID3
        tax_id_3                   TYPE c LENGTH 18,
        "! TaxID4
        tax_id_4                   TYPE c LENGTH 18,
        "! TaxID5
        tax_id_5                   TYPE c LENGTH 60,
        "! OneTmeAccountIsVATLiable
        one_tme_account_is_vatliab TYPE abap_bool,
        "! OneTmeAcctIsEqualizationTxSubj
        one_tme_acct_is_equalizati TYPE abap_bool,
        "! Region
        region                     TYPE c LENGTH 3,
        "! BankControlKey
        bank_control_key           TYPE c LENGTH 2,
        "! DataExchangeInstructionKey
        data_exchange_instruction  TYPE c LENGTH 2,
        "! DataMediumExchangeIndicator
        data_medium_exchange_indic TYPE c LENGTH 1,
        "! LanguageCode
        language_code              TYPE c LENGTH 2,
        "! IsOneTimeAccount
        is_one_time_account        TYPE abap_bool,
        "! PaymentRecipient
        payment_recipient          TYPE c LENGTH 16,
        "! AccountTaxType
        account_tax_type           TYPE c LENGTH 2,
        "! TaxNumberType
        tax_number_type            TYPE c LENGTH 2,
        "! IsNaturalPerson
        is_natural_person          TYPE abap_bool,
        "! BankDetailReference
        bank_detail_reference      TYPE c LENGTH 20,
        "! RepresentativeName
        representative_name        TYPE c LENGTH 10,
        "! BusinessType
        business_type              TYPE c LENGTH 30,
        "! IndustryType
        industry_type              TYPE c LENGTH 30,
        "! FormOfAddressName
        form_of_address_name       TYPE c LENGTH 15,
        "! VATRegistration
        vatregistration            TYPE c LENGTH 20,
        "! OneTimeAcctCntrySpecificRef1
        one_time_acct_cntry_specif TYPE c LENGTH 140,
        "! IBAN
        iban                       TYPE c LENGTH 34,
        "! SWIFTCode
        swiftcode                  TYPE c LENGTH 11,
        "! OneTimeBusinessPartnerEmail
        one_time_business_partner  TYPE c LENGTH 130,
      END OF tys_a_suplr_invoice_addition_2,
      "! <p class="shorttext synchronized">List of A_SuplrInvoiceAdditionalDataType</p>
      tyt_a_suplr_invoice_addition_2 TYPE STANDARD TABLE OF tys_a_suplr_invoice_addition_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SupplierInvoiceItemAssetType</p>
      BEGIN OF tys_a_supplier_invoice_item__2,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> SupplierInvoiceItem
        supplier_invoice_item      TYPE c LENGTH 4,
        "! CompanyCode
        company_code               TYPE c LENGTH 4,
        "! MasterFixedAsset
        master_fixed_asset         TYPE c LENGTH 12,
        "! FixedAsset
        fixed_asset                TYPE c LENGTH 4,
        "! ProfitCenter
        profit_center              TYPE c LENGTH 10,
        "! GLAccount
        glaccount                  TYPE c LENGTH 10,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! SupplierInvoiceItemAmount
        supplier_invoice_item_amou TYPE p LENGTH 8 DECIMALS 3,
        "! TaxCode
        tax_code                   TYPE c LENGTH 2,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! TaxCountry
        tax_country                TYPE c LENGTH 3,
        "! TaxDeterminationDate
        tax_determination_date     TYPE datn,
        "! DebitCreditCode
        debit_credit_code          TYPE c LENGTH 1,
        "! SupplierInvoiceItemText
        supplier_invoice_item_text TYPE c LENGTH 50,
        "! AssignmentReference
        assignment_reference       TYPE c LENGTH 18,
        "! IsNotCashDiscountLiable
        is_not_cash_discount_liabl TYPE abap_bool,
        "! AssetValueDate
        asset_value_date           TYPE datn,
        "! QuantityUnit
        quantity_unit              TYPE c LENGTH 3,
        "! SuplrInvcItmQtyUnitSAPCode
        suplr_invc_itm_qty_unit_sa TYPE c LENGTH 3,
        "! SuplrInvcItmQtyUnitISOCode
        suplr_invc_itm_qty_unit_is TYPE c LENGTH 3,
        "! Quantity
        quantity                   TYPE p LENGTH 7 DECIMALS 3,
      END OF tys_a_supplier_invoice_item__2,
      "! <p class="shorttext synchronized">List of A_SupplierInvoiceItemAssetType</p>
      tyt_a_supplier_invoice_item__2 TYPE STANDARD TABLE OF tys_a_supplier_invoice_item__2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SupplierInvoiceItemGLAcctType</p>
      BEGIN OF tys_a_supplier_invoice_item__3,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> SupplierInvoiceItem
        supplier_invoice_item      TYPE c LENGTH 4,
        "! CompanyCode
        company_code               TYPE c LENGTH 4,
        "! CostCenter
        cost_center                TYPE c LENGTH 10,
        "! ControllingArea
        controlling_area           TYPE c LENGTH 4,
        "! BusinessArea
        business_area              TYPE c LENGTH 4,
        "! ProfitCenter
        profit_center              TYPE c LENGTH 10,
        "! FunctionalArea
        functional_area            TYPE c LENGTH 16,
        "! GLAccount
        glaccount                  TYPE c LENGTH 10,
        "! SalesOrder
        sales_order                TYPE c LENGTH 10,
        "! SalesOrderItem
        sales_order_item           TYPE c LENGTH 6,
        "! CostObject
        cost_object                TYPE c LENGTH 12,
        "! CostCtrActivityType
        cost_ctr_activity_type     TYPE c LENGTH 6,
        "! BusinessProcess
        business_process           TYPE c LENGTH 12,
        "! WBSElement
        wbselement                 TYPE c LENGTH 24,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! SupplierInvoiceItemAmount
        supplier_invoice_item_amou TYPE p LENGTH 8 DECIMALS 3,
        "! TaxCode
        tax_code                   TYPE c LENGTH 2,
        "! PersonnelNumber
        personnel_number           TYPE c LENGTH 8,
        "! WorkItem
        work_item                  TYPE c LENGTH 10,
        "! DebitCreditCode
        debit_credit_code          TYPE c LENGTH 1,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! SupplierInvoiceItemText
        supplier_invoice_item_text TYPE c LENGTH 50,
        "! AssignmentReference
        assignment_reference       TYPE c LENGTH 18,
        "! IsNotCashDiscountLiable
        is_not_cash_discount_liabl TYPE abap_bool,
        "! InternalOrder
        internal_order             TYPE c LENGTH 12,
        "! ProjectNetwork
        project_network            TYPE c LENGTH 12,
        "! NetworkActivity
        network_activity           TYPE c LENGTH 4,
        "! CommitmentItem
        commitment_item            TYPE c LENGTH 24,
        "! FundsCenter
        funds_center               TYPE c LENGTH 16,
        "! TaxBaseAmountInTransCrcy
        tax_base_amount_in_trans_c TYPE p LENGTH 8 DECIMALS 3,
        "! Fund
        fund                       TYPE c LENGTH 10,
        "! GrantID
        grant_id                   TYPE c LENGTH 20,
        "! QuantityUnit
        quantity_unit              TYPE c LENGTH 3,
        "! SuplrInvcItmQtyUnitSAPCode
        suplr_invc_itm_qty_unit_sa TYPE c LENGTH 3,
        "! SuplrInvcItmQtyUnitISOCode
        suplr_invc_itm_qty_unit_is TYPE c LENGTH 3,
        "! Quantity
        quantity                   TYPE p LENGTH 7 DECIMALS 3,
        "! PartnerBusinessArea
        partner_business_area      TYPE c LENGTH 4,
        "! FinancialTransactionType
        financial_transaction_type TYPE c LENGTH 3,
        "! TaxCountry
        tax_country                TYPE c LENGTH 3,
        "! EarmarkedFundsDocument
        earmarked_funds_document   TYPE c LENGTH 10,
        "! EarmarkedFundsDocumentItem
        earmarked_funds_document_i TYPE c LENGTH 3,
        "! BudgetPeriod
        budget_period              TYPE c LENGTH 10,
      END OF tys_a_supplier_invoice_item__3,
      "! <p class="shorttext synchronized">List of A_SupplierInvoiceItemGLAcctType</p>
      tyt_a_supplier_invoice_item__3 TYPE STANDARD TABLE OF tys_a_supplier_invoice_item__3 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SupplierInvoiceItemMaterialType</p>
      BEGIN OF tys_a_supplier_invoice_item__4,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> SupplierInvoiceItem
        supplier_invoice_item      TYPE c LENGTH 6,
        "! Material
        material                   TYPE c LENGTH 40,
        "! ValuationArea
        valuation_area             TYPE c LENGTH 4,
        "! CompanyCode
        company_code               TYPE c LENGTH 4,
        "! Plant
        plant                      TYPE c LENGTH 4,
        "! InventoryValuationType
        inventory_valuation_type   TYPE c LENGTH 10,
        "! TaxCode
        tax_code                   TYPE c LENGTH 2,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! TaxCountry
        tax_country                TYPE c LENGTH 3,
        "! TaxDeterminationDate
        tax_determination_date     TYPE datn,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! SupplierInvoiceItemAmount
        supplier_invoice_item_amou TYPE p LENGTH 8 DECIMALS 3,
        "! QuantityUnit
        quantity_unit              TYPE c LENGTH 3,
        "! SuplrInvcItmQtyUnitSAPCode
        suplr_invc_itm_qty_unit_sa TYPE c LENGTH 3,
        "! SuplrInvcItmQtyUnitISOCode
        suplr_invc_itm_qty_unit_is TYPE c LENGTH 3,
        "! Quantity
        quantity                   TYPE p LENGTH 7 DECIMALS 3,
        "! DebitCreditCode
        debit_credit_code          TYPE c LENGTH 1,
        "! IsNotCashDiscountLiable
        is_not_cash_discount_liabl TYPE abap_bool,
      END OF tys_a_supplier_invoice_item__4,
      "! <p class="shorttext synchronized">List of A_SupplierInvoiceItemMaterialType</p>
      tyt_a_supplier_invoice_item__4 TYPE STANDARD TABLE OF tys_a_supplier_invoice_item__4 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SupplierInvoiceTaxType</p>
      BEGIN OF tys_a_supplier_invoice_tax_typ,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! <em>Key property</em> TaxCode
        tax_code                   TYPE c LENGTH 2,
        "! <em>Key property</em> SupplierInvoiceTaxCounter
        supplier_invoice_tax_count TYPE c LENGTH 6,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! TaxAmount
        tax_amount                 TYPE p LENGTH 8 DECIMALS 3,
        "! TaxBaseAmountInTransCrcy
        tax_base_amount_in_trans_c TYPE p LENGTH 9 DECIMALS 3,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! TaxCountry
        tax_country                TYPE c LENGTH 3,
        "! TaxDeterminationDate
        tax_determination_date     TYPE datn,
        "! TaxRateValidityStartDate
        tax_rate_validity_start_da TYPE datn,
      END OF tys_a_supplier_invoice_tax_typ,
      "! <p class="shorttext synchronized">List of A_SupplierInvoiceTaxType</p>
      tyt_a_supplier_invoice_tax_typ TYPE STANDARD TABLE OF tys_a_supplier_invoice_tax_typ WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SupplierInvoiceType</p>
      BEGIN OF tys_a_supplier_invoice_type,
        "! <em>Key property</em> SupplierInvoice
        supplier_invoice           TYPE c LENGTH 10,
        "! <em>Key property</em> FiscalYear
        fiscal_year                TYPE c LENGTH 4,
        "! CompanyCode
        company_code               TYPE c LENGTH 4,
        "! DocumentDate
        document_date              TYPE datn,
        "! PostingDate
        posting_date               TYPE datn,
        "! CreationDate
        creation_date              TYPE datn,
        "! SupplierInvoiceIDByInvcgParty
        supplier_invoice_idby_invc TYPE c LENGTH 16,
        "! InvoicingParty
        invoicing_party            TYPE c LENGTH 10,
        "! DocumentCurrency
        document_currency          TYPE c LENGTH 5,
        "! InvoiceGrossAmount
        invoice_gross_amount       TYPE p LENGTH 8 DECIMALS 3,
        "! UnplannedDeliveryCost
        unplanned_delivery_cost    TYPE p LENGTH 8 DECIMALS 3,
        "! DocumentHeaderText
        document_header_text       TYPE c LENGTH 25,
        "! ManualCashDiscount
        manual_cash_discount       TYPE p LENGTH 8 DECIMALS 3,
        "! PaymentTerms
        payment_terms              TYPE c LENGTH 4,
        "! DueCalculationBaseDate
        due_calculation_base_date  TYPE datn,
        "! CashDiscount1Percent
        cash_discount_1_percent    TYPE p LENGTH 3 DECIMALS 3,
        "! CashDiscount1Days
        cash_discount_1_days       TYPE p LENGTH 2 DECIMALS 0,
        "! CashDiscount2Percent
        cash_discount_2_percent    TYPE p LENGTH 3 DECIMALS 3,
        "! CashDiscount2Days
        cash_discount_2_days       TYPE p LENGTH 2 DECIMALS 0,
        "! NetPaymentDays
        net_payment_days           TYPE p LENGTH 2 DECIMALS 0,
        "! PaymentBlockingReason
        payment_blocking_reason    TYPE c LENGTH 1,
        "! AccountingDocumentType
        accounting_document_type   TYPE c LENGTH 2,
        "! BPBankAccountInternalID
        bpbank_account_internal_id TYPE c LENGTH 4,
        "! SupplierInvoiceStatus
        supplier_invoice_status    TYPE c LENGTH 1,
        "! IndirectQuotedExchangeRate
        indirect_quoted_exchange_r TYPE p LENGTH 5 DECIMALS 5,
        "! DirectQuotedExchangeRate
        direct_quoted_exchange_rat TYPE p LENGTH 5 DECIMALS 5,
        "! StateCentralBankPaymentReason
        state_central_bank_payment TYPE c LENGTH 3,
        "! SupplyingCountry
        supplying_country          TYPE c LENGTH 3,
        "! PaymentMethod
        payment_method             TYPE c LENGTH 1,
        "! PaymentMethodSupplement
        payment_method_supplement  TYPE c LENGTH 2,
        "! PaymentReference
        payment_reference          TYPE c LENGTH 30,
        "! InvoiceReference
        invoice_reference          TYPE c LENGTH 10,
        "! InvoiceReferenceFiscalYear
        invoice_reference_fiscal_y TYPE c LENGTH 4,
        "! FixedCashDiscount
        fixed_cash_discount        TYPE c LENGTH 1,
        "! UnplannedDeliveryCostTaxCode
        unplanned_delivery_cost_ta TYPE c LENGTH 2,
        "! UnplndDelivCostTaxJurisdiction
        unplnd_deliv_cost_tax_juri TYPE c LENGTH 15,
        "! UnplndDeliveryCostTaxCountry
        unplnd_delivery_cost_tax_c TYPE c LENGTH 3,
        "! AssignmentReference
        assignment_reference       TYPE c LENGTH 18,
        "! SupplierPostingLineItemText
        supplier_posting_line_item TYPE c LENGTH 50,
        "! TaxIsCalculatedAutomatically
        tax_is_calculated_automati TYPE abap_bool,
        "! BusinessPlace
        business_place             TYPE c LENGTH 4,
        "! BusinessSectionCode
        business_section_code      TYPE c LENGTH 4,
        "! BusinessArea
        business_area              TYPE c LENGTH 4,
        "! SuplrInvcIsCapitalGoodsRelated
        suplr_invc_is_capital_good TYPE abap_bool,
        "! SupplierInvoiceIsCreditMemo
        supplier_invoice_is_credit TYPE c LENGTH 1,
        "! PaytSlipWthRefSubscriber
        payt_slip_wth_ref_subscrib TYPE c LENGTH 11,
        "! PaytSlipWthRefCheckDigit
        payt_slip_wth_ref_check_di TYPE c LENGTH 2,
        "! PaytSlipWthRefReference
        payt_slip_wth_ref_referenc TYPE c LENGTH 27,
        "! TaxDeterminationDate
        tax_determination_date     TYPE datn,
        "! TaxReportingDate
        tax_reporting_date         TYPE datn,
        "! TaxFulfillmentDate
        tax_fulfillment_date       TYPE datn,
        "! InvoiceReceiptDate
        invoice_receipt_date       TYPE datn,
        "! DeliveryOfGoodsReportingCntry
        delivery_of_goods_reportin TYPE c LENGTH 3,
        "! SupplierVATRegistration
        supplier_vatregistration   TYPE c LENGTH 20,
        "! IsEUTriangularDeal
        is_eutriangular_deal       TYPE abap_bool,
        "! SuplrInvcDebitCrdtCodeDelivery
        suplr_invc_debit_crdt_code TYPE c LENGTH 1,
        "! SuplrInvcDebitCrdtCodeReturns
        suplr_invc_debit_crdt_co_2 TYPE c LENGTH 1,
        "! RetentionDueDate
        retention_due_date         TYPE datn,
        "! PaymentReason
        payment_reason             TYPE c LENGTH 4,
        "! HouseBank
        house_bank                 TYPE c LENGTH 5,
        "! HouseBankAccount
        house_bank_account         TYPE c LENGTH 5,
        "! SupplierInvoiceOrigin
        supplier_invoice_origin    TYPE c LENGTH 1,
        "! ReverseDocument
        reverse_document           TYPE c LENGTH 10,
        "! ReverseDocumentFiscalYear
        reverse_document_fiscal_ye TYPE c LENGTH 4,
        "! IsReversal
        is_reversal                TYPE abap_bool,
        "! IsReversed
        is_reversed                TYPE abap_bool,
        "! IN_GSTPartner
        in_gstpartner              TYPE c LENGTH 10,
        "! IN_GSTPlaceOfSupply
        in_gstplace_of_supply      TYPE c LENGTH 3,
        "! IN_InvoiceReferenceNumber
        in_invoice_reference_numbe TYPE c LENGTH 64,
        "! JrnlEntryCntrySpecificRef1
        jrnl_entry_cntry_specific  TYPE c LENGTH 80,
        "! JrnlEntryCntrySpecificDate1
        jrnl_entry_cntry_specifi_2 TYPE datn,
        "! JrnlEntryCntrySpecificRef2
        jrnl_entry_cntry_specifi_3 TYPE c LENGTH 25,
        "! JrnlEntryCntrySpecificDate2
        jrnl_entry_cntry_specifi_4 TYPE datn,
        "! JrnlEntryCntrySpecificRef3
        jrnl_entry_cntry_specifi_5 TYPE c LENGTH 25,
        "! JrnlEntryCntrySpecificDate3
        jrnl_entry_cntry_specifi_6 TYPE datn,
        "! JrnlEntryCntrySpecificRef4
        jrnl_entry_cntry_specifi_7 TYPE c LENGTH 50,
        "! JrnlEntryCntrySpecificDate4
        jrnl_entry_cntry_specifi_8 TYPE datn,
        "! JrnlEntryCntrySpecificRef5
        jrnl_entry_cntry_specifi_9 TYPE c LENGTH 50,
        "! JrnlEntryCntrySpecificDate5
        jrnl_entry_cntry_specif_10 TYPE datn,
        "! JrnlEntryCntrySpecificBP1
        jrnl_entry_cntry_specif_11 TYPE c LENGTH 10,
        "! JrnlEntryCntrySpecificBP2
        jrnl_entry_cntry_specif_12 TYPE c LENGTH 10,
      END OF tys_a_supplier_invoice_type,
      "! <p class="shorttext synchronized">List of A_SupplierInvoiceType</p>
      tyt_a_supplier_invoice_type TYPE STANDARD TABLE OF tys_a_supplier_invoice_type WITH DEFAULT KEY.


    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the entity sets</p>
      BEGIN OF gcs_entity_set,
        "! A_SuplrInvcHeaderWhldgTax
        "! <br/> Collection of type 'A_SuplrInvcHeaderWhldgTaxType'
        a_suplr_invc_header_whldg  TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVC_HEADER_WHLDG',
        "! A_SuplrInvcItemAcctAssgmt
        "! <br/> Collection of type 'A_SuplrInvcItemAcctAssgmtType'
        a_suplr_invc_item_acct_ass TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVC_ITEM_ACCT_ASS',
        "! A_SuplrInvcItemPurOrdRef
        "! <br/> Collection of type 'A_SuplrInvcItemPurOrdRefType'
        a_suplr_invc_item_pur_ord  TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVC_ITEM_PUR_ORD',
        "! A_SuplrInvcSeldInbDeliveryNote
        "! <br/> Collection of type 'A_SuplrInvcSeldInbDeliveryNoteType'
        a_suplr_invc_seld_inb_deli TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVC_SELD_INB_DELI',
        "! A_SuplrInvcSeldPurgDocument
        "! <br/> Collection of type 'A_SuplrInvcSeldPurgDocumentType'
        a_suplr_invc_seld_purg_doc TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVC_SELD_PURG_DOC',
        "! A_SuplrInvcSeldSrvcEntrShtLean
        "! <br/> Collection of type 'A_SuplrInvcSeldSrvcEntrShtLeanType'
        a_suplr_invc_seld_srvc_ent TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVC_SELD_SRVC_ENT',
        "! A_SuplrInvoiceAdditionalData
        "! <br/> Collection of type 'A_SuplrInvoiceAdditionalDataType'
        a_suplr_invoice_additional TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPLR_INVOICE_ADDITIONAL',
        "! A_SupplierInvoice
        "! <br/> Collection of type 'A_SupplierInvoiceType'
        a_supplier_invoice         TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPPLIER_INVOICE',
        "! A_SupplierInvoiceItemAsset
        "! <br/> Collection of type 'A_SupplierInvoiceItemAssetType'
        a_supplier_invoice_item_as TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPPLIER_INVOICE_ITEM_AS',
        "! A_SupplierInvoiceItemGLAcct
        "! <br/> Collection of type 'A_SupplierInvoiceItemGLAcctType'
        a_supplier_invoice_item_gl TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPPLIER_INVOICE_ITEM_GL',
        "! A_SupplierInvoiceItemMaterial
        "! <br/> Collection of type 'A_SupplierInvoiceItemMaterialType'
        a_supplier_invoice_item_ma TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPPLIER_INVOICE_ITEM_MA',
        "! A_SupplierInvoiceTax
        "! <br/> Collection of type 'A_SupplierInvoiceTaxType'
        a_supplier_invoice_tax     TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SUPPLIER_INVOICE_TAX',
      END OF gcs_entity_set .

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the function imports</p>
      BEGIN OF gcs_function_import,
        "! Cancel
        "! <br/> See structure type {@link ..tys_parameters_1} for the parameters
        cancel  TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'CANCEL',
        "! Post
        "! <br/> See structure type {@link ..tys_parameters_2} for the parameters
        post    TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'POST',
        "! Release
        "! <br/> See structure type {@link ..tys_parameters_3} for the parameters
        release TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'RELEASE',
      END OF gcs_function_import.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the bound functions</p>
      BEGIN OF gcs_bound_function,
         "! Dummy field - Structure must not be empty
         dummy TYPE int1 VALUE 0,
      END OF gcs_bound_function.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal names for complex types</p>
      BEGIN OF gcs_complex_type,
        "! <p class="shorttext synchronized">Internal names for CancelInvoiceExportParameters</p>
        "! See also structure type {@link ..tys_cancel_invoice_export_para}
        BEGIN OF cancel_invoice_export_para,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF cancel_invoice_export_para,
        "! <p class="shorttext synchronized">Internal names for PostInvoiceExportParameters</p>
        "! See also structure type {@link ..tys_post_invoice_export_parame}
        BEGIN OF post_invoice_export_parame,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF post_invoice_export_parame,
        "! <p class="shorttext synchronized">Internal names for ReleaseInvoiceExportParameters</p>
        "! See also structure type {@link ..tys_release_invoice_export_par}
        BEGIN OF release_invoice_export_par,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF release_invoice_export_par,
      END OF gcs_complex_type.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal names for entity types</p>
      BEGIN OF gcs_entity_type,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvcHeaderWhldgTaxType</p>
        "! See also structure type {@link ..tys_a_suplr_invc_header_whld_2}
        BEGIN OF a_suplr_invc_header_whld_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_suplr_invc_header_whld_2,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvcItemAcctAssgmtType</p>
        "! See also structure type {@link ..tys_a_suplr_invc_item_acct_a_2}
        BEGIN OF a_suplr_invc_item_acct_a_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_suplr_invc_item_acct_a_2,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvcItemPurOrdRefType</p>
        "! See also structure type {@link ..tys_a_suplr_invc_item_pur_or_2}
        BEGIN OF a_suplr_invc_item_pur_or_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_SupplierInvoiceItmAcctAssgmt
            to_supplier_invoice_itm_ac TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPPLIER_INVOICE_ITM_AC',
          END OF navigation,
        END OF a_suplr_invc_item_pur_or_2,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvcSeldInbDeliveryNoteType</p>
        "! See also structure type {@link ..tys_a_suplr_invc_seld_inb_de_2}
        BEGIN OF a_suplr_invc_seld_inb_de_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_suplr_invc_seld_inb_de_2,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvcSeldPurgDocumentType</p>
        "! See also structure type {@link ..tys_a_suplr_invc_seld_purg_d_2}
        BEGIN OF a_suplr_invc_seld_purg_d_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_suplr_invc_seld_purg_d_2,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvcSeldSrvcEntrShtLeanType</p>
        "! See also structure type {@link ..tys_a_suplr_invc_seld_srvc_e_2}
        BEGIN OF a_suplr_invc_seld_srvc_e_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_suplr_invc_seld_srvc_e_2,
        "! <p class="shorttext synchronized">Internal names for A_SuplrInvoiceAdditionalDataType</p>
        "! See also structure type {@link ..tys_a_suplr_invoice_addition_2}
        BEGIN OF a_suplr_invoice_addition_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_suplr_invoice_addition_2,
        "! <p class="shorttext synchronized">Internal names for A_SupplierInvoiceItemAssetType</p>
        "! See also structure type {@link ..tys_a_supplier_invoice_item__2}
        BEGIN OF a_supplier_invoice_item__2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_supplier_invoice_item__2,
        "! <p class="shorttext synchronized">Internal names for A_SupplierInvoiceItemGLAcctType</p>
        "! See also structure type {@link ..tys_a_supplier_invoice_item__3}
        BEGIN OF a_supplier_invoice_item__3,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_supplier_invoice_item__3,
        "! <p class="shorttext synchronized">Internal names for A_SupplierInvoiceItemMaterialType</p>
        "! See also structure type {@link ..tys_a_supplier_invoice_item__4}
        BEGIN OF a_supplier_invoice_item__4,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_supplier_invoice_item__4,
        "! <p class="shorttext synchronized">Internal names for A_SupplierInvoiceTaxType</p>
        "! See also structure type {@link ..tys_a_supplier_invoice_tax_typ}
        BEGIN OF a_supplier_invoice_tax_typ,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_supplier_invoice_tax_typ,
        "! <p class="shorttext synchronized">Internal names for A_SupplierInvoiceType</p>
        "! See also structure type {@link ..tys_a_supplier_invoice_type}
        BEGIN OF a_supplier_invoice_type,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_SelectedDeliveryNotes
            to_selected_delivery_notes TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SELECTED_DELIVERY_NOTES',
            "! to_SelectedPurchaseOrders
            to_selected_purchase_order TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SELECTED_PURCHASE_ORDER',
            "! to_SelectedServiceEntrySheets
            to_selected_service_entry  TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SELECTED_SERVICE_ENTRY',
            "! to_SuplrInvcItemAsset
            to_suplr_invc_item_asset   TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPLR_INVC_ITEM_ASSET',
            "! to_SuplrInvcItemMaterial
            to_suplr_invc_item_materia TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPLR_INVC_ITEM_MATERIA',
            "! to_SuplrInvcItemPurOrdRef
            to_suplr_invc_item_pur_ord TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPLR_INVC_ITEM_PUR_ORD',
            "! to_SuplrInvoiceAdditionalData
            to_suplr_invoice_additiona TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPLR_INVOICE_ADDITIONA',
            "! to_SupplierInvoiceItemGLAcct
            to_supplier_invoice_item_g TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPPLIER_INVOICE_ITEM_G',
            "! to_SupplierInvoiceTax
            to_supplier_invoice_tax    TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPPLIER_INVOICE_TAX',
            "! to_SupplierInvoiceWhldgTax
            to_supplier_invoice_whldg  TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SUPPLIER_INVOICE_WHLDG',
          END OF navigation,
        END OF a_supplier_invoice_type,
      END OF gcs_entity_type.


    METHODS /iwbep/if_v4_mp_basic_pm~define REDEFINITION.

