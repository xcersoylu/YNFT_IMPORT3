  METHOD if_http_service_extension~handle_request.

    TYPES BEGIN OF ty_business_data.
    INCLUDE TYPE ycl_nft_supplier_invoice=>tys_a_supplier_invoice_type.
    TYPES to_suplr_invc_item_pur_ord TYPE ycl_nft_supplier_invoice=>tyt_a_suplr_invc_item_pur_or_2.
    TYPES to_supplier_invoice_item_g TYPE ycl_nft_supplier_invoice=>tyt_a_supplier_invoice_item__3.
    TYPES END OF ty_business_data.

    DATA: ls_supplier        TYPE ty_business_data,
          ls_supplier_return TYPE ty_business_data,
          lo_http_client     TYPE REF TO if_web_http_client,
          lo_client_proxy    TYPE REF TO /iwbep/if_cp_client_proxy,
          lo_request         TYPE REF TO /iwbep/if_cp_request_create,
          lo_response        TYPE REF TO /iwbep/if_cp_response_create,
          lt_header_pro      TYPE TABLE OF string,
          lt_gl_pro          TYPE TABLE OF string,
          lt_po_pro          TYPE TABLE OF string,
          lt_r005            TYPE TABLE OF ynft_t_r005.

    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).

    DATA(lv_company_code) = VALUE #( ms_request-items[ 1 ]-companycode OPTIONAL ).
    SELECT SINGLE *
      FROM ynft_t_t011
      WHERE bukrs = @lv_company_code
      INTO @DATA(ls_parameter).

    SELECT
      FROM ynft_t_parameter
      FIELDS *
      WHERE parametername = 'SUPPLIER_INVOICE'
      INTO TABLE @DATA(lt_parameters).

    SELECT
      FROM @ms_request-items AS selected_lines
      INNER JOIN ynft_t_r005 AS r005 ON r005~companycode            = selected_lines~companycode
                                    AND r005~accountingdocument     = selected_lines~accountingdocument
                                    AND r005~fiscalyear             = selected_lines~fiscalyear
                                    AND r005~accountingdocumentitem = selected_lines~accountingdocumentitem
     FIELDS r005~companycode            AS companycode,
            r005~accountingdocument     AS accountingdocument,
            r005~fiscalyear             AS fiscalyear,
            r005~accountingdocumentitem AS accountingdocumentitem,
            MAX( r005~referenceitem ) AS referenceitem
     GROUP BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem
     ORDER BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem
     INTO TABLE @DATA(lt_r005_check).

    APPEND 'COMPANY_CODE'               TO lt_header_pro.
    APPEND 'DOCUMENT_DATE'              TO lt_header_pro.
    APPEND 'POSTING_DATE'               TO lt_header_pro.
    APPEND 'SUPPLIER_INVOICE_IDBY_INVC' TO lt_header_pro.
    APPEND 'INVOICING_PARTY'            TO lt_header_pro.
    APPEND 'DOCUMENT_CURRENCY'          TO lt_header_pro.
    APPEND 'DOCUMENT_HEADER_TEXT'       TO lt_header_pro.
    APPEND 'ACCOUNTING_DOCUMENT_TYPE'   TO lt_header_pro.
    APPEND 'TAX_DETERMINATION_DATE'     TO lt_header_pro.
    APPEND 'SUPPLIER_INVOICE_IS_CREDIT' TO lt_header_pro.
    APPEND 'PAYMENT_TERMS'              TO lt_header_pro.

    APPEND 'SUPPLIER_INVOICE_ITEM'      TO lt_gl_pro.
    APPEND 'DEBIT_CREDIT_CODE'          TO lt_gl_pro.
    APPEND 'GLACCOUNT'                  TO lt_gl_pro.
    APPEND 'COMPANY_CODE'               TO lt_gl_pro.
    APPEND 'TAX_CODE'                   TO lt_gl_pro.
    APPEND 'DOCUMENT_CURRENCY'          TO lt_gl_pro.
    APPEND 'SUPPLIER_INVOICE_ITEM_AMOU' TO lt_gl_pro.

    APPEND 'SUPPLIER_INVOICE_ITEM'      TO lt_po_pro.
    APPEND 'PURCHASE_ORDER'             TO lt_po_pro.
    APPEND 'PURCHASE_ORDER_ITEM'        TO lt_po_pro.
    APPEND 'DOCUMENT_CURRENCY'          TO lt_po_pro.
    APPEND 'SUPPLIER_INVOICE_ITEM_AMOU' TO lt_po_pro.
    APPEND 'PURCHASE_ORDER_PRICE_UNIT'  TO lt_po_pro.
    APPEND 'PURCHASE_ORDER_QUANTITY_UN' TO lt_po_pro.
    APPEND 'QUANTITY_IN_PURCHASE_ORDER' TO lt_po_pro.
    APPEND 'TAX_CODE'                   TO lt_po_pro.
    APPEND 'IS_SUBSEQUENT_DEBIT_CREDIT' TO lt_po_pro.

    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                     comm_scenario  = VALUE #( lt_parameters[ parameterkey = 'COMM_SCENARIO' ]-value OPTIONAL )
                                                     comm_system_id = VALUE #( lt_parameters[ parameterkey = 'COMM_SYSTEM_ID' ]-value OPTIONAL )
                                                     service_id     = VALUE #( lt_parameters[ parameterkey = 'SERVICE_ID' ]-value OPTIONAL ) ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING
             is_proxy_model_key      = VALUE #( repository_id       = 'DEFAULT'
                                                proxy_model_id      = 'YSCM_NFT_SUPPLIER_INVOICE'
                                                proxy_model_version = '0001' )
            io_http_client           = lo_http_client
            iv_relative_service_root = '/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV' ).
        ASSERT lo_http_client IS BOUND.
        LOOP AT ms_request-items INTO DATA(ls_selected_line).
          CLEAR ls_supplier.
          ls_supplier =  VALUE #( company_code                = ls_selected_line-companycode
                                  document_date               = ms_request-documentdate
                                  posting_date                = ms_request-postingdate
                                  supplier_invoice_idby_invc  = ls_selected_line-accountingdocument && ls_selected_line-accountingdocumentitem
                                  invoicing_party             = ls_selected_line-supplier
                                  document_currency           = ls_selected_line-documentcurrency
                                  document_header_text        = ls_selected_line-deliverydocument && 'Dosya Kapama'
                                  accounting_document_type    = ls_parameter-blart
                                  tax_determination_date      = ls_selected_line-postingdate
                                  supplier_invoice_is_credit  = ''
                                  payment_terms               = ls_parameter-zterm
                                  to_suplr_invc_item_pur_ord  = VALUE #( ( supplier_invoice_item      = '0001'
                                                                           purchase_order             = ls_selected_line-referencesddocument
                                                                           purchase_order_item        = ls_selected_line-referencesddocumentitem+1(*)
                                                                           document_currency          = ls_selected_line-documentcurrency
                                                                           supplier_invoice_item_amou = ls_selected_line-documentcurrenyamount
                                                                           purchase_order_price_unit  = ls_selected_line-deliveryquantityunit
                                                                           purchase_order_quantity_un = ls_selected_line-deliveryquantityunit
                                                                           quantity_in_purchase_order = ls_selected_line-deliveryquantity
                                                                           tax_code                   = ls_parameter-mwskz
                                                                           is_subsequent_debit_credit = '' ) )
                                  to_supplier_invoice_item_g  = VALUE #( ( supplier_invoice_item      = '0001'
                                                                           debit_credit_code          = 'H'
                                                                           glaccount                  = ls_selected_line-accountnumber
                                                                           company_code               = ls_selected_line-companycode
                                                                           tax_code                   = ls_parameter-mwskz
                                                                           document_currency          = ls_selected_line-documentcurrency
                                                                           supplier_invoice_item_amou = ls_selected_line-documentcurrenyamount ) ) ).

          TRY.
              lo_request = lo_client_proxy->create_resource_for_entity_set( 'A_SUPPLIER_INVOICE' )->create_request_for_create( ).
              DATA(lo_data_description_node) = lo_request->create_data_descripton_node( ).
              lo_data_description_node->set_properties( lt_header_pro  ).
              DATA(lo_item_child) = lo_data_description_node->add_child( 'TO_SUPLR_INVC_ITEM_PUR_ORD' ).
              lo_item_child->set_properties( lt_po_pro ).
              DATA(lo_item_child2) = lo_data_description_node->add_child( 'TO_SUPPLIER_INVOICE_ITEM_G' ).
              lo_item_child2->set_properties( lt_gl_pro ).
              lo_request->set_deep_business_data( is_business_data = ls_supplier
                                                  io_data_description = lo_data_description_node ).
              lo_response = lo_request->execute( ).
              lo_response->get_business_data( IMPORTING es_business_data = ls_supplier_return ).
            CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
              DATA(lv_error)               = lx_remote->if_message~get_longtext(  ).
              DATA(lv_error_body)          = lx_remote->http_error_body.
              DATA(ls_odata_error)         = lx_remote->s_odata_error.
              DATA(lv_http_status_message) = lx_remote->http_status_message.
              APPEND lv_error TO ms_response-error_messages.
          ENDTRY.
          IF ls_supplier_return-supplier_invoice IS NOT INITIAL.
            READ TABLE lt_r005_check INTO DATA(ls_r005_check) WITH KEY companycode            = ls_selected_line-companycode
                                                                       accountingdocument     = ls_selected_line-accountingdocument
                                                                       fiscalyear             = ls_selected_line-fiscalyear
                                                                       accountingdocumentitem = ls_selected_line-accountingdocumentitem BINARY SEARCH.

            APPEND VALUE #( companycode             = ls_selected_line-companycode
                            accountingdocument      = ls_selected_line-accountingdocument
                            fiscalyear              = ls_selected_line-fiscalyear
                            accountingdocumentitem  = ls_selected_line-accountingdocumentitem
                            referenceitem           = ls_r005_check-referenceitem + 1
                            deliverydocument        = |{ ls_selected_line-deliverydocument ALPHA = IN }|
                            deliverydocumentitem    = ls_selected_line-deliverydocumentitem
                            quantity                = ls_selected_line-deliveryquantity
                            quantityunit            = ls_selected_line-deliveryquantityunit
                            amount                  = ls_selected_line-documentcurrenyamount
                            currency                = ls_selected_line-documentcurrency
                            accountingdocument_inv  = ls_supplier_return-supplier_invoice
                            fiscalyear_inv          = ls_supplier_return-fiscal_year ) TO lt_r005.
            ms_response-supplierinvoice = ls_supplier_return-supplier_invoice.
            MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
               TYPE 'S'
               NUMBER 016
               WITH ls_supplier_return-supplier_invoice INTO DATA(lv_message).
            APPEND lv_message TO ms_response-error_messages.
            FREE: lo_request, lo_item_child, lo_item_child2, lo_response.
            CLEAR: ls_r005_check, lv_message.
          ENDIF.
        ENDLOOP.
        IF lt_r005 IS NOT INITIAL.
          MODIFY ynft_t_r005 FROM TABLE @lt_r005.
        ENDIF.

      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        lv_error = lx_gateway->if_message~get_longtext(  ).
        APPEND lv_error TO ms_response-error_messages.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        lv_error = lx_web_http_client_error->if_message~get_longtext(  ).
        APPEND lv_error TO ms_response-error_messages.
      CATCH cx_http_dest_provider_error INTO DATA(lx_provider_error).
        lv_error = lx_provider_error->if_message~get_longtext(  ).
        APPEND lv_error TO ms_response-error_messages.
    ENDTRY.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).

  ENDMETHOD.