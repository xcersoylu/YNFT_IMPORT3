  METHOD if_http_service_extension~handle_request.
    DATA(lv_request_body) = request->get_text( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).

    SELECT SINGLE *
      FROM ynft_t_r001
      WHERE companycode        = @ms_request-companycode
        AND accountingdocument = @ms_request-accountingdocument
        AND fiscalyear         = @ms_request-fiscalyear
      INTO @DATA(ls_r001).

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM I_JournalEntry
        WHERE companycode        = @ms_request-companycode
          AND fiscalyear         = @ms_request-fiscalyear
          AND accountingdocument = @ms_request-accountingdocument
        INTO @DATA(ls_bkpf).

      SELECT r002~DeliveryDocument,
             r002~DeliveryDocumentitem,
             r002~costtype,
             ctype~description AS costtypetext,
             r002~documentcurrenyamount,
             r002~taxcode
       FROM ynft_t_r002 AS r002
       LEFT OUTER JOIN yvh_nft_ctype AS ctype ON ctype~ctype = r002~costtype
       WHERE companycode        = @ms_request-companycode
         AND accountingdocument = @ms_request-accountingdocument
         AND fiscalyear         = @ms_request-fiscalyear
       INTO TABLE @DATA(lt_r002).

      SELECT
       FROM yi_nft_ddl_imp_cost_data AS data
       INNER JOIN @lt_r002 AS r002 ON r002~deliverydocument     EQ data~deliverydocument
                                  AND r002~deliverydocumentitem EQ data~deliverydocumentitem
       FIELDS data~deliverydocument,
              data~deliverydocumentitem,
              data~purchaseorder,
              data~purchaseorderitem,
              data~material,
              data~materialtext,
              data~deliveryquantity,
              data~salesunit,
              data~vendor,
              data~vendorname,
              data~orderquantity,
              data~documentcurrency,
              data~profitcenter,
              data~netvalue,
              data~unitofmeasure,
              data~netpriceamount,
              data~documenttype
        INTO TABLE @DATA(lt_cost_list).

      SELECT LedgerGLLineItem,
             FinancialAccountType,
             AmountIntransactionCurrency,
             DocumentItemText,
             TransactionTypeDetermination
        FROM I_JournalEntryItem
        WHERE companycode        = @ms_request-companycode
          AND fiscalyear         = @ms_request-fiscalyear
          AND accountingdocument = @ms_request-accountingdocument
          AND ledger             = '0L'
        INTO TABLE @DATA(lt_jornalitem).

      "HEADER verilerini doldur
      ms_response-header-companycode                  = ls_r001-companycode.
      ms_response-header-costsource                   = ls_r001-costsource.
      ms_response-header-vorgang                      = ls_r001-vorgang.
      ms_response-header-automatictaxcalculate        = ls_r001-automatictaxcalculate.
      ms_response-header-letterofcreditnumber         = ls_r001-letterofcreditnumber.
      ms_response-header-agencytotransfer             = ls_r001-agencytotransfer.
      ms_response-header-documentreferenceid          = ls_bkpf-documentreferenceid.
      ms_response-header-accountingdocumenttype       = ls_bkpf-accountingdocumenttype.
      ms_response-header-documentdate                 = ls_bkpf-documentdate.
      ms_response-header-postingdate                  = ls_bkpf-postingdate.
      ms_response-header-accountingdocumentheadertext = ls_bkpf-accountingdocumentheadertext.
      ms_response-header-exchangeratedate             = ls_bkpf-exchangeratedate.
      ms_response-header-documentcurrency             = ls_bkpf-transactioncurrency.
      ms_response-header-documentitemtext             = VALUE #( lt_jornalitem[ FinancialAccountType = 'K' ]-DocumentItemText OPTIONAL ).
      ms_response-header-taxamount                    = VALUE #( lt_jornalitem[ TransactionTypeDetermination = 'VST' ]-AmountIntransactionCurrency OPTIONAL ).
      ms_response-header-journalentryitemamount       = VALUE #( lt_jornalitem[ FinancialAccountType = 'K' ]-AmountIntransactionCurrency OPTIONAL ).
      IF ms_response-header-vorgang = 3.
        ms_response-header-journalentryitemamount *= -1.
      ENDIF.

      "ITEM verilerini doldur
      LOOP AT lt_r002 INTO DATA(ls_r002).
        DATA(ls_cost_list) = VALUE #( lt_cost_list[ DeliveryDocument     = ls_r002-deliverydocument
                                                    DeliveryDocumentItem = ls_r002-deliverydocumentitem ] OPTIONAL ).

        APPEND VALUE #( deliverydocument     = ls_cost_list-deliverydocument
                        deliverydocumentitem = ls_cost_list-deliverydocumentitem
                        purchaseorder        = ls_cost_list-PurchaseOrder
                        purchaseorderitem    = ls_cost_list-PurchaseOrderItem
                        vendor               = ls_cost_list-vendor
                        vendorname           = ls_cost_list-vendorname
                        material             = ls_cost_list-material
                        materialtext         = ls_cost_list-materialtext
                        orderquantity        = ls_cost_list-OrderQuantity
                        unitofmeasure        = ls_cost_list-unitofmeasure
                        deliveryquantity     = ls_cost_list-deliveryquantity
                        salesunit            = ls_cost_list-salesunit
                        netvalue             = ls_cost_list-netvalue
                        documentcurrency     = ls_cost_list-DocumentCurrency
                        profitcenter         = ls_cost_list-ProfitCenter
                        netpriceamount       = ls_cost_list-NetPriceAmount
                        documenttype         = ls_cost_list-documenttype
                        costtype             = ls_r002-costtype
                        costtypetext         = ls_r002-costtypetext
                        amount               = ls_r002-documentcurrenyamount
                        taxcode              = ls_r002-taxcode
                      ) TO ms_response-item.

        CLEAR: ls_cost_list.
      ENDLOOP.
    ENDIF.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.