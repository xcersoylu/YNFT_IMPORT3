  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
    DATA lr_deliverydocument TYPE RANGE OF vbeln_vl.
    DATA lt_deliverydocument TYPE RANGE OF ynft_e_vbeln.
    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ls_request ).
    IF ls_request-deliverydocuments IS NOT INITIAL.
      lt_deliverydocument = VALUE #(  FOR wa IN ls_request-deliverydocuments ( sign = 'I' option = 'EQ'  low = |{ wa-deliverydocument ALPHA = IN }| ) ).
    ENDIF.

    SELECT
      FROM ynft_t_dlv_cus
      FIELDS deliverydocument , documenttype
      WHERE deliverydocument IN @lt_deliverydocument
      ORDER BY deliverydocument
      INTO TABLE @DATA(lt_documenttype).
    IF sy-subrc = 0.
      CASE ls_request-costsource.
        WHEN '1'. "Mal bedeli
          DATA(lt_shipment) = lt_documenttype.
          DATA(lt_clearence) = lt_documenttype.
          DELETE lt_shipment WHERE documenttype <> '1'.
          DELETE lt_clearence WHERE documenttype = '1'.

          IF lt_shipment IS NOT INITIAL.
            SELECT 'I' AS sign, 'EQ' AS option, ynft_t_dlvit_cus~deliverydocument AS low
             FROM @lt_shipment AS shipment INNER JOIN ynft_t_dlvit_cus ON ynft_t_dlvit_cus~referencedocument = shipment~deliverydocument
             INTO TABLE @lr_deliverydocument.
          ENDIF.
          IF lt_clearence IS NOT INITIAL.
            SELECT 'I' AS sign, 'EQ' AS option, ynft_t_dlvit_cus~referencedocument AS low
             FROM @lt_clearence AS clearence INNER JOIN ynft_t_dlvit_cus ON ynft_t_dlvit_cus~deliverydocument = clearence~deliverydocument
             APPENDING TABLE @lr_deliverydocument.
          ENDIF.

          lr_deliverydocument = VALUE #( BASE lr_deliverydocument FOR wa_documenttype IN lt_documenttype
                                         ( sign = 'I' option = 'EQ' low = wa_documenttype-deliverydocument ) ).
          SELECT r002~documentcurrenyamount, r002~debitcreditindicator
                  FROM ynft_t_r002 AS r002
                  INNER JOIN i_journalentry AS bkpf
                  ON  bkpf~companycode = r002~companycode
                  AND bkpf~accountingdocument = r002~accountingdocument
                  AND bkpf~fiscalyear = r002~fiscalyear
                  INNER JOIN ynft_t_r001 AS r001
                  ON  r001~companycode = r002~companycode
                  AND r001~accountingdocument = r002~accountingdocument
                  AND r001~fiscalyear = r002~fiscalyear
                  WHERE r001~costsource = @ls_request-costsource
                    AND bkpf~reversedocument = @space
                    AND bkpf~isreversed = @space
                    AND bkpf~accountingdocumentcategory NE 'Z'
                    AND r002~deliverydocument IN @lr_deliverydocument
                  INTO TABLE @DATA(lt_ginv).
          IF  sy-subrc = 0.
            MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
               TYPE 'E'
               NUMBER 008
               INTO ls_response-message.
            response->set_status('400').
          ENDIF.

        WHEN '2'. "dosya masrafı
          IF NOT line_exists( lt_documenttype[ documenttype = '1' ] ).
            MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
               TYPE 'E'
               NUMBER 007
               INTO ls_response-message.
            response->set_status('400').
          ENDIF.
        WHEN '3'. "çekme masrafı
          IF line_exists( lt_documenttype[ documenttype = '1' ] ).
            MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
               TYPE 'E'
               NUMBER 007
               INTO ls_response-message.
            response->set_status('400').
          ENDIF.
      ENDCASE.
    ELSE.
      MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
         TYPE 'E'
         NUMBER 009
         INTO ls_response-message.
      response->set_status('400').
    ENDIF.
    IF ls_response-message IS INITIAL.
      SELECT
       FROM yi_nft_ddl_imp_cost_data
       FIELDS deliverydocument,
              deliverydocumentitem,
              purchaseorder,
              purchaseorderitem,
              material,
              materialtext,
              deliveryquantity,
              salesunit,
              vendor,
              vendorname,
              orderquantity,
              documentcurrency,
              profitcenter,
              netvalue,
              unitofmeasure,
              netpriceamount,
              documenttype
        WHERE deliverydocument IN @lt_deliverydocument
        INTO CORRESPONDING FIELDS OF TABLE @ls_response-data.
      IF sy-subrc = 0.
        response->set_status('200').
      ELSE.
        MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
           TYPE 'E'
           NUMBER 006
           INTO ls_response-message.
        response->set_status('400').
      ENDIF.
    ENDIF.
    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ls_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = lc_header_content i_value = lc_content_type ).
  endmethod.