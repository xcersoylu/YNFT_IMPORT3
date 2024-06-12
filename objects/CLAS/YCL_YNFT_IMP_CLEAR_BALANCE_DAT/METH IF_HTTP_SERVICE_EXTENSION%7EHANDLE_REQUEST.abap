  METHOD if_http_service_extension~handle_request.
    DATA lr_delivery TYPE RANGE OF i_deliverydocument-deliverydocument.
    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).

    "Önce tüm masraf belgelerini oku
    SELECT r002~companycode,
           r002~accountingdocument,
           r002~fiscalyear,
           r002~accountingdocumentitem,
           r002~deliverydocument,
           r002~deliverydocumentitem,
           r002~documentcurrenyamount,
           r002~documentcurrency,
           r002~costtype,
           r002~accountnumber,
           ynft_t_dlv_cus~documenttype
      FROM ynft_t_r002 AS r002
      INNER JOIN ynft_t_dlv_cus ON ynft_t_dlv_cus~deliverydocument = r002~deliverydocument
      INTO TABLE @DATA(lt_r002).

    "Tüm masrafların dosya kapama kayıtlarını oku
    SELECT r005~companycode,
           r005~accountingdocument,
           r005~fiscalyear,
           r005~accountingdocumentitem,
           SUM( r005~amount ) AS amount
      FROM @lt_r002 AS r002
      INNER JOIN ynft_t_r005 AS r005 ON r002~companycode            = r005~companycode
                                    AND r002~accountingdocument     = r005~accountingdocument
                                    AND r002~fiscalyear             = r005~fiscalyear
                                    AND r002~accountingdocumentitem = r005~accountingdocumentitem
      GROUP BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem
      ORDER BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem
      INTO TABLE @DATA(lt_r005).

    "Dosya kapaması yapılmış masrafları sil
    LOOP AT lt_r002 ASSIGNING FIELD-SYMBOL(<ls_r002>).
      READ TABLE lt_r005 INTO DATA(ls_r005) WITH KEY companycode            = <ls_r002>-companycode
                                                     accountingdocument     = <ls_r002>-accountingdocument
                                                     fiscalyear             = <ls_r002>-fiscalyear
                                                     accountingdocumentitem = <ls_r002>-accountingdocumentitem BINARY SEARCH.
      IF sy-subrc = 0 AND ls_r005-amount >= <ls_r002>-documentcurrenyamount.
        <ls_r002>-accountingdocument = 'XXDELETEXX'.
      ENDIF.
      CLEAR ls_r005.
    ENDLOOP.
    DELETE lt_r002 WHERE accountingdocument = 'XXDELETEXX'.

    IF lt_r002 IS NOT INITIAL.

      "Yükleme belgesi ile girilmiş masrafların çekme belgelerini bul
      DATA(lt_clearence) = lt_r002.
      DATA(lt_shipment)  = lt_r002.

      DELETE lt_clearence WHERE documenttype = '1'.
      DELETE lt_shipment  WHERE documenttype <> '1'.

      SELECT ynft_t_dlvit_cus~deliverydocument , ynft_t_dlvit_cus~deliverydocumentitem
        FROM @lt_shipment AS shipment
        INNER JOIN ynft_t_dlvit_cus ON ynft_t_dlvit_cus~referencedocument     = shipment~deliverydocument
                                   AND ynft_t_dlvit_cus~referencedocumentitem = shipment~deliverydocumentitem
        INTO TABLE @DATA(lt_shipment_clearence).

      "Direkt veya endirekt masraf girilmiş tüm çekmeleri bir range'e topla
      lr_delivery = VALUE #( FOR wa_clearence IN lt_clearence          ( sign = 'I' option = 'EQ' low = wa_clearence-deliverydocument ) ).
      lr_delivery = VALUE #( BASE lr_delivery
                             FOR wa_shipment  IN lt_shipment_clearence ( sign = 'I' option = 'EQ' low = wa_shipment-deliverydocument ) ).
      SORT lr_delivery BY low.
      DELETE ADJACENT DUPLICATES FROM lr_delivery.

      "Çekme belgelerinin ekrana verilecek bilgilerini oku
      SELECT delivery~deliverydocument,
             delivery~supplier,
             delivery~creationdate,
             i_supplier~suppliername,
             dlv_cus~documenttype,
             ddtyp_tx~text AS documenttype_tx
          FROM ynft_t_dlv_cus AS dlv_cus
          INNER JOIN i_deliverydocument AS delivery ON delivery~DeliveryDocument = dlv_cus~deliverydocument
          LEFT OUTER JOIN i_supplier ON i_supplier~supplier = delivery~supplier
          LEFT OUTER JOIN ddcds_customer_domain_value_t( p_domain_name = 'YNFT_D_DDTYP' ) AS ddtyp_tx ON ddtyp_tx~value_low = dlv_cus~documenttype
                                                                                                     AND ddtyp_tx~language  = @sy-langu
          WHERE dlv_cus~deliverydocument IN @lr_delivery
          INTO CORRESPONDING FIELDS OF TABLE @ms_response-header.

      IF ms_response-header IS NOT INITIAL.
        LOOP AT ms_response-header ASSIGNING FIELD-SYMBOL(<fs_header>).
          <fs_header>-deliverydocument = |{ <fs_header>-deliverydocument ALPHA = OUT }|.
          <fs_header>-supplier         = |{ <fs_header>-supplier ALPHA = OUT }|.
        ENDLOOP.

        "Her bir çekmenin altında gösterilecek kalem tablosunu hazırla
        SELECT item~deliverydocument,
               item~deliverydocumentitem,
               item~companycode,
               item~accountingdocument,
               item~fiscalyear,
               item~accountingdocumentitem,
               bkpf~documentdate,
               bkpf~postingdate,
               item~costtype,
               costtype~description        AS costtype_tx ,
               lips~referencesddocument,
               lips~referencesddocumentitem,
               item~documentcurrenyamount,
               item~documentcurrency,
               lips~material               AS product,
               producttext~productname,
               bseg~supplier,
               item~accountnumber,
               lips~actualdeliveryquantity AS deliveryquantity,
               lips~deliveryquantityunit,
               lips~plant
         FROM @lt_r002 AS item
         INNER JOIN i_journalentry AS bkpf ON bkpf~companycode        = item~companycode
                                          AND bkpf~accountingdocument = item~accountingdocument
                                          AND bkpf~fiscalyear         = item~fiscalyear
         INNER JOIN i_journalentryitem AS bseg ON bseg~companycode          = item~companycode
                                              AND bseg~accountingdocument   = item~accountingdocument
                                              AND bseg~fiscalyear           = item~fiscalyear
                                              AND bseg~financialaccounttype = 'K'
                                              AND bseg~ledger               = '0L'
         INNER JOIN i_deliverydocumentitem AS lips ON lips~deliverydocument     = item~deliverydocument
                                                  AND lips~deliverydocumentitem = item~deliverydocumentitem
         INNER JOIN i_purchaseorderapi01 AS po ON po~purchaseorder = lips~referencesddocument
         LEFT OUTER JOIN yvh_nft_ctype AS costtype ON costtype~ctype = item~costtype
         LEFT OUTER JOIN i_producttext AS producttext ON producttext~product  = lips~material
                                                     AND producttext~language = @sy-langu
         INTO CORRESPONDING FIELDS OF TABLE @ms_response-items.

        LOOP AT ms_response-items ASSIGNING FIELD-SYMBOL(<fs_item>).
          <fs_item>-deliverydocument     = |{ <fs_item>-deliverydocument ALPHA = OUT }|.
          <fs_item>-supplier             = |{ <fs_item>-supplier ALPHA = OUT }|.
          <fs_item>-deliveryquantityunit = ycl_nft_imp_util_class=>cunit_output( <fs_item>-deliveryquantityunit ).
        ENDLOOP.

        SORT: ms_response-header BY deliverydocument,
              ms_response-items  BY accountingdocument accountingdocumentitem.
      ENDIF.
    ENDIF.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.