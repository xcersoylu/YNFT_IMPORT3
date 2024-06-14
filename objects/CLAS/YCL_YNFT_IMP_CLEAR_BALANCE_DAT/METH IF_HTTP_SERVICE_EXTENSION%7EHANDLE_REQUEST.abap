  METHOD if_http_service_extension~handle_request.
*    DATA lr_delivery TYPE RANGE OF i_deliverydocument-deliverydocument.
*    DATA(lv_request_body) = request->get_text( ).
*    DATA(lv_get_method) = request->get_method( ).
*
*    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).
*
*    "Önce tüm masraf belgelerini oku
*    SELECT r002~companycode,
*           r002~accountingdocument,
*           r002~fiscalyear,
*           r002~accountingdocumentitem,
*           r002~deliverydocument,
*           r002~deliverydocumentitem,
*           r002~documentcurrenyamount,
*           r002~documentcurrency,
*           r002~costtype,
*           r002~accountnumber,
*           ynft_t_dlv_cus~documenttype
*      FROM ynft_t_r002 AS r002
*      INNER JOIN ynft_t_dlv_cus ON ynft_t_dlv_cus~deliverydocument = r002~deliverydocument
*      INTO TABLE @DATA(lt_r002).
*
*    "Tüm masrafların dosya kapama kayıtlarını oku
*    SELECT r005~companycode,
*           r005~accountingdocument,
*           r005~fiscalyear,
*           r005~accountingdocumentitem,
*           SUM( r005~amount ) AS amount
*      FROM @lt_r002 AS r002
*      INNER JOIN ynft_t_r005 AS r005 ON r002~companycode            = r005~companycode
*                                    AND r002~accountingdocument     = r005~accountingdocument
*                                    AND r002~fiscalyear             = r005~fiscalyear
*                                    AND r002~accountingdocumentitem = r005~accountingdocumentitem
*      GROUP BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem
*      ORDER BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem
*      INTO TABLE @DATA(lt_r005).
*
*    "Dosya kapaması yapılmış masrafları sil
*    LOOP AT lt_r002 ASSIGNING FIELD-SYMBOL(<ls_r002>).
*      READ TABLE lt_r005 INTO DATA(ls_r005) WITH KEY companycode            = <ls_r002>-companycode
*                                                     accountingdocument     = <ls_r002>-accountingdocument
*                                                     fiscalyear             = <ls_r002>-fiscalyear
*                                                     accountingdocumentitem = <ls_r002>-accountingdocumentitem BINARY SEARCH.
*      IF sy-subrc = 0 AND ls_r005-amount >= <ls_r002>-documentcurrenyamount.
*        <ls_r002>-accountingdocument = 'XXDELETEXX'.
*      ENDIF.
*      CLEAR ls_r005.
*    ENDLOOP.
*    DELETE lt_r002 WHERE accountingdocument = 'XXDELETEXX'.
*
*    IF lt_r002 IS NOT INITIAL.
*
*      "Yükleme belgesi ile girilmiş masrafların çekme belgelerini bul
*      DATA(lt_clearence) = lt_r002.
*      DATA(lt_shipment)  = lt_r002.
*
*      DELETE lt_clearence WHERE documenttype = '1'.
*      DELETE lt_shipment  WHERE documenttype <> '1'.
*
*      SELECT ynft_t_dlvit_cus~deliverydocument , ynft_t_dlvit_cus~deliverydocumentitem
*        FROM @lt_shipment AS shipment
*        INNER JOIN ynft_t_dlvit_cus ON ynft_t_dlvit_cus~referencedocument     = shipment~deliverydocument
*                                   AND ynft_t_dlvit_cus~referencedocumentitem = shipment~deliverydocumentitem
*        INTO TABLE @DATA(lt_shipment_clearence).
*
*      "Direkt veya endirekt masraf girilmiş tüm çekmeleri bir range'e topla
*      lr_delivery = VALUE #( FOR wa_clearence IN lt_clearence          ( sign = 'I' option = 'EQ' low = wa_clearence-deliverydocument ) ).
*      lr_delivery = VALUE #( BASE lr_delivery
*                             FOR wa_shipment  IN lt_shipment_clearence ( sign = 'I' option = 'EQ' low = wa_shipment-deliverydocument ) ).
*      SORT lr_delivery BY low.
*      DELETE ADJACENT DUPLICATES FROM lr_delivery.
*
*      "Çekme belgelerinin ekrana verilecek bilgilerini oku
*      SELECT delivery~deliverydocument,
*             delivery~supplier,
*             delivery~creationdate,
*             i_supplier~suppliername,
*             dlv_cus~documenttype,
*             ddtyp_tx~text AS documenttype_tx
*          FROM ynft_t_dlv_cus AS dlv_cus
*          INNER JOIN i_deliverydocument AS delivery ON delivery~DeliveryDocument = dlv_cus~deliverydocument
*          LEFT OUTER JOIN i_supplier ON i_supplier~supplier = delivery~supplier
*          LEFT OUTER JOIN ddcds_customer_domain_value_t( p_domain_name = 'YNFT_D_DDTYP' ) AS ddtyp_tx ON ddtyp_tx~value_low = dlv_cus~documenttype
*                                                                                                     AND ddtyp_tx~language  = @sy-langu
*          WHERE dlv_cus~deliverydocument IN @lr_delivery
*          INTO CORRESPONDING FIELDS OF TABLE @ms_response-header.
*
*      IF ms_response-header IS NOT INITIAL.
*        LOOP AT ms_response-header ASSIGNING FIELD-SYMBOL(<fs_header>).
*          <fs_header>-deliverydocument = |{ <fs_header>-deliverydocument ALPHA = OUT }|.
*          <fs_header>-supplier         = |{ <fs_header>-supplier ALPHA = OUT }|.
*        ENDLOOP.
*
*        "Her bir çekmenin altında gösterilecek kalem tablosunu hazırla
*        SELECT item~deliverydocument,
*               item~deliverydocumentitem,
*               item~companycode,
*               item~accountingdocument,
*               item~fiscalyear,
*               item~accountingdocumentitem,
*               bkpf~documentdate,
*               bkpf~postingdate,
*               item~costtype,
*               costtype~description        AS costtype_tx ,
*               lips~referencesddocument,
*               lips~referencesddocumentitem,
*               item~documentcurrenyamount,
*               item~documentcurrency,
*               lips~material               AS product,
*               producttext~productname,
*               bseg~supplier,
*               item~accountnumber,
*               lips~actualdeliveryquantity AS deliveryquantity,
*               lips~deliveryquantityunit,
*               lips~plant
*         FROM @lt_r002 AS item
*         INNER JOIN i_journalentry AS bkpf ON bkpf~companycode        = item~companycode
*                                          AND bkpf~accountingdocument = item~accountingdocument
*                                          AND bkpf~fiscalyear         = item~fiscalyear
*         INNER JOIN i_journalentryitem AS bseg ON bseg~companycode          = item~companycode
*                                              AND bseg~accountingdocument   = item~accountingdocument
*                                              AND bseg~fiscalyear           = item~fiscalyear
*                                              AND bseg~financialaccounttype = 'K'
*                                              AND bseg~ledger               = '0L'
*         INNER JOIN i_deliverydocumentitem AS lips ON lips~deliverydocument     = item~deliverydocument
*                                                  AND lips~deliverydocumentitem = item~deliverydocumentitem
*         INNER JOIN i_purchaseorderapi01 AS po ON po~purchaseorder = lips~referencesddocument
*         LEFT OUTER JOIN yvh_nft_ctype AS costtype ON costtype~ctype = item~costtype
*         LEFT OUTER JOIN i_producttext AS producttext ON producttext~product  = lips~material
*                                                     AND producttext~language = @sy-langu
*         INTO CORRESPONDING FIELDS OF TABLE @ms_response-items.
*
*        LOOP AT ms_response-items ASSIGNING FIELD-SYMBOL(<fs_item>).
*          <fs_item>-deliverydocument     = |{ <fs_item>-deliverydocument ALPHA = OUT }|.
*          <fs_item>-supplier             = |{ <fs_item>-supplier ALPHA = OUT }|.
*          <fs_item>-deliveryquantityunit = ycl_nft_imp_util_class=>cunit_output( <fs_item>-deliveryquantityunit ).
*        ENDLOOP.
*
*        SORT: ms_response-header BY deliverydocument,
*              ms_response-items  BY accountingdocument accountingdocumentitem.
*      ENDIF.
*    ENDIF.
*
*    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
*    response->set_text( lv_response_body ).
*    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).

    DATA: lr_delivery           TYPE RANGE OF i_deliverydocument-deliverydocument,
          lt_ship_costs_on_clea TYPE ynft_tt_imp_clr_blnc_i.

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

      SELECT DISTINCT cus~deliverydocument,
                      cus~deliverydocumentitem,
                      cus~clearencequantity,
                      cus~referencedocument,
                      cus~referencedocumentitem
        FROM @lt_shipment AS shipment
        INNER JOIN ynft_t_dlvit_cus AS cus ON cus~referencedocument     = shipment~deliverydocument
                                          AND cus~referencedocumentitem = shipment~deliverydocumentitem
        INTO TABLE @DATA(lt_shipment_clearence).

      "Direkt veya endirekt masraf girilmiş tüm çekmeleri bir range'e topla
      lr_delivery = VALUE #( FOR wa_clearence IN lt_clearence          ( sign = 'I' option = 'EQ' low = wa_clearence-deliverydocument ) ).
      lr_delivery = VALUE #( BASE lr_delivery
                             FOR wa_shipment  IN lt_shipment_clearence ( sign = 'I' option = 'EQ' low = wa_shipment-deliverydocument ) ).

      SORT lr_delivery BY low.
      DELETE ADJACENT DUPLICATES FROM lr_delivery.

      "Çekme belgelerinin bilgilerini oku (İlk başlık ekranında gösterilecek)
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
             cus_it~purchaseorder        AS referencesddocument,
             cus_it~purchaseorderitem    AS referencesddocumentitem,
             item~documentcurrenyamount,
             item~documentcurrency,
             po~material                 AS product,
             producttext~productname,
             bseg~supplier,
             item~accountnumber,
             CASE cus~documenttype WHEN '1' THEN cus_it~shipquantity ELSE cus_it~clearencequantity END AS deliveryquantity,
             cus_it~quantityunit         AS deliveryquantityunit,
             po~plant
        FROM @lt_r002                       AS item
        INNER JOIN i_journalentry           AS bkpf        ON bkpf~companycode            = item~companycode
                                                          AND bkpf~accountingdocument     = item~accountingdocument
                                                          AND bkpf~fiscalyear             = item~fiscalyear
        INNER JOIN i_journalentryitem       AS bseg        ON bseg~companycode            = item~companycode
                                                          AND bseg~accountingdocument     = item~accountingdocument
                                                          AND bseg~fiscalyear             = item~fiscalyear
                                                          AND bseg~financialaccounttype   = 'K'
                                                          AND bseg~ledger                 = '0L'
        INNER JOIN ynft_t_dlv_cus           AS cus         ON cus~deliverydocument        = item~deliverydocument
        INNER JOIN ynft_t_dlvit_cus         AS cus_it      ON cus_it~deliverydocument     = item~deliverydocument
                                                          AND cus_it~deliverydocumentitem = item~deliverydocumentitem
        INNER JOIN I_PurchaseOrderItemAPI01 AS po          ON po~purchaseorder            = cus_it~purchaseorder
                                                          AND po~PurchaseOrderItem        = cus_it~purchaseorderitem
        LEFT OUTER JOIN yvh_nft_ctype       AS costtype    ON costtype~ctype              = item~costtype
        LEFT OUTER JOIN i_producttext       AS producttext ON producttext~product         = po~Material
                                                          AND producttext~language        = @sy-langu
        INTO CORRESPONDING FIELDS OF TABLE @ms_response-items.


      LOOP AT ms_response-items INTO DATA(ls_item).
        "Yükleme ile girilen masrafları çekme belgeleri üzerine dağıt
        LOOP AT lt_shipment_clearence INTO DATA(ls_shipment_clearence) WHERE referencedocument     = ls_item-deliverydocument
                                                                         AND referencedocumentitem = ls_item-deliverydocumentitem.
          DATA(lv_del) = abap_true.
          APPEND INITIAL LINE TO lt_ship_costs_on_clea ASSIGNING FIELD-SYMBOL(<fs_ship_costs_on_clea>).
          MOVE-CORRESPONDING ls_item TO <fs_ship_costs_on_clea>.

          "Yükleme belgelerininin yerine çekme belgelerini yaz
          <fs_ship_costs_on_clea>-deliverydocument     = ls_shipment_clearence-deliverydocument.
          <fs_ship_costs_on_clea>-deliverydocumentitem = ls_shipment_clearence-deliverydocumentitem.
          <fs_ship_costs_on_clea>-deliveryquantity     = ls_shipment_clearence-clearencequantity.

          "Yükleme ile girilen masraf tutarını yükleme miktarı/çekme miktarı oranına göre dağıt
          <fs_ship_costs_on_clea>-documentcurrenyamount = <fs_ship_costs_on_clea>-documentcurrenyamount * ( ls_shipment_clearence-clearencequantity / ls_item-deliveryquantity ).
        ENDLOOP.

        IF lv_del IS NOT INITIAL.
          DELETE ms_response-items.
          CLEAR lv_del.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF lt_ship_costs_on_clea TO ms_response-items.

      "Sort & Conversion işlemleri
      SORT: ms_response-header BY deliverydocument,
            ms_response-items  BY accountingdocument accountingdocumentitem.

      LOOP AT ms_response-header ASSIGNING FIELD-SYMBOL(<fs_header>).
        <fs_header>-deliverydocument = |{ <fs_header>-deliverydocument ALPHA = OUT }|.
        <fs_header>-supplier         = |{ <fs_header>-supplier ALPHA = OUT }|.
      ENDLOOP.
      LOOP AT ms_response-items ASSIGNING FIELD-SYMBOL(<fs_item>).
        <fs_item>-deliverydocument     = |{ <fs_item>-deliverydocument ALPHA = OUT }|.
        <fs_item>-supplier             = |{ <fs_item>-supplier ALPHA = OUT }|.
        <fs_item>-product              = |{ <fs_item>-product ALPHA = OUT }|.
*        <fs_item>-deliveryquantityunit = ycl_nft_imp_util_class=>cunit_output( <fs_item>-deliveryquantityunit ).
      ENDLOOP.
    ENDIF.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.