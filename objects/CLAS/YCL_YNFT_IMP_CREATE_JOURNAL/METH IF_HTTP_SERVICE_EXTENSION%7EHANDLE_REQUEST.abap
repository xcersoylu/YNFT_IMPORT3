  METHOD if_http_service_extension~handle_request.
    DATA ls_r001 TYPE ynft_t_r001.
    DATA lt_r002 TYPE TABLE OF ynft_t_r002.
    DATA lt_je TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post.
    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).
    APPEND INITIAL LINE TO lt_je ASSIGNING FIELD-SYMBOL(<fs_je>).
    TRY.
        <fs_je>-%cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error INTO DATA(lx_error).
    ENDTRY.
    SELECT ctype ,bezei
      from ynft_t_x004
      where spras = @sy-langu
      ORDER BY ctype
      into TABLE @data(lt_x004).
    DATA(lt_glitem) = ms_request-glitem[].

    SELECT glitem~deliverydocument ,glitem~deliverydocumentitem,glitem~costtype,t010~saknr
     FROM @lt_glitem AS glitem INNER JOIN i_deliverydocumentitem AS lips ON lips~deliverydocument = glitem~deliverydocument
                                                                        AND lips~deliverydocumentitem = glitem~deliverydocumentitem
     INNER JOIN i_productvaluationbasic AS mbew ON mbew~product = lips~material
                                               AND mbew~valuationtype = lips~inventoryvaluationtype
                                               AND mbew~valuationarea = lips~plant
     INNER JOIN ynft_t_t010 AS t010 ON t010~bukrs = @ms_request-header-companycode
                                   AND t010~bklas = mbew~valuationclass
                                   AND t010~ctype = glitem~costtype
     INTO TABLE @DATA(lt_saknr).
    IF sy-subrc = 0.
      LOOP AT lt_glitem ASSIGNING FIELD-SYMBOL(<ls_glitem>).
        <ls_glitem>-glaccount = VALUE #( lt_saknr[ deliverydocument = <ls_glitem>-deliverydocument
                                                   deliverydocumentitem = <ls_glitem>-deliverydocumentitem
                                                   costtype = <ls_glitem>-costtype ]-saknr OPTIONAL ).
        IF <ls_glitem>-glaccount IS INITIAL.
          MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
                 TYPE 'E'
                 NUMBER 023
                 WITH VALUE #( lt_x004[ ctype = <ls_glitem>-costtype ]-bezei OPTIONAL )
                 INTO DATA(lv_message).
          APPEND lv_message TO ms_response-error_messages.
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT t010~bukrs,t010~ctype,t010~saknr
       FROM @lt_glitem AS glitem INNER JOIN ynft_t_t010 AS t010 ON t010~bukrs = @ms_request-header-companycode
                                                               AND t010~ctype = glitem~costtype
       ORDER BY glitem~deliverydocument ,glitem~deliverydocumentitem
       INTO TABLE @DATA(lt_t010).
      LOOP AT lt_glitem ASSIGNING <ls_glitem>.
        <ls_glitem>-glaccount = VALUE #( lt_t010[ bukrs = ms_request-header-companycode
                                                  ctype = <ls_glitem>-costtype ]-saknr OPTIONAL ).
        IF <ls_glitem>-glaccount IS INITIAL.
          MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
                 TYPE 'E'
                 NUMBER 023
                 WITH VALUE #( lt_x004[ ctype = <ls_glitem>-costtype ]-bezei OPTIONAL )
                 INTO lv_message.
          APPEND lv_message TO ms_response-error_messages.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF ms_response-error_messages IS INITIAL.
      <fs_je>-%param = VALUE #( companycode                  = ms_request-header-companycode
                                documentreferenceid          = ms_request-header-documentreferenceid
                                createdbyuser                = sy-uname
                                businesstransactiontype      = 'RFBU'
                                accountingdocumenttype       = ms_request-header-accountingdocumenttype
                                documentdate                 = ms_request-header-documentdate
                                postingdate                  = ms_request-header-postingdate
                                accountingdocumentheadertext = ms_request-header-accountingdocumentheadertext
                                taxdeterminationdate         = ms_request-header-taxdeterminationdate
                                exchangeratedate             = ms_request-header-exchangeratedate
                                 _apitems = VALUE #( FOR wa_apitem IN ms_request-apitem ( CORRESPONDING #( wa_apitem MAPPING _currencyamount = currencyamount ) ) )
*                               _glitems = VALUE #( FOR wa_glitem IN ms_request-glitem ( CORRESPONDING #( wa_glitem MAPPING _currencyamount = currencyamount ) ) )
                                 _glitems = VALUE #( FOR wa_glitem IN lt_glitem ( CORRESPONDING #( wa_glitem MAPPING _currencyamount = currencyamount ) ) )
                                 _taxitems = VALUE #( FOR wa_taxitem IN ms_request-taxitem ( CORRESPONDING #( wa_taxitem MAPPING _currencyamount = currencyamount ) ) )
                              ).
      MODIFY ENTITIES OF i_journalentrytp
       ENTITY journalentry
       EXECUTE post FROM lt_je
       FAILED DATA(ls_failed)
       REPORTED DATA(ls_reported)
       MAPPED DATA(ls_mapped).

      IF ls_failed IS NOT INITIAL.
        ms_response-error_messages = VALUE #( FOR wa IN ls_reported-journalentry ( wa-%msg->if_message~get_text( ) ) ).
        response->set_status('400').
      ELSE.
        COMMIT ENTITIES BEGIN
         RESPONSE OF i_journalentrytp
         FAILED DATA(ls_commit_failed)
         REPORTED DATA(ls_commit_reported).
        COMMIT ENTITIES END.
        IF ls_commit_failed IS INITIAL.
          response->set_status('200').
          ms_response-accountingdocument = VALUE #( ls_commit_reported-journalentry[ 1 ]-accountingdocument OPTIONAL ).
          ls_r001 = VALUE #( client                = sy-mandt
                             companycode           = ms_request-header-companycode
                             accountingdocument    = ms_response-accountingdocument
                             fiscalyear            = ms_request-header-documentdate(4)
                             costsource            = ms_request-header-costsource
                             vorgang               = ms_request-header-vorgang
                             automatictaxcalculate = ms_request-header-automatictaxcalculate
                             letterofcreditnumber  = ms_request-header-letterofcreditnumber
                             agencytotransfer      = ms_request-header-agencytotransfer ).
*        lt_r002 = VALUE #( FOR wa_glitem2 IN ms_request-glitem ( client                = sy-mandt
          lt_r002 = VALUE #( FOR wa_glitem2 IN lt_glitem ( client                = sy-mandt
                                                                   companycode           = ms_request-header-companycode
                                                                   accountingdocument    = ms_response-accountingdocument
                                                                   fiscalyear            = ms_request-header-documentdate(4)
                                                                   accountingdocumentitem = wa_glitem2-glaccountlineitem
                                                                   costtype              = wa_glitem2-costtype
                                                                   deliverydocument      = wa_glitem2-deliverydocument
                                                                   deliverydocumentitem  = wa_glitem2-deliverydocumentitem
                                                                   documentcurrenyamount = wa_glitem2-currencyamount[ 1 ]-journalentryitemamount
                                                                   documentcurrency      = wa_glitem2-currencyamount[ 1 ]-currency
                                                                   accountnumber         = wa_glitem2-glaccount
                                                                   taxcode               = wa_glitem2-taxcode
                                                                   taxamount             = wa_glitem2-taxamount
                                                                   debitcreditindicator  = wa_glitem2-debitcreditindicator
                                                                   ) ).
          MODIFY ynft_t_r001 FROM @ls_r001.
          MODIFY ynft_t_r002 FROM TABLE @lt_r002.
          COMMIT WORK AND WAIT.
        ELSE.

        ENDIF.
      ENDIF.
    ENDIF.
    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).

  ENDMETHOD.