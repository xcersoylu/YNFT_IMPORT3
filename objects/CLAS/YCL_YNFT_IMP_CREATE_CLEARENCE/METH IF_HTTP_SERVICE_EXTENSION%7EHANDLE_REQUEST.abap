  METHOD if_http_service_extension~handle_request.
    DATA lt_delivery_custom_fields TYPE TABLE OF ynft_t_dlv_cus.
    DATA ls_delivery_custom_fields TYPE ynft_t_dlv_cus.
    DATA lt_delivery_item_custom_fields TYPE TABLE OF ynft_t_dlvit_cus.
    DATA ls_delivery_item_custom_fields TYPE ynft_t_dlvit_cus.
    DATA lv_ok(1) TYPE c.
    DATA lv_posnr TYPE ynft_e_posnr.
    DATA(ls_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).
    /ui2/cl_json=>deserialize( EXPORTING json = ls_request_body CHANGING data = ms_request ).
    IF line_exists( ms_request-item[ clearencequantity = 0 ] ).
      MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
             TYPE 'E'
            NUMBER 022
            WITH ms_response-deliverydocument INTO DATA(lv_message).
      APPEND lv_message TO ms_response-messages.
    ENDIF.
    IF ms_response-messages IS INITIAL.
      DATA(ls_line) = VALUE #( ms_request-item[ 1 ] OPTIONAL ).
      MODIFY ENTITIES OF i_inbounddeliverytp
          ENTITY inbounddelivery
        EXECUTE createdlvfrmpurchasingdocument
        FROM VALUE #( (  %cid = 'CREATE1'
                         %param = VALUE #( supplier = |{ ls_line-supplier ALPHA = IN }|
                                          deliverydate = cl_abap_context_info=>get_system_date( )
                                          deliverytime = cl_abap_context_info=>get_system_time(  )
                                          deliverydocumentbysupplier = ''
                                          meansoftransporttype = ''
                                          meansoftransport = ''
                          %control = VALUE #( supplier = if_abap_behv=>mk-on
                                              deliverydate = if_abap_behv=>mk-on
                                              deliverytime = if_abap_behv=>mk-on
                                              deliverydocumentbysupplier = if_abap_behv=>mk-on
                                              meansoftransporttype = if_abap_behv=>mk-on
                                              meansoftransport = if_abap_behv=>mk-on
                                              _referencepurgdocumentitem = if_abap_behv=>mk-on )
                          _referencepurgdocumentitem = VALUE #( FOR wa IN ms_request-item ( referencesddocument = wa-purchaseorder
                                                                                            referencesddocumentitem = wa-purchaseorderitem
                                                                                            %control = VALUE #( referencesddocument = if_abap_behv=>mk-on ) )
                                                             ) ) ) )
            MAPPED DATA(mapped_create)
            REPORTED DATA(reported_create)
            FAILED DATA(failed_create).
      IF failed_create IS INITIAL.
        COMMIT ENTITIES BEGIN
         RESPONSE OF i_inbounddeliverytp
         FAILED DATA(ls_commit_create_failed)
         REPORTED DATA(ls_commit_create_reported).
        COMMIT ENTITIES END.
        IF ls_commit_create_failed IS INITIAL.
          ms_response-deliverydocument = VALUE #( ls_commit_create_reported-inbounddelivery[ 1 ]-inbounddelivery OPTIONAL ).
          DO 5 TIMES.
            SELECT deliverydocument , deliverydocumentitem ,referencesddocument , referencesddocumentitem
            FROM i_deliverydocumentitem
            WHERE deliverydocument = @ms_response-deliverydocument
            INTO TABLE @DATA(lt_deliveryitems).
            IF sy-subrc = 0.
              lv_ok = abap_true.
              MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
                     TYPE 'S'
                    NUMBER 019
                    WITH ms_response-deliverydocument INTO lv_message.
              APPEND lv_message TO ms_response-messages.
              EXIT.
            ELSE.
              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDDO.

          IF lv_ok = abap_true.
            MODIFY ENTITIES OF i_inbounddeliverytp
             ENTITY inbounddeliveryitem
             UPDATE
             FIELDS ( actualdeliveredqtyinorderunit )
             WITH VALUE #( FOR wa_update IN lt_deliveryitems
                            ( %key-inbounddelivery = wa_update-deliverydocument
                              %key-inbounddeliveryitem = wa_update-deliverydocumentitem
                              actualdeliveredqtyinorderunit = VALUE #( ms_request-item[ purchaseorder = wa_update-referencesddocument
                                                                                        purchaseorderitem = wa_update-referencesddocumentitem ]-clearencequantity OPTIONAL ) ) )

             FAILED DATA(failed_update)
             REPORTED DATA(reported_update).
            IF failed_update IS INITIAL.
              COMMIT ENTITIES BEGIN
               RESPONSE OF i_inbounddeliverytp
               FAILED DATA(ls_commit_update_failed)
               REPORTED DATA(ls_commit_update_reported).
              COMMIT ENTITIES END.
            ELSE.
              CLEAR lv_message.
              MESSAGE ID ycl_nft_imp_util_class=>mc_msgid
                     TYPE 'E'
                    NUMBER 020
                    WITH ms_response-deliverydocument INTO lv_message.
              APPEND lv_message TO ms_response-messages.
              ms_response-messages = VALUE #( FOR wa_update_reported IN reported_update-inbounddelivery ( wa_update_reported-%msg->if_message~get_text( ) ) ).
              response->set_status( '400' ).
            ENDIF.
          ENDIF.

          ls_delivery_custom_fields = CORRESPONDING #( ms_request-header ).
          ls_delivery_custom_fields-deliverydocument = ms_response-deliverydocument.
          ls_delivery_custom_fields-companycode = ls_line-companycode.
          CLEAR lv_posnr.
          LOOP AT ms_request-item INTO DATA(ls_selected_line).
            lv_posnr += 10.
            ls_delivery_item_custom_fields = CORRESPONDING #( ls_selected_line MAPPING quantityunit = purchaseorderquantityunit EXCEPT shipquantity ).
            ls_delivery_item_custom_fields-deliverydocument = ms_response-deliverydocument.
            ls_delivery_item_custom_fields-deliverydocumentitem = lv_posnr.
            ls_delivery_item_custom_fields-referencedocument = |{ ls_selected_line-deliverydocument ALPHA = IN }|.
            ls_delivery_item_custom_fields-referencedocumentitem = ls_selected_line-deliverydocumentitem.
            APPEND ls_delivery_item_custom_fields TO lt_delivery_item_custom_fields.
          ENDLOOP.
          IF ls_delivery_custom_fields IS NOT INITIAL.
            INSERT ynft_t_dlv_cus FROM @ls_delivery_custom_fields.
            INSERT ynft_t_dlvit_cus FROM TABLE @lt_delivery_item_custom_fields.
            COMMIT WORK AND WAIT.
          ENDIF.
        ELSE.
          ms_response-messages = VALUE #( FOR wa_commit_create_reported IN ls_commit_create_reported-inbounddelivery ( wa_commit_create_reported-%msg->if_message~get_text( ) ) ).
          response->set_status( '400' ).
        ENDIF.
      ELSE.
        ms_response-messages = VALUE #( FOR wa_create_reported IN reported_create-inbounddelivery ( wa_create_reported-%msg->if_message~get_text( ) ) ).
        response->set_status( '400' ).
      ENDIF.
    ENDIF.
    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.