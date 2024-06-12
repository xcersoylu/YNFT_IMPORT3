  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
    DATA lv_calculation_type TYPE ynft_e_gross_net.
    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).
    CASE ms_Request-costsource.
      WHEN '1'. "Mal bedeli faturası
        lv_calculation_type = mc_net.
      WHEN '2'. "Dosya masrafları
        lv_calculation_type = mc_gross.
      WHEN '3'. "Çekme masrafları
        lv_calculation_type = mc_gross.
      WHEN '4'. "Diğer masraflar
        lv_calculation_type = mc_gross.
      WHEN '5'. "Kur farkı kaydı
        lv_calculation_type = mc_gross.
      WHEN '6'. "Peşinat
        lv_calculation_type = mc_net.
    ENDCASE.
    IF lv_calculation_type IS INITIAL.
      "hata ver
    ELSE.
      CLEAR mv_total_tax.
      LOOP AT ms_request-taxlines INTO DATA(ls_tax_line).
        APPEND VALUE #( value = ls_tax_line-value
                        currency = ls_tax_line-currency
                        taxcode  = ls_tax_line-taxcode
                        taxvalue = calculate_tax( is_line = ls_tax_line iv_calculation_Type = lv_calculation_Type ) ) TO ms_response-taxlines.
      ENDLOOP.
      IF sy-subrc = 0.
        ms_response-totaltax = mv_total_tax.
      ENDIF.
    ENDIF.
    response->set_status('200').
    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  endmethod.