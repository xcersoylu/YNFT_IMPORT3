  METHOD if_http_service_extension~handle_request.
    DATA(ls_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).

    /ui2/cl_json=>deserialize( EXPORTING json = ls_request_body CHANGING data = ms_request ).

    SELECT *
      FROM ynft_t_dlv_cus
      WHERE deliverydocument = @ms_request-deliverydocument
      INTO CORRESPONDING FIELDS OF @ms_response
      UP TO 1 ROWS.
    ENDSELECT.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.