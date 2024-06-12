private section.
    DATA: ls_request        TYPE ynft_s_imp_cost_data_req,
          ls_response       TYPE ynft_s_imp_cost_data_res,
          lc_header_content TYPE string VALUE 'content-type',
          lc_content_type   TYPE string VALUE 'text/json'.