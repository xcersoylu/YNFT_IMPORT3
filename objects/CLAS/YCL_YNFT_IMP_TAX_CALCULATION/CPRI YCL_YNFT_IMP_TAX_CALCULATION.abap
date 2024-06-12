private section.
    DATA: ms_request   TYPE ynft_s_imp_tax_calculate_req,
          ms_response  TYPE ynft_s_imp_tax_calculate_res,
          mv_total_tax TYPE ynft_e_bwert.
    CONSTANTS: mc_header_content TYPE string VALUE 'content-type',
               mc_content_type   TYPE string VALUE 'text/json',
               mc_Gross          TYPE ynft_e_gross_net VALUE 'G',
               mc_net            TYPE ynft_e_gross_net VALUE 'N'.
    METHODS calculate_tax IMPORTING is_line             TYPE znft_s_import_tax_line
                                    iv_calculation_Type TYPE znft_e_gross_net
                          RETURNING VALUE(rv_tax)       TYPE znft_e_bwert.