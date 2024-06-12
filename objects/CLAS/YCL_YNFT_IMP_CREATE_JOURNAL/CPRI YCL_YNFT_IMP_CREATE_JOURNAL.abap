  PRIVATE SECTION.
    DATA: ms_request  TYPE ynft_s_imp_create_journal_req,
          ms_response TYPE ynft_s_imp_create_journal_res.
    CONSTANTS: mc_header_content TYPE string VALUE 'content-type',
               mc_content_type   TYPE string VALUE 'text/json'.