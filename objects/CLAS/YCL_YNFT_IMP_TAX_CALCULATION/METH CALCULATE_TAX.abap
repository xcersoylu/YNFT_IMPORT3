  METHOD calculate_tax.
    DATA lv_tax_ratio TYPE i.
    lv_tax_ratio = SWITCH #( is_line-taxcode WHEN 'V0' THEN 0
                                             WHEN 'V1' THEN 1
                                             WHEN 'V2' THEN 10
                                             WHEN 'V3' THEN 20 ).
    CASE iv_calculation_Type.
      WHEN mc_gross.
        rv_Tax = is_line-value - ( is_line-value / ( 1 + ( lv_Tax_ratio / 100 ) ) ).
      WHEN mc_net.
        rv_tax = is_line-value * lv_tax_ratio / 100.
    ENDCASE.
    mv_total_tax += rv_tax.
  ENDMETHOD.