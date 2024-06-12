  METHOD cunit_input.
    SELECT SINGLE unitofmeasure
        FROM i_unitofmeasure
        WHERE unitofmeasure_e = @iv_unitofmeasure
        INTO @rv_unitofmeasure.
  ENDMETHOD.