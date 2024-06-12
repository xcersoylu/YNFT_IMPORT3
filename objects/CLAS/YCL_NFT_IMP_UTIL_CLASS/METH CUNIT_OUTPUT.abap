  METHOD cunit_output.
    SELECT SINGLE unitofmeasure_e
        FROM i_unitofmeasure
        WHERE unitofmeasure = @iv_unitofmeasure
        INTO @rv_unitofmeasure.
  ENDMETHOD.