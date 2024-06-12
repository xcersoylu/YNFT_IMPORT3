  METHOD def_release.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'RELEASE' ).
    lo_function->set_edm_name( 'Release' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'RELEASE' ).
    lo_function_import->set_edm_name( 'Release' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'SUPPLIER_INVOICE' ).
    lo_parameter->set_edm_name( 'SupplierInvoice' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SUPPLIER_INVOICE_2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'FISCAL_YEAR' ).
    lo_parameter->set_edm_name( 'FiscalYear' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'FISCAL_YEAR_2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DISCOUNT_DAYS_HAVE_TO_BE_S' ).
    lo_parameter->set_edm_name( 'DiscountDaysHaveToBeShifted' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DISCOUNT_DAYS_HAVE_TO_BE_S' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'RELEASE_INVOICE_EXPORT_PAR' ).

  ENDMETHOD.