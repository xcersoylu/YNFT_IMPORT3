  METHOD def_post.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'POST' ).
    lo_function->set_edm_name( 'Post' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'POST' ).
    lo_function_import->set_edm_name( 'Post' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'SUPPLIER_INVOICE' ).
    lo_parameter->set_edm_name( 'SupplierInvoice' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SUPPLIER_INVOICE' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'FISCAL_YEAR' ).
    lo_parameter->set_edm_name( 'FiscalYear' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'FISCAL_YEAR' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'POST_INVOICE_EXPORT_PARAME' ).

  ENDMETHOD.