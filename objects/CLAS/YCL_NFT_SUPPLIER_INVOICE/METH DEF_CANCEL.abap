  METHOD def_cancel.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'CANCEL' ).
    lo_function->set_edm_name( 'Cancel' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'CANCEL' ).
    lo_function_import->set_edm_name( 'Cancel' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'POSTING_DATE' ).
    lo_parameter->set_edm_name( 'PostingDate' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'POSTING_DATE' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'REVERSAL_REASON' ).
    lo_parameter->set_edm_name( 'ReversalReason' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'REVERSAL_REASON' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'FISCAL_YEAR' ).
    lo_parameter->set_edm_name( 'FiscalYear' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'FISCAL_YEAR_3' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'SUPPLIER_INVOICE' ).
    lo_parameter->set_edm_name( 'SupplierInvoice' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SUPPLIER_INVOICE_3' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'CANCEL_INVOICE_EXPORT_PARA' ).

  ENDMETHOD.