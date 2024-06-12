CLASS ycl_nft_imp_util_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS cunit_output IMPORTING iv_unitofmeasure TYPE msehi RETURNING VALUE(rv_unitofmeasure) TYPE ynft_e_mseh3.
    CLASS-METHODS cunit_input IMPORTING iv_unitofmeasure TYPE ynft_e_mseh3 RETURNING VALUE(rv_unitofmeasure) TYPE msehi .
    CONSTANTS mc_msgid TYPE sy-msgid VALUE 'YNFT_IMP_MC'.