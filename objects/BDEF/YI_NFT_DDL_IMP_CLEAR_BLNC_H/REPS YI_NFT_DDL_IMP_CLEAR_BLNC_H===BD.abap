managed implementation in class ybp_i_nft_ddl_imp_clear_blnc_h unique;
strict ( 1 );

define behavior for YI_NFT_DDL_IMP_CLEAR_BLNC_H //alias <alias_name>
with unmanaged save
lock master
authorization master ( instance )
//etag master <field_name>
{
  field ( readonly ) DeliveryDocument;
  association _item { }
}

define behavior for YI_NFT_DDL_IMP_CLEAR_BLNC_I //alias <alias_name>
with unmanaged save
lock dependent by _header
authorization dependent by _header
//etag master <field_name>
{
  field ( readonly ) deliverydocument, deliverydocumentitem, accountingdocument, companycode, fiscalyear, accountingdocumentitem;
  action ( features : instance ) ContinueButton parameter YI_NFT_DDL_IMP_CLR_BLNC_POPUP result [1] $self;
  association _header;
}