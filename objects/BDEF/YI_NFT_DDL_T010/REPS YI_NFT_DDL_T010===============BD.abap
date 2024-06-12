managed implementation in class ybp_i_nft_ddl_t010 unique;
strict ( 2 );

define behavior for YI_NFT_DDL_T010 //alias <alias_name>
persistent table ynft_t_t010
lock master
authorization master ( instance )
//etag master <field_name>
{
  field(readonly : update) bukrs;
  field(readonly : update) bklas;
  field(readonly : update) ctype;
  field(readonly) Ctype_tx;
  create;
  update;
  delete;
  mapping for znft_t_t010
  {
    Bklas = bklas ;
    Bukrs = bukrs ;
    Ctype = ctype ;
    Saknr = saknr ;
    Trssk = trssk ;
  }
}