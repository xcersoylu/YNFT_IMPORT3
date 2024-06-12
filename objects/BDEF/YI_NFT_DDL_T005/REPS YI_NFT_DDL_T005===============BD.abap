managed implementation in class ybp_i_nft_ddl_t005 unique;
strict ( 2 );

define behavior for YI_NFT_DDL_T005 //alias <alias_name>
persistent table ynft_t_t005
lock master
authorization master ( instance )
//etag master <field_name>
{
  field( readonly : update ) Csrce;
  field( readonly : update ) Ctype;
  field( readonly) Csrce_Tx , Ctype_Tx;
  create;
  update;
  delete;
  mapping for znft_t_t005
  {
    Csrce = csrce;
    Ctype = ctype;
  }
}