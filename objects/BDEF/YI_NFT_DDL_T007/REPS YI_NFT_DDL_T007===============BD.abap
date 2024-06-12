managed implementation in class ybp_i_nft_ddl_t007 unique;
strict ( 2 );

define behavior for YI_NFT_DDL_T007 //alias <alias_name>
persistent table ynft_t_t007
lock master
authorization master ( instance )
//etag master <field_name>
{
  field( readonly : update ) bukrs;
  create;
  update;
  delete;
}