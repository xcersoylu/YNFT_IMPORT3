managed implementation in class ybp_i_nft_ddl_t006 unique;
strict ( 2 );

define behavior for YI_NFT_DDL_T006 //alias <alias_name>
persistent table ynft_t_t006
lock master
authorization master ( instance )
//etag master <field_name>
{
  field( readonly : update ) Vorgang;
  field( readonly : update ) Blart;
  create;
  update;
  delete;
  mapping for ynft_t_t006
  {
    Vorgang = vorgang;
    Blart = blart ;
  }
}