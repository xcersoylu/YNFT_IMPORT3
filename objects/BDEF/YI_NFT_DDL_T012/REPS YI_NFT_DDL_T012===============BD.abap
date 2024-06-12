managed implementation in class ybp_i_nft_ddl_t012 unique;
strict ( 2 );

define behavior for YI_NFT_DDL_T012 //alias <alias_name>
persistent table ynft_t_t012
lock master
authorization master ( instance )
//etag master <field_name>
{
   field ( readonly : update ) Vorgang, Kalsm, Mwskz;
  create;
  update;
  delete;
  mapping for znft_t_t012
    {
      Vorgang = vorgang;
      Kalsm   = kalsm;
      Mwskz   = mwskz;
    }
}