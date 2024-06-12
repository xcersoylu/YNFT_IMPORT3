managed implementation in class ybp_i_nft_ddl_t001 unique;
strict ( 2 );

define behavior for YI_NFT_DDL_T001 //alias <alias_name>
persistent table ynft_t_t001
lock master
authorization master ( instance )
//etag master <field_name>
{
  field ( readonly : update ) bsart;
  create;
  update;
  delete;
  association _T002 { create; }
  mapping for ynft_t_t001
    {
      Bsart   = bsart;
      Bstae   = bstae;
    }
}

define behavior for yI_NFT_DDL_T002 //alias <alias_name>
persistent table ynft_t_t002
lock dependent by _T001
authorization dependent by _T001
//etag master <field_name>
{
  field ( readonly ) Bsart;
  field ( readonly : update ) Ddtyp;
  update;
  delete;
  association _T001;
  mapping for ynft_t_t002
    {
      Bsart   = bsart;
      Ddtyp   = ddtyp;
      Lfart   = lfart;
      Gmpos   = gmpos;
    }
}