managed implementation in class ybp_i_nft_ddl_imp_parameters unique;
strict ( 2 );

define behavior for YI_NFT_DDL_IMP_PARAMETERS //alias <alias_name>
persistent table ynft_t_parameter
lock master
authorization master ( instance )
//etag master <field_name>
{
  create;
  update;
  delete;
  field ( readonly : update ) ParameterName, ParameterKey;
}