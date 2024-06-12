projection;
strict ( 1 );

define behavior for YC_NFT_DDL_IMP_CLEAR_BLNC_H //alias <alias_name>
{

  use association _item;
}

define behavior for YC_NFT_DDL_IMP_CLEAR_BLNC_I //alias <alias_name>
{

  use action ContinueButton;

  use association _header;
}