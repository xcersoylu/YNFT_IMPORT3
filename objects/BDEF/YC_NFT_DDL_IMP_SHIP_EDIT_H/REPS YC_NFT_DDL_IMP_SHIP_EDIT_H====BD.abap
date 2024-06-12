projection;
strict ( 2 );

define behavior for YC_NFT_DDL_IMP_SHIP_EDIT_H //alias <alias_name>
{
  use update;
  use delete;

  use association _item;
}

define behavior for YC_NFT_DDL_IMP_SHIP_EDIT_I //alias <alias_name>
{
  use update;
  use delete;

  use association _header;
}