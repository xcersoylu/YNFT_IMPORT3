projection;
strict ( 2 );

define behavior for YC_NFT_DDL_T001 //alias <alias_name>
{
  use create;
  use update;
  use delete;

  use association _T002 { create; }
}

define behavior for YC_NFT_DDL_T002 //alias <alias_name>
{
  use update;
  use delete;

  use association _T001;
}