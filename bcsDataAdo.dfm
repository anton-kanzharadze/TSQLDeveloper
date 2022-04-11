object DataADO: TDataADO
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 308
  Width = 489
  object ADOConnection: TADOConnection
    ConnectionString = 
      'Provider=SQLNCLI11.1;Persist Security Info=False;User ID=sa;Init' +
      'ial Catalog=master;Data Source=(local);Initial File Name="";Serv' +
      'er SPN="";'
    LoginPrompt = False
    Provider = 'SQLNCLI11.1'
    Left = 88
    Top = 72
  end
  object ADOQuery: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    Left = 208
    Top = 72
  end
end
