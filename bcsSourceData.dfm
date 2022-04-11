object DataSourceData: TDataSourceData
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 274
  Width = 447
  object Strings_Servers: TtsdStringsKeeper
    Lines.Strings = (
      'SELECT'
      'CAST(serverproperty(N'#39'Servername'#39') AS sysname) AS [Server_Name],'
      
        #39'Server[@Name='#39' + quotename(CAST(serverproperty(N'#39'Servername'#39') A' +
        'S sysname),'#39#39#39#39') + '#39']'#39' AS [Server_Urn],'
      'CAST(SERVERPROPERTY(N'#39'Edition'#39') AS sysname) AS [Product],'
      'SERVERPROPERTY(N'#39'ProductVersion'#39') AS [Version]'
      'ORDER BY'
      '[Server_Name] ASC'
      '')
    Left = 296
    Top = 24
  end
  object Strings_Databases: TtsdStringsKeeper
    Lines.Strings = (
      'SELECT'
      'dtb.name AS [Database_Name],'
      #39'Server[@Name='#39' + quotename(CAST('
      '        serverproperty(N'#39'Servername'#39')'
      
        '        AS sysname),'#39#39#39#39') + '#39']'#39' + '#39'/Database[@Name='#39' + quotename' +
        '(dtb.name,'#39#39#39#39') + '#39']'#39' AS [Database_Urn],'
      'ISNULL(suser_sname(dtb.owner_sid),'#39#39') AS [Database_Owner],'
      'drs.database_guid AS [Database_DatabaseGuid]'
      'FROM'
      'master.sys.databases AS dtb'
      
        'LEFT OUTER JOIN sys.database_mirroring AS dmi ON dmi.database_id' +
        ' = dtb.database_id'
      
        'LEFT OUTER JOIN sys.database_recovery_status AS drs ON drs.datab' +
        'ase_id = dtb.database_id'
      'WHERE'
      
        '(CAST(case when dtb.name in ('#39'master'#39','#39'model'#39','#39'msdb'#39','#39'tempdb'#39') t' +
        'hen 1 else dtb.is_distributor end AS bit)=0 and CAST(isnull(dtb.' +
        'source_database_id, 0) AS bit)=0)'
      
        'and CAST(serverproperty(N'#39'Servername'#39') AS sysname) = '#39'$(server_n' +
        'ame)'#39
      'ORDER BY'
      '[Database_Name] ASC'
      '')
    Left = 296
    Top = 72
  end
  object Strings_Tables: TtsdStringsKeeper
    Lines.Strings = (
      'SELECT'
      'tbl.name AS [Name],'
      'SCHEMA_NAME(tbl.schema_id) AS [Schema]'
      'FROM'
      'sys.tables AS tbl                                         '
      'WHERE'
      '(CAST('
      'case '
      '    when tbl.is_ms_shipped = 1 then 1'
      '    when ('
      '        select '
      '            major_id '
      '        from '
      '            sys.extended_properties '
      '        where '
      '            major_id = tbl.object_id and '
      '            minor_id = 0 and '
      '            class = 1 and '
      '            name = N'#39'microsoft_database_tools_support'#39') '
      '        is not null then 1'
      '    else 0'
      'end          '
      '            AS bit)=0 and tbl.is_filetable=0)'
      'ORDER BY'
      '[Schema] ASC,[Name] ASC'
      ''
      '')
    Left = 296
    Top = 128
  end
end
