object DataGenerator: TDataGenerator
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 222
  Width = 215
  object tsdStrings_Header: TtsdStringsKeeper
    Lines.Strings = (
      '@echo off'
      ''
      ':: '#1055#1091#1090#1080' '
      
        'SET BCP="C:\Program Files\Microsoft SQL Server\110\Tools\Binn\bc' +
        'p.exe"'
      'SET WRKPATH=%~dp0Tmp\'
      'SET LOG=%WRKPATH%bcp.log'
      ''
      ':get_srcdb'
      ':: '#1041#1072#1079#1072'-'#1080#1089#1090#1086#1095#1085#1080#1082
      'SET SRCSRV=$(source_server)'
      'SET SRCDB=Shturman3.dbo.'
      'SET SRCUSER=sa'
      'SET /P SRCPASS="Enter source password: "'
      ''
      '@ECHO see log: %LOG%'
      ''
      'rem Create or clear work folder'
      'IF NOT EXIST Tmp (MD Tmp) ELSE (DEL Tmp\*.* /Q)'
      ''
      'rem Delete log file'
      'IF EXIST "%LOG%" DEL /Q %LOG% >NUL'
      ''
      'rem Start logging'
      'ECHO %time% %OPERATION% started >> %LOG%'
      ''
      '')
    Left = 48
    Top = 24
  end
  object tsdStrings_Table: TtsdStringsKeeper
    Lines.Strings = (
      'SET FILENAME=$(file_name)'
      'SET TABLENAME=$(full_table_name)'
      'SET EXPRESSION=$(full_table_name)'
      'SET CMD=out'
      'CALL :export_import')
    Left = 144
    Top = 24
  end
  object tsdStrings_Query: TtsdStringsKeeper
    Lines.Strings = (
      'SET TABLENAME=Stations'
      
        'SET EXPRESSION="select * from %SRCDB%%TABLENAME% where Stations_' +
        'Types_Id = 1"'
      'SET CMD=queryout'
      'CALL :export_import')
    Left = 144
    Top = 88
  end
  object tsdStrings_Footer: TtsdStringsKeeper
    Lines.Strings = (
      'rem Stop logging'
      'ECHO %time% %OPERATION% finished >> %LOG%'
      'pause'
      'EXIT /B %ERRORLEVEL%'
      ''
      ':export_import'
      ''
      ':export'
      'SET STARTTIME=%TIME%'
      
        'ECHO %STARTTIME% Starting export "%TABLENAME%" from %SRCSRV% >> ' +
        '%LOG%'
      
        '%BCP% %EXPRESSION% %CMD% %WRKPATH%%FILENAME%.dat -n -e %WRKPATH%' +
        '%FILENAME%_export.error -o %WRKPATH%%FILENAME%_export.log -S %SR' +
        'CSRV% -U %SRCUSER% -P %SRCPASS%'
      
        '%BCP% %TABLENAME% format nul -f %WRKPATH%%FILENAME%.fmt -n -S %S' +
        'RCSRV% -U sa -P %SRCPASS%'
      'set ENDTIME=%TIME%'
      
        'set /A STARTTIME=(1%STARTTIME:~0,2%-100)*360000 + (1%STARTTIME:~' +
        '3,2%-100)*6000 + (1%STARTTIME:~6,2%-100)*100 + (1%STARTTIME:~9,2' +
        '%-100)'
      
        'set /A ENDTIME=(1%ENDTIME:~0,2%-100)*360000 + (1%ENDTIME:~3,2%-1' +
        '00)*6000 + (1%ENDTIME:~6,2%-100)*100 + (1%ENDTIME:~9,2%-100)'
      'set /A DURATION=(%ENDTIME%-%STARTTIME%)/100'
      'ECHO %TIME% Done in %DURATION% sec >> %LOG%'
      'EXIT /B 0')
    Left = 48
    Top = 88
  end
end
