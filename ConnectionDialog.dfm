object FormConnectDlg: TFormConnectDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Connect to Server'
  ClientHeight = 295
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label_ServerType: TLabel
    Left = 14
    Top = 84
    Width = 61
    Height = 13
    Caption = 'Server type:'
  end
  object Label_ServerName: TLabel
    Left = 14
    Top = 108
    Width = 65
    Height = 13
    Caption = 'Server name:'
  end
  object Label_Authentication: TLabel
    Left = 14
    Top = 133
    Width = 74
    Height = 13
    Caption = 'Authentication:'
  end
  object Label1: TLabel
    Left = 31
    Top = 158
    Width = 29
    Height = 13
    Caption = 'Login:'
  end
  object Label_Password: TLabel
    Left = 31
    Top = 183
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object Bevel1: TBevel
    Left = 12
    Top = 229
    Width = 453
    Height = 13
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 4
    Top = 61
    Width = 470
    Height = 13
    Shape = bsBottomLine
  end
  object Label_SQLServer: TLabel
    Left = 163
    Top = 18
    Width = 150
    Height = 37
    Caption = 'SQL Server'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -31
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Combo_ServerType: TComboBox
    Left = 170
    Top = 80
    Width = 295
    Height = 21
    Enabled = False
    ItemIndex = 0
    TabOrder = 0
    Text = 'Database Engine'
    Items.Strings = (
      'Database Engine')
  end
  object Combo_ServerName: TComboBox
    Left = 170
    Top = 105
    Width = 295
    Height = 21
    ItemIndex = 0
    TabOrder = 1
    Text = '(local)'
    Items.Strings = (
      '(local)')
  end
  object Combo_Authentication: TComboBox
    Left = 170
    Top = 130
    Width = 295
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 2
    Text = 'SQL Server AUthentication'
    Items.Strings = (
      'Windows Authentication'
      'SQL Server AUthentication')
  end
  object Combo_Login: TComboBox
    Left = 187
    Top = 155
    Width = 278
    Height = 21
    TabOrder = 3
  end
  object Edit_Password: TEdit
    Left = 187
    Top = 180
    Width = 278
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
  end
  object CheckBox_Remember: TCheckBox
    Left = 186
    Top = 206
    Width = 126
    Height = 17
    Caption = ' Remember password'
    TabOrder = 5
  end
  object Button_Connect: TButton
    Left = 301
    Top = 251
    Width = 75
    Height = 24
    Caption = 'Connect'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object Button_Cancel: TButton
    Left = 381
    Top = 251
    Width = 75
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
end
