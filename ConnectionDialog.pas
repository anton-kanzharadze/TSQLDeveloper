unit ConnectionDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage;

type
  TFormConnectDlg = class(TForm)
    Combo_ServerType: TComboBox;
    Combo_ServerName: TComboBox;
    Combo_Authentication: TComboBox;
    Combo_Login: TComboBox;
    Edit_Password: TEdit;
    CheckBox_Remember: TCheckBox;
    Label_ServerType: TLabel;
    Label_ServerName: TLabel;
    Label_Authentication: TLabel;
    Label1: TLabel;
    Label_Password: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button_Connect: TButton;
    Label_SQLServer: TLabel;
    Button_Cancel: TButton;
  private
    procedure FormToParams(AParams: TStrings);
    procedure ParamsToForm(AParams: TStrings);
  public
    class function GetConnectionParams(AParams: TStrings; var ARemember: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TForm3 }

procedure TFormConnectDlg.FormToParams(AParams: TStrings);
begin
  Assert(AParams <> nil);
  AParams.Clear;
  AParams.Add('Provider=SQLNCLI11.1');
  AParams.Add('Data Source=' + Combo_ServerName.Text);
  AParams.Add('Initial Catalog=master');
  AParams.Add('app=' + Application.Title);
  if Combo_Authentication.ItemIndex = 0 then
  begin
    AParams.Add('Integrated Security=SSPI');
    AParams.Add('Persist Security Info=False');
  end
  else
  begin
    AParams.Add('User ID=' + Combo_Login.Text);
    AParams.Add('Password=' + Edit_Password.Text);
  end;
end;

class function TFormConnectDlg.GetConnectionParams(AParams: TStrings; var ARemember: Boolean): Boolean;
var
  Dlg: TFormConnectDlg;
begin
  Dlg := TFormConnectDlg.Create(nil);
  try
    Dlg.CheckBox_Remember.Checked := ARemember;
    Dlg.ParamsToForm(AParams);
    Result := Dlg.ShowModal = mrOK;
    if Result then
    begin
      Dlg.FormToParams(AParams);
      ARemember := Dlg.CheckBox_Remember.Checked;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TFormConnectDlg.ParamsToForm(AParams: TStrings);
begin
  Combo_ServerName.Text := AParams.Values['Data Source'];
  if AParams.IndexOfName('User ID') >= 0 then
  begin
    Combo_Authentication.ItemIndex := 1;
    Combo_Login.Text := AParams.Values['User ID'];
    Edit_Password.Text := AParams.Values['Password'];
  end
  else
    Combo_Authentication.ItemIndex := 0;
  Combo_Login.Enabled := Combo_Authentication.ItemIndex = 1;
  Edit_Password.Enabled := Combo_Authentication.ItemIndex = 1;
end;

end.
