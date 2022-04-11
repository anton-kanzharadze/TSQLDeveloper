unit bcsDataAdo;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Data.Win.ADODB,
  tsdSQLExecutor;

type
  TDataADO = class(TDataModule, ISQLExecutor)
    ADOConnection: TADOConnection;
    ADOQuery: TADOQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FParams: TStrings;
    function GetConnected: Boolean;
    procedure SetConnected(AValue: Boolean);
  public
    procedure ExecSQL(const ASQL: String);
    function OpenDataset(const ASQL: String): TDataSet;
    procedure AssignParams(AParams: TStrings);
    property Connected: Boolean read GetConnected write SetConnected;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataADO }

procedure TDataADO.AssignParams(AParams: TStrings);
begin
  FParams.Assign(AParams);
  ADOConnection.CursorLocation := clUseServer;
  ADOConnection.LoginPrompt := False;
  ADOConnection.ConnectionTimeOut := 30;

  FParams.Delimiter := ';';
  FParams.QuoteChar := #0;
  ADOConnection.ConnectionString := FParams.DelimitedText;
end;

procedure TDataADO.DataModuleCreate(Sender: TObject);
begin
  FParams := TStringList.Create;
end;

procedure TDataADO.DataModuleDestroy(Sender: TObject);
begin
  FParams.Free;
end;

procedure TDataADO.ExecSQL(const ASQL: String);
begin
  ADOQuery.Close;
  ADOQuery.SQL.Text := ASQL;
  ADOQuery.ExecSQL;
end;

function TDataADO.GetConnected: Boolean;
begin
  Result := ADOConnection.Connected;
end;

function TDataADO.OpenDataset(const ASQL: String): TDataSet;
begin
  ADOQuery.Close;
  ADOQuery.SQL.Text := ASQL;
  ADOQuery.Open;
  Result := ADOQuery;
end;

procedure TDataADO.SetConnected(AValue: Boolean);
begin
  //CoInitialize(nil);!! CoUninitialize
  ADOConnection.Connected := AValue;
end;

end.
