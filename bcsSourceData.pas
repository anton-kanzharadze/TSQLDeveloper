unit bcsSourceData;

interface

uses
  System.SysUtils, System.Classes, DB, System.Generics.Collections, Controls,
  clJsonSerializerBase, clJsonSerializer,
  tsdStrings, tsdSQLExecutor;

{In order to make object serializable do:
 1) define object in the interface part;
 2) mark properties with TclJsonString or TclJsonProperty attribute;
    if all props are Strings and all are empty, mark one as TclJsonRequired;
 3) use clJsonSerializerBase in the interface part.
 }
type
  TOEObject = class
  protected
    function GetDisplayName: String; virtual; abstract;
    function GetFullyQualifiedName: String; virtual;
  public
    property DisplayName: String read GetDisplayName;
    property FullyQualifiedName: String read GetFullyQualifiedName;
  end;

type
  TOEDatabase = class;

  TOEServer = class(TOEObject)
  protected
    function GetDisplayName: String; override;
  public
    Name,
    Urn,
    Product,
    Version: String;
    DatabaseListRead: Boolean;
    DatabaseList: TObjectList<TOEDatabase>;
    constructor Create(const AName, AUrn, AProduct, AVersion: String);
    destructor Destroy; override;
  end;

  TOETable = class;

  TOEDatabase = class(TOEObject)
  protected
    function GetDisplayName: String; override;
  private
    FServerName: String;
    FIsTableListRead: Boolean;
    FName,
    FUrn,
    FOwner: String;
    FGuid: TGuid;
    FTableList: TObjectList<TOETable>;
    FServer: TOEServer;
    function GetGuidAsString: String;
    procedure SetGuidAsString(const Value: String);
  public
    constructor Create(AServer: TOEServer; const AName, AUrn, AOwner: String; AGuid: TGuid);
    destructor Destroy; override;
    property ServerName: String read FServerName;
    property IsTableListRead: Boolean read FIsTableListRead;
    property Name: String read FName;
    property Urn: String read FUrn;
    property Owner: String read FOwner;
    {TGuid "as is" causes EInsufficientRTTI in REST.Json.TJson.ObjectToJsonString;
     it's also possible to use TJSONInterceptor:
     https://stackoverflow.com/questions/39980021/einsufficientrtti-exception-with-message-insufficient-rtti-available-to-support;
     }
    property GuidAsString: String read GetGuidAsString write SetGuidAsString;
    property TableList: TObjectList<TOETable> read FTableList;
  end;

  TOETable = class(TOEObject)
  private
    FDatabase: TOEDatabase;
    FServerName,
    FDatabaseName,
    FSchema,
    FName: String;
  protected
    function GetDisplayName: String; override;
    function GetFullyQualifiedName: String; override;
  public
    [TclJsonString('ServerName')]
    property ServerName: String read FServerName write FServerName;
    [TclJsonString('DatabaseName')]
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    [TclJsonString('Schema')]
    property Schema: String read FSchema write FSchema;
    [TclJsonString('Name')]
    property Name: String read FName write FName;
    constructor CreateDB(ADatabase: TOEDatabase; const ASchema, AName: String);
  end;

type
  TOEServerList = class(TObjectList<TOEServer>);


type
  TObjectKind = (
    okNone,
    okServer,
    okDatabaseList,
    okDatabase,
    okTableList,
    okTable
  );

type
  POENodeData = ^TOENodeData;
  TOENodeData = record
    ObjectKind: TObjectKind;
    OEObject: TOEObject;
  end;

type
  TDataSourceData = class(TDataModule)
    Strings_Servers: TtsdStringsKeeper;
    Strings_Databases: TtsdStringsKeeper;
    Strings_Tables: TtsdStringsKeeper;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FConnectionParams: TStrings;
    [weak] FSQLExecutor: ISQLExecutor;
    FServerList: TOEServerList;
    procedure ReadParams;
  public
    procedure Connect;
    procedure FillServerList(ATarget: TStrings);
    procedure ReadServerList;
    procedure FillDatabaseList(ATarget: TStrings; const AServerName: String);
    procedure ReadDatabaseList(AServer: TOEServer);
    procedure FillTableList(ATarget: TStrings; const ADatabaseName: String);
    procedure ReadTableList(ADatabase: TOEDatabase);
    property SQLExecutor: ISQLExecutor read FSQLExecutor write FSQLExecutor;
    property ServerList: TOEServerList read FServerList;
  end;

type
  TOEDragObject = class(TDragObjectEx)
    OEObject: TOEObject;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses Forms, System.StrUtils, ConnectionDialog;

procedure TDataSourceData.Connect;
var
  Remember: Boolean;
begin
  Remember := True;
  if TFormConnectDlg.GetConnectionParams(FConnectionParams, Remember) then
  begin
    FSQLExecutor.AssignParams(FConnectionParams);
    FSQLExecutor.Connected := True;
  end;
end;

procedure TDataSourceData.DataModuleCreate(Sender: TObject);
begin
  FConnectionParams := TStringList.Create;
  FServerList := TOEServerList.Create(True);
  ReadParams;
end;

procedure TDataSourceData.DataModuleDestroy(Sender: TObject);
begin
  FServerList.Free;
  FConnectionParams.Free;
end;

procedure TDataSourceData.FillDatabaseList(ATarget: TStrings; const AServerName: String);
var
  DS: TDataSet;
  SQL: String;
begin
  if not FSQLExecutor.Connected then
    Exit;

  Assert(ATarget <> nil);
  ATarget.Clear;
  SQL := Strings_Databases.SubstitutedText(['$(server_name)'], [AServerName]);
  DS:= FSQLExecutor.OpenDataset(SQL);
  try
    while not DS.Eof do
    begin
      ATarget.AddPair(DS.FieldByName('Database_Name').AsString, '');
      DS.Next;
    end;
  finally
    DS.Close;
  end;
end;

procedure TDataSourceData.FillTableList(ATarget: TStrings; const ADatabaseName: String);
var
  DS: TDataSet;
begin
  if not FSQLExecutor.Connected then
    Exit;

  Assert(ATarget <> nil);

  ATarget.Clear;
  FSQLExecutor.ExecSQL('USE ' + ADatabaseName);

  DS := FSQLExecutor.OpenDataset(Strings_Tables.Text);
  try
    while not DS.Eof do
    begin
      ATarget.AddPair(DS.FieldByName('Schema').AsString + '.' +
        DS.FieldByName('Name').AsString, '');
      DS.Next;
    end;
  finally
    DS.Close;
  end;
end;

procedure TDataSourceData.FillServerList(ATarget: TStrings);
var
  DS: TDataSet;
begin
  if not FSQLExecutor.Connected then
    Exit;

  Assert(ATarget <> nil);
  ATarget.Clear;

  DS := FSQLExecutor.OpenDataset(Strings_Servers.Text);
  try
    while not DS.Eof do
    begin
      ATarget.AddPair(DS.FieldByName('Server_Name').AsString,
        DS.FieldByName('Product').AsString + ';' +
        DS.FieldByName('Version').AsString);

      FServerList.Add(TOEServer.Create(
        DS.FieldByName('Server_Name').AsString,
        DS.FieldByName('Server_Urn').AsString,
        DS.FieldByName('Product').AsString,
        DS.FieldByName('Version').AsString)
        );

      DS.Next;
    end;
  finally
    DS.Close;
  end;
end;

procedure TDataSourceData.ReadParams;
const
  SQLServerAuthentication = True;
begin
//Integrated Security=SSPI;
//Persist Security Info=False;

  FConnectionParams.Clear;
  FConnectionParams.Add('Provider=SQLNCLI11.1');
  FConnectionParams.Add('Data Source=(local)');
  FConnectionParams.Add('Initial Catalog=master');
  FConnectionParams.Add('app=' + Application.Title);

  if SQLServerAuthentication then
  begin
    FConnectionParams.Add('User ID=sa');
    FConnectionParams.Add('Password=P@ssw0rd');
  end
  else
  begin
    FConnectionParams.Add('Integrated Security=SSPI');
    FConnectionParams.Add('Persist Security Info=False');
  end;
end;

procedure TDataSourceData.ReadServerList;
var
  DS: TDataSet;
begin
  FServerList.Clear;
  if not FSQLExecutor.Connected then
    Exit;

  DS := FSQLExecutor.OpenDataset(Strings_Servers.Text);
  try
    while not DS.Eof do
    begin
      FServerList.Add(TOEServer.Create(
        DS.FieldByName('Server_Name').AsString,
        DS.FieldByName('Server_Urn').AsString,
        DS.FieldByName('Product').AsString,
        DS.FieldByName('Version').AsString)
        );

      DS.Next;
    end;
  finally
    DS.Close;
  end;
end;

procedure TDataSourceData.ReadTableList(ADatabase: TOEDatabase);
var
  DS: TDataSet;
begin
  if not FSQLExecutor.Connected then
    Exit;

  FSQLExecutor.ExecSQL('USE ' + ADatabase.Name);

  DS := FSQLExecutor.OpenDataset(Strings_Tables.Text);
  try
    while not DS.Eof do
    begin
      ADatabase.TableList.Add(TOETable.CreateDB(
        ADatabase,
        DS.FieldByName('Schema').AsString,
        DS.FieldByName('Name').AsString
        ));
      DS.Next;
    end;
  finally
    DS.Close;
  end;end;

procedure TDataSourceData.ReadDatabaseList(AServer: TOEServer);
const
  method = 'ReadDatabaseList';
var
  DS: TDataSet;
  SQL: String;
  ServerName: String;
begin
  Assert(AServer <> nil);
  try
    AServer.DatabaseList.Clear;
    if not FSQLExecutor.Connected then
      Exit;
    ServerName := AServer.Name;
    SQL := Strings_Databases.SubstitutedText(['$(server_name)'], [ServerName]);
    DS:= FSQLExecutor.OpenDataset(SQL);
    try
      while not DS.Eof do
      begin
        AServer.DatabaseList.Add(TOEDatabase.Create(AServer,
          DS.FieldByName('Database_Name').AsString,
          DS.FieldByName('Database_Urn').AsString,
          DS.FieldByName('Database_Owner').AsString,
          DS.FieldByName('Database_DatabaseGuid').AsGuid
          ));
        DS.Next;
      end;
    finally
      DS.Close;
    end;
    AServer.DatabaseListRead := True;
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

{ TOEServer }

constructor TOEServer.Create(const AName, AUrn, AProduct, AVersion: String);
begin
  DatabaseList := TObjectList<TOEDatabase>.Create(True);
  Name := AName;
  Urn := AUrn;
  Product := AProduct;
  Version := AVersion;
end;

destructor TOEServer.Destroy;
begin
  DatabaseList.Free;
  inherited;
end;

function TOEServer.GetDisplayName: String;
begin
  Result := Name;
end;

{ TOEDatabase }

constructor TOEDatabase.Create(AServer: TOEServer; const AName, AUrn, AOwner: String; AGuid: TGuid);
begin
  FServer := AServer;
  FTableList := TObjectList<TOETable>.Create(True);
  FName := AName;
  FUrn := AUrn;
  FOwner := AOwner;
  FGuid := AGuid;
  FServerName := IfThen(FServer = nil, '<nil>', FServer.Name);
end;

destructor TOEDatabase.Destroy;
begin
  TableList.Free;
  inherited;
end;

function TOEDatabase.GetDisplayName: String;
begin
  Result := Name;
end;

function TOEDatabase.GetGuidAsString: String;
begin
  FGuid.ToString;
end;

procedure TOEDatabase.SetGuidAsString(const Value: String);
begin
  FGuid := StringToGuid(Value);
end;

{ TOETable }

constructor TOETable.CreateDB(ADatabase: TOEDatabase; const ASchema, AName: String);
begin
  FDatabase := ADatabase;
  FDatabaseName := IfThen(FDatabase = nil, '<nil>', FDatabase.Name);
  FServerName := IfThen(FDatabase = nil, '<nil>', FDatabase.ServerName);
  Schema := ASchema;
  Name := AName;
end;

function TOETable.GetDisplayName: String;
begin
  Result := Schema + '.' + Name;
end;

function TOETable.GetFullyQualifiedName: String;
begin
  Result := FDatabaseName + '.' + GetDisplayName;
end;

{ TOEObject }

function TOEObject.GetFullyQualifiedName: String;
begin
  Result := GetDisplayName;
end;

end.
