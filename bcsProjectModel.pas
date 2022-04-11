unit bcsProjectModel;

interface

uses Classes, System.Generics.Collections;

const
  defServerName = '(local)';

type
  TExpressionType = (etTable, etQuery);

const
  strExpressionType: array[TExpressionType] of String = (
    'table',
    'query'
  );

type
  TPMTableExpression = class
    ExpressionType: TExpressionType;
    Database,
    Schema,
    TableName: String;
    FileName: String;
    procedure Assign(Source: TPMTableExpression);
    function Clone: TPMTableExpression;
    function GetFullyQualifiedName: String;
  end;

type
  TPMSourceList = class(TObjectList<TPMTableExpression>)
  protected
    procedure Assign(Source: TPMSourceList);
  end;

type
  TPMTableList = class(TPMSourceList);

type
  TPMBasket = class(TPersistent)
  private
    FSourceTableList: TPMTableList;
    FSourceServer,
    FTargetServer: String;
    FOnListNotify: TCollectionNotifyEvent<TPMTableExpression>;
    function GetExpressions(AIndex: Integer): TPMTableExpression;
    function GetExpressionCount: Integer;
  protected
    procedure HandleNotifyEvent(Sender: TObject; const Item: TPMTableExpression;
      Action: TCollectionNotification);
    procedure AssignTo(Dest: TPersistent); override;
    property SourceTableList: TPMTableList read FSourceTableList;
  public
    constructor Create;
    destructor Destroy; override;
    {}
    procedure Clear;
    //function IsEqual(AOther: TPMBasket): Boolean;
    function AddExpression(AExpression: TPMTableExpression): Integer;
    procedure RemoveExpression(AExpression: TPMTableExpression);
    property Expressions[AIndex: Integer]: TPMTableExpression read GetExpressions;
    property ExpressionCount: Integer read GetExpressionCount;
    {}
    property SourceServer: String read FSourceServer write FSourceServer;
    property TargetServer: String read FTargetServer write FTargetServer;
    property OnListNotify: TCollectionNotifyEvent<TPMTableExpression> read FOnListNotify
      write FOnListNotify;
  end;

function ComputerName: String;
function ServerNameIsLocal(const AName: String): Boolean;

implementation

{ TPMBasket }

uses Windows, SysUtils;

function ComputerName: String;
var
  P: array[0..Windows.MAX_COMPUTERNAME_LENGTH] of Char;
  Len: DWORD;
begin
  Len := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName(P, Len);
  Result := String(P);
end;

function ServerNameIsLocal(const AName: String): Boolean;
begin
  Result := AName = defServerName;
  if not Result then
    Result := Trim(Uppercase(AName)) = Trim(Uppercase(ComputerName));
end;

function TPMBasket.AddExpression(AExpression: TPMTableExpression): Integer;
begin
  Result := FSourceTableList.Add(AExpression);
end;

procedure TPMBasket.AssignTo(Dest: TPersistent);
var
  DestBasket: TPMBasket;
begin
  if not (Dest is TPMBasket) then
    inherited AssignTo(Dest)
  else
  begin
    DestBasket := TPMBasket(Dest);
    DestBasket.Clear;
    DestBasket.FSourceTableList.Assign(FSourceTableList);
  end;
end;

procedure TPMBasket.Clear;
begin
  FSourceTableList.Clear;
  FSourceServer := '';
  FTargetServer := '';
end;

constructor TPMBasket.Create;
begin
  inherited Create;
  FSourceTableList := TPMTableList.Create({AOwnObjects=}True);
  FSourceTableList.OnNotify := HandleNotifyEvent;
end;

destructor TPMBasket.Destroy;
begin
  FSourceTableList.Free;
  inherited Destroy;
end;

function TPMBasket.GetExpressionCount: Integer;
begin
  Result := FSourceTableList.Count;
end;

function TPMBasket.GetExpressions(AIndex: Integer): TPMTableExpression;
begin
  Result := SourceTableList[AIndex];
end;

procedure TPMBasket.HandleNotifyEvent(Sender: TObject; const Item: TPMTableExpression;
  Action: TCollectionNotification);
begin
  if Assigned(FOnListNotify) then
    FOnListNotify(Sender, Item, Action);
end;

procedure TPMBasket.RemoveExpression(AExpression: TPMTableExpression);
begin
  FSourceTableList.Remove(AExpression);
  if FSourceTableList.Count = 0 then
    FSourceServer := '';
end;

{ TPMSourceList }

procedure TPMSourceList.Assign(Source: TPMSourceList);
var
  I: Integer;
begin
  Assert(Source <> nil);
  Clear;
  for I := 0 to Source.Count - 1 do
    Add(Source.Items[I].Clone);
end;

{ TPMTableExpression }

procedure TPMTableExpression.Assign(Source: TPMTableExpression);
begin
  ExpressionType := Source.ExpressionType;
  Database := Source.Database;
  Schema := Source.Schema;
  TableName := Source.TableName;
  FileName := Source.FileName;
end;

function TPMTableExpression.Clone: TPMTableExpression;
begin
  Result := TPMTableExpression.Create;
  Result.Assign(Self);
end;

function TPMTableExpression.GetFullyQualifiedName: String;
begin
  Result := Database + '.' + Schema + '.' + TableName;
end;

end.
