unit bcsProjectXML;

interface

uses Classes, NativeXml,
  bcsProjectModel;

procedure BasketToXml(ABasket: TPMBasket; AXml: TNativeXml);
function XmlToBasket(AXml: TNativeXml; ABasket: TPMBasket): Boolean;
procedure SaveBasket(ABasket: TPMBasket; const AFileName: String);
procedure LoadBasket(ABasket: TPMBasket; const AFileName: String);

procedure BasketToStrings(ABasket: TPMBasket; ATarget: TStrings);
function StringsToBasket(ASource: TStrings; ABasket: TPMBasket): Boolean;

implementation

uses SysUtils, StrUtils;

const
  nodeProject = 'project';
  nodeSourceServer = 'source_server';
  nodeSourceTables = 'source_tables';
  attrName = 'name';
  nodeTable = 'table';
  attrDatabase = 'database';
  attrSchema = 'schema';
  attrTableName = 'name';
  attrFileName = 'file_name';

procedure SaveBasket(ABasket: TPMBasket; const AFileName: String);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.Create(nil);
  Xml.XmlFormat := xfReadable;
  try
    BasketToXml(ABasket, XML);
    Xml.SaveToFile(AFileName);
  finally
    Xml.Free
  end;
end;

procedure BasketToStrings(ABasket: TPMBasket; ATarget: TStrings);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.Create(nil);
  Xml.XmlFormat := xfReadable;
  try
    BasketToXml(ABasket, XML);
    ATarget.Text := Xml.WriteToString;
  finally
    Xml.Free
  end;
end;

function StringsToBasket(ASource: TStrings; ABasket: TPMBasket): Boolean;
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.Create(nil);
  try
    Xml.ReadFromString(ASource.Text);
    Result := XmlToBasket(XML, ABasket);
    Xml.Free;
  except
    Xml.Free;
    Result := False;
  end;
end;

procedure LoadBasket(ABasket: TPMBasket; const AFileName: String);
var
  Xml: TNativeXml;
begin
  Xml := TNativeXml.Create(nil);
  try
    Xml.LoadFromFile(AFileName);
    XmlToBasket(XML, ABasket);
  finally
    Xml.Free
  end;
end;

procedure BasketToXml(ABasket: TPMBasket; AXml: TNativeXml);
var
  I: Integer;
  ParentNode, Node: TXmlNode;
  Expr: TPMTableExpression;
begin
  AXml.New;
  AXml.Root.Name := nodeProject;

  ParentNode := AXml.Root;

  Node := TsdElement.CreateParent(AXml, ParentNode);
  Node.Name := nodeSourceServer;
  Node.AttributeAdd(attrName,
    IfThen(ABasket.SourceServer.IsEmpty, defServerName, ABasket.SourceServer));

  Node := TsdElement.CreateParent(AXml, ParentNode);
  Node.Name := nodeSourceTables;

  ParentNode := Node;
  for I := 0 to ABasket.ExpressionCount - 1 do
  begin
    Node := TsdElement.CreateParent(AXml, ParentNode);
    Node.Name := nodeTable;
    Expr := ABasket.Expressions[I];
    Node.AttributeAdd(attrDatabase, Expr.Database);
    Node.AttributeAdd(attrSchema, Expr.Schema);
    Node.AttributeAdd(attrTableName, Expr.TableName);
    Node.AttributeAdd(attrFileName, Expr.FileName);
  end;
end;

function XmlToBasket(AXml: TNativeXml; ABasket: TPMBasket): Boolean;
var
  I, J: Integer;
  Project, Node, TableList, TableNode: TXmlNode;
  Attr: TsdAttribute;
  Expr: TPMTableExpression;
  ErrCount: Integer;
begin
  ABasket.Clear;
  ErrCount := 0;

  Project := AXml.Root;

  ABasket.SourceServer := defServerName;{??}
  Node := Project.NodeByName(nodeSourceServer);
  if Node = nil then
    ErrCount := ErrCount + 1
  else
  begin
    Attr := Node.AttributeByName[attrName];
    ABasket.SourceServer := Attr.Value;
  end;

  TableList := Project.NodeByName(nodeSourceTables);
  if TableList = nil then
    ErrCount := ErrCount + 1
  else
    for J := 0 to TableList.NodeCount - 1 do
    begin
      TableNode := TableList.Nodes[J];
      if TableNode.Name <> nodeTable then
        ErrCount := ErrCount + 1
      else
      begin
        Expr := TPMTableExpression.Create;
        ABasket.AddExpression(Expr);
        Expr.ExpressionType := etTable;
        Expr.Database := TableNode.AttributeValueByName[attrDatabase];
        Expr.Schema := TableNode.AttributeValueByName[attrSchema];
        Expr.TableName := TableNode.AttributeValueByName[attrTableName];
        Expr.FileName := TableNode.AttributeValueByName[attrFileName];
      end;
    end;

  Result := ErrCount = 0;
end;

end.
