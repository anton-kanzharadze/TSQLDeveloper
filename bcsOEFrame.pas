unit bcsOEFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, CommCtrl,
  System.ImageList, Vcl.ImgList, System.Actions,
  Vcl.ActnList, Vcl.Menus, Vcl.ExtCtrls, Vcl.StdActns,
  VirtualTrees,
  clJsonSerializerBase, clJsonSerializer,
  tsdCommunicate,
  bcsSourceData;

{ with VirtualStringTree do begin
    ClipboardFormats.Strings := (
      'Plain text'
      'Unicode text'
      'Virtual Tree Data'); //CFSTR_VIRTUALTREE
    DragType := dtOLE;
    DragMode := dmAutomatic;
    //TreeOptions.MiscOptions += toInitOnSave;
  end;
}

type
  TFrameObjectExplorer = class(TFrame)
    ImageList: TImageList;
    tsdCommunicator: TtsdCommunicator;
    PopupMenu: TPopupMenu;
    ActionList: TActionList;
    Action_Connected: TAction;
    Connected1: TMenuItem;
    VirtualStringTree: TVirtualStringTree;
    Action_Expand: TAction;
    Action_Collapse: TAction;
    Expand1: TMenuItem;
    Collapse1: TMenuItem;
    EditCopy1: TEditCopy;
    N1: TMenuItem;
    Copy1: TMenuItem;
{$IFDEF USETREEVIEW}
    procedure TreeView_OEExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeView_OEMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
{$ENDIF}
    procedure CommunicatorFunctions0Execute(Sender: TFunction;
      const AInParams: array of Variant; out AOutParam: Variant);
    procedure Action_ConnectedExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure VirtualStringTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VirtualStringTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VirtualStringTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VirtualStringTreeStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure VirtualStringTreeDragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeView1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure Action_ExpandExecute(Sender: TObject);
    procedure Action_CollapseExecute(Sender: TObject);
    procedure VirtualStringTreeGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
    procedure VirtualStringTreeSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
  private
    FData: TDataSourceData;
    FBuffer: TStrings;
    FServers: TStrings;
    FSerializer: TclJsonSerializer;
  protected
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  public
{$IFDEF USETREEVIEW}
    procedure FillServers;
    procedure FillDatabases(ANode: TTreeNode);
    procedure FillTables(ANode: TTreeNode);
{$ENDIF}
    property Data: TDataSourceData read FData write FData;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses REST.Json;

const
  lvlDatabases = 1;
  lvlTables = 3;

const
  cptEtcetera = '...';

const
  imgFolder = 0;
  imgServer = 1;
  imgDatabase = 2;
  imgTable = 3;

const
  objImages: array[TObjectKind] of Integer = (
    -1, 1, 0, 2, 0, 3
  );

const
  objLevels: array[TObjectKind] of Integer = (
    -1, 0, 1, 2, 3, 4
  );

const
  strObjectKind: array[TObjectKind] of String = (
    'None',
    '?',
    'Databases',
    '?',
    'Tables',
    '?'
  );

const
  hasChildren: array[TObjectKind] of Boolean = (
    False, True, True, True, True, False
  );

{ TFrameObjectExplorer }

procedure TFrameObjectExplorer.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action = Action_Connected then
    TAction(Action).Checked := FData.SQLExecutor.Connected
  else if (Action = Action_Collapse) or (Action = Action_Expand) then
     TAction(Action).Enabled := VirtualStringTree.FocusedNode <> nil
  ;
  Handled := True;
end;

procedure TFrameObjectExplorer.Action_ConnectedExecute(Sender: TObject);
begin
  FData.Connect;
  if FData.SQLExecutor.Connected then
  begin
{$IFDEF USETREEVIEW}
    FillServers;
{$ENDIF}
    FData.ReadServerList;
    VirtualStringTree.RootNodeCount := FData.ServerList.Count; {-> VirtualStringTreeInitNode}
  end;
end;

procedure TFrameObjectExplorer.Action_ExpandExecute(Sender: TObject);
begin
  if VirtualStringTree.FocusedNode <> nil then
    VirtualStringTree.FullExpand(VirtualStringTree.FocusedNode);
end;

procedure TFrameObjectExplorer.Action_CollapseExecute(Sender: TObject);
begin
  if VirtualStringTree.FocusedNode <> nil then
    VirtualStringTree.FullCollapse(VirtualStringTree.FocusedNode);
end;

procedure TFrameObjectExplorer.CMShowingChanged(var Message: TMessage);
begin
  inherited;
//  TreeView_OE.Perform(TVM_SETITEMHEIGHT, 18, 0);
end;

procedure TFrameObjectExplorer.CommunicatorFunctions0Execute(Sender: TFunction;
  const AInParams: array of Variant; out AOutParam: Variant);
begin
  AOutParam := '';
end;

constructor TFrameObjectExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSerializer := TclJsonSerializer.Create;
  FBuffer := TStringList.Create;
  VirtualStringTree.NodeDataSize := SizeOf(TOENodeData);
  FServers := TStringList.Create;

  {Important for Drag&drop work properly:}
  Assert(VirtualStringTree.DragType = dtOLE);
  Assert(VirtualStringTree.ClipboardFormats.IndexOf(CFSTR_VIRTUALTREE) >= 0);
end;

destructor TFrameObjectExplorer.Destroy;
begin
  FServers.Free;
  FBuffer.Free;
  FSerializer.Free;
  inherited;
end;

procedure TFrameObjectExplorer.TreeView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragObject := TOEDragObject.Create;
  TOEDragObject(DragObject).OEObject := nil;
end;

{$IFDEF USETREEVIEW}
procedure TFrameObjectExplorer.FillDatabases(ANode: TTreeNode);
var
  Node,
  NodeChild: TTreeNode;
  ServerName: String;
  I: Integer;
  ObjectName: String;
begin
  ServerName := ANode.Parent.Text;
  FData.FillDatabaseList(FBuffer, ServerName);

  for I := ANode.Count - 1 downto 0 do
    ANode.Item[I].Free;

  for I := 0 to FBuffer.Count - 1 do
  begin
    ObjectName := FBuffer.Names[I];
    Node := TreeView_OE.Items.AddChild(ANode, ObjectName);
    Node.ImageIndex := imgDatabase;
    Node.SelectedIndex := imgDatabase;
    NodeChild := TreeView_OE.Items.AddChild(Node, 'Tables');
    NodeChild.ImageIndex := imgFolder;
    NodeChild.SelectedIndex := imgFolder;
    {NodeDummy := }TreeView_OE.Items.AddChild(NodeChild, cptEtcetera);
  end;
end;

procedure TFrameObjectExplorer.FillTables(ANode: TTreeNode);
var
  Node,
  NodeChild: TTreeNode;
  DatabaseName: String;
  I: Integer;
  ObjectName: String;
begin
  DatabaseName := ANode.Parent.Text;
  FData.FillTableList(FBuffer, DatabaseName);
  for I := ANode.Count - 1 downto 0 do
    ANode.Item[I].Free;

  for I := 0 to FBuffer.Count - 1 do
  begin
    ObjectName := FBuffer.Names[I];
    Node := TreeView_OE.Items.AddChild(ANode, ObjectName);
    Node.ImageIndex := imgTable;
    Node.SelectedIndex := imgTable;
    NodeChild := TreeView_OE.Items.AddChild(Node, 'Columns');
    NodeChild.ImageIndex := imgFolder;
    NodeChild.SelectedIndex := imgFolder;
    {NodeDummy := }TreeView_OE.Items.AddChild(NodeChild, cptEtcetera);
  end;
end;

procedure TFrameObjectExplorer.FillServers;
var
  NodeServer,
  NodeDatabases: TTreeNode;
  I: Integer;
  ObjectName: String;
begin
  TreeView_OE.Items.Clear;
  FData.FillServerList(FBuffer);
  FServers.Assign(FBuffer);
  for I := 0 to FBuffer.Count - 1 do
  begin
    ObjectName := FBuffer.Names[I];
    NodeServer := TreeView_OE.Items.Add(nil, ObjectName);
    NodeServer.ImageIndex := imgServer;
    NodeServer.SelectedIndex := imgServer;
    NodeDatabases := TreeView_OE.Items.AddChild(NodeServer, 'Databases');
    NodeDatabases.ImageIndex := imgFolder;
    NodeDatabases.SelectedIndex := imgFolder;
    NodeServer.Expand(False);
    {NodeDummy := }TreeView_OE.Items.AddChild(NodeDatabases, cptEtcetera);
  end;
end;

procedure TFrameObjectExplorer.TreeView_OEExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.Level = lvlDatabases then
    if not Node.HasChildren or (Node.Count = 1) and (Node.Item[0].Text = cptEtcetera) then
      FillDatabases(Node)
    else
  else if Node.Level = lvlTables then
    if not Node.HasChildren or (Node.Count = 1) and (Node.Item[0].Text = cptEtcetera) then
      FillTables(Node)
    else
  ;
end;

procedure TFrameObjectExplorer.TreeView_OEMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TTreeView).BeginDrag(False);
end;

{$ENDIF}

procedure TFrameObjectExplorer.VirtualStringTreeInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
const
  method = 'VirtualStringTreeInitChildren';
var
  Data,
  ParentData: POENodeData;
  Srv: TOEServer;
  Db: TOEDatabase;
  Lvl: Cardinal;
begin
  Lvl := 0;{wr}
  try
    Lvl := Sender.GetNodeLevel(Node);
    Data := Sender.GetNodeData(Node);

    if Data^.ObjectKind in [okServer, okDatabase] then
      ChildCount := 1
    else if Data^.ObjectKind = okDatabaseList then
    begin
      ParentData := Sender.GetNodeData(Node.Parent);
      Assert(ParentData^.ObjectKind = okServer);
      Srv := ParentData^.OEObject as TOEServer;
      if not Srv.DatabaseListRead then
        FData.ReadDatabaseList(Srv);
      ChildCount := Srv.DatabaseList.Count;
    end
    else if Data^.ObjectKind = okTableList then
    begin
      ParentData := Sender.GetNodeData(Node.Parent);
      Assert(ParentData^.ObjectKind = okDatabase);
      Db := ParentData^.OEObject as TOEDatabase;
      if not Db.IsTableListRead then
        FData.ReadTableList(Db);
      ChildCount := Db.TableList.Count;
    end;

  except
    on E: Exception do
      raise Exception.CreateFmt('%d, %s: %s', [Lvl, method, E.Message]);
  end;
end;

procedure TFrameObjectExplorer.VirtualStringTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
const
  method = 'VirtualStringTreeInitNode';
var
  Data,
  ParentData,
  GrandParentData: POENodeData;
  Lvl: Cardinal;
begin
  try
    Data := Sender.GetNodeData(Node);
    Lvl := Sender.GetNodeLevel(Node);
    if Lvl = 0 then
    begin
      Data^.ObjectKind := okServer;
      Data^.OEObject := FData.ServerList[Node.Index];
    end
    else
    begin
      ParentData := Sender.GetNodeData(Node.Parent);
      Data^.OEObject := nil;
      case ParentData^.ObjectKind of
        okServer:
          Data^.ObjectKind := okDatabaseList;
        okDatabaseList:
          begin
            GrandParentData := Sender.GetNodeData(Node.Parent.Parent);
            Assert(GrandParentData^.ObjectKind = okServer);
            Data^.OEObject := (GrandParentData^.OEObject as TOEServer).
              DatabaseList[Node.Index];
            Data^.ObjectKind := okDatabase;
          end;
        okDatabase:
          Data^.ObjectKind := okTableList;
        okTableList:
          begin
            GrandParentData := Sender.GetNodeData(Node.Parent.Parent);
            Assert(GrandParentData^.ObjectKind = okDatabase);
            Data^.OEObject := (GrandParentData.OEObject as TOEDatabase).
              TableList[Node.Index];
            Data^.ObjectKind := okTable;
          end;
      end;
    end;
    if hasChildren[Data^.ObjectKind] then
      Include(InitialStates, ivsHasChildren)
    else
      Exclude(InitialStates, ivsHasChildren);
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

procedure TFrameObjectExplorer.VirtualStringTreeDragAllowed(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
const
  method = 'VirtualStringTreeDragAllowed';
var
  Data: POENodeData;
begin
  try
    Data := Sender.GetNodeData(Node);
    Allowed := Data^.ObjectKind = okTable;
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

procedure TFrameObjectExplorer.VirtualStringTreeSaveNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);

  procedure WriteStringToStream(const AValue: String);
  var
    Kind: Word;
    Len: Integer;
    Bytes: TBytes;
  begin
    Kind := varString;
    Stream.WriteData(Kind);

    Bytes := TEncoding.UTF8.GetBytes(AValue);
    Len := Length(Bytes);
    Stream.WriteData(Len);

    Stream.Write(Bytes, Len);
  end;

const
  method = 'VirtualStringTreeSaveNode';
var
  Data: POENodeData;
  OETable: TOETable;
  Str: String;
begin
  try
    Data := Sender.GetNodeData(Node);
    if Data.OEObject is TOETable then
    begin
      OETable := TOETable(Data.OEObject);
      WriteStringToStream(OETable.ClassName);
      Str := FSerializer.ObjectToJson(OETable);
      //REST.Json.TJson.ObjectToJsonString(OETable);
      WriteStringToStream(Str);{Data.OEObject.FullyQualifiedName}
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

procedure TFrameObjectExplorer.VirtualStringTreeStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  {DragObject в VirtualTree не работает!}
end;

procedure TFrameObjectExplorer.VirtualStringTreeFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
const
  method = 'VirtualStringTreeFreeNode';
var
  Data: POENodeData;
begin
  try
    Data := Sender.GetNodeData(Node);
    Finalize(Data^);
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

procedure TFrameObjectExplorer.VirtualStringTreeGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
const
  method = 'VirtualStringTreeGetImageIndex';
var
  Data: POENodeData;
begin
  try
    if Kind = ikState {.StateImages} then
      //ikNormal{.Images} then
    begin
      Data := Sender.GetNodeData(Node);
      ImageIndex := objImages[Data^.ObjectKind];
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

procedure TFrameObjectExplorer.VirtualStringTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  {see VirtualStringTreeGetCellText}
end;

procedure TFrameObjectExplorer.VirtualStringTreeGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
const
  method = 'VirtualStringTreeGetCellText';
var
  Data: POENodeData;
begin
  try
    Data := Sender.GetNodeData(E.Node);
    if Data^.OEObject = nil then {both ttNormal and ttStatic}
      E.CellText := strObjectKind[Data^.ObjectKind]
    else
      E.CellText := Data^.OEObject.DisplayName;
    E.StaticText := 'wtf??';
    E.ExportType := etText;
  except
    on E: Exception do
      raise Exception.CreateFmt('%s: %s', [method, E.Message]);
  end;
end;

end.
