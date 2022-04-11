unit bcsProjectFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Generics.Collections, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.StdCtrls,
  Vcl.Menus, tsdFileCircuit,
  bcsProjectModel, VirtualTrees, NativeXml, System.ImageList, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls,
  Winapi.ActiveX, tsdCommunicate, Vcl.WinXCtrls,
  clJsonSerializer,
  bcsSourceData;

type
  TFrameProject = class(TFrame, IFileCircuit)
    ActionList: TActionList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Action_AddTableFromOE: TAction;
    Action_AddTableManually: TAction;
    PopupMenu_AddTable: TPopupMenu;
    FileCircuit: TtsdFileCircuit;
    PageControl: TPageControl;
    TabSheet_Design: TTabSheet;
    TabSheet_Code: TTabSheet;
    ImageList: TImageList;
    Add1: TMenuItem;
    FromObjectExplorer1: TMenuItem;
    Manually1: TMenuItem;
    Panel_Status: TPanel;
    Label_Status: TLabel;
    SignalBox: TtsdSignalBox;
    Action_Delete: TAction;
    Delete1: TMenuItem;
    Memo_Code: TMemo;
    Action_Build: TAction;
    Build1: TMenuItem;
    Panel: TPanel;
    VirtualStringTree: TVirtualStringTree;
    RelativePanel1: TRelativePanel;
    Label_SourceServer: TLabel;
    Edit_SourceServer: TEdit;
    Label_TargetServer: TLabel;
    Edit_TargetServer: TEdit;
    procedure FileEditorChangedModified(Sender: TObject; AValue: Boolean);
    procedure FileEditorChangedFileName(Sender: TObject; const AValue: string);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure Action_AddTableManuallyExecute(Sender: TObject);
    procedure VirtualStringTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure FileCircuitEditorSave(Sender: TObject; const AFileName: String);
    procedure FileCircuitEditorLoad(Sender: TObject; const AFileName: string);
    procedure VirtualStringTreeDragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure VirtualStringTreeDragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure VirtualStringTreeLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure VirtualStringTreeNodeCopying(Sender: TBaseVirtualTree; Node,
      Target: PVirtualNode; var Allowed: Boolean);
    procedure VirtualStringTreeNodeMoving(Sender: TBaseVirtualTree; Node,
      Target: PVirtualNode; var Allowed: Boolean);
    procedure SignalBoxSignals0Receive(Sender: TSignal; WParam,
      LParam: Integer);
    procedure Action_DeleteExecute(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure Action_BuildExecute(Sender: TObject);
    procedure Edit_SourceServerExit(Sender: TObject);
  private
    FSerializer: clJsonSerializer.TclJsonSerializer;
    FFileCircuitIntf: IFileCircuit;
    FBasket: TPMBasket;
    FBasketBackup: TPMBasket;
    FBuffer: TStrings;
    FDroppedObjects: TObjectList<bcsSourceData.TOEObject>;
    function GetPageCaption: String;
  protected
   procedure HandleTableListNotify(Sender: TObject; const Item: TPMTableExpression;
     Action: System.Generics.Collections.TCollectionNotification);
   procedure AddTableFromString(const AFullyQualifiedName: String);
   function AddOEObjectToBasket(AOEObject: TOEObject): Boolean;
   procedure BasketToView;
   procedure ViewToBasket;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileCircuitIntf: IFileCircuit read FFileCircuitIntf implements IFileCircuit;
    property PageCaption: String read GetPageCaption;
  end;

implementation

{$R *.dfm}

uses UITypes, REST.Json,
  bcsProjectXML, bcsScriptGenerator;

const
  colNumber = 0;
  colType = 1;
  colExpression = 2;
  colFileName = 3;

{ Utils }

{see VirtualTree Demo "OLE"}
procedure GetUnicodeText(DataObject: IDataObject; ATarget: TStrings);
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  OLEData,
  Head, Tail: PWideChar;
  Line: String;
begin
  Assert(ATarget <> nil);
  ATarget.Clear;
  // fill the structure used to get the Unicode string
  with FormatEtc do
  begin
    cfFormat := CF_UNICODETEXT;
    // no specific target device
    ptd := nil;
    // normal content to render
    dwAspect := DVASPECT_CONTENT;
    // no specific page of multipage data
    lindex := -1;
    // pass the data via memory
    tymed := TYMED_HGLOBAL;
  end;

  // Check if we can get the Unicode text data.
  if DataObject.QueryGetData(FormatEtc) = S_OK then
  begin
    // Data is accessible so finally get a pointer to it
    if DataObject.GetData(FormatEtc, Medium) = S_OK then
    begin
      OLEData := GlobalLock(Medium.hGlobal);
      if Assigned(OLEData) then
      begin
        Head := OLEData;
        try
          while Head^ <> #0 do
          begin
            Tail := Head;
            while not CharInSet(Tail^, [WideChar(#0)
              {, WideChar(#13), WideChar(#10), WideChar(#9)}]) do
              Inc(Tail);
            if Head <> Tail then
            begin
              SetString(Line, Head, Tail - Head);
              ATarget.Add(Line);
            end;
            // Skip any tab.
            if Tail^ = #9 then
              Inc(Tail);
            // skip line separators
            if Tail^ = #13 then
              Inc(Tail);
            if Tail^ = #10 then
              Inc(Tail);
            Head := Tail;
          end;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      end;
      // never forget to free the storage medium
      ReleaseStgMedium(Medium);
    end;
  end;
end;

{ TFrameProject }

procedure TFrameProject.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  HExists: Boolean;
begin
  if Action = Action_AddTableFromOE then
  begin
    HExists := tsdCommunicate.HandlerExists('#get_selected_table');
    TAction(Action).Enabled := HExists and
      (tsdCommunicate.CallFunc('#get_selected_table', []) <> '');
  end
  else if Action = Action_AddTableManually then
    TAction(Action).Enabled := True
  ;
  Handled := True;
end;

procedure TFrameProject.Action_AddTableManuallyExecute(Sender: TObject);
var
  TN: String;
begin
  if InputQuery('Add Table Name', 'Enter Fully Qulified Table Name', TN) then
    AddTableFromString(TN);
end;

procedure TFrameProject.Action_BuildExecute(Sender: TObject);
begin
  if FileCircuit.Save then
    if TDataGenerator.BuildExport(FBasket,
      ChangeFileExt(FileCircuit.FileName, '_export.bat')) then
      ShowMessage('Done');
end;

procedure TFrameProject.Action_DeleteExecute(Sender: TObject);
var
  PMExpr: TPMTableExpression;
begin
  if VirtualStringTree.FocusedNode = nil then
    Exit;
  PMExpr := VirtualStringTree.GetNodeData<TPMTableExpression>
    (VirtualStringTree.FocusedNode);
  FBasket.RemoveExpression(PMExpr);
end;

function TFrameProject.AddOEObjectToBasket(AOEObject: TOEObject): Boolean;
const
  method = 'AddOEObjectToBasket';
var
  OETbl: TOETable;
  PMTable: TPMTableExpression;
  Msg: String;
  Answ: TModalResult;
begin
  try
    if not (AOEObject is TOETable) then
      Exit(False);

    OETbl := TOETable(AOEObject);

    if not FBasket.SourceServer.IsEmpty
      and (FBasket.SourceServer <> OETbl.ServerName)
      and not (ServerNameIsLocal(FBasket.SourceServer) and
               ServerNameIsLocal(OETbl.ServerName)) then
      begin
        Msg := 'Server name "' + OETbl.ServerName + '" will be ignored.';
        Answ := MessageDlg(Msg, mtWarning, [mbOK, mbCancel], 0);
        if Answ = mrCancel then
          Exit(False);
      end;

    PMTable := TPMTableExpression.Create;
    PMTable.ExpressionType := etTable;

    PMTable.Database := OETbl.DatabaseName;
    PMTable.Schema := OETbl.Schema;
    PMTable.TableName := OETbl.Name;

    PMTable.FileName := PMTable.TableName + '.dat';

    FBasket.AddExpression(PMTable);
    Result := True;
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.AddTableFromString(const AFullyQualifiedName: String);
const
  method = 'AddTableFromString';
var
  PMTable: TPMTableExpression;
  Tbl: TArray<String>;
begin
  try
    Tbl := AFullyQualifiedName.Split(['.']);
    PMTable := TPMTableExpression.Create;
    PMTable.ExpressionType := etTable;
    PMTable.Database := Tbl[0];
    PMTable.Schema := Tbl[1];
    PMTable.TableName := Tbl[2];
    PMTable.FileName := PMTable.TableName + '.dat';

    FBasket.AddExpression(PMTable)
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.BasketToView;
begin
  Edit_SourceServer.Text := FBasket.SourceServer;
end;

procedure TFrameProject.ViewToBasket;
begin
  FBasket.SourceServer := Edit_SourceServer.Text;
end;

constructor TFrameProject.Create(AOwner: TComponent);
begin
  inherited;
  FSerializer := clJsonSerializer.TclJsonSerializer.Create;
  FDroppedObjects := TObjectList<bcsSourceData.TOEObject>.Create(True);
  FFileCircuitIntf := FileCircuit;
  FBuffer := TStringList.Create;
  FBasket := TPMBasket.Create;
  FBasketBackup := TPMBasket.Create;
  FBasket.OnListNotify := HandleTableListNotify;
  PageControl.ActivePageIndex := 0;
end;

destructor TFrameProject.Destroy;
begin
  FDroppedObjects.Free;
  FBasketBackup.Free;
  FBasket.Free;
  FBuffer.Free;
  FSerializer.Free;
  inherited;
end;

procedure TFrameProject.Edit_SourceServerExit(Sender: TObject);
begin
  ViewToBasket;
end;

procedure TFrameProject.FileCircuitEditorLoad(Sender: TObject;
  const AFileName: string);
begin
  LoadBasket(FBasket, AFileName);
  BasketToView;
end;

procedure TFrameProject.FileCircuitEditorSave(Sender: TObject;
  const AFileName: String);
begin
  ViewToBasket;
  SaveBasket(FBasket, AFileName);
end;

procedure TFrameProject.FileEditorChangedFileName(Sender: TObject; const AValue: string);
begin
  (Parent as TTabSheet).Caption := PageCaption;
//  Label_FileName.Caption := AValue;
end;

procedure TFrameProject.FileEditorChangedModified(Sender: TObject; AValue: Boolean);
begin
  (Parent as TTabSheet).Caption := PageCaption;
  if AValue then
    Label_Status.Caption := 'Modified'
  else
    Label_Status.Caption := 'Saved';
end;

function TFrameProject.GetPageCaption: String;
const
  modStr: array[Boolean] of Char = (' ', '*');
begin
  Result := ChangeFileExt(ExtractFileName(FileCircuit.FileName), '') +
    modStr[FileCircuit.Modified];
end;

procedure TFrameProject.HandleTableListNotify(Sender: TObject;
  const Item: TPMTableExpression; Action: System.Generics.Collections.TCollectionNotification);
const
  method = 'HandleTableListNotify';
var
  TableList: TPMTableList;
  Enum: TVTVirtualNodeEnumerator;
begin
  try
    if Action = System.Generics.Collections.cnAdded then
    begin
      TableList := Sender as TPMTableList;
      VirtualStringTree.RootNodeCount := TableList.Count;
      FileCircuit.Modified := True;
    end
    else {if Action in [cnRemoved, cnExtracted]}
    begin
      Enum := VirtualStringTree.Nodes(True).GetEnumerator;
      {Enumerate nodes by data}
      while Enum.MoveNext do
      begin
        if VirtualStringTree.GetNodeData<TPMTableExpression>(Enum.Current) =  Item then
        begin
          VirtualStringTree.DeleteNode(Enum.Current);
          //Break;
        end;
      end;
    end;
    FileCircuit.Modified := True;
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
var
  Answ: Integer;
begin
  AllowChange := True;
  if PageControl.ActivePage = TabSheet_Design then
  begin
    FBasketBackup.Assign(FBasket);
    BasketToStrings(FBasket, Memo_Code.Lines);
  end
  else if PageControl.ActivePage = TabSheet_Code then
  begin
    if not StringsToBasket(Memo_Code.Lines, FBasket) then
    begin
      Answ := MessageBox(0, 'XML parsing failed.'#13 +
        'Revert to previous version?', 'Error', MB_ICONERROR or MB_OKCANCEL);
      if Answ = IDOK then
        FBasket.Assign(FBasketBackup);
      if Answ = IDCANCEL then
        AllowChange := False;
    end
  end;
end;

procedure TFrameProject.SignalBoxSignals0Receive(Sender: TSignal; WParam,
  LParam: Integer);
var
  OEObj: TOEObject;
begin
  if FDroppedObjects.Count > 0 then
  begin
    OEObj := FDroppedObjects[FDroppedObjects.Count - 1];
    AddOEObjectToBasket(OEObj);
    FDroppedObjects.Remove(OEObj);
  end;
end;

procedure TFrameProject.VirtualStringTreeDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
const
  method = 'VirtualStringTreeDragDrop';
begin
  try
    {Purpose of this call is to fire OnLoadNode event;
     appending the node is side effect, it will be eleminated in OnNodeCopying.}
    Sender.ProcessDrop(DataObject, Sender.DropTargetNode, Effect, amAddChildFirst);
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.VirtualStringTreeDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TFrameProject.VirtualStringTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
const
  method = 'VirtualStringTreeGetText';
var
  PMExpr: TPMTableExpression;
begin
  try
    if Column = colNumber then
      CellText := IntToStr(Node.Index)
    else
    begin
      PMExpr := Sender.GetNodeData<TPMTableExpression>(Node);
      if Column = colType then
        CellText := strExpressionType[PMExpr.ExpressionType]
      else if Column = colExpression then
        CellText := PMExpr.Database + '.' + PMExpr.Schema + '.' + PMExpr.TableName
      else if Column = colFileName then
        CellText := PMExpr.FileName
    end;
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.VirtualStringTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
const
  method = 'VirtualStringTreeInitNode';
var
  PMExpr: TPMTableExpression;
begin
  try
    PMExpr := FBasket.Expressions[Node.Index];
    Sender.SetNodeData<TPMTableExpression>(Node, PMExpr);
    {Memory is not allocated here, so OnFreeNode is not required}
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.VirtualStringTreeLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);

  function ReadStringFromStream: String;
  var
    Len: Integer;
    Bytes: TBytes;
  begin
    Stream.ReadData(Len);
    SetLength(Bytes, Len);
    Stream.ReadData(Bytes, Len);
    Result := TEncoding.UTF8.GetString(Bytes);
  end;

  function ReadString(out AValue: String): Boolean;
  var
    Kind: Word;
    Len: Integer;
    Bytes: TBytes;
  begin
    Stream.ReadData(Kind);
    Result := Kind = varString;
    if Result then
    begin
      Stream.ReadData(Len);
      SetLength(Bytes, Len);
      Stream.ReadData(Bytes, Len);
      AValue := TEncoding.UTF8.GetString(Bytes);
    end;
  end;

const
  method = 'VirtualStringTreeLoadNode';
var
  Str: String;
  OETbl: bcsSourceData.TOETable;
begin
  try
    if ReadString(Str) then
    begin
      if Str = 'TOETable' then
      begin
        if ReadString(Str) then
        begin
          {TJson.JsonToObject ultimately will instantiate objects using
           their default constructor
           (see REST.JsonReflect.TJSONUnMarshal.ObjectInstance).
          //REST.Json.TJson.JsonToObject<bcsSourceData.TOETable>(Str);
          }
          OETbl := FSerializer.JsonToObject(TOETable, Str) as TOETable;
          FDroppedObjects.Add(OETbl);
        end;
      end;
    end;

    SignalBox.Post(1);{call AddOEObjectToBasket asynchronously}
  except
    on E: Exception do
    begin
      E.Message := method + ': ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFrameProject.VirtualStringTreeNodeCopying(Sender: TBaseVirtualTree;
  Node, Target: PVirtualNode; var Allowed: Boolean);
begin
  Allowed := False;
end;

procedure TFrameProject.VirtualStringTreeNodeMoving(Sender: TBaseVirtualTree;
  Node, Target: PVirtualNode; var Allowed: Boolean);
begin
  Allowed := False;
end;

end.

