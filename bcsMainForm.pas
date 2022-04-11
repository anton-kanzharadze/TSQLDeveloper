unit bcsMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections,
  tsdSQLExecutor,
  bcsSourceData, bcsOEFrame, Vcl.ExtCtrls, bcsProjectFrame, System.Actions,
  Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls, tsdFileCircuit, Vcl.StdCtrls;

type
  TPoppedPair = record
    FileCircuit: IFileCircuit;
    Page: TTabSheet;
    Valid: Boolean;
  end;

type
  TFormMain = class(TForm)
    Splitter1: TSplitter;
    MainMenu: TMainMenu;
    MenuItem_File: TMenuItem;
    ActionList: TActionList;
    Action_QueryNew: TAction;
    NewQuery1: TMenuItem;
    PageControl: TPageControl;
    MenuItem_NewBulk: TMenuItem;
    Action_ProjectNew: TAction;
    Action_ProjectOpen: TAction;
    OpenProject1: TMenuItem;
    Action_Close: TAction;
    Close1: TMenuItem;
    Action_Save: TAction;
    Action_SaveAs: TAction;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    MenuItem_Reopen: TMenuItem;
    PopupMenu_PageCaption: TPopupMenu;
    MenuItem_ClosePage: TMenuItem;
    MenuItem_ShowInExplorer: TMenuItem;
    Panel_OE: TPanel;
    FrameObjectExplorer1: TFrameObjectExplorer;
    Project1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Action_ProjectNewExecute(Sender: TObject);
    procedure Action_ProjectOpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure Action_CloseExecute(Sender: TObject);
    procedure Action_SaveExecute(Sender: TObject);
    procedure Action_SaveAsExecute(Sender: TObject);
    procedure MenuItem_FileClick(Sender: TObject);
    procedure PageControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure MenuItem_ClosePageClick(Sender: TObject);
    procedure PopupMenu_PageCaptionPopup(Sender: TObject);
    procedure MenuItem_ShowInExplorerClick(Sender: TObject);
  private
    FProjectMRUList: TMRUList;
    FSourceData: TDataSourceData;
    FSQLExecutor: ISQLExecutor;
    FPoppedPair: TPoppedPair;
    procedure HandleOpenProject(Sender: TObject);
    procedure TryClosePage(APage: TTabSheet);
    class function GetFileCircuitIntf(ATabSheet: TTabSheet;
      out AFileCircuit: IFileCircuit): Boolean;
    function NewProjectPage: TTabSheet;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses bcsDataAdo, tsdUtils;

procedure TFormMain.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action = Action_Close then
    TAction(Action).Enabled := PageControl.ActivePage <> nil
  ;
  Handled := True;
end;

procedure TFormMain.Action_CloseExecute(Sender: TObject);
begin
  TryClosePage(PageControl.ActivePage);
end;

procedure TFormMain.Action_ProjectNewExecute(Sender: TObject);
var
  Page: TTabSheet;
  PrjFrm: TFrameProject;
begin
  Page := NewProjectPage;
  PrjFrm := bcsProjectFrame.TFrameProject.Create(Page);
  PrjFrm.FileCircuit.MRUList := FProjectMRUList;
  PrjFrm.Parent := Page;
  PrjFrm.Align := alClient;

  Page.PageControl := PageControl;

  Page.Caption := PrjFrm.PageCaption;
  PageControl.ActivePage := Page;
end;

procedure TFormMain.Action_ProjectOpenExecute(Sender: TObject);
var
  Page: TTabSheet;
  PrjFrm: TFrameProject;
  NoOpenReason: TNoOpenReason;
begin
  Page := NewProjectPage;
  PrjFrm := bcsProjectFrame.TFrameProject.Create(Page);
  PrjFrm.FileCircuit.MRUList := FProjectMRUList;
  PrjFrm.Parent := Page;
  PrjFrm.Align := alClient;

  if (PrjFrm as IFileCircuit).Open then
  begin
    Page.PageControl := PageControl;
    Page.Caption := PrjFrm.PageCaption;
    PageControl.ActivePage := Page;
  end
  else
  begin
    NoOpenReason := (PrjFrm as IFileCircuit).GetNoOpenReason;
    Page.Free;
    if NoOpenReason = nrAlreadyOpen then
      ShowMessage('File already open');
  end;
end;

procedure TFormMain.Action_SaveAsExecute(Sender: TObject);
var
  FC: IFileCircuit;
begin
  if GetFileCircuitIntf(PageControl.ActivePage, {out}FC) then
    FC.SaveAs;
end;

procedure TFormMain.Action_SaveExecute(Sender: TObject);
var
  FC: IFileCircuit;
begin
  if GetFileCircuitIntf(PageControl.ActivePage, {out}FC) then
    FC.Save;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FSQLExecutor := bcsDataAdo.TDataADO.Create(Self);
  FSourceData := TDataSourceData.Create(Self);
  FSourceData.SQLExecutor := FSQLExecutor;
  FrameObjectExplorer1.Data := FSourceData;
  FProjectMRUList := TMRUList.Create;
  FProjectMRUList.Load;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FProjectMRUList.Save;
  FProjectMRUList.Free;
end;

class function TFormMain.GetFileCircuitIntf(ATabSheet: TTabSheet;
  out AFileCircuit: IFileCircuit): Boolean;
var
  I: Integer;
begin
  AFileCircuit := nil;
  for I := 0 to ATabSheet.ControlCount - 1 do
    if ATabSheet.Controls[I] is TFrame then
      if Supports(ATabSheet.Controls[I], IFileCircuit) then
      begin
        AFileCircuit := ATabSheet.Controls[I] as IFileCircuit;
        Break;
      end;
  Result := AFileCircuit <> nil;
end;

procedure TFormMain.HandleOpenProject(Sender: TObject);
var
  Page: TTabSheet;
  PrjFrm: TFrameProject;
  FileName: String;
begin
  FileName := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  if not FileExists(FileName) then
    raise Exception.Create('File not found');

  Page := NewProjectPage;
  Page.PageControl := PageControl;
  PrjFrm := bcsProjectFrame.TFrameProject.Create(Page);
  PrjFrm.FileCircuit.MRUList := FProjectMRUList;
  PrjFrm.FileCircuit.OpenFile(FileName);
  PrjFrm.Parent := Page;
  PrjFrm.Align := alClient;
  Page.Caption := PrjFrm.PageCaption;
  PageControl.ActivePage := Page;
end;

procedure TFormMain.MenuItem_ClosePageClick(Sender: TObject);
begin
  if FPoppedPair.Valid then
    if FPoppedPair.FileCircuit.CanClose then
    begin
      FPoppedPair.Page.Free;
      FPoppedPair.Valid := False;
    end;
end;

procedure TFormMain.PopupMenu_PageCaptionPopup(Sender: TObject);
begin
  MenuItem_ShowInExplorer.Enabled :=
    FPoppedPair.Valid and FPoppedPair.FileCircuit.WasSaved;
end;

procedure TFormMain.MenuItem_ShowInExplorerClick(Sender: TObject);
begin
  tsdUtils.ShowFile(FPoppedPair.FileCircuit.FileName);
end;

procedure TFormMain.MenuItem_FileClick(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
  FN: String;
  Dummy: TtsdFileCircuit;
begin
  MenuItem_Reopen.Clear;
  for I := 0 to 9 do
  begin
    if I > FProjectMRUList.Count - 1 then
      Break;
    FN := FProjectMRUList[I];
    if not TtsdFileCircuit.FindByFileName(FN, Dummy) then
    begin
      MenuItem := TMenuItem.Create(MenuItem_Reopen);
      MenuItem.Caption := FProjectMRUList[I];
      MenuItem_Reopen.Add(MenuItem);
      MenuItem.OnClick := HandleOpenProject;
    end;
  end;
end;


function TFormMain.NewProjectPage: TTabSheet;
begin
  Result := TTabSheet.Create(PageControl);
end;

procedure TFormMain.PageControlContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  PC: TPageControl;
  Pt: TPoint;
  Idx: Integer;
  HitTests: THitTests;
begin
  PC := Sender as TPageControl;
  HitTests := PC.GetHitTestInfoAt(MousePos.X, MousePos.Y);
  if [htOnItem, htOnLabel] * HitTests <> [] then
  begin {htOnItem, htOnLabel}
    Pt := TPoint.Create(MousePos.X, MousePos.Y);
    Idx := PC.IndexOfTabAt(MousePos.X, MousePos.Y);
    FPoppedPair.Page := PC.Pages[Idx];
    MenuItem_ClosePage.Caption := 'Close "' + PC.Pages[Idx].Caption + '"';

    FPoppedPair.Valid := GetFileCircuitIntf(FPoppedPair.Page,
      FPoppedPair.FileCircuit);

    Pt := PC.ClientToScreen(Pt);
    PopupMenu_PageCaption.Popup(Pt.X, Pt.Y);
    Handled := True;
  end;
end;


procedure TFormMain.TryClosePage(APage: TTabSheet);
var
  FC: IFileCircuit;
begin
  if GetFileCircuitIntf(APage, {out}FC) then
    if FC.CanClose then
      APage.Free;
end;

end.
