program BCS;

uses
  Vcl.Forms,
  bcsMainForm in 'bcsMainForm.pas' {Form1},
  bcsSourceData in 'bcsSourceData.pas' {DataSourceData: TDataModule},
  bcsOEFrame in 'bcsOEFrame.pas' {FrameObjectExplorer: TFrame},
  bcsProjectFrame in 'bcsProjectFrame.pas' {FrameProject: TFrame},
  bcsProjectModel in 'bcsProjectModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
