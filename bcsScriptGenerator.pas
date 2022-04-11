unit bcsScriptGenerator;

interface

uses
  System.SysUtils, System.Classes, tsdStrings,
  bcsProjectModel;

type
  TDataGenerator = class(TDataModule)
    tsdStrings_Header: TtsdStringsKeeper;
    tsdStrings_Table: TtsdStringsKeeper;
    tsdStrings_Query: TtsdStringsKeeper;
    tsdStrings_Footer: TtsdStringsKeeper;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FResult: TStrings;
  protected
    function SaveExportBat(ABasket: TPMBasket; const AFileName: String): Boolean;
  public
    class function BuildExport(ABasket: TPMBasket; const AFileName: String): Boolean;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataGenerator }

class function TDataGenerator.BuildExport(ABasket: TPMBasket;
  const AFileName: String): Boolean;
var
  Dm: TDataGenerator;
begin
  Dm := TDataGenerator.Create(nil);
  try
    Result := Dm.SaveExportBat(ABasket, AFileName);
  finally
    Dm.Free;
  end;
end;

procedure TDataGenerator.DataModuleCreate(Sender: TObject);
begin
  FResult := TStringList.Create;
end;

procedure TDataGenerator.DataModuleDestroy(Sender: TObject);
begin
  FResult.Free;
end;

function TDataGenerator.SaveExportBat(ABasket: TPMBasket;
  const AFileName: String): Boolean;
var
  I: Integer;
begin
  FResult.Clear;
  FResult.Add(tsdStrings_Header.SubstitutedText(
    ['$(source_server)'], [ABasket.SourceServer]) );
  for I := 0 to ABasket.ExpressionCount - 1 do
  begin
    FResult.Add(
      tsdStrings_Table.SubstitutedText(['$(full_table_name)', '$(file_name)'],
        [ABasket.Expressions[I].GetFullyQualifiedName,
         ABasket.Expressions[I].FileName])
    );
  end;
  FResult.AddStrings(tsdStrings_Footer.Lines);
  FResult.SaveToFile(AFileName);

  Result := True;
end;

end.
