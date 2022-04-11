unit tsdSQLExecutor;

interface

uses DB, Classes;

type
  TSQLExecutor = class
    procedure ExecSQL(const ASQL: String); virtual; abstract;
    function OpenDataset(const ASQL: String): TDataSet; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    procedure SetConnected(AValue: Boolean); virtual; abstract;
    procedure AssignParams(AParams: TStrings); virtual; abstract;
    property Connected: Boolean read GetConnected write SetConnected;
  end;

type
  ISQLExecutor = interface
    ['{4006E017-66BD-4687-A960-10A405865BBB}']
    procedure ExecSQL(const ASQL: String);
    function OpenDataset(const ASQL: String): TDataSet;
    function GetConnected: Boolean;
    procedure SetConnected(AValue: Boolean);
    procedure AssignParams(AParams: TStrings);
    property Connected: Boolean read GetConnected write SetConnected;
  end;

implementation

end.
