unit DW.PermissionsRequester {$IF CompilerVersion > 32} deprecated 'use System.Permissions'{$ENDIF};

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  DW.PermissionsTypes;

type
  TPermissionsResultEvent = procedure(Sender: TObject; const RequestCode: Integer; const Results: TPermissionResults) of object;

  TPermissionsRequester = class;

  TCustomPlatformPermissionsRequester = class(TObject)
  private
    FPermissionsRequester: TPermissionsRequester;
  protected
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer); virtual; abstract;
  public
    constructor Create(const APermissionsRequester: TPermissionsRequester); virtual;
    property PermissionsRequester: TPermissionsRequester read FPermissionsRequester;
  end;

  TPermissionsRequester = class(TObject)
  private
    FIsRequesting: Boolean;
    FPlatformPermissionsRequester: TCustomPlatformPermissionsRequester;
    FOnPermissionsResult: TPermissionsResultEvent;
  protected
    procedure DoPermissionsResult(const RequestCode: Integer; const Results: TPermissionResults);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
    property IsRequesting: Boolean read FIsRequesting;
    property OnPermissionsResult: TPermissionsResultEvent read FOnPermissionsResult write FOnPermissionsResult;
  end;

implementation

uses
  DW.OSLog,
  DW.OSDevice,
{$IF Defined(ANDROID)}
  DW.PermissionsRequester.Android;
{$ELSE}
  DW.PermissionsRequester.Default;
{$ENDIF}

{ TCustomPlatformPermissionsRequester }

constructor TCustomPlatformPermissionsRequester.Create(const APermissionsRequester: TPermissionsRequester);
begin
  inherited Create;
  FPermissionsRequester := APermissionsRequester;
end;

{ TPermissionsRequester }

constructor TPermissionsRequester.Create;
begin
  inherited;
  FPlatformPermissionsRequester := TPlatformPermissionsRequester.Create(Self);
end;

destructor TPermissionsRequester.Destroy;
begin
  FPlatformPermissionsRequester.Free;
  inherited;
end;

procedure TPermissionsRequester.DoPermissionsResult(const RequestCode: Integer; const Results: TPermissionResults);
begin
  if Assigned(FOnPermissionsResult) then
    FOnPermissionsResult(Self, RequestCode, Results);
  FIsRequesting := False;
end;

procedure TPermissionsRequester.RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
var
  LResults: TPermissionResults;
begin
  FIsRequesting := True;
  if TOSDevice.CheckPermissions(APermissions, LResults) then
    DoPermissionsResult(ARequestCode, LResults)
  else
    FPlatformPermissionsRequester.RequestPermissions(APermissions, ARequestCode);
end;

end.


