unit DW.SystemHelper;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  TPermissionResult = record
    Permission: string;
    Granted: Boolean
  end;

  TPermissionResults = TArray<TPermissionResult>;

  TPermissionResultsHelper = record helper for TPermissionResults
  public
    function Add(const AValue: TPermissionResult): Integer;
    function AreAllGranted: Boolean;
    procedure Clear;
    function Count: Integer;
    function DeniedResults: TPermissionResults;
  end;

  TPermissionsResultEvent = procedure(Sender: TObject; const RequestCode: Integer; const Results: TPermissionResults) of object;

  TSystemHelper = class;

  TCustomPlatformSystemHelper = class(TObject)
  private
    FSystemHelper: TSystemHelper;
  protected
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer); virtual; abstract;
  public
    constructor Create(const ASystemHelper: TSystemHelper); virtual;
    property SystemHelper: TSystemHelper read FSystemHelper;
  end;

  TSystemHelper = class(TObject)
  private
    FPlatformSystemHelper: TCustomPlatformSystemHelper;
    FOnPermissionsResult: TPermissionsResultEvent;
  protected
    procedure DoPermissionsResult(const RequestCode: Integer; const Results: TPermissionResults);
  public
    constructor Create;
    destructor Destroy; override;
    class function CheckPermissions(const APermissions: array of string): Boolean; overload;
    class function CheckPermissions(const APermissions: array of string; var AResults: TPermissionResults): Boolean; overload;
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
    property OnPermissionsResult: TPermissionsResultEvent read FOnPermissionsResult write FOnPermissionsResult;
  end;

implementation

uses
  DW.OSLog,
{$IF Defined(ANDROID)}
  DW.SystemHelper.Android;
{$ELSE}
  DW.SystemHelper.Default;
{$ENDIF}

{ TPermissionResultsHelper }

function TPermissionResultsHelper.Add(const AValue: TPermissionResult): Integer;
begin
  SetLength(Self, Count + 1);
  Self[Count - 1] := AValue;
  Result := Count - 1;
end;

function TPermissionResultsHelper.AreAllGranted: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    if not Self[I].Granted then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TPermissionResultsHelper.Clear;
begin
  SetLength(Self, 0);
end;

function TPermissionResultsHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TPermissionResultsHelper.DeniedResults: TPermissionResults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if not Self[I].Granted then
      Result.Add(Self[I]);
  end;
end;

{ TCustomPlatformSystemHelper }

constructor TCustomPlatformSystemHelper.Create(const ASystemHelper: TSystemHelper);
begin
  inherited Create;
  FSystemHelper := ASystemHelper;
end;

{ TSystemHelper }

constructor TSystemHelper.Create;
begin
  inherited;
  FPlatformSystemHelper := TPlatformSystemHelper.Create(Self);
end;

destructor TSystemHelper.Destroy;
begin
  FPlatformSystemHelper.Free;
  inherited;
end;

class function TSystemHelper.CheckPermissions(const APermissions: array of string; var AResults: TPermissionResults): Boolean;
var
  I: Integer;
begin
  SetLength(AResults, Length(APermissions));
  for I := 0 to AResults.Count - 1 do
  begin
    AResults[I].Permission := APermissions[I];
    AResults[I].Granted := TPlatformSystemHelper.CheckPermission(AResults[I].Permission);
    if AResults[I].Granted then
      TOSLog.d('%s granted', [AResults[I].Permission])
    else
      TOSLog.d('%s denied', [AResults[I].Permission]);
  end;
  Result := AResults.AreAllGranted;
end;

class function TSystemHelper.CheckPermissions(const APermissions: array of string): Boolean;
var
  LResults: TPermissionResults;
begin
  Result := TSystemHelper.CheckPermissions(APermissions, LResults);
end;

procedure TSystemHelper.DoPermissionsResult(const RequestCode: Integer; const Results: TPermissionResults);
begin
  if Assigned(FOnPermissionsResult) then
    FOnPermissionsResult(Self, RequestCode, Results);
end;

procedure TSystemHelper.RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
begin
  FPlatformSystemHelper.RequestPermissions(APermissions, ARequestCode);
end;

end.
