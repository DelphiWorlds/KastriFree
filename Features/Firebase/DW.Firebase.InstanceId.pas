unit DW.Firebase.InstanceId;

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
  System.SysUtils;

type
  TFirebaseExceptionEvent = procedure(Sender: TObject; const AException: Exception) of object;
  TFirebaseTokenRefreshEvent = procedure(Sender: TObject; const AToken: string) of object;

  TFirebaseInstanceId = class;

  TCustomPlatformFirebaseInstanceId = class(TObject)
  private
    FFirebaseInstanceId: TFirebaseInstanceId;
  protected
    function Start: Boolean; virtual; abstract;
    procedure DoException(const AException: Exception);
    procedure DoTokenRefresh(const AToken: string);
    function GetToken: string; virtual; abstract;
  public
    constructor Create(const AFirebaseInstanceId: TFirebaseInstanceId); virtual;
  end;

  TFirebaseInstanceId = class(TObject)
  private
    FIsActive: Boolean;
    FPlatformFirebaseInstanceId: TCustomPlatformFirebaseInstanceId;
    FOnException: TFirebaseExceptionEvent;
    FOnTokenRefresh: TFirebaseTokenRefreshEvent;
    function GetToken: string;
  protected
    procedure DoException(const AException: Exception);
    procedure DoTokenRefresh(const AToken: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Start: Boolean;
    property IsActive: Boolean read FIsActive;
    property Token: string read GetToken;
    property OnException: TFirebaseExceptionEvent read FOnException write FOnException;
    property OnTokenRefresh: TFirebaseTokenRefreshEvent read FOnTokenRefresh write FOnTokenRefresh;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Firebase.InstanceId.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Firebase.InstanceId.Android;
  {$ELSE}
  DW.Firebase.Default;
  {$ENDIF}

{ TCustomPlatformFirebaseInstanceId }

constructor TCustomPlatformFirebaseInstanceId.Create(const AFirebaseInstanceId: TFirebaseInstanceId);
begin
  inherited Create;
  FFirebaseInstanceId := AFirebaseInstanceId;
end;

procedure TCustomPlatformFirebaseInstanceId.DoException(const AException: Exception);
begin
  //
end;

procedure TCustomPlatformFirebaseInstanceId.DoTokenRefresh(const AToken: string);
begin
  FFirebaseInstanceId.DoTokenRefresh(AToken);
end;

{ TFirebaseInstanceId }

constructor TFirebaseInstanceId.Create;
begin
  inherited;
  FPlatformFirebaseInstanceId := TPlatformFirebaseInstanceId.Create(Self);
end;

destructor TFirebaseInstanceId.Destroy;
begin
  FPlatformFirebaseInstanceId.Free;
  inherited;
end;

procedure TFirebaseInstanceId.DoException(const AException: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Self, AException);
end;

procedure TFirebaseInstanceId.DoTokenRefresh(const AToken: string);
begin
  if Assigned(FOnTokenRefresh) then
    FOnTokenRefresh(Self, AToken);
end;

function TFirebaseInstanceId.GetToken: string;
begin
  Result := FPlatformFirebaseInstanceId.GetToken;
end;

function TFirebaseInstanceId.Start: Boolean;
begin
  Result := FIsActive;
  if not Result then
    Result := FPlatformFirebaseInstanceId.Start;
end;

end.
