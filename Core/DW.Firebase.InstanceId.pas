unit DW.Firebase.InstanceId;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

interface

type
  TFirebaseTokenRefreshEvent = procedure(Sender: TObject; const AToken: string) of object;

  TFirebaseInstanceId = class;

  TCustomPlatformFirebaseInstanceId = class(TObject)
  private
    FFirebaseInstanceId: TFirebaseInstanceId;
  protected
    procedure DoTokenRefresh(const AToken: string);
    function GetToken: string; virtual; abstract;
  public
    constructor Create(const AFirebaseInstanceId: TFirebaseInstanceId); virtual;
  end;

  TFirebaseInstanceId = class(TObject)
  private
    FPlatformFirebaseInstanceId: TCustomPlatformFirebaseInstanceId;
    FOnTokenRefresh: TFirebaseTokenRefreshEvent;
    function GetToken: string;
  protected
    procedure DoTokenRefresh(const AToken: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Token: string read GetToken;
    property OnTokenRefresh: TFirebaseTokenRefreshEvent read FOnTokenRefresh write FOnTokenRefresh;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Firebase.InstanceId.iOS;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.Firebase.InstanceId.Android;
  {$ENDIF}

{ TCustomPlatformFirebaseInstanceId }

constructor TCustomPlatformFirebaseInstanceId.Create(const AFirebaseInstanceId: TFirebaseInstanceId);
begin
  inherited Create;
  FFirebaseInstanceId := AFirebaseInstanceId;
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

procedure TFirebaseInstanceId.DoTokenRefresh(const AToken: string);
begin
  if Assigned(FOnTokenRefresh) then
    FOnTokenRefresh(Self, AToken);
end;

function TFirebaseInstanceId.GetToken: string;
begin
  Result := FPlatformFirebaseInstanceId.GetToken;
end;

end.
