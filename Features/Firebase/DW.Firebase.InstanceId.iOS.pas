unit DW.Firebase.InstanceId.iOS;

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
  // RTL
  System.TypInfo,
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.Firebase.InstanceId;

type
  TPlatformFirebaseInstanceId = class;

  ITokenRefreshNotification = interface(NSObject)
    ['{93CAC2BE-AF2C-4934-AC49-D1D0381EE355}']
    procedure onTokenRefresh(notification: Pointer); cdecl;
  end;

  TTokenRefreshNotificationListener = class(TOCLocal)
  private
    FFirebaseInstanceId: TPlatformFirebaseInstanceId;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AFirebaseInstanceId: TPlatformFirebaseInstanceId);
    procedure onTokenRefresh(notification: Pointer); cdecl;
  end;

  TPlatformFirebaseInstanceId = class(TCustomPlatformFirebaseInstanceId)
  private
    FTokenRefreshListener: TTokenRefreshNotificationListener;
  protected
    function Start: Boolean; override;
    function GetToken: string; override;
    procedure HandleTokenRefresh;
  public
    constructor Create(const AFirebaseInstanceId: TFirebaseInstanceId); override;
  end;

implementation

uses
  // RTL
  System.Classes, System.SysUtils,
  // Mac
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Helpers,
  // DW
  DW.iOSapi.Firebase;

{ TTokenRefreshNotificationListener }

constructor TTokenRefreshNotificationListener.Create(const AFirebaseInstanceId: TPlatformFirebaseInstanceId);
begin
  inherited Create;
  FFirebaseInstanceId := AFirebaseInstanceId;
end;

function TTokenRefreshNotificationListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(ITokenRefreshNotification);
end;

procedure TTokenRefreshNotificationListener.onTokenRefresh(notification: Pointer);
begin
  FFirebaseInstanceId.HandleTokenRefresh;
end;

{ TPlatformFirebaseInstanceId }

constructor TPlatformFirebaseInstanceId.Create(const AFirebaseInstanceId: TFirebaseInstanceId);
var
  LName: Pointer;
begin
  inherited;
  FTokenRefreshListener := TTokenRefreshNotificationListener.Create(Self);
  LName := NSObjectToID(kFIRInstanceIdTokenRefreshNotification);
  TiOSHelper.DefaultNotificationCenter.addObserver(FTokenRefreshListener.GetObjectID, sel_getUid('onTokenRefresh:'), LName, nil);
end;

function TPlatformFirebaseInstanceId.Start: Boolean;
begin
  Result := False;
  try
    TFIRApp.OCClass.configure;
    Result := True;
  except
    on E: Exception do
      DoException(E)
  end;
end;

function TPlatformFirebaseInstanceId.GetToken: string;
begin
  Result := NSStrToStr(TFIRInstanceId.Wrap(TFIRInstanceId.OCClass.instanceID).token);
end;

procedure TPlatformFirebaseInstanceId.HandleTokenRefresh;
begin
  TThread.Queue(nil,
    procedure
    begin
      DoTokenRefresh(GetToken);
    end
  );
end;

end.
