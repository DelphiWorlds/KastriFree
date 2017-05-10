unit DW.Firebase.InstanceId.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  // DW
  DW.Firebase.InstanceId;

type
  TPlatformFirebaseInstanceId = class;

  TFirebaseInstanceIdReceiverListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
  private
    FFirebaseInstanceId: TPlatformFirebaseInstanceId;
  public
    { JFMXBroadcastReceiverListener }
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  public
    constructor Create(const AFirebaseInstanceId: TPlatformFirebaseInstanceId);
  end;

  TPlatformFirebaseInstanceId = class(TCustomPlatformFirebaseInstanceId)
  private
    FFirebaseInstanceIdBroadcastReceiver: JFMXBroadcastReceiver;
    FFirebaseInstanceIdReceiverListener: TFirebaseInstanceIdReceiverListener;
  protected
    function GetToken: string; override;
    procedure HandleTokenRefresh(const AToken: string);
  public
    constructor Create(const AFirebaseInstanceId: TFirebaseInstanceId); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Classes,
  // Android
  Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.Firebase, DW.FirebaseApp.Android, DW.Androidapi.JNI.FirebaseServiceHelpers, DW.Androidapi.JNI.LocalBroadcastManager;

{ TFirebaseInstanceIdReceiverListener }

constructor TFirebaseInstanceIdReceiverListener.Create(const AFirebaseInstanceId: TPlatformFirebaseInstanceId);
begin
  inherited Create;
  FFirebaseInstanceId := AFirebaseInstanceId;
end;

procedure TFirebaseInstanceIdReceiverListener.onReceive(context: JContext; intent: JIntent);
begin
  if (intent <> nil) and (intent.getAction.compareTo(TJDWFirebaseInstanceIdService.JavaClass.ACTION_TOKEN_REFRESHED) = 0) then
    FFirebaseInstanceId.HandleTokenRefresh(JStringToString(intent.getStringExtra(StringToJString('token'))));
end;

{ TPlatformFirebaseInstanceId }

constructor TPlatformFirebaseInstanceId.Create(const AFirebaseInstanceId: TFirebaseInstanceId);
var
  LIntentFilter: JIntentFilter;
begin
  inherited;
  TPlatformFirebaseApp.Start;
  FFirebaseInstanceIdReceiverListener := TFirebaseInstanceIdReceiverListener.Create(Self);
  FFirebaseInstanceIdBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FFirebaseInstanceIdReceiverListener);
  LIntentFilter := TJIntentFilter.JavaClass.init(TJDWFirebaseInstanceIdService.JavaClass.ACTION_TOKEN_REFRESHED);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FFirebaseInstanceIdBroadcastReceiver, LIntentFilter);
end;

destructor TPlatformFirebaseInstanceId.Destroy;
begin
  FFirebaseInstanceIdReceiverListener.Free;
  FFirebaseInstanceIdBroadcastReceiver := nil;
  inherited;
end;

function TPlatformFirebaseInstanceId.GetToken: string;
begin
  Result := JStringToString(TJFirebaseInstanceId.JavaClass.getInstance().getToken());
end;

procedure TPlatformFirebaseInstanceId.HandleTokenRefresh(const AToken: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      DoTokenRefresh(AToken);
    end
  );
end;

end.
