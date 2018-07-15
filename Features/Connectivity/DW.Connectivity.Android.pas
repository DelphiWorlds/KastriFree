unit DW.Connectivity.Android;

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
  Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.MultiReceiver.Android, DW.Connectivity;

type
  TPlatformConnectivity = class;

  TConnectivityReceiver = class(TMultiReceiver)
  private
    FPlatformConnectivity: TPlatformConnectivity;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const APlatformConnectivity: TPlatformConnectivity);
  end;

  TPlatformConnectivity = class(TObject)
  private
    FConnectivity: TConnectivity;
    FReceiver: TConnectivityReceiver;
  private
    class function ConnectivityManager: JConnectivityManager; static;
  protected
    procedure ConnectivityChange;
  public
    class function IsConnectedToInternet: Boolean; static;
    class function IsWifiInternetConnection: Boolean; static;
  public
    constructor Create(const AConnectivity: TConnectivity);
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers;

type
  TOpenConnectivity = class(TConnectivity);

{ TConnectivityReceiver }

constructor TConnectivityReceiver.Create(const APlatformConnectivity: TPlatformConnectivity);
begin
  inherited Create;
  FPlatformConnectivity := APlatformConnectivity;
end;

procedure TConnectivityReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJConnectivityManager.JavaClass.CONNECTIVITY_ACTION);
end;

procedure TConnectivityReceiver.Receive(context: JContext; intent: JIntent);
begin
  FPlatformConnectivity.ConnectivityChange;
end;

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FConnectivity := AConnectivity;
  FReceiver := TConnectivityReceiver.Create(Self);
end;

destructor TPlatformConnectivity.Destroy;
begin
  FReceiver.Free;
  inherited;
end;

class function TPlatformConnectivity.ConnectivityManager: JConnectivityManager;
var
  LService: JObject;
begin
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE);
  Result := TJConnectivityManager.Wrap(LService);
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
var
  LInfo: JNetworkInfo;
begin
  LInfo := ConnectivityManager.getActiveNetworkInfo;
  Result := (LInfo <> nil) and LInfo.isConnectedOrConnecting;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
var
  LInfo: JNetworkInfo;
begin
  LInfo := ConnectivityManager.getActiveNetworkInfo;
  Result := (LInfo <> nil) and (LInfo.getType = TJConnectivityManager.JavaClass.TYPE_WIFI) and LInfo.isConnectedOrConnecting;
end;

procedure TPlatformConnectivity.ConnectivityChange;
begin
  TOpenConnectivity(FConnectivity).DoConnectivityChange(IsConnectedToInternet);
end;

end.
