unit DW.Location.Android;

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
  System.Sensors,
  // Android
  Androidapi.JNI.App, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNIBridge, Androidapi.JNI.Location,
  AndroidApi.JNI.JavaTypes;

type
  TLocation = class;

  TLocationListener = class(TJavaLocal, JLocationListener)
  private
    FLocation: TLocation;
  public
    constructor Create(const ALocation: TLocation);
    procedure onLocationChanged(location: JLocation); cdecl;
    procedure onProviderDisabled(provider: JString); cdecl;
    procedure onProviderEnabled(provider: JString); cdecl;
    procedure onStatusChanged(provider: JString; status: Integer; extras: JBundle); cdecl;
  end;

  TLocationChangeEvent = procedure(Sender: TObject; const Location: TLocationCoord2D) of object;

  TLocation = class(TObject)
  private
    FGPSLocationListener: JLocationListener;
    FIsPaused: Boolean;
    FLocationManager: JLocationManager;
    FMonitoringDistance: Integer;
    FMonitoringInterval: Integer;
    FNetworkLocationListener: JLocationListener;
    FOnLocationChange: TLocationChangeEvent;
    function AreListenersInstalled: Boolean;
    procedure CreateListeners;
    function GetLocationMode: Integer;
    function HasPermissions: Boolean;
    procedure RemoveListeners;
    procedure SetIsPaused(const AValue: Boolean);
    procedure UpdateFromLastKnownLocation;
    procedure SetMonitoringDistance(const Value: Integer);
    procedure SetMonitoringInterval(const Value: Integer);
  protected
    procedure LocationChange(const ALocation: TLocationCoord2D);
  public
    constructor Create;
    destructor Destroy; override;
    function GetLastKnownLocation: TLocationCoord2D;
    procedure Pause;
    procedure Resume;
    property IsPaused: Boolean read FIsPaused;
    property MonitoringDistance: Integer read FMonitoringDistance write SetMonitoringDistance;
    property MonitoringInterval: Integer read FMonitoringInterval write SetMonitoringInterval;
    property OnLocationChange: TLocationChangeEvent read FOnLocationChange write FOnLocationChange;
  end;

implementation

uses
  // RTL
  System.Permissions,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Provider,
  // DW
  DW.Consts.Android;

const
  cDefaultLocationMonitoringInterval = 15000;
  cDefaultLocationMonitoringDistance = 10;

{ TLocationListener }

constructor TLocationListener.Create(const ALocation: TLocation);
begin
  inherited Create;
  FLocation := ALocation;
end;

procedure TLocationListener.onLocationChanged(location: JLocation);
begin
  FLocation.LocationChange(TLocationCoord2D.Create(location.getLatitude, location.getLongitude));
end;

procedure TLocationListener.onProviderDisabled(provider: JString);
begin
  //
end;

procedure TLocationListener.onProviderEnabled(provider: JString);
begin
  //
end;

procedure TLocationListener.onStatusChanged(provider: JString; status: Integer; extras: JBundle);
begin
  //
end;

{ TLocation }

constructor TLocation.Create;
begin
  inherited;
  FIsPaused := True;
  FMonitoringDistance := cDefaultLocationMonitoringDistance;
  FMonitoringInterval := cDefaultLocationMonitoringInterval;
end;

destructor TLocation.Destroy;
begin
  //
  inherited;
end;

function TLocation.GetLastKnownLocation: TLocationCoord2D;
var
  LLocation: JLocation;
begin
  Result := TLocationCoord2D.Create(91, 181);
  LLocation := FLocationManager.getLastKnownLocation(TJLocationManager.JavaClass.GPS_PROVIDER);
  if LLocation <> nil then
    Result := TLocationCoord2D.Create(LLocation.getLatitude, LLocation.getLongitude);
end;

function TLocation.GetLocationMode: Integer;
begin
  Result := TJSettings_Secure.JavaClass.getInt(TAndroidHelper.ContentResolver, TJSettings_Secure.JavaClass.LOCATION_MODE);
end;

procedure TLocation.CreateListeners;
var
  LObject: JObject;
begin
  try
    if HasPermissions and not AreListenersInstalled then
    begin
      LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE);
      if LObject <> nil then
      begin
        FLocationManager := TJLocationManager.Wrap((LObject as ILocalObject).GetObjectID);
        if FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.GPS_PROVIDER) then
        begin
          FGPSLocationListener := TLocationListener.Create(Self);
          FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.GPS_PROVIDER, FMonitoringInterval, FMonitoringDistance,
            FGPSLocationListener, TJLooper.JavaClass.getMainLooper);
        end;
        if FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.NETWORK_PROVIDER) then
        begin
          FNetworkLocationListener := TLocationListener.Create(Self);
          FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.NETWORK_PROVIDER, FMonitoringInterval, FMonitoringDistance,
            FNetworkLocationListener, TJLooper.JavaClass.getMainLooper);
        end;
      end;
    end
    else if not HasPermissions and AreListenersInstalled then
      RemoveListeners;
  finally
    SetIsPaused(not AreListenersInstalled);
  end;
end;

procedure TLocation.RemoveListeners;
begin
  try
    if FLocationManager <> nil then
    begin
      FLocationManager.removeUpdates(FGPSLocationListener);
      FLocationManager.removeUpdates(FNetworkLocationListener);
    end;
    FGPSLocationListener := nil;
    FNetworkLocationListener := nil;
    FLocationManager := nil;
  finally
    SetIsPaused(not AreListenersInstalled);
  end;
end;

procedure TLocation.SetIsPaused(const AValue: Boolean);
begin
  FIsPaused := AValue;
end;

procedure TLocation.SetMonitoringDistance(const Value: Integer);
var
  LWasListening: Boolean;
begin
  if Value <> FMonitoringDistance then
  begin
    LWasListening := AreListenersInstalled;
    RemoveListeners;
    FMonitoringDistance := Value;
    if LWasListening then
      CreateListeners;
  end;
end;

procedure TLocation.SetMonitoringInterval(const Value: Integer);
var
  LWasListening: Boolean;
begin
  if Value <> FMonitoringInterval then
  begin
    LWasListening := AreListenersInstalled;
    RemoveListeners;
    FMonitoringInterval := Value;
    if LWasListening then
      CreateListeners;
  end;
end;

function TLocation.HasPermissions: Boolean;
begin
  Result := TPermissionsService.DefaultService.IsEveryPermissionGranted([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation]);
end;

function TLocation.AreListenersInstalled: Boolean;
begin
  Result := (FGPSLocationListener <> nil) or (FNetworkLocationListener <> nil);
end;

procedure TLocation.UpdateFromLastKnownLocation;
var
  LLocation: JLocation;
begin
  if FLocationManager = nil then
    Exit; // <======
  LLocation := FLocationManager.getLastKnownLocation(TJLocationManager.JavaClass.GPS_PROVIDER);
  if LLocation <> nil then
    LocationChange(TLocationCoord2D.Create(LLocation.getLatitude, LLocation.getLongitude));
end;

procedure TLocation.LocationChange(const ALocation: TLocationCoord2D);
begin
  if Assigned(FOnLocationChange) then
    FOnLocationChange(Self, ALocation);
end;

procedure TLocation.Pause;
begin
  RemoveListeners;
end;

procedure TLocation.Resume;
begin
  CreateListeners;
end;

end.
