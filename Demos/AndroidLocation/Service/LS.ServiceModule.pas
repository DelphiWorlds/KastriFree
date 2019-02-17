unit LS.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.Sensors, System.Notification,
  Androidapi.JNI.App, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNIBridge, Androidapi.JNI.Location,
  AndroidApi.JNI.JavaTypes,
  DW.FileWriter, DW.MultiReceiver.Android, DW.Androidapi.JNI.KeyguardManager,
  LS.AndroidTimer, LS.Config;

type
  TServiceModule = class;

  TLocationListener = class(TJavaLocal, JLocationListener)
  private
    FService: TServiceModule;
  public
    constructor Create(const AService: TServiceModule);
    procedure onLocationChanged(P1: JLocation); cdecl;
    procedure onStatusChanged(P1: JString; P2: Integer; P3: JBundle); cdecl;
    procedure onProviderEnabled(P1: JString); cdecl;
    procedure onProviderDisabled(P1: JString); cdecl;
  end;

  /// <summary>
  ///   Acts as a receiver of broadcasts sent by Android
  /// </summary>
  TServiceReceiver = class(TMultiReceiver)
  private
    FService: TServiceModule;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AService: TServiceModule);
  end;

  /// <summary>
  ///   Acts as a receiver of broadcasts sent by the application
  /// </summary>
  TLocalReceiver = class(TMultiReceiver)
  private
    FService: TServiceModule;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AService: TServiceModule);
  end;

  TLocationChangeFrom = (Listener, Timer, Alarm);

  TServiceModule = class(TAndroidService)
    NotificationCenter: TNotificationCenter;
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceDestroy(Sender: TObject);
  private
    FConfig: TLocationConfig;
    FDozeAlarmIntent: JPendingIntent;
    FGPSLocationListener: JLocationListener;
    FIsDozed: Boolean;
    FIsForeground: Boolean;
    FKeyguardManager: JKeyguardManager;
    FLastSend: TDateTime;
    FLocalReceiver: TLocalReceiver;
    FNetworkLocationListener: JLocationListener;
    FLocationManager: JLocationManager;
    FLogWriter: TFileWriter;
    FPowerManager: JPowerManager;
    FSending: Boolean;
    FServiceReceiver: TServiceReceiver;
    FTimer: TAndroidTimer;
    FWakeLock: JPowerManager_WakeLock;
    function AreListenersInstalled: Boolean;
    function CreateDozeAlarm(const AAction: string; const AStartAt: Int64): Boolean;
    procedure CreateListeners;
    procedure CreateTimer;
    procedure DoMessage(const AMsg: string);
    procedure DoStatus;
    procedure DozeModeChange(const ADozed: Boolean);
    procedure EnableWakeLock(const AEnable: Boolean);
    function HasPermissions: Boolean;
    procedure LocalReceiverReceive(intent: JIntent);
    procedure Pause;
    procedure PostRequest(const ARequest: TStream);
    procedure RemoveListeners;
    procedure RestartService;
    procedure Resume;
    procedure SendNewLocation(const NewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
    function Service: JService;
    procedure ScreenLockChange(const ALocked: Boolean);
    procedure ServiceReceiverReceive(intent: JIntent);
    procedure SetIsPaused(const AValue: Boolean);
    procedure StartDozeAlarm;
    procedure StartForeground;
    procedure StopDozeAlarm;
    procedure StopForeground;
    procedure TimerEventHandler(Sender: TObject);
    procedure UpdateFromLastKnownLocation(const AFrom: TLocationChangeFrom);
    procedure SendNotification(const NewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
  protected
    procedure LocationChanged(const ANewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
    procedure WriteLog(const AMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.IOUtils, System.DateUtils, System.NetConsts, System.Net.URLClient, System.Net.HttpClient, REST.Types, REST.Json,
  Androidapi.Helpers, Androidapi.JNI.Support,
  DW.Androidapi.JNI.LocalBroadcastManager, DW.OSLog, DW.OSDevice, DW.Android.Helpers,
  LS.Consts;

const
  cServiceName = 'com.embarcadero.services.LocationService';
  // Defining these constants here just saves having to import it from Java code
  cReceiverName = 'com.delphiworlds.kastri.DWMultiBroadcastReceiver';
  cActionServiceAlarm = cReceiverName + '.ACTION_SERVICE_ALARM';
  cActionServiceRestart = cReceiverName + '.ACTION_SERVICE_RESTART';
  cExtraServiceRestart = cReceiverName + '.EXTRA_SERVICE_RESTART';
  cWakeLockTag = 'com.delphiworlds.locationservice.wakelock';

  cServiceForegroundId = 3987; // Just a random number
  cServiceNotificationCaption = 'Location Service';
  // https://developer.android.com/training/monitoring-device-state/doze-standby.html#assessing_your_app
  cMinDozeAlarmIntervalSecs = 9 * 60; // Once per 9 minutes is the minimum when "dozed", apparently
  // ***** Modify the following 2 lines to suit your requirements *****
  cLocationUpdateURL = 'http://your.locationupdate.url';
  cLocationRequestJSON = '{"deviceid":"%s", "latitude":"%2.6f","longitude":"%2.6f", "tag":"%S", "inactive":"%d"}';

  cHTTPResultOK = 200;
  cTimerIntervalMinimum = 240000; // = 4 minutes
  cSendIntervalMinimum = 30; // seconds
  cLocationMonitoringInterval = 15000;
  cLocationMonitoringDistance = 10;
  cLocationFromCaptions: array[TLocationChangeFrom] of string = ('Listener', 'Timer', 'Alarm');

function GetTimeFromNowInMillis(const ASeconds: Integer): Int64;
var
  LCalendar: JCalendar;
begin
  LCalendar := TJCalendar.JavaClass.getInstance;
  LCalendar.add(TJCalendar.JavaClass.SECOND, ASeconds);
  Result := LCalendar.getTimeInMillis;
end;

function GetLogTime: string;
begin
  Result := FormatDateTime('mm-dd hh:nn:ss.zzz', Now);
end;

{ TServiceReceiver }

constructor TServiceReceiver.Create(const AService: TServiceModule);
begin
  inherited Create;
  FService := AService;
end;

procedure TServiceReceiver.ConfigureActions;
begin
  // Filtering for various system events
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_SCREEN_ON);
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_SCREEN_OFF);
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_USER_PRESENT);
  if TOSVersion.Check(6) then
    IntentFilter.addAction(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED);
end;

procedure TServiceReceiver.Receive(context: JContext; intent: JIntent);
begin
  FService.ServiceReceiverReceive(intent);
end;

{ TLocalReceiver }

constructor TLocalReceiver.Create(const AService: TServiceModule);
begin
  inherited Create(True);
  FService := AService;
end;

procedure TLocalReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceCommandAction));
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  FService.LocalReceiverReceive(intent);
end;

{ TLocationListener }

constructor TLocationListener.Create(const AService: TServiceModule);
begin
  inherited Create;
  FService := AService;
end;

procedure TLocationListener.onLocationChanged(P1: JLocation);
begin
  FService.WriteLog('TLocationListener.onLocationChanged - LocationChanged');
  FService.LocationChanged(TLocationCoord2D.Create(P1.getLatitude, P1.getLongitude), TLocationChangeFrom.Listener);
end;

procedure TLocationListener.onProviderDisabled(P1: JString);
begin
  //
end;

procedure TLocationListener.onProviderEnabled(P1: JString);
begin
  //
end;

procedure TLocationListener.onStatusChanged(P1: JString; P2: Integer; P3: JBundle);
begin
  //
end;

{ TServiceModule }

constructor TServiceModule.Create(AOwner: TComponent);
var
  LService: JObject;
begin
  inherited;
  FConfig := TLocationConfig.GetConfig;
  // Creating a log file that can be read by both the service and the application
  FLogWriter := TFileWriter.Create(TPath.Combine(TPath.GetDocumentsPath, 'Location.log'), True);
  // AutoFlush means that writes are committed immediately
  FLogWriter.AutoFlush := True;
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.KEYGUARD_SERVICE);
  if LService <> nil then
    FKeyguardManager := TJKeyguardManager.Wrap((LService as ILocalObject).GetObjectID);
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.POWER_SERVICE);
  if LService <> nil then
    FPowerManager := TJPowerManager.Wrap((LService as ILocalObject).GetObjectID);
  FServiceReceiver := TServiceReceiver.Create(Self);
  FLocalReceiver := TLocalReceiver.Create(Self);
  CreateTimer;
  if not FConfig.IsPaused then
    Resume;
end;

destructor TServiceModule.Destroy;
begin
  // Do not use an overridden Destroy for cleanup in a service - use the OnDestroy event
  inherited;
end;

function TServiceModule.Service: JService;
begin
  Result := TJService.Wrap(System.JavaContext);
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  EnableWakeLock(False);
  FWakeLock := nil;
  FServiceReceiver.DisposeOf;
  FLocalReceiver.DisposeOf;
  FTimer.DisposeOf;
  RemoveListeners;
  FLogWriter.DisposeOf;
  FConfig.DisposeOf;
  RestartService;
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  LRestart: Boolean;
begin
  // The broadcast receiver will send a start command when the doze alarm goes off, so there is a check here to see if that is why it was "started"
  if FIsDozed then
  begin
    if (Intent <> nil) and JStringToString(Intent.getAction).Equals(cActionServiceAlarm) then
    begin
      WriteLog('TServiceModule.AndroidServiceStartCommand from doze alarm');
      UpdateFromLastKnownLocation(TLocationChangeFrom.Alarm);
      // Starts the next alarm
      StartDozeAlarm;
    end
  end;
  LRestart := (Intent <> nil) and (Intent.getIntExtra(StringToJString(cExtraServiceRestart), 0) = 1);
  // If the screen is locked when the service starts, it should start in "foreground" mode to ensure it can still access the network
  if (LRestart and TAndroidHelperEx.CheckBuildAndTarget(26)) or ((FKeyguardManager <> nil) and FKeyguardManager.inKeyguardRestrictedInputMode) then
    StartForeground;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.RestartService;
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cActionServiceRestart));
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cReceiverName));
  LIntent.putExtra(StringToJString('ServiceName'), StringToJString(cServiceName));
  TAndroidHelper.Context.sendBroadcast(LIntent);
end;

procedure TServiceModule.StartForeground;
var
  LBuilder: JNotificationCompat_Builder;
begin
  if FIsForeground or not TAndroidHelperEx.CheckBuildAndTarget(26) then
    Exit; // <======
  TOSLog.d('TServiceModule.StartForeground');
  EnableWakeLock(True);
  LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context);
  LBuilder.setAutoCancel(True);
  LBuilder.setContentTitle(StrToJCharSequence(cServiceNotificationCaption));
  LBuilder.setContentText(StrToJCharSequence('Monitoring location changes'));
  LBuilder.setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon);
  LBuilder.setTicker(StrToJCharSequence(cServiceNotificationCaption));
  Service.startForeground(cServiceForegroundId, LBuilder.build);
  FIsForeground := True;
end;

procedure TServiceModule.StopForeground;
begin
  TOSLog.d('TServiceModule.StopForeground');
  if FIsForeground then
  begin
    EnableWakeLock(False);
    Service.stopForeground(True);
    FIsForeground := False;
  end;
end;

procedure TServiceModule.EnableWakeLock(const AEnable: Boolean);
begin
  if AEnable then
  begin
    if FWakeLock = nil then
      FWakeLock := FPowerManager.newWakeLock(TJPowerManager.JavaClass.PARTIAL_WAKE_LOCK, StringToJString(cWakeLockTag));
    if not FWakeLock.isHeld then
      FWakeLock.acquire;
  end
  else if (FWakeLock <> nil) and FWakeLock.isHeld then
    FWakeLock.release;
end;

function TServiceModule.CreateDozeAlarm(const AAction: string; const AStartAt: Int64): Boolean;
var
  LIntent: JIntent;
begin
  Result := False;
  // Make doubly sure that the old alarm is removed
  StopDozeAlarm;
  LIntent := TJIntent.JavaClass.init(StringToJString(AAction));
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cReceiverName));
  // The brodcast receiver that monitors for alarms needs to know whether it was requested by a service
  LIntent.putExtra(StringToJString('ServiceName'), StringToJString(cServiceName));
  FDozeAlarmIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
  if FDozeAlarmIntent <> nil then
  begin
    TAndroidHelper.AlarmManager.setAndAllowWhileIdle(TJAlarmManager.JavaClass.RTC_WAKEUP, AStartAt, FDozeAlarmIntent);
    Result := True;
  end
  else
    WriteLog('Unable to create a pending intent for action: ' + AAction);
end;

procedure TServiceModule.CreateTimer;
begin
  FTimer := TAndroidTimer.Create;
  FTimer.Interval := cTimerIntervalMinimum;
  FTimer.OnTimer := TimerEventHandler;
  FTimer.Enabled := True;
end;

procedure TServiceModule.CreateListeners;
var
  LObject: JObject;
begin
  TOSLog.d('+TServiceModule.CreateListeners');
  // Must use the app (UI) to request permissions
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
          FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.GPS_PROVIDER, cLocationMonitoringInterval, cLocationMonitoringDistance,
            FGPSLocationListener, TJLooper.JavaClass.getMainLooper);
          WriteLog('GPS Location Listener created and listening');
        end;
        if FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.NETWORK_PROVIDER) then
        begin
          FNetworkLocationListener := TLocationListener.Create(Self);
          FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.NETWORK_PROVIDER, cLocationMonitoringInterval, cLocationMonitoringDistance,
            FNetworkLocationListener, TJLooper.JavaClass.getMainLooper);
          WriteLog('Network Location Listener created and listening');
        end;
      end;
    end
    else if not HasPermissions and AreListenersInstalled then
      RemoveListeners;
  finally
    SetIsPaused(not AreListenersInstalled);
  end;
  TOSLog.d('-TServiceModule.CreateListeners');
end;

procedure TServiceModule.RemoveListeners;
begin
  TOSLog.d('+TServiceModule.RemoveListeners');
  try
    if FLocationManager <> nil then
    begin
      FLocationManager.removeUpdates(FGPSLocationListener);
      FLocationManager.removeUpdates(FNetworkLocationListener);
    end;
    FGPSLocationListener := nil;
    FNetworkLocationListener := nil;
    FLocationManager := nil;
    WriteLog('Listeners removed');
  finally
    SetIsPaused(not AreListenersInstalled);
  end;
  TOSLog.d('-TServiceModule.RemoveListeners');
end;

procedure TServiceModule.DoMessage(const AMsg: string);
var
  LIntent: JIntent;
begin
  // Sends a local broadcast containing a debug message
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceMessageAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamMessage), StringToJString(GetLogTime + ': ' + AMsg));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TServiceModule.DoStatus;
var
  LIntent: JIntent;
begin
  // Sends a local broadcast that informs the app that the status has changed
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceStatusAction));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TServiceModule.WriteLog(const AMsg: string);
begin
  TOSLog.d(AMsg);
  FLogWriter.WriteLine(GetLogTime + ': ' + AMsg);
end;

procedure TServiceModule.StartDozeAlarm;
begin
  // setAndAllowWhileIdle is available only on Android 6 or greater
  if FIsDozed and TOSVersion.Check(6) then
  begin
    if FLastSend = 0 then
      FLastSend := Now;
    // Set an alarm for the difference between the last update and the minimum doze alarm
    CreateDozeAlarm(cActionServiceAlarm, GetTimeFromNowInMillis(cMinDozeAlarmIntervalSecs - SecondsBetween(FLastSend, Now)));
  end;
end;

procedure TServiceModule.StopDozeAlarm;
begin
  if FDozeAlarmIntent <> nil then
    TAndroidHelper.AlarmManager.cancel(FDozeAlarmIntent);
  FDozeAlarmIntent := nil;
end;

procedure TServiceModule.LocalReceiverReceive(intent: JIntent);
var
  LCommand: Integer;
begin
  if intent.getAction.equals(StringToJString(cServiceCommandAction)) then
  begin
    LCommand := intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0);
    TOSLog.d('TServiceModule.LocalReceiverReceive received command: %d', [LCommand]);
    // Commands from the app
    case intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0) of
      cServiceCommandPause:
        Pause;
      cServiceCommandResume:
        Resume;
      cServiceCommandAppBecameActive:
        StopForeground;
      cServiceCommandAppEnteredBackground:
        StartForeground;
    end;
  end;
  DoStatus;
end;

procedure TServiceModule.ServiceReceiverReceive(intent: JIntent);
begin
  // Handles the intent that was sent to the broadcast receiver
  // First, screenlock changes
  if intent.getAction.equals(TJIntent.JavaClass.ACTION_USER_PRESENT) or intent.getAction.equals(TJIntent.JavaClass.ACTION_SCREEN_OFF)
    or intent.getAction.equals(TJIntent.JavaClass.ACTION_SCREEN_ON) then
  begin
    ScreenLockChange(FKeyguardManager.inKeyguardRestrictedInputMode);
  end
  // Otherwise, check for "doze" mode changes
  else if TOSVersion.Check(6) and intent.getAction.equals(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED) then
    DozeModeChange(FPowerManager.isDeviceIdleMode);
end;

procedure TServiceModule.SetIsPaused(const AValue: Boolean);
begin
  FConfig.IsPaused := AValue;
  FConfig.Save;
  DoStatus;
end;

procedure TServiceModule.ScreenLockChange(const ALocked: Boolean);
begin
  WriteLog('TServiceModule.ScreenLockChange: ' + BoolToStr(ALocked, True));
  // If the screen is being locked, put the service into foreground mode so that it can still have network access
  if ALocked then
    StartForeground
  else
    StopForeground;
end;

procedure TServiceModule.DozeModeChange(const ADozed: Boolean);
begin
  WriteLog('TServiceModule.DozeModeChange: ' + BoolToStr(ADozed, True));
  // If the device is going into "doze" mode, set an alarm
  FIsDozed := ADozed;
  if FIsDozed then
    StartDozeAlarm
  else
    StopDozeAlarm;
end;

function TServiceModule.HasPermissions: Boolean;
begin
  Result := TOSDevice.CheckPermissions([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation])
end;

function TServiceModule.AreListenersInstalled: Boolean;
begin
  Result := (FGPSLocationListener <> nil) or (FNetworkLocationListener <> nil);
end;

procedure TServiceModule.TimerEventHandler(Sender: TObject);
begin
  WriteLog('TServiceModule.TimerEventHandler');
  UpdateFromLastKnownLocation(TLocationChangeFrom.Timer);
end;

procedure TServiceModule.UpdateFromLastKnownLocation(const AFrom: TLocationChangeFrom);
var
  LLocation: JLocation;
begin
  if FLocationManager = nil then
    Exit; // <======
  LLocation := FLocationManager.getLastKnownLocation(TJLocationManager.JavaClass.GPS_PROVIDER);
  if LLocation <> nil then
  begin
    WriteLog('TServiceModule.UpdateFromLastKnownLocation from: ' + cLocationFromCaptions[AFrom]);
    LocationChanged(TLocationCoord2D.Create(LLocation.getLatitude, LLocation.getLongitude), AFrom);
  end;
end;

procedure TServiceModule.LocationChanged(const ANewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
begin
  // Only send if a location change has been obtained within the specified interval
  if not FSending and (SecondsBetween(Now, FLastSend) >= cSendIntervalMinimum) then
  begin
    WriteLog('Starting send task (Time difference >= cSendIntervalMinimum)');
    // FSending flag is used to ensure that a request is not sent if it is already sending
    FSending := True;
    try
      SendNewLocation(ANewLocation, AFrom); // <---- Comment out this line and uncomment the next line to test notifications
      // SendNotification(ANewLocation, AFrom); // <---- This line was added to test notifications inside an Android service.
    finally
      FSending := False;
    end;
  end;
end;

procedure TServiceModule.SendNewLocation(const NewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
const
  cLocationUpdateTag = '%s: %s @ %s';
var
  LStream: TStringStream;
  LTag: string;
begin
  // Format the request, and post it
  // ****** Modify the following 2 lines to suit your location update request requirements *******
  LTag := Format(cLocationUpdateTag, ['DW', cLocationFromCaptions[AFrom], FormatDateTime('mm-dd hh:nn:ss.zzz', Now)]);
  LStream := TStringStream.Create(Format(cLocationRequestJSON, ['x', NewLocation.Latitude, NewLocation.Longitude, LTag, Ord(FIsDozed)]));
  try
    TOSLog.d('Posting request: %s', [LStream.DataString]);
    try
      PostRequest(LStream);
      // Accessing a reference outside of the task, however in theory this one should be safe
      FLastSend := Now;
    except
      on E: Exception do
      begin
        WriteLog('Exception in PostRequest: ' + E.Message);
      end;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TServiceModule.PostRequest(const ARequest: TStream);
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
begin
  // Posts the JSON request to the server
  WriteLog('Sending new location..');
  LHTTP := THTTPClient.Create;
  try
    LHTTP.Accept := CONTENTTYPE_APPLICATION_JSON;
    LHTTP.ContentType := CONTENTTYPE_APPLICATION_JSON;
    LResponse := LHTTP.Post(cLocationUpdateURL, ARequest);
    if LResponse.StatusCode = cHTTPResultOK then
    begin
      TOSLog.d('Successful - Response: %s', [LResponse.ContentAsString]);
      DoMessage('Send of new location successful');
    end
    else
    begin
      TOSLog.d('Unsuccessful - Status: %s, Response: %s', [LResponse.StatusText, LResponse.ContentAsString]);
      DoMessage('Send of new location unsuccessful -  response: ' + LResponse.ContentAsString);
    end;
  finally
    LHTTP.Free;
  end;
  WriteLog('Successfully sent new location');
end;

procedure TServiceModule.SendNotification(const NewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
var
  LNotification: TNotification;
begin
  LNotification := NotificationCenter.CreateNotification;
  try
    LNotification.EnableSound := False;
    LNotification.AlertBody := Format('Received location update from %s @ %.4f, %.4f', [cLocationFromCaptions[AFrom],
      NewLocation.Latitude, NewLocation.Longitude]);
    NotificationCenter.PresentNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

procedure TServiceModule.Pause;
begin
  RemoveListeners;
end;

procedure TServiceModule.Resume;
begin
  CreateListeners;
end;

end.
