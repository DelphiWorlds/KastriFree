unit LS.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.Sensors,
  Androidapi.JNI.App, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNIBridge, Androidapi.JNI.Location,
  AndroidApi.JNI.JavaTypes,
  DW.FileWriter, DW.MultiReceiver.Android, DW.Androidapi.JNI.KeyguardManager,
  LS.AndroidTimer;

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

  TLocationChangeFrom = (Listener, Timer, Alarm);

  TServiceModule = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    FDozeAlarmIntent: JPendingIntent;
    FGPSLocationListener: JLocationListener;
    FIsDozed: Boolean;
    FIsForeground: Boolean;
    FKeyguardManager: JKeyguardManager;
    FLastSend: TDateTime;
    FNetworkLocationListener: JLocationListener;
    FLocationManager: JLocationManager;
    FLogWriter: TFileWriter;
    FPowerManager: JPowerManager;
    FReceiver: TServiceReceiver;
    FSending: Boolean;
    FTimer: TAndroidTimer;
    function CreateDozeAlarm(const AAction: string; const AStartAt: Int64): Boolean;
    procedure CreateListeners;
    procedure CreateTimer;
    procedure DoMessage(const AMsg: string);
    procedure DozeModeChange(const ADozed: Boolean);
    procedure PostRequest(const ARequest: TStream);
    procedure RemoveListeners;
    procedure SendNewLocation(const NewLocation: TLocationCoord2D; const AFrom: TLocationChangeFrom);
    function Service: JService;
    procedure ScreenLockChange(const ALocked: Boolean);
    procedure ServiceReceiverReceive(intent: JIntent);
    procedure StartDozeAlarm;
    procedure StartForeground;
    procedure StopDozeAlarm;
    procedure StopForeground;
    procedure TimerEventHandler(Sender: TObject);
    procedure UpdateFromLastKnownLocation(const AFrom: TLocationChangeFrom);
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
  System.IOUtils, System.Threading, System.DateUtils, System.NetConsts, System.Net.URLClient, System.Net.HttpClient, REST.Types, REST.Json,
  Androidapi.Helpers, Androidapi.JNI.Support,
  DW.Androidapi.JNI.LocalBroadcastManager, DW.OSLog,
  LS.Consts;

const
  cServiceName = 'com.embarcadero.services.LocationService';
  // Defining these constants here just saves having to import it from Java code
  cReceiverName = 'com.delphiworlds.kastri.DWMultiBroadcastReceiver';
  cActionServiceAlarm = cReceiverName + '.ACTION_SERVICE_ALARM';

  cServiceForegroundId = 3987; // Just a random number
  cServiceNotificationCaption = 'Location Service';
  // https://developer.android.com/training/monitoring-device-state/doze-standby.html#assessing_your_app
  cMinDozeAlarmIntervalSecs = 9 * 60; // Once per 9 minutes is the minimum when "dozed", apparently
  // ***** Modify the following 2 lines to suit your requirements *****
  cLocationUpdateURL = 'http://your.locationupdate.url';
  cLocationRequestJSON = '{"deviceid":"%s", "latitude":"%2.6f","longitude":"%2.6f", "tag":"%S"}';

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
  IntentFilter.addAction(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED);
end;

procedure TServiceReceiver.Receive(context: JContext; intent: JIntent);
begin
  FService.ServiceReceiverReceive(intent);
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
  FReceiver := TServiceReceiver.Create(Self);
  CreateListeners;
  CreateTimer;
end;

destructor TServiceModule.Destroy;
begin
  FReceiver.Free;
  FTimer.Enabled := False;
  FTimer.Free;
  RemoveListeners;
  FLogWriter.Free;
  inherited;
end;

function TServiceModule.Service: JService;
begin
  Result := TJService.Wrap(System.JavaContext);
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  // The broadcast receiver will send a start command when the doze alarm goes off, so there is a check here to see if that is why it was "started"
  if FIsDozed and JStringToString(Intent.getAction).Equals(cActionServiceAlarm) then
  begin
    WriteLog('TServiceModule.AndroidServiceStartCommand from doze alarm');
    UpdateFromLastKnownLocation(TLocationChangeFrom.Alarm);
    // Starts the next alarm
    StartDozeAlarm;
  end
  else
  // If the screen is locked when the service starts, it should start in "foreground" mode to ensure it can still access the network
  if (FKeyguardManager <> nil) and FKeyguardManager.inKeyguardRestrictedInputMode then
    StartForeground;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.StartForeground;
var
  LBuilder: JNotificationCompat_Builder;
begin
  if FIsForeground then
    Exit; // <======
  TOSLog.d('TServiceModule.StartForeground');
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
  Service.stopForeground(True);
  FIsForeground := False;
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
  LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE);
  if LObject <> nil then
  begin
    FLocationManager := TJLocationManager.Wrap((LObject as ILocalObject).GetObjectID);
    if FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.GPS_PROVIDER) then
    begin
      FGPSLocationListener := TLocationListener.Create(Self);
      FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.GPS_PROVIDER, cLocationMonitoringInterval, cLocationMonitoringDistance,
        FGPSLocationListener, TJLooper.JavaClass.getMainLooper);
      TOSLog.d('GPS Location Listener created and listening');
    end;
    if FLocationManager.isProviderEnabled(TJLocationManager.JavaClass.NETWORK_PROVIDER) then
    begin
      FNetworkLocationListener := TLocationListener.Create(Self);
      FLocationManager.requestLocationUpdates(TJLocationManager.JavaClass.NETWORK_PROVIDER, cLocationMonitoringInterval, cLocationMonitoringDistance,
        FNetworkLocationListener, TJLooper.JavaClass.getMainLooper);
      TOSLog.d('Network Location Listener created and listening');
    end;
  end;
end;

procedure TServiceModule.RemoveListeners;
begin
  if FLocationManager <> nil then
  begin
    FLocationManager.removeUpdates(FGPSLocationListener);
    FLocationManager.removeUpdates(FNetworkLocationListener);
  end;
end;

procedure TServiceModule.DoMessage(const AMsg: string);
var
  LIntent: JIntent;
begin
  // Sends a local broadcast that the app can receive. This *should* be thread-safe since it does not access any outside references
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceMessageAction));
  LIntent.putExtra(StringToJString(cServiceMessageParamMessage), StringToJString(GetLogTime + ': ' + AMsg));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TServiceModule.WriteLog(const AMsg: string);
begin
  // In theory, this should probably be queued on the main thread, however it hasn't had any problems as yet
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
  else if intent.getAction.equals(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED) then
    DozeModeChange(FPowerManager.isDeviceIdleMode);
end;

procedure TServiceModule.ScreenLockChange(const ALocked: Boolean);
begin
  TOSLog.d('TServiceModule.ScreenLockChange: ' + BoolToStr(ALocked, True));
  WriteLog('TServiceModule.ScreenLockChange: ' + BoolToStr(ALocked, True));
  // If the screen is being locked, put the service into foreground mode so that it can still have network access
  if ALocked then
    StartForeground
  else
    StopForeground;
end;

procedure TServiceModule.DozeModeChange(const ADozed: Boolean);
begin
  TOSLog.d('TServiceModule.DozeModeChange: ' + BoolToStr(ADozed, True));
  WriteLog('TServiceModule.DozeModeChange: ' + BoolToStr(ADozed, True));
  // If the device is going into "doze" mode, set an alarm
  FIsDozed := ADozed;
  if FIsDozed then
    StartDozeAlarm
  else
    StopDozeAlarm;
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
    TOSLog.d('Time difference >= cSendIntervalMinimum');
    WriteLog('Starting send task');
    // Send the new location in a separate task
    TTask.Run(
      procedure
      begin
        // FSending flag is used to ensure that a request is not sent if it is already sending
        FSending := True;
        try
          SendNewLocation(ANewLocation, AFrom);
        finally
          FSending := False;
        end;
      end
    )
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
  LStream := TStringStream.Create(Format(cLocationRequestJSON, ['x', NewLocation.Latitude, NewLocation.Longitude, LTag]));
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
  TOSLog.d('+TServiceModule.PostRequest');
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
  TOSLog.d('-TServiceModule.PostRequest');
end;

end.
