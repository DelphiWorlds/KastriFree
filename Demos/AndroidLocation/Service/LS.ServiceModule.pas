unit LS.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.Sensors, System.Notification,
  Androidapi.JNI.App, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNIBridge,
  AndroidApi.JNI.JavaTypes,
  DW.FileWriter, DW.MultiReceiver.Android, DW.Location.Android,
  LS.AndroidTimer, LS.Config;

type
  TServiceModule = class;

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
    FIsDozed: Boolean;
    FIsForeground: Boolean;
    FLastSend: TDateTime;
    FLocalReceiver: TLocalReceiver;
    FLocation: TLocation;
    FNotificationChannel: JNotificationChannel;
    FLogWriter: TFileWriter;
    FSending: Boolean;
    FServiceReceiver: TServiceReceiver;
    FTimer: TAndroidTimer;
    FWakeLock: JPowerManager_WakeLock;
    function CreateDozeAlarm(const AAction: string; const AStartAt: Int64): Boolean;
    procedure CreateNotificationChannel;
    procedure CreateTimer;
    procedure DoMessage(const AMsg: string);
    procedure DoStatus;
    procedure DozeModeChange(const ADozed: Boolean);
    procedure EnableWakeLock(const AEnable: Boolean);
    procedure LocalReceiverReceive(intent: JIntent);
    procedure LocationChangeHandler(Sender: TObject; const ALocation: TLocationCoord2D);
    procedure Pause;
    procedure PostRequest(const ARequest: TStream);
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
    procedure UpdateConfig;
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
  Androidapi.Helpers, Androidapi.JNI.Support, Androidapi.JNI.Provider,
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
  cNotificationChannelName = 'AndroidLocation';

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

{ TServiceModule }

constructor TServiceModule.Create(AOwner: TComponent);
begin
  inherited;
  FLocation := TLocation.Create;
  FLocation.OnLocationChange := LocationChangeHandler;
  FConfig := TLocationConfig.GetConfig;
  // Creating a log file that can be read by both the service and the application
  FLogWriter := TFileWriter.Create(TPath.Combine(TPath.GetDocumentsPath, 'Location.log'), True);
  // AutoFlush means that writes are committed immediately
  FLogWriter.AutoFlush := True;
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
  if (LRestart and TAndroidHelperEx.CheckBuildAndTarget(26)) or TAndroidHelperEx.KeyguardManager.inKeyguardRestrictedInputMode then
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

procedure TServiceModule.CreateNotificationChannel;
begin
  if FNotificationChannel <> nil then
    Exit; // <======
  FNotificationChannel := TJNotificationChannel.JavaClass.init(TAndroidHelper.Context.getPackageName, StrToJCharSequence(cNotificationChannelName),
    TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
  FNotificationChannel.enableLights(True);
  FNotificationChannel.enableVibration(True);
  FNotificationChannel.setLightColor(TJColor.JavaClass.GREEN);
  FNotificationChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
  TAndroidHelperEx.NotificationManager.createNotificationChannel(FNotificationChannel);
end;

procedure TServiceModule.StartForeground;
var
  LBuilder: JNotificationCompat_Builder;
begin
  if FIsForeground or not TAndroidHelperEx.CheckBuildAndTarget(TAndroidHelperEx.OREO) then
    Exit; // <======
  TOSLog.d('TServiceModule.StartForeground');
  EnableWakeLock(True);
  CreateNotificationChannel;
  LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, StringToJString(cNotificationChannelName));
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
      FWakeLock := TAndroidHelperEx.PowerManager.newWakeLock(TJPowerManager.JavaClass.PARTIAL_WAKE_LOCK, StringToJString(cWakeLockTag));
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
    ScreenLockChange(TAndroidHelperEx.KeyguardManager.inKeyguardRestrictedInputMode);
  end
  // Otherwise, check for "doze" mode changes
  else if TOSVersion.Check(6) and intent.getAction.equals(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED) then
    DozeModeChange(TAndroidHelperEx.PowerManager.isDeviceIdleMode);
end;

procedure TServiceModule.SetIsPaused(const AValue: Boolean);
begin
  //
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

procedure TServiceModule.TimerEventHandler(Sender: TObject);
begin
  WriteLog('TServiceModule.TimerEventHandler');
  UpdateFromLastKnownLocation(TLocationChangeFrom.Timer);
end;

procedure TServiceModule.UpdateFromLastKnownLocation(const AFrom: TLocationChangeFrom);
var
  LLocation: TLocationCoord2D;
begin
  LLocation := FLocation.GetLastKnownLocation;
  if Abs(LLocation.Latitude) <= 90 then
  begin
    WriteLog('TServiceModule.UpdateFromLastKnownLocation from: ' + cLocationFromCaptions[AFrom]);
    LocationChanged(LLocation, AFrom);
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

procedure TServiceModule.LocationChangeHandler(Sender: TObject; const ALocation: TLocationCoord2D);
begin
  LocationChanged(ALocation, TLocationChangeFrom.Listener);
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
    DoMessage(LStream.DataString);
//!!!!!!
{
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
}
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
  FLocation.Pause;
  UpdateConfig;
end;

procedure TServiceModule.Resume;
begin
  FLocation.Resume;
  UpdateConfig;
end;

procedure TServiceModule.UpdateConfig;
begin
  FConfig.IsPaused := FLocation.IsPaused;
  FConfig.Save;
  DoStatus;
end;

end.
