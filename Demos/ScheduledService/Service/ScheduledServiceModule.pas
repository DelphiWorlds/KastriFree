unit ScheduledServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  Androidapi.JNI.App, AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  TServiceModule = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    FAlarmIntent: JPendingIntent;
    FInterval: Int64;
    procedure CreateAlarmIntent(const AAction: string);
    procedure SetAlarm(const AAction: string; AStartAt: Int64 = 0; AInterval: Int64 = 0);
    procedure SetDailyAlarm(const AAction: string; const AStartAt: Int64);
    procedure StopAlarm;
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.DateUtils,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.OSLog;

const
  cServiceName = 'com.embarcadero.services.ScheduledService';
  // Defining these constants here just saves having to import it from Java code
  cReceiverName = 'com.delphiworlds.kastri.DWMultiBroadcastReceiver';
  cActionServiceAlarm = cReceiverName + '.ACTION_SERVICE_ALARM';

function SecondsUntilMidnight: Int64;
begin
  Result := SecondsBetween(Now, Trunc(Now + 1));
end;

function GetTimeFromNowInMillis(const ASeconds: Integer): Int64;
var
  LCalendar: JCalendar;
begin
  LCalendar := TJCalendar.JavaClass.getInstance;
  if ASeconds > 0 then
    LCalendar.add(TJCalendar.JavaClass.SECOND, ASeconds);
  Result := LCalendar.getTimeInMillis;
end;

function GetMidnightInMillis: Int64;
begin
  Result := GetTimeFromNowInMillis(SecondsUntilMidnight);
end;

procedure TServiceModule.CreateAlarmIntent(const AAction: string);
var
  LActionIntent: JIntent;
begin
  LActionIntent := TJIntent.JavaClass.init(StringToJString(AAction));
  LActionIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cReceiverName));
  LActionIntent.putExtra(StringToJString('ServiceName'), StringToJString(cServiceName));
  FAlarmIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LActionIntent, TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
end;

procedure TServiceModule.SetAlarm(const AAction: string; AStartAt: Int64 = 0; AInterval: Int64 = 0);
var
  LIntent: JIntent;
begin
  if AStartAt = 0 then
    AStartAt := GetTimeFromNowInMillis(0);
  StopAlarm;
  FInterval := AInterval;
  CreateAlarmIntent(AAction);
  if FInterval > 0 then
  begin
    // Allow for alarms while in "doze" mode
    if TOSVersion.Check(6) then
      TAndroidHelper.AlarmManager.setExactAndAllowWhileIdle(TJAlarmManager.JavaClass.RTC_WAKEUP, GetTimeFromNowInMillis(AInterval), FAlarmIntent)
    else
      TAndroidHelper.AlarmManager.setRepeating(TJAlarmManager.JavaClass.RTC_WAKEUP, AStartAt, AInterval, FAlarmIntent);
  end
  else
    TAndroidHelper.AlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP, AStartAt, FAlarmIntent);
end;

procedure TServiceModule.SetDailyAlarm(const AAction: string; const AStartAt: Int64);
begin
  SetAlarm(AAction, AStartAt, TJAlarmManager.JavaClass.INTERVAL_DAY);
end;

procedure TServiceModule.StopAlarm;
begin
  if FAlarmIntent <> nil then
    TAndroidHelper.AlarmManager.cancel(FAlarmIntent);
  FAlarmIntent := nil;
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  TOSLog.d('Service started');
  if (Intent <> nil) and JStringToString(Intent.getAction).Equals(cActionServiceAlarm) then
  begin
    TOSLog.d('Alarm was triggered');
    // Do whatever should happen as a result of the alarm
    // Reset the alarm if on Android 6 or greater, to allow for alarms while in "doze" mode
    if TOSVersion.Check(6) and (FInterval > 0) then
      SetAlarm(cActionServiceAlarm, 0, FInterval);
  end
  // Set an alarm - a result of True means one was set (i.e. one had not been set before), so you might want to take some action in that case
  // if SetDailyAlarm(cActionServiceAlarm, MillisecondsUntilMidnight) then  // <----- Daily at midnight example
  else
    SetAlarm(cActionServiceAlarm, GetTimeFromNowInMillis(60)); // <----- One off, in 1 minutes time
  Result := TJService.JavaClass.START_STICKY;
end;

end.
