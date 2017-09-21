unit ScheduledServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  TServiceModule = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    function SetAlarm(const AAction: string; const AStartAt, AInterval: Int64): Boolean;
    function SetDailyAlarm(const AAction: string; const AStartAt: Int64): Boolean;
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
  Androidapi.JNI.App, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
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
  LCalendar.add(TJCalendar.JavaClass.SECOND, ASeconds);
  Result := LCalendar.getTimeInMillis;
end;

function GetMidnightInMillis: Int64;
begin
  Result := GetTimeFromNowInMillis(SecondsUntilMidnight);
end;

function TServiceModule.SetAlarm(const AAction: string; const AStartAt, AInterval: Int64): Boolean;
var
  LIntent: JIntent;
  LPendingIntent: JPendingIntent;
begin
  Result := False;
  LIntent := TJIntent.JavaClass.init(StringToJString(AAction));
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cReceiverName));
  LIntent.putExtra(StringToJString('ServiceName'), StringToJString(cServiceName));
  // Using FLAG_NO_CREATE means that the return result is nil if NONE ALREADY EXISTS - weird
  LPendingIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_NO_CREATE);
  if LPendingIntent = nil then
  begin
    LPendingIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
    if AInterval = 0 then
      TAndroidHelper.AlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP, AStartAt, LPendingIntent)
    else
      TAndroidHelper.AlarmManager.setRepeating(TJAlarmManager.JavaClass.RTC_WAKEUP, AStartAt, AInterval, LPendingIntent);
    Result := True;
  end
  else
    TOSLog.d('Pending intent with action %s already exists, apparently', [AAction]);
end;

function TServiceModule.SetDailyAlarm(const AAction: string; const AStartAt: Int64): Boolean;
begin
  Result := SetAlarm(AAction, AStartAt, TJAlarmManager.JavaClass.INTERVAL_DAY);
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  TOSLog.d('Service started');
  if JStringToString(Intent.getAction).Equals(cActionServiceAlarm) then
  begin
    TOSLog.d('Alarm was triggered');
    // Do whatever should happen as a result of the alarm
  end
  // Set an alarm - a result of True means one was set (i.e. one had not been set before), so you might want to take some action in that case
  // if SetDailyAlarm(cActionServiceAlarm, MillisecondsUntilMidnight) then  // <----- Daily at midnight example
  else if SetAlarm(cActionServiceAlarm, GetTimeFromNowInMillis(60), 0) then // <----- One off, in 1 minutes time
  begin
    TOSLog.d('Alarm has been set');
    // Maybe do something here if alarm was not already set
  end;
  Result := TJService.JavaClass.START_STICKY;
end;

end.
