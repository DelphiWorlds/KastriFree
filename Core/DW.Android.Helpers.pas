unit DW.Android.Helpers;

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
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText;

type
  TAndroidHelperEx = record
  public
    const
      ICE_CREAM_SANDWICH = 14;
      ICE_CREAM_SANDWICH_MR1 = 15;
      JELLY_BEAN = 16;
      JELLY_BEAN_MR1 = 17;
      JELLY_BEAN_MR2 = 18;
      KITKAT = 19;
      KITKAT_MR1 = 20;
      LOLLIPOP = 21;
      LOLLIPOP_MR1 = 22;
      MARSHMALLOW = 23;
      NOUGAT = 24;
      NOUGAT_MR1 = 25;
      OREO = 26;
      OREO_MR1 = 27;
      ANDROID_P = 28;
    /// <summary>
    ///   Checks if both build and target are greater or equal to the tested value
    /// </summary>
    class function CheckBuildAndTarget(const AValue: Integer): Boolean; static;
    /// <summary>
    ///   Returns the equivalent of "AndroidClass.class"
    /// </summary>
    class function GetClass(const APackageClassName: string): Jlang_Class; static;
    /// <summary>
    ///   Returns the application default icon ID
    /// </summary>
    class function GetDefaultIconID: Integer; static;
    /// <summary>
    ///   Returns a URI to the notification sound
    /// </summary>
    class function GetDefaultNotificationSound: Jnet_Uri; static;
    /// <summary>
    ///   Returns target Sdk version
    /// </summary>
    class function GetTargetSdkVersion: Integer; static;
    /// <summary>
    ///   Returns installed Sdk version
    /// </summary>
    class function GetBuildSdkVersion: Integer; static;
    /// <summary>
    ///   Returns whether or not a service is running
    /// </summary>
    class function IsServiceRunning(const AServiceName: string): Boolean; static;
    /// <summary>
    ///   Call this to start an activity from an alarm
    /// </summary>
    /// <remarks>
    ///   Used in conjunction with dw-multireceiver.jar
    /// </remarks>
    class procedure SetStartAlarm(const AAlarm: TDateTime; const AStartFromLock: Boolean); static;
    /// <summary>
    ///   Converts file to uri, using FileProvider if target API >= 24
    /// </summary>
    /// <remarks>
    ///   Use this only when accessing files with an "external" URI
    /// </remarks>
    class function UriFromFile(const AFile: JFile): Jnet_Uri; static;
    /// <summary>
    ///   Converts filename to uri, using FileProvider if target API >= 24
    /// </summary>
    /// <remarks>
    ///   Use this only when accessing files with an "external" URI
    /// </remarks>
    class function UriFromFileName(const AFileName: string): Jnet_Uri; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.DateUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.Media,
  // DW
  DW.Androidapi.JNI.FileProvider;

const
  cReceiverClassName = 'DWMultiBroadcastReceiver';
  cReceiverName = 'com.delphiworlds.kastri.' + cReceiverClassName;
  cActionStartAlarm = cReceiverName + '.ACTION_START_ALARM';
  cExtraStartUnlock = cReceiverClassName + '.EXTRA_START_UNLOCK';

{ TAndroidHelperEx }

class function TAndroidHelperEx.CheckBuildAndTarget(const AValue: Integer): Boolean;
begin
  Result := (GetBuildSdkVersion >= AValue) and (GetTargetSdkVersion >= AValue);
end;

class function TAndroidHelperEx.GetBuildSdkVersion: Integer;
begin
   Result := TJBuild_VERSION.JavaClass.SDK_INT;
end;

class function TAndroidHelperEx.GetClass(const APackageClassName: string): Jlang_Class;
begin
  Result := TJLang_Class.JavaClass.forName(StringToJString(APackageClassName), True, TAndroidHelper.Activity.getClassLoader);
end;

class function TAndroidHelperEx.GetDefaultIconID: Integer;
begin
  Result := TAndroidHelper.Context.getApplicationInfo.icon;
end;

class function TAndroidHelperEx.GetDefaultNotificationSound: Jnet_Uri;
begin
  Result := TJRingtoneManager.JavaClass.getDefaultUri(TJRingtoneManager.JavaClass.TYPE_NOTIFICATION);
end;

class function TAndroidHelperEx.UriFromFile(const AFile: JFile): Jnet_Uri;
var
  LAuthority: JString;
begin
  if CheckBuildAndTarget(NOUGAT) then
  begin
    LAuthority := StringToJString(JStringToString(TAndroidHelper.Context.getApplicationContext.getPackageName) + '.fileprovider');
    Result := TJFileProvider.JavaClass.getUriForFile(TAndroidHelper.Context, LAuthority, AFile);
  end
  else
    Result := TJnet_uri.JavaClass.fromFile(AFile);
end;

class function TAndroidHelperEx.UriFromFileName(const AFileName: string): Jnet_Uri;
begin
  Result := UriFromFile(TJFile.JavaClass.init(StringToJString(AFileName)));
end;

class function TAndroidHelperEx.GetTargetSdkVersion: Integer;
var
  LApplicationInfo: JApplicationInfo;
begin
  LApplicationInfo := TAndroidHelper.Context.getPackageManager.getApplicationInfo(TAndroidHelper.Context.getPackageName, 0);
  Result := LApplicationInfo.targetSdkVersion;
end;

class function TAndroidHelperEx.IsServiceRunning(const AServiceName: string): Boolean;
var
  LIntent: JIntent;
  LPendingIntent: JPendingIntent;
begin
  LIntent := TJIntent.JavaClass.init(TAndroidHelper.Context, GetClass(AServiceName));
  LPendingIntent := TJPendingIntent.JavaClass.getService(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_NO_CREATE);
  Result := LPendingIntent <> nil;
end;

function GetTimeFromNowInMillis(const ASecondsFromNow: Integer): Int64;
var
  LCalendar: JCalendar;
begin
  LCalendar := TJCalendar.JavaClass.getInstance;
  if ASecondsFromNow > 0 then
    LCalendar.add(TJCalendar.JavaClass.SECOND, ASecondsFromNow);
  Result := LCalendar.getTimeInMillis;
end;

class procedure TAndroidHelperEx.SetStartAlarm(const AAlarm: TDateTime; const AStartFromLock: Boolean);
var
  LActionIntent: JIntent;
  LAlarmIntent: JPendingIntent;
  LStartAt: Int64;
begin
  LActionIntent := TJIntent.JavaClass.init(StringToJString(cActionStartAlarm));
  LActionIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cReceiverName));
  LActionIntent.putExtra(StringToJString(cExtraStartUnlock), AStartFromLock);
  LAlarmIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LActionIntent, TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
  LStartAt := GetTimeFromNowInMillis(SecondsBetween(Now, AAlarm));
  // Allow for alarms while in "doze" mode
  if TOSVersion.Check(6) then
    TAndroidHelper.AlarmManager.setExactAndAllowWhileIdle(TJAlarmManager.JavaClass.RTC_WAKEUP, LStartAt, LAlarmIntent)
  else
    TAndroidHelper.AlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP, LStartAt, LAlarmIntent);
end;

end.
