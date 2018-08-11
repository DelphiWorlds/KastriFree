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
    ///   Converts file to uri, using FileProvider if target API >= 24
    /// </summary>
    /// <remarks>
    ///   Use this only when accessing files with an "external" URI
    /// </remarks>
    class function UriFromFile(const AFile: JFile): Jnet_Uri; static;
  end;

  TPreferences = class(TObject)
  private
    FPreferences: JSharedPreferences;
  public
    constructor Create(const AID: string);
    destructor Destroy; override;
    function GetValue(const AKey: string; const ADefaultValue: string = ''): string;
    procedure SetValue(const AKey: string; const AValue: string);
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.Media,
  // DW
  DW.Androidapi.JNI.FileProvider;

{ TPreferences }

constructor TPreferences.Create(const AID: string);
begin
  inherited Create;
  FPreferences := TAndroidHelper.Context.getSharedPreferences(StringToJString(AID), TJContext.JavaClass.MODE_PRIVATE);
end;

destructor TPreferences.Destroy;
begin
  FPreferences := nil;
  inherited;
end;

function TPreferences.GetValue(const AKey: string; const ADefaultValue: string = ''): string;
begin
  Result := JStringToString(FPreferences.getString(StringToJString(AKey), StringToJString(ADefaultValue)));
end;

procedure TPreferences.SetValue(const AKey, AValue: string);
var
  LEditor: JSharedPreferences_Editor;
begin
  LEditor := FPreferences.edit;
  try
    LEditor.putString(StringToJString(AKey), StringToJString(AValue));
  finally
    LEditor.commit;
  end;
end;

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

class function TAndroidHelperEx.GetTargetSdkVersion: Integer;
var
  LApplicationInfo: JApplicationInfo;
begin
  LApplicationInfo := TAndroidHelper.Context.getPackageManager.getApplicationInfo(TAndroidHelper.Context.getPackageName, 0);
  Result := LApplicationInfo.targetSdkVersion;
end;

end.
