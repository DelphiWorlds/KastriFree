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
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Net;

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

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Os,
  // DW
  DW.Androidapi.JNI.FileProvider;

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
