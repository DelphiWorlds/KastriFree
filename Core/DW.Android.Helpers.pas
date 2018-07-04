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
    /// <summary>
    ///   Returns the equivalent of "AndroidClass.class"
    /// </summary>
    class function GetClass(const APackageClassName: string): Jlang_Class; static;
    /// <summary>
    ///   Returns target Sdk version
    /// </summary>
    class function GetTargetSdkVersion: Integer; static;
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
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App,
  // DW
  DW.Androidapi.JNI.FileProvider;

{ TAndroidHelperEx }

class function TAndroidHelperEx.GetClass(const APackageClassName: string): Jlang_Class;
begin
  Result := TJLang_Class.JavaClass.forName(StringToJString(APackageClassName), True, TAndroidHelper.Activity.getClassLoader);
end;

class function TAndroidHelperEx.UriFromFile(const AFile: JFile): Jnet_Uri;
var
  LAuthority: JString;
begin
  if GetTargetSdkVersion >= 24 then
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
