unit DW.Android.Helpers;

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
    ///   Returns the equivalent of "AndroidClass.class"
    /// </summary>
    class function UriFromFile(const AImageFile: JFile): Jnet_Uri; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.OSDevice, DW.Androidapi.JNI.FileProvider;

{ TAndroidHelperEx }

class function TAndroidHelperEx.GetClass(const APackageClassName: string): Jlang_Class;
begin
  Result := TJLang_Class.JavaClass.forName(StringToJString(APackageClassName), True, TAndroidHelper.Activity.getClassLoader);
end;

class function TAndroidHelperEx.UriFromFile(const AImageFile: JFile): Jnet_Uri;
var
  LAuthority: JString;
begin
  if GetTargetSdkVersion >= 24 then
  begin
    LAuthority := StringToJString(JStringToString(TAndroidHelper.Context.getApplicationContext.getPackageName) + '.fileprovider');
    Result := TJFileProvider.JavaClass.getUriForFile(TAndroidHelper.Context, LAuthority, AImageFile);
  end
  else
    Result := TJnet_uri.JavaClass.fromFile(AImageFile);
end;

class function TAndroidHelperEx.GetTargetSdkVersion: Integer;
var
  LApplicationInfo: JApplicationInfo;
begin
  LApplicationInfo := TAndroidHelper.Context.getPackageManager.getApplicationInfo(TAndroidHelper.Context.getPackageName, 0);
  Result := LApplicationInfo.targetSdkVersion;
end;

end.
