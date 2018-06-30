unit DW.Android.Helpers;

interface

uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Net;

type
  TAndroidHelperEx = record
  public
    /// <summary>
    ///   Returns the equivalent of "AndroidClass.class"
    /// </summary>
    class function GetClass(const APackageClassName: string): Jlang_Class; static;
    /// <summary>
    ///   Returns the equivalent of "AndroidClass.class"
    /// </summary>
    class function UriFromFile(const AImageFile: JFile): Jnet_Uri; static;
  end;

implementation

uses
  Androidapi.Helpers,
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
  if TOSDevice.GetTargetSdkVersion >= 24 then
  begin
    LAuthority := StringToJString(JStringToString(TAndroidHelper.Context.getApplicationContext.getPackageName) + '.fileprovider');
    Result := TJFileProvider.JavaClass.getUriForFile(TAndroidHelper.Context, LAuthority, AImageFile);
  end
  else
    Result := TJnet_uri.JavaClass.fromFile(AImageFile);
end;

end.
