unit DW.SystemHelper.Android;

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
  Androidapi.JNI,
  // DW
  DW.SystemHelper;

type
  TPlatformSystemHelper = class(TCustomPlatformSystemHelper)
  private
    class var FInstance: TPlatformSystemHelper;
    class procedure OnRequestPermissionsResultNative(AEnv: PJNIEnv; AThis: JNIObject; requestCode: Integer;
      permissions: JNIObjectArray; granted: JNIIntArray); cdecl; static;
  private
    FIsRegistered: Boolean;
    procedure RegisterDelphiNativeMethods;
  protected
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer); override;
  public
    constructor Create(const ASystemHelper: TSystemHelper); override;
    class function CheckPermission(const APermission: string): Boolean;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.NativeActivity,
  Androidapi.JNI.App, Androidapi.JNI.Os,
  // DW
  DW.Android.Helpers;

type
  TOpenSystemHelper = class(TSystemHelper);

{ TPlatformSystemHelper }

constructor TPlatformSystemHelper.Create(const ASystemHelper: TSystemHelper);
begin
  inherited;
  if FInstance = nil then
    FInstance := Self;
end;

class function TPlatformSystemHelper.CheckPermission(const APermission: string): Boolean;
begin
  Result := TAndroidHelper.Context.checkSelfPermission(StringToJString(APermission)) = TJPackageManager.JavaClass.PERMISSION_GRANTED;
end;

class procedure TPlatformSystemHelper.OnRequestPermissionsResultNative(AEnv: PJNIEnv; AThis: JNIObject; requestCode: Integer;
  permissions: JNIObjectArray; granted: JNIIntArray);
var
  LPermissionsArray: TJavaObjectArray<JString>;
  LGrantedArray: TJavaArray<Integer>;
  LPermissions: TPermissionResults;
  I: Integer;
begin
  // A bug in JNI Bridge means we get a (harmless) logcat here for each
  // WrapJNIArray call as a GlobalRef is deleted as if it were a LocalRef:
  // W/art: Attempt to remove non-JNI local reference, dumping thread
  LGrantedArray := TJavaArray<Integer>.Wrap(WrapJNIArray(granted, TypeInfo(TJavaArray<Integer>)));
  LPermissionsArray := TJavaObjectArray<JString>.Wrap(WrapJNIArray(permissions, TypeInfo(TJavaObjectArray<JString>)));
  if (LGrantedArray.Length > 0) and (LPermissionsArray.Length > 0) and (LGrantedArray.Length = LPermissionsArray.Length) then
  begin
    SetLength(LPermissions, LGrantedArray.Length);
    for I := 0 to LGrantedArray.Length - 1 do
    begin
      LPermissions[I].Granted := LGrantedArray.Items[I] = TJPackageManager.JavaClass.PERMISSION_GRANTED;
      LPermissions[I].Permission := JStringToString(LPermissionsArray.Items[I]);
    end;
    TThread.Queue(nil,
      procedure
      begin
        TOpenSystemHelper(FInstance.SystemHelper).DoPermissionsResult(requestCode, LPermissions);
      end);
  end;
end;

procedure TPlatformSystemHelper.RegisterDelphiNativeMethods;
var
  PEnv: PJNIEnv;
  ActivityClass: JNIClass;
  NativeMethod: JNINativeMethod;
begin
  if FIsRegistered then
    Exit; // <======
  PEnv := TJNIResolver.GetJNIEnv;
  NativeMethod.Name := 'onRequestPermissionsResultNative';
  NativeMethod.Signature := '(I[Ljava/lang/String;[I)V'; // Integer, String [], Integer[] (VOID)
  NativeMethod.FnPtr := @OnRequestPermissionsResultNative;
  ActivityClass := PEnv^.GetObjectClass(PEnv, PANativeActivity(System.DelphiActivity).clazz);
  PEnv^.RegisterNatives(PEnv, ActivityClass, @NativeMethod, 1);
  PEnv^.DeleteLocalRef(PEnv, ActivityClass);
  FIsRegistered := True;
end;

procedure TPlatformSystemHelper.RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
var
  LPermissions: TJavaObjectArray<JString>;
  I: Integer;
begin
  if not TAndroidHelperEx.CheckBuildAndTarget(TAndroidHelperEx.MARSHMALLOW)  then
    Exit; //
  RegisterDelphiNativeMethods;
  LPermissions := TJavaObjectArray<JString>.Create(Length(APermissions));
  for I := Low(APermissions) to High(APermissions) do
    LPermissions.Items[I] := StringToJString(APermissions[I]);
  TAndroidHelper.Activity.requestPermissions(LPermissions, ARequestCode);
end;

end.
