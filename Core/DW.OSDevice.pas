unit DW.OSDevice;

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
  // RTL
  System.Types,
  // DW
  DW.PermissionsTypes;

type
  /// <summary>
  ///   Operating System specific functions that operate below FMX
  /// </summary>
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TOSDevice = record
  public
    /// <summary>
    ///   Checks whether or not a single permissions has been granted
    /// </summary>
    class function CheckPermission(const APermission: string; const ALog: Boolean = False): Boolean; static;
    /// <summary>
    ///   Checks whether or not a set of permissions have been granted
    /// </summary>
    class function CheckPermissions(const APermissions: array of string): Boolean; overload; static;
    class function CheckPermissions(const APermissions: array of string; var AResults: TPermissionResults): Boolean; overload; static;
    /// <summary>
    ///   Returns the name of the device, whether it is mobile or desktop
    /// </summary>
    class function GetDeviceName: string; static;
    /// <summary>
    ///   Special function for handling of iPhoneX
    /// </summary>
    class function GetOffsetRect: TRectF; static;
    /// <summary>
    ///   Returns build for the application package, if any exists
    /// </summary>
    class function GetPackageBuild: string; static;
    /// <summary>
    ///   Returns id for the application package, if any exists
    /// </summary>
    class function GetPackageID: string; static;
    /// <summary>
    ///   Returns version for the application package, if any exists
    /// </summary>
    class function GetPackageVersion: string; static;
    /// <summary>
    ///   Returns the unique id for the device, if any exists
    /// </summary>
    class function GetUniqueDeviceID: string; static;
    /// <summary>
    ///   Returns whether the device is a mobile device
    /// </summary>
    class function IsMobile: Boolean; static;
    /// <summary>
    ///   Returns whether the device has touch capability
    /// </summary>
    class function IsTouchDevice: Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  DW.OSLog,
  {$IF Defined(ANDROID)}
  DW.OSDevice.Android;
  {$ELSEIF Defined(IOS)}
  DW.OSDevice.iOS;
  {$ELSEIF Defined(MACOS)}
  DW.OSDevice.Mac;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.OSDevice.Win;
  {$ELSEIF Defined(LINUX)}
  DW.OSDevice.Linux;
  {$ENDIF}

{ TOSDevice }

class function TOSDevice.CheckPermission(const APermission: string; const ALog: Boolean = False): Boolean;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformOSDevice.CheckPermission(APermission);
  if ALog then
  begin
    if Result then
      TOSLog.d('%s granted', [APermission])
    else
      TOSLog.d('%s denied', [APermission]);
  end;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

class function TOSDevice.CheckPermissions(const APermissions: array of string; var AResults: TPermissionResults): Boolean;
var
  I: Integer;
begin
  SetLength(AResults, Length(APermissions));
  for I := 0 to AResults.Count - 1 do
  begin
    AResults[I].Permission := APermissions[I];
    AResults[I].Granted := CheckPermission(AResults[I].Permission);
  end;
  Result := AResults.AreAllGranted;
end;

class function TOSDevice.CheckPermissions(const APermissions: array of string): Boolean;
var
  LResults: TPermissionResults;
begin
  Result := CheckPermissions(APermissions, LResults);
end;

class function TOSDevice.GetDeviceName: string;
begin
  Result := TPlatformOSDevice.GetDeviceName;
end;

class function TOSDevice.GetPackageBuild: string;
begin
  {$IF Defined(MSWINDOWS)}
  Result := TPlatformOSDevice.GetPackageBuild;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TOSDevice.GetPackageID: string;
begin
  Result := TPlatformOSDevice.GetPackageID;
end;

class function TOSDevice.GetPackageVersion: string;
begin
  Result := TPlatformOSDevice.GetPackageVersion;
end;

class function TOSDevice.GetOffsetRect: TRectF;
begin
  {$IF Defined(IOS)}
  Result := TPlatformOSDevice.GetOffsetRect;
  {$ELSE}
  Result := RectF(0, 0, 0, 0);
  {$ENDIF}
end;

class function TOSDevice.GetUniqueDeviceID: string;
begin
  Result := TPlatformOSDevice.GetUniqueDeviceID;
end;

class function TOSDevice.IsMobile: Boolean;
begin
  Result := TOSVersion.Platform in [TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid];
end;

class function TOSDevice.IsTouchDevice: Boolean;
begin
  Result := TPlatformOSDevice.IsTouchDevice;
end;

end.
