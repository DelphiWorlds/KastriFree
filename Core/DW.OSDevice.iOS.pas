unit DW.OSDevice.iOS;

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
  System.Types;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSDevice = record
  public
    class function GetDeviceName: string; static;
    class function GetPackageID: string; static;
    class function GetPackageVersion: string; static;
    class function GetUniqueDeviceID: string; static;
    class function IsTouchDevice: Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers,
  // DW
  DW.Macapi.Helpers;

{ TPlatformOSDevice }

class function TPlatformOSDevice.GetDeviceName: string;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.name);
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.identifierForVendor.UUIDString);
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := True;
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := GetBundleValue('CFBundleIdentifier');
end;

class function TPlatformOSDevice.GetPackageVersion: string;
begin
  Result := GetBundleValue('CFBundleVersion');
end;

end.
