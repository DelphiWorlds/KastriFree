unit DW.OSDevice.Mac;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

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
  Macapi.CoreFoundation, Macapi.Foundation, Macapi.Helpers, Macapi.IOKit,
  // DW
  DW.Macapi.IOKit, DW.Macapi.Helpers;

{ TPlatformOSDevice }

class function TPlatformOSDevice.GetDeviceName: string;
begin
  Result := NSStrToStr(TNSHost.Wrap(TNSHost.OCClass.currentHost).localizedName);
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
var
  LService: io_service_t;
  LSerialRef: CFTypeRef;
begin
  Result := '';
  LService := IOServiceGetMatchingService(kIOMasterPortDefault, CFDictionaryRef(IOServiceMatching('IOPlatformExpertDevice')));
  if LService > 0 then
  try
    LSerialRef := IORegistryEntryCreateCFProperty(LService, kIOPlatformSerialNumberKey, kCFAllocatorDefault, 0);
    if LSerialRef <> nil then
      Result := CFStringRefToStr(LSerialRef);
  finally
    IOObjectRelease(LService);
  end;
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := False;
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



