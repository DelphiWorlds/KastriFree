unit DW.OSDevice.Linux;

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
  // Posix
  Posix.UniStd;

{ TPlatformOSDevice }

class function TPlatformOSDevice.GetDeviceName: string;
const
  cMaxHostName = 255;
var
  LHost: array[0..cMaxHostName] of Byte;
  LHostPtr: TPtrWrapper;
begin
  LHostPtr := TPtrWrapper.Create(@LHost[0]);
  gethostname(LHostPtr.ToPointer, cMaxHostName);
  Result := TMarshal.ReadStringAsAnsi(LHostPtr);
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := ''; // None yet
end;

class function TPlatformOSDevice.GetPackageVersion: string;
begin
  Result := ''; // None yet
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
begin
  Result := '';
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := False;
end;

end.
