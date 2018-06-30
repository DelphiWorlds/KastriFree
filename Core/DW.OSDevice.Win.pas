unit DW.OSDevice.Win;

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
  // DW
  DW.FileVersionInfo.Win;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSDevice = class(TObject)
  private
    class var FFileVersionInfo: TFileVersionInfo;
    class destructor DestroyClass;
    class function GetFileVersionInfo: TFileVersionInfo; static;
  public
    class function GetDeviceName: string; static;
    class function GetPackageBuild: string; static;
    class function GetPackageID: string; static;
    class function GetPackageVersion: string; static;
    class function GetUniqueDeviceID: string; static;
    class function IsTouchDevice: Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Win.Registry,
  // Windows
  Winapi.Windows;

const
  cMicrosoftCryptographyKey = 'SOFTWARE\Microsoft\Cryptography';
  cMachineGuidValueName = 'MachineGuid';

{ TPlatformOSDevice }

class destructor TPlatformOSDevice.DestroyClass;
begin
  FFileVersionInfo.Free;
end;

class function TPlatformOSDevice.GetDeviceName: string;
var
  LComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  LSize: DWORD;
begin
  LSize := Length(LComputerName);
  if GetComputerName(LComputerName, LSize) then
    SetString(Result, LComputerName, LSize)
  else
    Result := '';
end;

class function TPlatformOSDevice.GetFileVersionInfo: TFileVersionInfo;
begin
  if FFileVersionInfo = nil then
    FFileVersionInfo := TFileVersionInfo.Create(GetModuleName(HInstance));
  Result := FFileVersionInfo;
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := GetFileVersionInfo.InternalName;
end;

class function TPlatformOSDevice.GetPackageVersion: string;
var
  LVersion: TLongVersion;
begin
  LVersion := GetFileVersionInfo.FileLongVersion;
  Result := Format('%d.%d.%d', [LVersion.All[2], LVersion.All[1], LVersion.All[4]]);
end;

class function TPlatformOSDevice.GetPackageBuild: string;
begin
  Result := GetFileVersionInfo.FileLongVersion.All[3].ToString;
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
var
  LRegistry: TRegistry;
  LAccess: Cardinal;
begin
  // **** BEWARE!!!! ****
  // VM's that are clones will have identical IDs.
  Result := '';
  LAccess := KEY_READ;
  if TOSVersion.Architecture = TOSVersion.TArchitecture.arIntelX86 then
    LAccess := LAccess or KEY_WOW64_32KEY
  else
    LAccess := LAccess or KEY_WOW64_64KEY;
  LRegistry := TRegistry.Create(LAccess);
  try
    LRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if LRegistry.OpenKey(cMicrosoftCryptographyKey, False) then
      Result := LRegistry.ReadString(cMachineGuidValueName);
  finally
    LRegistry.Free;
  end;
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
var
  LValue: Integer;
begin
  LValue := GetSystemMetrics(SM_DIGITIZER);
  Result := ((LValue and NID_READY) = NID_READY) and (((LValue and NID_MULTI_INPUT) = NID_MULTI_INPUT));
end;

end.



