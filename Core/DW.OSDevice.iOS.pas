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
  // DW
  DW.OSDevice;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSDevice = record
  public
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    class function GetDeviceName: string; static;
    class function GetPackageID: string; static;
    class function GetPackageVersion: string; static;
    class function GetUniqueDeviceID: string; static;
    class function IsScreenLocked: Boolean; static;
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
  DW.Macapi.Helpers, DW.iOSapi.Foundation;

{ TPlatformOSDevice }

class function TPlatformOSDevice.GetCurrentLocaleInfo: TLocaleInfo;
var
  LLocale: NSLocale;
begin
  LLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  Result.LanguageCode := NSStrToStr(LLocale.languageCode);
  Result.LanguageDisplayName := NSStrToStr(LLocale.localizedStringForLanguageCode(LLocale.languageCode));
  Result.CountryCode := NSStrToStr(LLocale.countryCode);
  Result.CountryDisplayName := NSStrToStr(LLocale.localizedStringForCountryCode(LLocale.countryCode));
  Result.CurrencySymbol := NSStrToStr(LLocale.currencySymbol);
end;

class function TPlatformOSDevice.GetDeviceName: string;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.name);
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.identifierForVendor.UUIDString);
end;

class function TPlatformOSDevice.IsScreenLocked: Boolean;
begin
  Result := False; // To be implemented
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := True;
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleIdentifier');
end;

class function TPlatformOSDevice.GetPackageVersion: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleVersion');
end;

end.
