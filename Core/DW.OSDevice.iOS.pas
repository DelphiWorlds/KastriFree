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
    class function GetOffsetRect: TRectF; static;
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
  iOSapi.UIKit, iOSapi.Helpers,
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

class function TPlatformOSDevice.GetOffsetRect: TRectF;
const
  cIPhoneXHeight = 812;
  cNotchHeight = 40;
  cSwipeIndicatorHeight = 24;
var
  LOrientation: UIInterfaceOrientation;
begin
  Result := RectF(0, 0, 0, 0);
  LOrientation := TiOSHelper.SharedApplication.keyWindow.rootViewController.interfaceOrientation;
  case LOrientation of
    UIInterfaceOrientationPortrait: // , UIInterfaceOrientationPortraitUpsideDown - apparently there is none on iPhoneX?
    begin
      if TiOSHelper.MainScreen.bounds.size.height = cIPhoneXHeight then
      begin
        // In portrait mode, 10.2.2 accounts for the height of the notch
        {$IF CompilerVersion >= 32}
        Result := RectF(0, 0, 0, cSwipeIndicatorHeight);
        {$ELSE}
        Result := RectF(0, cNotchHeight, 0, cSwipeIndicatorHeight);
        {$ENDIF}
      end;
    end;
    UIInterfaceOrientationLandscapeLeft:
    begin
      if TiOSHelper.MainScreen.bounds.size.width = cIPhoneXHeight then
        Result := RectF(0, 0, cNotchHeight, cSwipeIndicatorHeight);
    end;
    UIInterfaceOrientationLandscapeRight:
    begin
      if TiOSHelper.MainScreen.bounds.size.width = cIPhoneXHeight then
        Result := RectF(cNotchHeight, 0, 0, cSwipeIndicatorHeight);
    end;
  end;
end;

end.
