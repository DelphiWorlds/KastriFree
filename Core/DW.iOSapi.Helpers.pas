unit DW.iOSapi.Helpers;

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
  // iOS
  iOSapi.Foundation;

/// <summary>
///   Converts values in an NSDictionary to JSON
/// </summary>
function NSDictionaryToJSON(const ADictionary: NSDictionary): string;
/// <summary>
///   Temporary helper function to support iPhone X quirks
/// </summary>
function IsIPhoneX: Boolean;

implementation

uses
  // iOS
  iOSapi.UIKit, iOSapi.Helpers,
  // Mac
  Macapi.ObjectiveC, Macapi.ObjCRuntime, Macapi.Helpers;

function NSDictionaryToJSON(const ADictionary: NSDictionary): string;
var
  LData: NSData;
  LString: NSString;
  LError: NSError;
begin
  LData := TNSJSONSerialization.OCClass.dataWithJSONObject(NSObjectToID(ADictionary), 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then
  begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
  end
  else
    Result := '';
end;

function IsIPhoneX: Boolean;
const
  cIPhoneXHeight = 812;
var
  LOrientation: UIInterfaceOrientation;
begin
  Result := False;
  // Might be safe enough to just use statusBarOrientation
  if TiOSHelper.SharedApplication.keyWindow = nil then
    LOrientation := TiOSHelper.SharedApplication.statusBarOrientation
  else
    LOrientation := TiOSHelper.SharedApplication.keyWindow.rootViewController.interfaceOrientation;
  case LOrientation of
    UIInterfaceOrientationPortrait, UIInterfaceOrientationPortraitUpsideDown:
      Result := TiOSHelper.MainScreen.bounds.size.height = cIPhoneXHeight;
    UIInterfaceOrientationLandscapeLeft, UIInterfaceOrientationLandscapeRight:
      Result := TiOSHelper.MainScreen.bounds.size.width = cIPhoneXHeight;
  end;
end;


end.
