unit DW.Macapi.Helpers;

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
  // Mac
  {$IF Defined(MACOS)}
  Macapi.CoreFoundation,
  {$ENDIF}
  {$IF Defined(MACDEV)}
  Macapi.Foundation;
  {$ENDIF}
  {$IF Defined(IOS)}
  iOSapi.Foundation;
  {$ENDIF}

/// <summary>
///   Retrieves cocoa double constant
/// </summary>
function CocoaDoubleConst(const AFwk: string; const AConstStr: string): Double;
/// <summary>
///   Retrieves a number value from an NSDictionary, with optional default (otherwise zero)
/// </summary>
function GetDictionaryNumberValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: Double = 0): Double;
/// <summary>
///   Retrieves a string value from an NSDictionary, with optional default (otherwise blank)
/// </summary>
function GetDictionaryStringValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: string = ''): string;
/// <summary>
///   Puts string values from an array into an NSArray
/// </summary>
function StringArrayToNSArray(const AArray: array of string): NSArray;
/// <summary>
///   Converts a string directly into an NSString reference (ID)
/// </summary>
function StrToObjectID(const AStr: string): Pointer;
/// <summary>
///   Converts a string into an CFStringRef
/// </summary>
function StrToCFStringRef(const AStr: string): CFStringRef;
/// <summary>
///   Converts GMT to local time
/// </summary>
function GetLocalDateTime(const ADateTime: TDateTime): TDateTime;
function GetMainBundle: NSBundle;
function GetBundleValue(const AKey: string): string;

implementation

uses
  // RTL
  System.DateUtils,
  // Mac
  Macapi.ObjectiveC, Macapi.Helpers;

function CocoaDoubleConst(const AFwk: string; const AConstStr: string): Double;
var
  LObj: Pointer;
begin
  LObj := CocoaPointerConst(AFwk, AConstStr);
  if LObj <> nil then
    Result := Double(LObj^)
  else
    Result := 0;
end;

function GetDictionaryNumberValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: Double = 0): Double;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := ADictionary.valueForKey(AKey);
  if LValuePtr <> nil then
    Result := TNSNumber.Wrap(LValuePtr).doubleValue;
end;

function GetDictionaryStringValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: string = ''): string;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := ADictionary.valueForKey(AKey);
  if LValuePtr <> nil then
    Result := NSStrToStr(TNSString.Wrap(LValuePtr));
end;

function StringArrayToNSArray(const AArray: array of string): NSArray;
var
  LArray: array of Pointer;
  I: Integer;
begin
  SetLength(LArray, Length(AArray));
  for I := 0 to Length(AArray) - 1 do
    LArray[I] := NSObjectToID(StrToNSStr(AArray[I]));
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LArray[0], Length(LArray)));
end;

function StrToObjectID(const AStr: string): Pointer;
begin
  Result := NSObjectToID(StrToNSStr(AStr));
end;

function StrToCFStringRef(const AStr: string): CFStringRef;
begin
  Result := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar(AStr), Length(AStr));
end;

function GetLocalDateTime(const ADateTime: TDateTime): TDateTime;
begin
  Result := IncSecond(ADateTime, TNSTimeZone.Wrap(TNSTimeZone.OCClass.localTimeZone).secondsFromGMT);
end;

function GetMainBundle: NSBundle;
begin
  Result := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
end;

function GetBundleValue(const AKey: string): string;
var
  LValueObject: Pointer;
begin
  Result := '';
  LValueObject := GetMainBundle.infoDictionary.objectForKey(StrToObjectID(AKey));
  if LValueObject <> nil then
    Result := NSStrToStr(TNSString.Wrap(LValueObject));
end;

end.
