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
  {$IF Defined(MACDEV)}
  Macapi.Foundation;
  {$ENDIF}
  {$IF Defined(IOS)}
  iOSapi.Foundation;
  {$ENDIF}

/// <summary>
///   Retrieves a number value from an NSDictionary, with optional default (otherwise zero)
/// </summary>
function GetDictionaryNumberValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: Double = 0): Double;
/// <summary>
///   Retrieves a string value from an NSDictionary, with optional default (otherwise blank)
/// </summary>
function GetDictionaryStringValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: string = ''): string;
/// <summary>
///   Converts values in an NSDictionary to JSON
/// </summary>
function NSDictionaryToJSON(const ADictionary: NSDictionary): string;
/// <summary>
///   Puts string values from an array into an NSArray
/// </summary>
function StringArrayToNSArray(const AArray: array of string): NSArray;
function StrToObjectID(const AStr: string): Pointer;

implementation

uses
  // Mac
  Macapi.ObjectiveC, Macapi.Helpers;

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

end.
