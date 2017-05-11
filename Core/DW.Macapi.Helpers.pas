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

function NSDictionaryToJSON(const ADictionary: NSDictionary): string;

implementation

uses
  // Mac
  Macapi.ObjectiveC, Macapi.Helpers;

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

end.
