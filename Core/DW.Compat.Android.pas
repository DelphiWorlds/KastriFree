unit DW.Compat.Android;

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
  Androidapi.JNI.JavaTypes;

function JObjectToID(const AObject: JObject): Pointer;

implementation

uses
  Androidapi.JNIBridge;

function JObjectToID(const AObject: JObject): Pointer;
begin
  if AObject <> nil then
    Result := (AObject as ILocalObject).GetObjectID
  else
    Result := nil;
end;

end.
