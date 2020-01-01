unit DW.OSMetadata;

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
  TOSMetadata = record
  public
    class function ContainsKey(const AKey: string): Boolean; static;
    class function GetValue(const AKey: string; var AValue: string): Boolean; static;
  end;

implementation

uses
  // DW
  {$IF Defined(ANDROID)}
  DW.OSMetadata.Android;
  {$ELSEIF Defined(MACOS)}
  DW.OSMetadata.Mac;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.OSMetadata.Win;
  {$ELSEIF Defined(LINUX)}
  DW.OSMetadata.Linux;
  {$ENDIF}

{ TOSMetadata }

class function TOSMetadata.ContainsKey(const AKey: string): Boolean;
begin
  Result := TPlatformOSMetadata.ContainsKey(AKey);
end;

class function TOSMetadata.GetValue(const AKey: string; var AValue: string): Boolean;
begin
  Result := TPlatformOSMetadata.GetValue(AKey, AValue);
end;

end.