unit DW.OSLog.Linux;

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
  DW.OSLog;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSLog = record
  public
    class procedure Log(const ALogType: TLogType; const AFmt: string; const AParams: array of const); static;
  end;

implementation

uses
  // RTL
  System.SysUtils;

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AFmt: string; const AParams: array of const);
begin
  Writeln(cLogTypeCaptions[ALogType] + ': ' + Format(AFmt, AParams));
end;

end.
