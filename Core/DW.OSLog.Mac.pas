unit DW.OSLog.Mac;

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
  System.SysUtils,
  // Mac
  Macapi.Helpers, Macapi.ObjCRuntime;

const
  libFoundation = '/System/Library/Frameworks/Foundation.framework/Foundation';

type
  PNSString = Pointer;

procedure NSLog(format: PNSString); cdecl; varargs; external libFoundation name _PU + 'NSLog';

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AFmt: string; const AParams: array of const);
begin
  NSLog(StringToID(cLogTypeCaptions[ALogType] + ': ' + Format(AFmt, AParams)));
end;

end.
