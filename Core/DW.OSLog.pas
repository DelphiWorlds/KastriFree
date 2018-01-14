unit DW.OSLog;

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
  TLogType = (Debug, Warning, Error);

  /// <summary>
  ///   Operating System specific logging
  /// </summary>
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TOSLog = record
  private
    /// <summary>
    ///   Timestamps ASrc if prefixed with an '@'
    /// </summary>
    class function ts(const ASrc: string): string; static;
  public
    /// <summary>
    ///   Replacement functions for IFMXLoggingService
    /// </summary>
    class procedure d(const AFmt: string); overload; static;
    class procedure d(const AFmt: string; const AParams: array of const); overload; static;
    class procedure e(const AFmt: string); overload; static;
    class procedure e(const AFmt: string; const AParams: array of const); overload; static;
    class procedure w(const AFmt: string); overload; static;
    class procedure w(const AFmt: string; const AParams: array of const); overload; static;
    /// <summary>
    ///   Dumps a stack trace to the OS log. ANDROID ONLY at present
    /// </summary>
    class procedure Trace; static;
  end;

const
  cLogTypeCaptions: array[TLogType] of string = ('DEBUG', 'WARN', 'ERROR');

implementation

uses
  // RTL
  System.SysUtils,
  {$IF Defined(ANDROID)}
  DW.OSLog.Android;
  {$ELSEIF Defined(MACOS)}
  DW.OSLog.Mac;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.OSLog.Win;
  {$ELSEIF Defined(LINUX)}
  DW.OSLog.Linux;
  {$ENDIF}

{ TOSLog }

class function TOSLog.ts(const ASrc: string): string;
begin
  if ASrc.StartsWith('@') then
    Result := Format('%s - %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now), ASrc.Substring(1)])
  else
    Result := ASrc;
end;

class procedure TOSLog.d(const AFmt: string);
begin
  TPlatformOSLog.Log(TLogType.Debug, ts(AFmt), []);
end;

class procedure TOSLog.d(const AFmt: string; const AParams: array of const);
begin
  TPlatformOSLog.Log(TLogType.Debug, ts(AFmt), AParams);
end;

class procedure TOSLog.e(const AFmt: string);
begin
  TPlatformOSLog.Log(TLogType.Error, ts(AFmt), []);
end;

class procedure TOSLog.e(const AFmt: string; const AParams: array of const);
begin
  TPlatformOSLog.Log(TLogType.Error, ts(AFmt), AParams);
end;

class procedure TOSLog.w(const AFmt: string);
begin
  TPlatformOSLog.Log(TLogType.Warning, ts(AFmt), []);
end;

class procedure TOSLog.w(const AFmt: string; const AParams: array of const);
begin
  TPlatformOSLog.Log(TLogType.Warning, ts(AFmt), AParams);
end;

class procedure TOSLog.Trace;
begin
  {$IF Defined(ANDROID)}
  TPlatformOSLog.Trace;
  {$ENDIF}
end;

end.
