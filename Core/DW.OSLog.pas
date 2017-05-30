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
  end;

const
  cLogTypeCaptions: array[TLogType] of string = ('DEBUG', 'WARN', 'ERROR');

implementation

uses
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

class procedure TOSLog.d(const AFmt: string);
begin
  TPlatformOSLog.Log(TLogType.Debug, AFmt, []);
end;

class procedure TOSLog.d(const AFmt: string; const AParams: array of const);
begin
  TPlatformOSLog.Log(TLogType.Debug, AFmt, AParams);
end;

class procedure TOSLog.e(const AFmt: string);
begin
  TPlatformOSLog.Log(TLogType.Error, AFmt, []);
end;

class procedure TOSLog.e(const AFmt: string; const AParams: array of const);
begin
  TPlatformOSLog.Log(TLogType.Error, AFmt, AParams);
end;

class procedure TOSLog.w(const AFmt: string);
begin
  TPlatformOSLog.Log(TLogType.Warning, AFmt, []);
end;

class procedure TOSLog.w(const AFmt: string; const AParams: array of const);
begin
  TPlatformOSLog.Log(TLogType.Warning, AFmt, AParams);
end;

end.
