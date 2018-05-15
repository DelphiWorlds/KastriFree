unit DW.OSLog.Android;

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
    class procedure Log(const ALogType: TLogType; const AMsg: string); static;
    class procedure Trace; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Log, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.Log;

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AMsg: string);
var
  LMarshaller: TMarshaller;
  LPointer: Pointer;
begin
  LPointer := LMarshaller.AsUtf8(AMsg).ToPointer;
  case ALogType of
    TLogType.Debug:
      LOGI(LPointer);
    TLogType.Warning:
      LOGW(LPointer);
    TLogType.Error:
      LOGE(LPointer);
  end;
end;

class procedure TPlatformOSLog.Trace;
var
  LTrace: JString;
begin
  LTrace := TJutil_Log.JavaClass.getStackTraceString(TJException.JavaClass.init);
  Log(TLogType.Debug, JStringToString(LTrace));
end;

end.

