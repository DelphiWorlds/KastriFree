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
    class procedure Log(const ALogType: TLogType; const AFmt: string; const AParams: array of const); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Log;

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AFmt: string; const AParams: array of const);
var
  LMarshaller: TMarshaller;
  LPointer: Pointer;
begin
  LPointer := LMarshaller.AsUtf8(Format(AFmt, AParams)).ToPointer;
  case ALogType of
    TLogType.Debug:
      LOGI(LPointer);
    TLogType.Warning:
      LOGW(LPointer);
    TLogType.Error:
      LOGE(LPointer);
  end;
end;

end.

