unit DW.Androidapi.JNI.SystemClock;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JSystemClock = interface;

  JSystemClockClass = interface(JObjectClass)
    ['{771C5E34-6252-4BA7-8292-DD6BC82AA9B8}']
    {class} function currentThreadTimeMillis: Int64; cdecl;
    {class} function elapsedRealtime: Int64; cdecl;
    {class} function elapsedRealtimeNanos: Int64; cdecl;
    {class} function setCurrentTimeMillis(millis: Int64): Boolean; cdecl;
    {class} procedure sleep(ms: Int64); cdecl;
    {class} function uptimeMillis: Int64; cdecl;
  end;

  [JavaSignature('android/os/SystemClock')]
  JSystemClock = interface(JObject)
    ['{6F88CF0F-2D6B-43D4-A23D-A04C1C56D88E}']
  end;
  TJSystemClock = class(TJavaGenericImport<JSystemClockClass, JSystemClock>) end;

implementation

end.

