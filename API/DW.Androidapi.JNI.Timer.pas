unit DW.Androidapi.JNI.Timer;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JTimerTask = interface;
  JTimer = interface;

  JTimerClass = interface(JObjectClass)
    ['{07C8270D-52FF-4B70-B364-A4E86A4F3411}']
    function init: JTimer; cdecl; overload;
    function init(name: JString): JTimer; cdecl; overload;
    function init(name: JString; isDaemon: boolean): JTimer; cdecl; overload;
    function init(isDaemon: boolean): JTimer; cdecl; overload;
  end;

  [JavaSignature('java/util/Timer')]
  JTimer = interface(JObject)
    ['{25D25103-F3A3-417F-AE53-7B434258D54D}']
    procedure cancel; cdecl;
    function purge: Integer; cdecl;
    procedure schedule(task: JTimerTask; delay: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; delay: Int64; period: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; firstTime: JDate; period: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; time: JDate); cdecl; overload;
    procedure scheduleAtFixedRate(task: JTimerTask; delay: Int64; period: Int64); cdecl; overload;
    procedure scheduleAtFixedRate(task: JTimerTask; firstTime: JDate; period: Int64); cdecl; overload;
  end;
  TJTimer = class(TJavaGenericImport<JTimerClass, JTimer>)
  end;

  JTimerTaskClass = interface(JObjectClass)
    ['{8A91DFA8-92B7-49CE-88DB-931B6D4D679F}']
  end;

  [JavaSignature('java/util/TimerTask')]
  JTimerTask = interface(JObject)
    ['{E5CEEEE4-88C0-4488-9E7C-86249258C30E}']
    function cancel: boolean; cdecl;
    procedure run; cdecl;
    function scheduledExecutionTime: Int64; cdecl;
  end;
  TJTimerTask = class(TJavaGenericImport<JTimerTaskClass, JTimerTask>)
  end;

implementation

end.
