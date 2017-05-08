unit DW.Androidapi.JNI.LocalBroadcastManager;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  JLocalBroadcastManager = interface;

  JLocalBroadcastManagerClass = interface(JObjectClass)
    ['{5CCF81B2-E170-47C4-873E-32E644085880}']
    {class} function getInstance(context: JContext): JLocalBroadcastManager; cdecl;
  end;

  [JavaSignature('android/support/v4/content/LocalBroadcastManager')]
  JLocalBroadcastManager = interface(JObject)
    ['{B5D9B2DA-E150-4CC5-BBDA-58FCD42C6C1E}']
    procedure registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter); cdecl;
    function sendBroadcast(intent: JIntent): Boolean; cdecl;
    procedure sendBroadcastSync(intent: JIntent); cdecl;
    procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
  end;
  TJLocalBroadcastManager = class(TJavaGenericImport<JLocalBroadcastManagerClass, JLocalBroadcastManager>) end;

implementation

end.
