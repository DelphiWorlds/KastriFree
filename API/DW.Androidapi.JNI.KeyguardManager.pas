unit DW.Androidapi.JNI.KeyguardManager;

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
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes;

type
  JKeyguardManager = interface;
  JKeyguardManager_KeyguardLock = interface;
  JKeyguardManager_OnKeyguardExitResult = interface;

  JKeyguardManagerClass = interface(JObjectClass)
    ['{44F9A483-0683-4391-94D1-F0BB6DB7340B}']
  end;

  [JavaSignature('android/app/KeyguardManager')]
  JKeyguardManager = interface(JObject)
    ['{6AECC681-2C45-48B7-87D9-5A255840BDFA}']
    function createConfirmDeviceCredentialIntent(title: JCharSequence; description: JCharSequence): JIntent; cdecl;
    procedure exitKeyguardSecurely(callback: JKeyguardManager_OnKeyguardExitResult); cdecl;
    function inKeyguardRestrictedInputMode: Boolean; cdecl;
    function isDeviceLocked: Boolean; cdecl;
    function isDeviceSecure: Boolean; cdecl;
    function isKeyguardLocked: Boolean; cdecl;
    function isKeyguardSecure: Boolean; cdecl;
    function newKeyguardLock(tag: JString): JKeyguardManager_KeyguardLock; cdecl;
  end;
  TJKeyguardManager = class(TJavaGenericImport<JKeyguardManagerClass, JKeyguardManager>) end;

  JKeyguardManager_KeyguardLockClass = interface(JObjectClass)
    ['{98306533-81FC-4498-91E6-19F58EA32BD7}']
    {class} procedure disableKeyguard; cdecl;
    {class} procedure reenableKeyguard; cdecl;
  end;

  [JavaSignature('android/app/KeyguardManager$KeyguardLock')]
  JKeyguardManager_KeyguardLock = interface(JObject)
    ['{6061FAD3-A869-40A5-BEAB-CFC481B8EC0B}']
  end;
  TJKeyguardManager_KeyguardLock = class(TJavaGenericImport<JKeyguardManager_KeyguardLockClass, JKeyguardManager_KeyguardLock>) end;

  JKeyguardManager_OnKeyguardExitResultClass = interface(IJavaClass)
    ['{40CF0367-E275-475B-8566-A6337DD56217}']
  end;

  [JavaSignature('android/app/KeyguardManager$OnKeyguardExitResult')]
  JKeyguardManager_OnKeyguardExitResult = interface(IJavaInstance)
    ['{A2E86C9B-0384-4A3A-8E4B-823923FBD083}']
    procedure onKeyguardExitResult(success: Boolean); cdecl;
  end;
  TJKeyguardManager_OnKeyguardExitResult = class(TJavaGenericImport<JKeyguardManager_OnKeyguardExitResultClass, JKeyguardManager_OnKeyguardExitResult>) end;

implementation

end.

