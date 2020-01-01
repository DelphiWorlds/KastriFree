unit DW.Precompile;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

implementation

// A cross-platform way of including units in a project for pre-compiling, specifically for the Kastri project source
// This unit should not be included in a regular application

{$IF Defined(ANDROID)}
uses
  DW.Androidapi.JNI.App, DW.Androidapi.JNI.ContextWrapper, DW.Androidapi.JNI.DWMultiBroadcastReceiver, DW.Androidapi.JNI.DWTimerTask,
  DW.Androidapi.JNI.DWWebChromeClient, DW.Androidapi.JNI.FileProvider, DW.Androidapi.JNI.Firebase, DW.Androidapi.JNI.FirebaseServiceHelpers,
  DW.Androidapi.JNI.KeyguardManager, DW.Androidapi.JNI.LocalBroadcastManager, DW.Androidapi.JNI.Log, DW.Androidapi.JNI.Nfc, DW.Androidapi.JNI.Os,
  DW.Androidapi.JNI.Print, DW.Androidapi.JNI.Runtime, DW.Androidapi.JNI.Support, DW.Androidapi.JNI.SystemClock, DW.Androidapi.JNI.Toast,
  DW.Androidapi.JNI.Usb, DW.Androidapi.JNI.VisionBarcode,
  {$IF CompilerVersion < 32}
  DW.Compat.Android,
  {$ENDIF}
  DW.Android.Helpers, DW.Android.Service, DW.Consts.Android, DW.IdleHandler.Android, DW.LocationReceiver.Android, DW.MultiReceiver.Android,
  DW.ServiceCommander.Android, DW.TimerTask.Android, DW.Toast.Android, DW.VirtualKeyboard.Android, DW.VirtualKeyboardRect.Android,
  DW.WebChromeClient.Android;
{$ENDIF}

{$IF Defined(MACOS)}
uses
  DW.Macapi.ObjCRuntime,
  DW.Macapi.Helpers, DW.Macapi.Dispatch, DW.Macapi.ObjCBlocks,
{$IF Defined(OSX32) or Defined(OSX64)}
  DW.Macapi.Foundation, DW.Macapi.IOKit, DW.StatusBarMenu.Mac;
{$ENDIF}
{$IF Defined(IOS)}
  DW.iOSapi.CoreNFC, DW.iOSapi.DeviceCheck, DW.iOSapi.FBAudienceNetwork, DW.iOSapi.Firebase, DW.iOSapi.Foundation, DW.iOSapi.SystemConfiguration,
  DW.iOSapi.UserNotifications,
  DW.iOSapi.Helpers, DW.iOS.Sensors, DW.Orientation.iOS;
{$ENDIF}
{$ENDIF}

{$IF Defined(MSWINDOWS)}
uses
  DW.FileVersionInfo.Win, DW.Swizzler.Win;
{$ENDIF}

end.
