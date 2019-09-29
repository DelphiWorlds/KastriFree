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
  DW.Androidapi.JNI.App, DW.Androidapi.JNI.ContextWrapper, DW.Androidapi.JNI.DWMultiBroadcastReceiver, DW.Androidapi.JNI.Debug,
  DW.Androidapi.JNI.FileProvider, DW.Androidapi.JNI.KeyguardManager, DW.Androidapi.JNI.LocalBroadcastManager, DW.Androidapi.JNI.Log,
  DW.Androidapi.JNI.Nfc, DW.Androidapi.JNI.Os, DW.Androidapi.JNI.Print, DW.Androidapi.JNI.Runtime, DW.Androidapi.JNI.Support,
  DW.Androidapi.JNI.SystemClock, DW.Androidapi.JNI.Toast, DW.Androidapi.JNI.Usb, DW.Androidapi.JNI.VisionBarcode,
  {$IF CompilerVersion < 32}
  DW.Compat.Android,
  {$ENDIF}
  DW.Toast.Android;
{$ENDIF}

{$IF Defined(MACOS)}
uses
  DW.Macapi.ObjCRuntime, DW.Macapi.Helpers, DW.Macapi.Dispatch, DW.Macapi.ObjCBlocks,
{$IF Defined(MACDEV)}
  DW.Macapi.IOKit, DW.StatusBarMenu.Mac;
{$ENDIF}
{$IF Defined(IOS)}
  DW.iOSapi.CoreNFC, DW.iOSapi.DeviceCheck, DW.iOSapi.SystemConfiguration, DW.iOSapi.UserNotifications, DW.iOSapi.Helpers,
  DW.iOS.Sensors;
{$ENDIF}
{$ENDIF}

end.
