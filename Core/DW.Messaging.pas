unit DW.Messaging;

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
  System.Messaging, System.Types;

type
  TLaunchReason = (Unknown, Normal, LocationChange, LocalNotification, RemoteNotification);

  TOrientationDidChangeMessage = class(TMessage);
  TOrientationWillChangeMessage = class(TMessage);
  TApplicationLaunchMessage = class(TMessage<TLaunchReason>);
  TVirtualKeyboardRectChangeMessage = class(TMessage<TRect>);

implementation

end.
