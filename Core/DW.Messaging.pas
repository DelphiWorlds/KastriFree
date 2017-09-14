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
  System.Messaging;

type
  TLaunchReason = (Unknown, Normal, LocationChange, LocalNotification, RemoteNotification);

  TOrientationDidChangeMessage = class(TMessage);
  TOrientationWillChangeMessage = class(TMessage);
  TApplicationLaunchMessage = class(TMessage<TLaunchReason>);

implementation

end.
