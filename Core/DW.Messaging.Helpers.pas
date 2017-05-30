unit DW.Messaging.Helpers;

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
  // RTL
  System.Messaging;

type
  TMessageManagerHelper = class helper for TMessageManager
  public
    function Sub(const AMessageClass: TClass; const AListener: TMessageListener): Integer; overload;
    function Sub(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer; overload;
  end;

implementation

end.
