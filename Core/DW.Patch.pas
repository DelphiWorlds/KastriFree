unit DW.Patch;

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

{$IF Defined(ANDROID)}
// ***** NOTE: Refer to the warning in the DW.IdleHandler.Android unit
uses
  DW.IdleHandler.Android;
{$ENDIF}

end.
