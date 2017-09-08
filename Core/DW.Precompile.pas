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

// DW
{$IF Defined(ANDROID) and (CompilerVersion < 32)}
uses
  DW.Compat.Android;
{$ENDIF}

{$IF Defined(MACOS)}
uses
{$IF Defined(IOS)}
  DW.iOSapi.Helpers,
{$ENDIF}
{$IF Defined(MACDEV)}

{$ENDIF}
  DW.Macapi.Helpers, DW.Macapi.Dispatch, DW.Macapi.ObjCRuntime;
{$ENDIF}

end.
