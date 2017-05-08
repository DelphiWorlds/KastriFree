{*******************************************************}
{                                                       }
{         DelphiWorlds Delphi Global Library            }
{                                                       }
{          Copyright(c) 2016 David Nottage              }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

// This include file contains defines that are application-wide for DelphiWorlds code

// All DelphiWorlds code makes use of scoped enumerations
{$SCOPEDENUMS ON}

// Special define to indicate the code is being compiled for an actual Mac device
{$IF Defined(MACOS) and not Defined(IOS)}
  {$DEFINE MACDEV}
{$ENDIF}

// Special defines to indicate the code is being compiled for an actual iOS device, or for simulator
{$IF Defined(IOS)}
  {$IF Defined(CPUARM)}
    {$DEFINE IOSDEV}
  {$ELSE}
    {$DEFINE IOSSIM}
  {$ENDIF}
{$ENDIF}

