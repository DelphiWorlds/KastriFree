{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{          FireDAC sources configuration file           }
{                                                       }
{ Copyright(c) 2004-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$TYPEDADDRESS OFF}
{$EXTENDEDSYNTAX ON}
{$ALIGN 8}
{$MINENUMSIZE 4}
{$ZEROBASEDSTRINGS OFF}

{$WARNINGS ON}
{$HINTS ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}

{$IFDEF BCB}
  {$DEFINE FireDAC_SkipIUnk}
  {$OBJEXPORTALL ON}
{$ENDIF}

{$IF DEFINED(CPUX64) or DEFINED(CPUARM64)}
  {$DEFINE FireDAC_64}
  {$UNDEF FireDAC_32}
{$ELSE}
  {$UNDEF FireDAC_64}
  {$DEFINE FireDAC_32}
{$ENDIF}

{$IF DEFINED(IOS) or DEFINED(ANDROID)}
  {$DEFINE FireDAC_Mobile}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Common                                                                       }
{------------------------------------------------------------------------------}
{.$DEFINE FireDAC_NOLOCALE_DATA}         // define, to use binary data comparision
{$DEFINE FireDAC_NOLOCALE_META}          // define, to use binary metadata comparision
{.$DEFINE FireDAC_DEBUG}                 // define, to enable profiling
{$DEFINE FireDAC_MONITOR}                // define, to enable FireDAC tracing
{$IFDEF FireDAC_MONITOR}
  {$DEFINE FireDAC_MONITOR_Comp}         // define, to enable FireDAC components tracing
{$ENDIF}
{.$DEFINE FireDAC_DesignTimeMenu}        // define, to enable FireDAC IDE menu

{------------------------------------------------------------------------------}
{ SynEdit                                                                      }
{------------------------------------------------------------------------------}
{$IFDEF WIN32}                           // SynEdit is supported only for Win32
  {$DEFINE FireDAC_SynEdit}
{$ENDIF}
{$UNDEF FireDAC_SynEdit}                 // undefine, to disable SynEdit usage

{------------------------------------------------------------------------------}
{ Oracle driver                                                                }
{------------------------------------------------------------------------------}
{.$DEFINE FireDAC_OCI_NLSParams}         // define to output V$NLS_PARAMETERS to trace
{.$DEFINE FireDAC_OCI_Versions}          // define to output V$VERSION to trace

{------------------------------------------------------------------------------}
{ SQLite driver                                                                }
{------------------------------------------------------------------------------}
{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS) and not DEFINED(IOS) or DEFINED(IOS) and (DEFINED(CPUARM) or DEFINED(CPUARM64)) or DEFINED(ANDROID)}
  {$DEFINE FireDAC_SQLITE_STATIC}        // Use SQLite3 static linking library
  {$IF DEFINED(MACOS) and not DEFINED(IOS) or DEFINED(IOS) and (DEFINED(CPUARM) or DEFINED(CPUARM64)) or DEFINED(ANDROID)}
    {$DEFINE FireDAC_SQLITE_EXTERNAL}    // Link SQLite3 library using "external" keyword
  {$ENDIF}
{$ENDIF}
{$UNDEF FireDAC_SQLITE_STATIC}          // remove '.' to enable dynamic linking

{------------------------------------------------------------------------------}
{ PostgreSQL driver                                                            }
{------------------------------------------------------------------------------}
{$UNDEF FireDAC_PGSQL_STATIC}            // is not yet implemented

{------------------------------------------------------------------------------}
{ ODBC driver                                                                  }
{------------------------------------------------------------------------------}
{$DEFINE FireDAC_ODBC_ALLMETA}           // define to link known DBMS metainfo's

{------------------------------------------------------------------------------}
{ IB and FB drivers                                                            }
{------------------------------------------------------------------------------}
{$IF DEFINED(IOS) and (DEFINED(CPUARM) or DEFINED(CPUARM64)) or DEFINED(ANDROID)}
  {$DEFINE FireDAC_IB_STATIC}            // Use IB driver static linking
  {$DEFINE FireDAC_IB_STATIC_IBBRAND}    // Use IbTogo static linking library
{$ENDIF}
