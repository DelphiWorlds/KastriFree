unit DW.Androidapi.JNI.Debug;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.JavaUtil, Androidapi.JNI.Os;

type
  JDebug = interface;

  JDebugClass = interface(JObjectClass)
    ['{5759F2AF-54E3-44F4-839A-008BD6964FC5}']
    {class} function _GetSHOW_CLASSLOADER: Integer; cdecl;
    {class} function _GetSHOW_FULL_DETAIL: Integer; cdecl;
    {class} function _GetSHOW_INITIALIZED: Integer; cdecl;
    {class} function _GetTRACE_COUNT_ALLOCS: Integer; cdecl;
    {class} procedure changeDebugPort(port: Integer); cdecl;
    {class} procedure dumpHprofData(fileName: JString); cdecl;
    {class} function dumpService(name: JString; fd: JFileDescriptor; args: TJavaObjectArray<JString>): Boolean; cdecl;
    {class} procedure enableEmulatorTraceOutput; cdecl;
    {class} function getBinderDeathObjectCount: Integer; cdecl;
    {class} function getBinderLocalObjectCount: Integer; cdecl;
    {class} function getBinderProxyObjectCount: Integer; cdecl;
    {class} function getBinderReceivedTransactions: Integer; cdecl;
    {class} function getBinderSentTransactions: Integer; cdecl;
    {class} function getGlobalAllocCount: Integer; cdecl;
    {class} function getGlobalAllocSize: Integer; cdecl;
    {class} function getGlobalClassInitCount: Integer; cdecl;
    {class} function getGlobalClassInitTime: Integer; cdecl;
    {class} function getGlobalExternalAllocCount: Integer; cdecl;
    {class} function getGlobalExternalAllocSize: Integer; cdecl;
    {class} function getGlobalExternalFreedCount: Integer; cdecl;
    {class} function getGlobalExternalFreedSize: Integer; cdecl;
    {class} function getGlobalFreedCount: Integer; cdecl;
    {class} function getGlobalFreedSize: Integer; cdecl;
    {class} function getGlobalGcInvocationCount: Integer; cdecl;
    {class} function getLoadedClassCount: Integer; cdecl;
    {class} procedure getMemoryInfo(memoryInfo: JDebug_MemoryInfo); cdecl;
    {class} function getNativeHeapAllocatedSize: Int64; cdecl;
    {class} function getNativeHeapFreeSize: Int64; cdecl;
    {class} function getNativeHeapSize: Int64; cdecl;
    {class} function getPss: Int64; cdecl;
    {class} function getRuntimeStat(statName: JString): JString; cdecl;
    {class} function getRuntimeStats: JMap; cdecl;
    {class} function getThreadAllocCount: Integer; cdecl;
    {class} function getThreadAllocSize: Integer; cdecl;
    {class} function getThreadExternalAllocCount: Integer; cdecl;
    {class} function getThreadExternalAllocSize: Integer; cdecl;
    {class} function getThreadGcInvocationCount: Integer; cdecl;
    {class} function isDebuggerConnected: Boolean; cdecl;
    {class} procedure printLoadedClasses(flags: Integer); cdecl;
    {class} procedure resetAllCounts; cdecl;
    {class} procedure resetGlobalAllocCount; cdecl;
    {class} procedure resetGlobalAllocSize; cdecl;
    {class} procedure resetGlobalClassInitCount; cdecl;
    {class} procedure resetGlobalClassInitTime; cdecl;
    {class} procedure resetGlobalExternalAllocCount; cdecl;
    {class} procedure resetGlobalExternalAllocSize; cdecl;
    {class} procedure resetGlobalExternalFreedCount; cdecl;
    {class} procedure resetGlobalExternalFreedSize; cdecl;
    {class} procedure resetGlobalFreedCount; cdecl;
    {class} procedure resetGlobalFreedSize; cdecl;
    {class} procedure resetGlobalGcInvocationCount; cdecl;
    {class} procedure resetThreadAllocCount; cdecl;
    {class} procedure resetThreadAllocSize; cdecl;
    {class} procedure resetThreadExternalAllocCount; cdecl;
    {class} procedure resetThreadExternalAllocSize; cdecl;
    {class} procedure resetThreadGcInvocationCount; cdecl;
    {class} function setAllocationLimit(limit: Integer): Integer; cdecl;
    {class} function setGlobalAllocationLimit(limit: Integer): Integer; cdecl;
    {class} procedure startAllocCounting; cdecl;
    {class} procedure startMethodTracing; cdecl; overload;
    {class} procedure startMethodTracing(traceName: JString); cdecl; overload;
    {class} procedure startMethodTracing(traceName: JString; bufferSize: Integer); cdecl; overload;
    {class} procedure startMethodTracing(traceName: JString; bufferSize: Integer; flags: Integer); cdecl; overload;
    {class} procedure startMethodTracingSampling(traceName: JString; bufferSize: Integer; intervalUs: Integer); cdecl;
    {class} procedure startNativeTracing; cdecl;
    {class} procedure stopAllocCounting; cdecl;
    {class} procedure stopMethodTracing; cdecl;
    {class} procedure stopNativeTracing; cdecl;
    {class} function threadCpuTimeNanos: Int64; cdecl;
    {class} procedure waitForDebugger; cdecl;
    {class} function waitingForDebugger: Boolean; cdecl;
    {class} property SHOW_CLASSLOADER: Integer read _GetSHOW_CLASSLOADER;
    {class} property SHOW_FULL_DETAIL: Integer read _GetSHOW_FULL_DETAIL;
    {class} property SHOW_INITIALIZED: Integer read _GetSHOW_INITIALIZED;
    {class} property TRACE_COUNT_ALLOCS: Integer read _GetTRACE_COUNT_ALLOCS;
  end;

  [JavaSignature('android/os/Debug')]
  JDebug = interface(JObject)
    ['{EE4BF6EE-020D-49EF-94F2-03A4787C3D3C}']
  end;
  TJDebug = class(TJavaGenericImport<JDebugClass, JDebug>) end;

implementation

end.

