unit DW.Androidapi.JNI.Os;

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
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNIBridge;

type
  JHandlerThread = interface;
  JEnvironment = interface;
  JStatFs = interface;
  JAsyncTask = interface;
  JAsyncTask_Status = interface;

  JHandlerThreadClass = interface(JThreadClass)
    ['{FAAD33B5-6400-4F38-B3F9-EE8C85500F15}']
    {class} function init(name: JString): JHandlerThread; cdecl; overload;
    {class} function init(name: JString; priority: Integer): JHandlerThread; cdecl; overload;
  end;

  [JavaSignature('android/os/HandlerThread')]
  JHandlerThread = interface(JThread)
    ['{BCBA93F8-F723-4299-97FA-DFD17D08135E}']
    function getLooper: JLooper; cdecl;
    function getThreadId: Integer; cdecl;
    function quit: Boolean; cdecl;
    function quitSafely: Boolean; cdecl;
    procedure run;
  end;
  TJHandlerThread = class(TJavaGenericImport<JHandlerThreadClass, JHandlerThread>) end;

  JEnvironmentClass = interface(JObjectClass)
    ['{847171A2-7B65-4251-9BD3-E0BC89DE31FD}']
    {class} function _GetDIRECTORY_ALARMS: JString; cdecl;
    {class} procedure _SetDIRECTORY_ALARMS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_DCIM: JString; cdecl;
    {class} function _GetDIRECTORY_DOCUMENTS: JString; cdecl;
    {class} function _GetDIRECTORY_DOWNLOADS: JString; cdecl;
    {class} procedure _SetDIRECTORY_DOWNLOADS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_MOVIES: JString; cdecl;
    {class} function _GetDIRECTORY_MUSIC: JString; cdecl;
    {class} procedure _SetDIRECTORY_MUSIC(Value: JString); cdecl;
    {class} function _GetDIRECTORY_NOTIFICATIONS: JString; cdecl;
    {class} procedure _SetDIRECTORY_NOTIFICATIONS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_PICTURES: JString; cdecl;
    {class} function _GetDIRECTORY_PODCASTS: JString; cdecl;
    {class} procedure _SetDIRECTORY_PODCASTS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_RINGTONES: JString; cdecl;
    {class} function _GetMEDIA_BAD_REMOVAL: JString; cdecl;
    {class} function _GetMEDIA_CHECKING: JString; cdecl;
    {class} function _GetMEDIA_EJECTING: JString; cdecl;
    {class} function _GetMEDIA_MOUNTED: JString; cdecl;
    {class} function _GetMEDIA_MOUNTED_READ_ONLY: JString; cdecl;
    {class} function _GetMEDIA_NOFS: JString; cdecl;
    {class} function _GetMEDIA_REMOVED: JString; cdecl;
    {class} function _GetMEDIA_SHARED: JString; cdecl;
    {class} function _GetMEDIA_UNKNOWN: JString; cdecl;
    {class} function _GetMEDIA_UNMOUNTABLE: JString; cdecl;
    {class} function _GetMEDIA_UNMOUNTED: JString; cdecl;
    {class} function init: JEnvironment; cdecl;
    {class} function getDataDirectory: JFile; cdecl;
    {class} function getDownloadCacheDirectory: JFile; cdecl;
    {class} function getExternalStorageDirectory: JFile; cdecl;
    {class} function getExternalStoragePublicDirectory(&type: JString): JFile; cdecl;
    {class} function getExternalStorageState: JString; cdecl; overload;
    {class} function getExternalStorageState(path: JFile): JString; cdecl; overload;
    {class} function getRootDirectory: JFile; cdecl;
    {class} function getStorageState(path: JFile): JString; cdecl;
    {class} function isExternalStorageEmulated: Boolean; cdecl; overload;
    {class} function isExternalStorageEmulated(path: JFile): Boolean; cdecl; overload;
    {class} function isExternalStorageRemovable: Boolean; cdecl; overload;
    {class} function isExternalStorageRemovable(path: JFile): Boolean; cdecl; overload;
    {class} property DIRECTORY_ALARMS: JString read _GetDIRECTORY_ALARMS write _SetDIRECTORY_ALARMS;
    {class} property DIRECTORY_DCIM: JString read _GetDIRECTORY_DCIM;
    {class} property DIRECTORY_DOCUMENTS: JString read _GetDIRECTORY_DOCUMENTS;
    {class} property DIRECTORY_DOWNLOADS: JString read _GetDIRECTORY_DOWNLOADS write _SetDIRECTORY_DOWNLOADS;
    {class} property DIRECTORY_MOVIES: JString read _GetDIRECTORY_MOVIES;
    {class} property DIRECTORY_MUSIC: JString read _GetDIRECTORY_MUSIC write _SetDIRECTORY_MUSIC;
    {class} property DIRECTORY_NOTIFICATIONS: JString read _GetDIRECTORY_NOTIFICATIONS write _SetDIRECTORY_NOTIFICATIONS;
    {class} property DIRECTORY_PICTURES: JString read _GetDIRECTORY_PICTURES;
    {class} property DIRECTORY_PODCASTS: JString read _GetDIRECTORY_PODCASTS write _SetDIRECTORY_PODCASTS;
    {class} property DIRECTORY_RINGTONES: JString read _GetDIRECTORY_RINGTONES;
    {class} property MEDIA_BAD_REMOVAL: JString read _GetMEDIA_BAD_REMOVAL;
    {class} property MEDIA_CHECKING: JString read _GetMEDIA_CHECKING;
    {class} property MEDIA_EJECTING: JString read _GetMEDIA_EJECTING;
    {class} property MEDIA_MOUNTED: JString read _GetMEDIA_MOUNTED;
    {class} property MEDIA_MOUNTED_READ_ONLY: JString read _GetMEDIA_MOUNTED_READ_ONLY;
    {class} property MEDIA_NOFS: JString read _GetMEDIA_NOFS;
    {class} property MEDIA_REMOVED: JString read _GetMEDIA_REMOVED;
    {class} property MEDIA_SHARED: JString read _GetMEDIA_SHARED;
    {class} property MEDIA_UNKNOWN: JString read _GetMEDIA_UNKNOWN;
    {class} property MEDIA_UNMOUNTABLE: JString read _GetMEDIA_UNMOUNTABLE;
    {class} property MEDIA_UNMOUNTED: JString read _GetMEDIA_UNMOUNTED;
  end;

  [JavaSignature('android/os/Environment')]
  JEnvironment = interface(JObject)
    ['{8A8591BC-BC01-4338-91D8-2671DAB231F8}']
  end;
  TJEnvironment = class(TJavaGenericImport<JEnvironmentClass, JEnvironment>) end;

  JStatFsClass = interface(JObjectClass)
    ['{F97A99DF-CDC1-4842-80F2-2EA53A906E3E}']
    {class} function init(path: JString): JStatFs; cdecl;
  end;

  [JavaSignature('android/os/StatFs')]
  JStatFs = interface(JObject)
    ['{C34856EE-443F-42CB-B25B-DEC0B8C938D0}']
    function getAvailableBlocks: Integer; cdecl;
    function getAvailableBlocksLong: Int64; cdecl;
    function getAvailableBytes: Int64; cdecl;
    function getBlockCount: Integer; cdecl;
    function getBlockCountLong: Int64; cdecl;
    function getBlockSize: Integer; cdecl;
    function getBlockSizeLong: Int64; cdecl;
    function getFreeBlocks: Integer; cdecl;
    function getFreeBlocksLong: Int64; cdecl;
    function getFreeBytes: Int64; cdecl;
    function getTotalBytes: Int64; cdecl;
    procedure restat(path: JString); cdecl;
  end;
  TJStatFs = class(TJavaGenericImport<JStatFsClass, JStatFs>) end;

  JAsyncTask_StatusClass = interface(JEnumClass)
    ['{9FCA442E-277F-4601-8D8E-241B24F15615}']
    {class} function _GetFINISHED: JAsyncTask_Status; cdecl;
    {class} function _GetPENDING: JAsyncTask_Status; cdecl;
    {class} function _GetRUNNING: JAsyncTask_Status; cdecl;
    {class} function valueOf(name: JString): JAsyncTask_Status; cdecl;
    {class} function values: TJavaObjectArray<JAsyncTask_Status>; cdecl;
    {class} property FINISHED: JAsyncTask_Status read _GetFINISHED;
    {class} property PENDING: JAsyncTask_Status read _GetPENDING;
    {class} property RUNNING: JAsyncTask_Status read _GetRUNNING;
  end;

  [JavaSignature('android/os/AsyncTask$Status')]
  JAsyncTask_Status = interface(JEnum)
    ['{DA6490CB-4B27-4CAF-9961-D9129CD65CE9}']
  end;
  TJAsyncTask_Status = class(TJavaGenericImport<JAsyncTask_StatusClass, JAsyncTask_Status>) end;

  JAsyncTaskClass = interface(JObjectClass)
    ['{C42EC518-9EB8-4F81-943F-8B699D7E81A3}']
    {class} function _GetSERIAL_EXECUTOR: JExecutor; cdecl;
    {class} function _GetTHREAD_POOL_EXECUTOR: JExecutor; cdecl;
    {class} function init: JAsyncTask; cdecl;
    {class} property SERIAL_EXECUTOR: JExecutor read _GetSERIAL_EXECUTOR;
    {class} property THREAD_POOL_EXECUTOR: JExecutor read _GetTHREAD_POOL_EXECUTOR;
  end;

  [JavaSignature('android/os/AsyncTask')]
  JAsyncTask = interface(JObject)
    ['{A87FEC05-8A98-4957-BABD-D11BA361F045}']
    function cancel(mayInterruptIfRunning: Boolean): Boolean; cdecl;
    procedure execute(runnable: JRunnable); cdecl; overload;
    function get: JObject; cdecl; overload;
    function get(timeout: Int64; &unit: JTimeUnit): JObject; cdecl; overload;
    function getStatus: JAsyncTask_Status; cdecl;
    function isCancelled: Boolean; cdecl;
  end;
  TJAsyncTask = class(TJavaGenericImport<JAsyncTaskClass, JAsyncTask>) end;

implementation

end.
