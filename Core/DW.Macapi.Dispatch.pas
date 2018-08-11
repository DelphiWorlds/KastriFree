unit DW.Macapi.Dispatch;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// This is an extension of the dispatch functions
// See also: http://ridingdelphi.blogspot.com.au/2014/01/the-quest-to-migrate-ios-squarecam-app_3169.html

interface

uses
  System.SysUtils,
  Macapi.Dispatch;

type
  dispatch_work_t = reference to procedure;
  dispatch_block_t = Pointer; // dispatch_work_t;
  dispatch_function_t = procedure(context: Pointer); cdecl;

  TGrandCentral = record
  private
    class var FMainQueue: dispatch_queue_t;
    class function GetMainQueue: dispatch_queue_t; static;
  public
    class procedure DispatchAsync(const AProc: dispatch_work_t; const AQueue: dispatch_queue_t = 0); static;
  end;

implementation

const
{$IF Defined(IOS)}
  libdispatch = '/usr/lib/libSystem.dylib';
{$ELSE}
  libdispatch = '/usr/lib/system/libdispatch.dylib';
{$ENDIF}

procedure dispatch_sync_f(queue: dispatch_queue_t; context: Pointer; work: dispatch_function_t); cdecl;
  external libdispatch name _PU + 'dispatch_sync_f';

//procedure dispatch_async(queue: dispatch_queue_t; work: dispatch_work_t);
//procedure dispatch_sync(queue: dispatch_queue_t; work: dispatch_work_t);
//function dispatch_get_main_queue: dispatch_queue_t;

//implementation

//uses
//  System.SysUtils;

{ Grand Central Dispatch implementation }

function dispatch_get_main_queue: dispatch_queue_t;
var
  FwkMod: HMODULE;
begin
  Result := 0;
  FwkMod := LoadLibrary(PWideChar(libdispatch));
  if FwkMod <> 0 then
  begin
    Result := dispatch_queue_t(GetProcAddress(FwkMod, PWideChar('_dispatch_main_q')));
    FreeLibrary(FwkMod);
  end;
end;

procedure DispatchCallback(context: Pointer); cdecl;
var
  CallbackProc: dispatch_work_t absolute context;
begin
  try
    CallbackProc;
  finally
    IInterface(context)._Release;
  end;
end;

procedure dispatch_async(queue: dispatch_queue_t; work: dispatch_work_t);
var
  callback: Pointer absolute work;
begin
  IInterface(callback)._AddRef;
  dispatch_async_f(queue, callback, DispatchCallback);
end;

procedure dispatch_sync(queue: dispatch_queue_t; work: dispatch_work_t);
var
  callback: Pointer absolute work;
begin
  IInterface(callback)._AddRef;
  dispatch_sync_f(queue, callback, DispatchCallback);
end;

{ TDispatch }

class procedure TGrandCentral.DispatchAsync(const AProc: dispatch_work_t; const AQueue: dispatch_queue_t = 0);
begin
  if AQueue = 0 then
    dispatch_async(GetMainQueue, AProc)
  else
    dispatch_async(AQueue, AProc)
end;

class function TGrandCentral.GetMainQueue: dispatch_queue_t;
begin
  if FMainQueue = 0 then
    FMainQueue := dispatch_get_main_queue;
  Result := FMainQueue;
end;

end.
