unit DW.Classes.Helpers;

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
  System.Classes;

type
  /// <summary>
  ///   Thread-based convenience methods.
  /// </summary>
  /// <remarks>
  ///   "TDo" as in "Do" something - shortest form of a word associated with running a task that I could think of
  /// </remarks>
  TDo = record
  public
    /// <summary>
    ///   Queues a method for execution after an optional delay
    /// </summary>
    class procedure Queue(const AProc: TThreadProcedure; const ADelay: Integer = 0); static;
    /// <summary>
    ///   Synchronizes a method for execution after an optional delay
    /// </summary>
    class procedure Sync(const AProc: TThreadProcedure; const ADelay: Integer = 0); static;
    /// <summary>
    ///   Runs a method in a thread and queues/syncs a callback if supplied
    /// </summary>
    class procedure RunQueue(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil); static;
    /// <summary>
    ///   Runs a method in a thread and queues/syncs a callback if supplied
    /// </summary>
    class procedure RunSync(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil); static;
  end;

  TStreamHelper = record
  public
    class function AsString(const AStream: TStream): string; static;
  end;

implementation

uses
  // RTL
  System.SysUtils;

{ TDo }

class procedure TDo.Queue(const AProc: TThreadProcedure; const ADelay: Integer = 0);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(ADelay);
      TThread.Queue(nil, AProc);
    end
  ).Start;
end;

class procedure TDo.Sync(const AProc: TThreadProcedure; const ADelay: Integer = 0);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(ADelay);
      TThread.Synchronize(nil, AProc);
    end
  ).Start;
end;

class procedure TDo.RunQueue(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      ARunProc;
      if Assigned(ACallbackProc) then
        TThread.Queue(nil, ACallbackProc);
    end
  ).Start;
end;

class procedure TDo.RunSync(const ARunProc: TThreadProcedure; const ACallbackProc: TThreadProcedure = nil);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      ARunProc;
      if Assigned(ACallbackProc) then
        TThread.Synchronize(nil, ACallbackProc);
    end
  ).Start;
end;

{ TStreamHelper }

class function TStreamHelper.AsString(const AStream: TStream): string;
var
  LStream: TStringStream;
begin
  AStream.Position := 0;
  LStream := TStringStream.Create;
  try
    LStream.CopyFrom(AStream, AStream.Size);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

end.
