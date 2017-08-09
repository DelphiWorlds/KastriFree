unit DW.IdleHandler.Android;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// ******************************** PLEASE NOTE ********************************
//
// This unit should be used ONLY in conjunction with the patch suggested here:
//   https://forums.embarcadero.com/thread.jspa?messageID=894946
//
// *****************************************************************************

interface

implementation

uses
  // RTL
  System.Classes,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.Os,
  // FMX
  FMX.Forms;

type
  TIdleHandler = class(TJavaLocal, JMessageQueue_IdleHandler)
  private
    class var FHandler: TIdleHandler;
  protected
    class procedure RegisterHandler;
    class procedure UnregisterHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function queueIdle: Boolean; cdecl;
  end;

{ TIdleHandler }

constructor TIdleHandler.Create;
begin
  inherited Create;
  TJLooper.JavaClass.myQueue.addIdleHandler(Self);
end;

destructor TIdleHandler.Destroy;
begin
  TJLooper.JavaClass.myQueue.removeIdleHandler(Self);
  inherited;
end;

class procedure TIdleHandler.RegisterHandler;
begin
  if FHandler = nil then
    FHandler := TIdleHandler.Create;
end;

class procedure TIdleHandler.UnregisterHandler;
begin
  FHandler.Free;
  FHandler := nil;
end;

function TIdleHandler.queueIdle: Boolean;
var
  LDone: Boolean;
begin
  Result := True;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    CheckSynchronize;
    if ApplicationState <> TApplicationState.Terminating then
    begin
      try
        LDone := False;
        Application.DoIdle(LDone);
      except
        Application.HandleException(Application);
      end;
    end;
  end;
end;

initialization
  TIdleHandler.RegisterHandler;

finalization
  TIdleHandler.UnregisterHandler;

end.
