unit DW.ThreadedTimer;

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
  System.Classes,
  // FMX
  FMX.Types;

type
  TThreadedTimer = class(TTimer)
  private
    FThread: TThread;
    procedure DoInterval;
  protected
    procedure CheckInterval;
    procedure KillTimer; override;
    procedure UpdateTimer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TTimerThread = class(TThread)
  private
    FTimer: TThreadedTimer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATimer: TThreadedTimer);
  end;

implementation

{ TThreadedTimer }

constructor TThreadedTimer.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := False;
  FThread := TTimerThread.Create(Self);
end;

destructor TThreadedTimer.Destroy;
begin
  Enabled := False;
  FThread.Free;
  inherited;
end;

procedure TThreadedTimer.CheckInterval;
begin
  if Enabled then
    DoInterval;
end;

procedure TThreadedTimer.DoInterval;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      DoOnTimer;
    end
  );
end;

procedure TThreadedTimer.KillTimer;
begin
  // No implementation of this method, as this timer does not use the platform timer service
end;

procedure TThreadedTimer.UpdateTimer;
begin
  // No implementation of this method, as this timer does not use the platform timer service
end;

{ TTimerThread }

constructor TTimerThread.Create(const ATimer: TThreadedTimer);
begin
  inherited Create;
  FTimer := ATimer;
end;

procedure TTimerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(FTimer.Interval);
    FTimer.CheckInterval;
  end;
end;

end.
