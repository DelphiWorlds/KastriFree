unit LS.AndroidTimer;

interface

uses
  System.Classes,
  Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

type
  TAndroidTimer = class;

  TTimerRunnable = class(TJavaLocal, JRunnable)
  private
    FTimer: TAndroidTimer;
  public
    { JRunnable }
    procedure run; cdecl;
  public
    constructor Create(ATimer: TAndroidTimer);
  end;

  TAndroidTimer = class(TObject)
  private
    FEnabled: Boolean;
    FHandler: JHandler;
    FInterval: Integer;
    FRunnable: JRunnable;
    FOnTimer: TNotifyEvent;
    procedure DoTimer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
    procedure StartTimer;
    procedure StopTimer;
    procedure TimerEvent;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Integer read FInterval write SetInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

// Note:
// As per the documentation for postDelayed:
//
//   https://developer.android.com/reference/android/os/Handler.html#postDelayed(java.lang.Runnable, long)
//
// The runnable's run method is called in the same thread as where postDelayed was called from, so there should be no
// need to synchronize

{ TTimerRunnable }

constructor TTimerRunnable.Create(ATimer: TAndroidTimer);
begin
  inherited Create;
  FTimer := ATimer;
end;

procedure TTimerRunnable.run;
begin
  FTimer.TimerEvent;
end;

{ TAndroidTimer }

constructor TAndroidTimer.Create;
begin
  inherited;
  FInterval := 1000;
  FHandler := TJHandler.JavaClass.init;
  FRunnable := TTimerRunnable.Create(Self);
end;

destructor TAndroidTimer.Destroy;
begin
  FHandler := nil;
  FRunnable := nil;
  inherited;
end;

procedure TAndroidTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TAndroidTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
      StartTimer
    else
      StopTimer;
  end;
end;

procedure TAndroidTimer.SetInterval(const Value: Integer);
begin
  if Value <> Interval then
  begin
    if FEnabled then
      StopTimer;
    FInterval := Value;
    if FEnabled then
      StartTimer;
  end;
end;

procedure TAndroidTimer.StartTimer;
begin
  FHandler.postDelayed(FRunnable, Interval);
end;

procedure TAndroidTimer.StopTimer;
begin
  FHandler.removeCallbacks(FRunnable);
end;

procedure TAndroidTimer.TimerEvent;
begin
  DoTimer;
  StartTimer;
end;

end.
