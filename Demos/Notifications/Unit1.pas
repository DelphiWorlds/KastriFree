unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  DW.Notifications;

type
  TForm1 = class(TForm)
    ImmediateButton: TButton;
    ScheduleButton: TButton;
    CancelScheduled: TButton;
    LogMemo: TMemo;
    ScheduleRepeatingButton: TButton;
    CancelImmediateButton: TButton;
    procedure ImmediateButtonClick(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure CancelScheduledClick(Sender: TObject);
    procedure ScheduleRepeatingButtonClick(Sender: TObject);
    procedure CancelImmediateButtonClick(Sender: TObject);
  private
    FNotifications: TNotifications;
    procedure ImmediateNotification;
    procedure NotificationReceivedHandler(Sender: TObject; const ANotification: TNotification);
    procedure ScheduleNotification(const ASeconds: Integer; const ARepeating: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FNotifications := DW.Notifications.TNotifications.Create;
  FNotifications.OnNotificationReceived := NotificationReceivedHandler;
end;

destructor TForm1.Destroy;
begin
  FNotifications.Free;
  inherited;
end;

procedure TForm1.ImmediateButtonClick(Sender: TObject);
begin
  ImmediateNotification;
end;

procedure TForm1.ImmediateNotification;
var
  LNotification: TNotification;
begin
  LNotification.EnableSound := False;
  LNotification.Name := 'ImmediateNotification';
  LNotification.Title := 'Immediate Notification';
  LNotification.Subtitle := 'Subtitles are cool';
  LNotification.AlertBody := 'This is an immediate notification';
  FNotifications.PresentNotification(LNotification);
end;

procedure TForm1.ScheduleButtonClick(Sender: TObject);
begin
  ScheduleNotification(30, False);
end;

procedure TForm1.ScheduleNotification(const ASeconds: Integer; const ARepeating: Boolean);
var
  LNotification: TNotification;
begin
  LNotification.Name := 'ScheduledNotification';
  LNotification.Title := 'I do not speak italian';
  LNotification.Subtitle := 'Io non parlo italiano';
  LNotification.EnableSound := False;
  LNotification.AlertBody := 'This notification was scheduled - so there';
  LNotification.FireDate := Now + EncodeTime(0, 0, ASeconds, 0);
  if ARepeating then
    LNotification.RepeatInterval := TRepeatInterval.Minute
  else
    LNotification.RepeatInterval := TRepeatInterval.None;
  FNotifications.ScheduleNotification(LNotification);
end;

procedure TForm1.ScheduleRepeatingButtonClick(Sender: TObject);
begin
  ScheduleNotification(10, True);
end;

procedure TForm1.CancelImmediateButtonClick(Sender: TObject);
begin
  FNotifications.CancelNotification('ImmediateNotification');
end;

procedure TForm1.CancelScheduledClick(Sender: TObject);
begin
  FNotifications.CancelNotification('ScheduledNotification');
end;

procedure TForm1.NotificationReceivedHandler(Sender: TObject; const ANotification: TNotification);
begin
  LogMemo.Lines.Add(ANotification.Name + ' received with text: ' + ANotification.AlertBody);
end;

end.
