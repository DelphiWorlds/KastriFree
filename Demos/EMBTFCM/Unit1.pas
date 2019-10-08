unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.PushNotification,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts,
  System.Notification;

type
  TfrmMain = class(TForm)
    MemoLog: TMemo;
    ButtonsLayout: TLayout;
    LocalButton: TButton;
    NotificationCenter: TNotificationCenter;
    ClearButton: TButton;
    procedure LocalButtonClick(Sender: TObject);
    procedure NotificationCenterReceiveLocalNotification(Sender: TObject; ANotification: TNotification);
    procedure ClearButtonClick(Sender: TObject);
  private
    FDeviceId: string;
    FDeviceToken: string;
    procedure OnReceiveNotificationEvent(Sender: TObject; const ServiceNotification: TPushServiceNotification);
    procedure OnServiceConnectionChange(Sender: TObject; PushChanges: TPushService.TChanges);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  {$IF Defined(ANDROID)}
  FMX.PushNotification.Android,
  {$ELSEIF Defined(IOS)}
  DW.PushNotification.iOS,
  {$ENDIF}
  System.Json;

constructor TfrmMain.Create(AOwner: TComponent);
var
  PushService: TPushService;
  ServiceConnection: TPushServiceConnection;
  LNotification: TPushServiceNotification;
begin
  inherited;
  PushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);
  ServiceConnection := TPushServiceConnection.Create(PushService);
  ServiceConnection.Active := True;
  ServiceConnection.OnChange := OnServiceConnectionChange;
  ServiceConnection.OnReceiveNotification := OnReceiveNotificationEvent;
  // Handle startup notifications
  for LNotification in ServiceConnection.Service.StartupNotifications do
    OnReceiveNotificationEvent(ServiceConnection, LNotification);
  FDeviceId := PushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
  MemoLog.Lines.Add('DeviceID: ' + FDeviceId);
  MemoLog.Lines.Add('Ready to receive!');
end;

procedure TfrmMain.OnReceiveNotificationEvent(Sender: TObject; const ServiceNotification: TPushServiceNotification);
var
  MessageText: string;
  LValue: TJsonValue;
begin
  MemoLog.Lines.Add('DataKey = ' + ServiceNotification.DataKey);
  if ServiceNotification.Json <> nil then
    MemoLog.Lines.Add('Json = ' + ServiceNotification.Json.ToString);
  if ServiceNotification.DataObject <> nil then
  begin
    MemoLog.Lines.Add('DataObject = ' + ServiceNotification.DataObject.ToString);
    // MessageText := ServiceNotification.DataObject.GetValue('gcm.notification.body').Value;
    LValue := ServiceNotification.DataObject.GetValue('gcm.notification.body');
    if LValue = nil then
      MemoLog.Lines.Add('LValue is nil')
    else
      MessageText := LValue.Value;
    MemoLog.Lines.Add(DateTimeToStr(Now) + ' Message = ' + MessageText);
  end;
end;

procedure TfrmMain.OnServiceConnectionChange(Sender: TObject; PushChanges: TPushService.TChanges);
var
  PushService: TPushService;
begin
  if TPushService.TChange.DeviceToken in PushChanges then
  begin
    PushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);
    FDeviceToken := PushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    MemoLog.Lines.Add('FireBase Token: ' + FDeviceToken);
    Log.d('Firebase device token: token=' + FDeviceToken);
  end;
end;

procedure TfrmMain.ClearButtonClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
end;

procedure TfrmMain.LocalButtonClick(Sender: TObject);
var
  LNotification: TNotification;
begin
  LNotification := TNotification.Create;
  try
    LNotification.Name := 'ScheduledNotification';
    LNotification.Title := 'Io non parlo italiano';
    LNotification.EnableSound := False;
    LNotification.AlertBody := 'This notification was scheduled - so there';
    LNotification.FireDate := Now + EncodeTime(0, 0, 15, 0);
    NotificationCenter.ScheduleNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

procedure TfrmMain.NotificationCenterReceiveLocalNotification(Sender: TObject; ANotification: TNotification);
begin
  MemoLog.Lines.Add('Received local notification:');
  MemoLog.Lines.Add('Name:' + ANotification.Name);
  MemoLog.Lines.Add('Title:' + ANotification.Title);
  MemoLog.Lines.Add('Body:' + ANotification.AlertBody);
end;

end.
