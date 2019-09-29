unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.PushNotification,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TfrmMain = class(TForm)
    MemoLog: TMemo;
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
begin
  inherited;
  PushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);
  ServiceConnection := TPushServiceConnection.Create(PushService);
  ServiceConnection.Active := True;
  ServiceConnection.OnChange := OnServiceConnectionChange;
  ServiceConnection.OnReceiveNotification := OnReceiveNotificationEvent;

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
  MemoLog.Lines.Add('Json = ' + ServiceNotification.Json.ToString);
  MemoLog.Lines.Add('DataObject = ' + ServiceNotification.DataObject.ToString);
  // MessageText := ServiceNotification.DataObject.GetValue('gcm.notification.body').Value;
  LValue := ServiceNotification.DataObject.GetValue('gcm.notification.body');
  if LValue = nil then
    MemoLog.Lines.Add('LValue is nil')
  else
    MessageText := LValue.Value;
  MemoLog.Lines.Add(DateTimeToStr(Now) + ' Message = ' + MessageText);
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

end.
