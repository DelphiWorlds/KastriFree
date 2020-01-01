unit DW.PushNotification;

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
  System.Classes, System.PushNotification, System.Json;

type
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;

  TPushNotifications = class(TObject)
  private
    FChannelTitle: string;
    FDeviceID: string;
    FDeviceToken: string;
    FServiceConnection: TPushServiceConnection;
    FShowBannerIfForeground: Boolean;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnNotificationReceived: TNotificationReceivedEvent;
    FOnTokenReceived: TNotifyEvent;
    procedure CheckStartupNotifications;
    procedure CreateChannel;
    procedure CreateConnection;
    function GetChannelId: string;
    function GetPushService: TPushService;
    procedure ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
    procedure ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
  public
    constructor Create(const AChannelTitle: string);
    destructor Destroy; override;
    procedure Start;
    property DeviceID: string read FDeviceID;
    property DeviceToken: string read FDeviceToken;
    property ShowBannerIfForeground: Boolean read FShowBannerIfForeground write FShowBannerIfForeground;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnNotificationReceived: TNotificationReceivedEvent read FOnNotificationReceived write FOnNotificationReceived;
    property OnTokenReceived: TNotifyEvent read FOnTokenReceived write FOnTokenReceived;
  end;

implementation

uses
  DW.OSLog,
  {$IF Defined(IOS)}
  DW.PushNotification.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  FMX.PushNotification.Android,
  DW.OSMetadata.Android,
  {$ENDIF}
  System.SysUtils, System.Notification,
  FMX.Platform;

{ TPushNotifications }

constructor TPushNotifications.Create(const AChannelTitle: string);
begin
  inherited Create;
  FChannelTitle := AChannelTitle;
end;

destructor TPushNotifications.Destroy;
begin
  FServiceConnection.Free;
  inherited;
end;

procedure TPushNotifications.CreateConnection;
var
  LPushService: TPushService;
begin
  LPushService := GetPushService;
  FServiceConnection := TPushServiceConnection.Create(LPushService);
  FServiceConnection.Active := True;
  FServiceConnection.OnChange := ServiceConnectionChangeHandler;
  FServiceConnection.OnReceiveNotification := ReceiveNotificationHandler;
  FDeviceId := LPushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
end;

procedure TPushNotifications.CreateChannel;
var
  LNotificationCenter: TNotificationCenter;
  LChannel: TChannel;
  LChannelId: string;
begin
  LChannelId := GetChannelId;
  if LChannelId.IsEmpty or FChannelTitle.IsEmpty then
    Exit; // <======
  LNotificationCenter := TNotificationCenter.Create(nil);
  try
    LChannel := TChannel.Create;
    try
      LChannel.Id := LChannelId;
      LChannel.Title := FChannelTitle;
      LChannel.Description := '';
      // Required for appearing as a banner when the app is not running
      LChannel.Importance := TImportance.High;
      LNotificationCenter.CreateOrUpdateChannel(LChannel);
    finally
      LChannel.Free;
    end;
  finally
    LNotificationCenter.Free;
  end;
end;

function TPushNotifications.GetChannelId: string;
{$IF Defined(ANDROID)}
begin
  TPlatformOSMetadata.GetValue('com.google.firebase.messaging.default_notification_channel_id', Result);
end;
{$ELSE}
begin
  Result := '';
end;
{$ENDIF}

procedure TPushNotifications.CheckStartupNotifications;
var
  LNotification: TPushServiceNotification;
begin
  // Handle startup notifications
  {$IF Defined(ANDROID)}
  for LNotification in FServiceConnection.Service.StartupNotifications do
    ReceiveNotificationHandler(FServiceConnection, LNotification);
  {$ENDIF}
end;

function TPushNotifications.GetPushService: TPushService;
begin
  Result := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.GCM);
end;

procedure TPushNotifications.ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
var
  LMessageText: string;
  LBodyValue: TJsonValue;
begin
  // An opportunity to handle the notification
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, AServiceNotification);
  if (AServiceNotification.Json <> nil) and Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AServiceNotification.Json);
  if AServiceNotification.DataObject <> nil then
    TOSLog.d('AServiceNotification.DataObject: %s', [AServiceNotification.DataObject.ToJSON])
  else
    TOSLog.d('AServiceNotification.DataObject is nil');
end;

procedure TPushNotifications.ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
begin
  if TPushService.TChange.DeviceToken in APushChanges then
  begin
    FDeviceToken := GetPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    if Assigned(FOnTokenReceived) then
      FOnTokenReceived(Self);
  end;
end;

procedure TPushNotifications.Start;
begin
  CreateChannel;
  CreateConnection;
  CheckStartupNotifications;
end;

end.
