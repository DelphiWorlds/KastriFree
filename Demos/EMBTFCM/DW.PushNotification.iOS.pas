unit DW.PushNotification.iOS;

interface

implementation

uses
  System.SysUtils, System.Classes, System.JSON, System.PushNotification, System.Messaging,
  Macapi.ObjectiveC, Macapi.Helpers,
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.Helpers,
  FMX.Platform,
  DW.iOSapi.Helpers, DW.iOSapi.Firebase, DW.iOSapi.UserNotifications;

type
  TFcmPushServiceNotification = class(TPushServiceNotification)
  private
    FRawData: TJSONObject;
  protected
    function GetDataKey: string; override;
    function GetJson: TJSONObject; override;
    function GetDataObject: TJSONObject; override;
  public
    constructor Create(const AJSON: string); overload;
  end;

  TFcmPushService = class;

  TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
  private
    FPushService: TFcmPushService;
    procedure ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
  public
    { FIRMessagingDelegate }
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didReceiveMessage:')]
    procedure didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didRefreshRegistrationToken:')]
    procedure didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
    [MethodName('messaging:didReceiveRegistrationToken:')]
    procedure didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
  public
    constructor Create(const APushService: TFcmPushService);
  end;

  TFcmPushService = class(TPushService)
  private
    FAuthOptions: UNAuthorizationOptions;
    FDeviceID: string;
    FDeviceToken: string;
    FFIRMessagingDelegate: TFIRMessagingDelegate;
    FMessaging: FIRMessaging;
    FStartupError: string;
    FStartupNotification: string;
    FStatus: TPushService.TStatus;
    procedure CheckNotificationsAuthorizationHandler(settings: UNNotificationSettings);
    function GetUserDefaultsTokenKey: NSString;
    procedure MessageReceived(const AJSON: string);
    function Messaging: FIRMessaging;
    procedure PushDeviceTokenMessageHandler(const Sender: TObject; const M: TMessage);
    procedure PushStartupNotificationMessageMessageHandler(const Sender: TObject; const M: TMessage);
    procedure Register;
    procedure RegisterRemoteNotificationsIOS10OrLater;
    procedure RegisterRemoteNotificationsIOS7OrEarlier;
    procedure RegisterRemoteNotificationsIOS8OrLater;
    procedure RequestAuthorization;
    procedure RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
    procedure Unregister;
    procedure DoAuthorizationResult(const AGranted: Boolean);
  protected
    constructor Create; reintroduce;
    function GetDeviceToken: TPushService.TPropArray; override;
    function GetDeviceID: TPushService.TPropArray; override;
    function GetStartupNotifications: TArray<TPushServiceNotification>; override;
    function GetStartupError: string; override;
    function GetStatus: TPushService.TStatus; override;
    procedure SetDeviceToken(const AToken: string);
    procedure StartService; override;
    procedure StopService; override;
  public
    destructor Destroy; override;
  end;

procedure RegisterPushServices;
begin
  TFcmPushService.Create;
end;

function IOSGetDeviceID: string;
var
  LDevice: UIDevice;
begin
  Result := '';
  LDevice := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
  if LDevice <> nil then
    Result := NSStrToStr(LDevice.identifierForVendor.UUIDString);
end;

{ TFcmPushServiceNotification }

constructor TFcmPushServiceNotification.Create(const AJSON: string);
begin
  FRawData := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
end;

function TFcmPushServiceNotification.GetDataKey: string;
begin
  Result := 'fcm';
end;

function TFcmPushServiceNotification.GetDataObject: TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := FRawData;
  if not GetDataKey.IsEmpty and (FRawData <> nil) then
  begin
    LValue := FRawData.Values[GetDataKey];
    if LValue <> nil then
      Result := LValue as TJSONObject;
  end;
end;

function TFcmPushServiceNotification.GetJson: TJSONObject;
begin
  Result := FRawData;
end;

{ TFIRMessagingDelegate }

constructor TFIRMessagingDelegate.Create(const APushService: TFcmPushService);
begin
  inherited Create;
  FPushService := APushService;
end;

procedure TFIRMessagingDelegate.applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage);
begin
  ReceivedMessage(remoteMessage);
end;

procedure TFIRMessagingDelegate.didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage);
begin
  ReceivedMessage(remoteMessage);
end;

procedure TFIRMessagingDelegate.didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
begin
  FPushService.SetDeviceToken(NSStrToStr(fcmToken));
end;

procedure TFIRMessagingDelegate.didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
begin
  FPushService.SetDeviceToken(NSStrToStr(fcmToken));
end;

procedure TFIRMessagingDelegate.ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
begin
  FPushService.MessageReceived(TiOSHelperEx.NSDictionaryToJSON(remoteMessage.appData));
end;

{ TFcmPushService }

constructor TFcmPushService.Create;
begin
  inherited Create(TPushServiceManager.Instance, TPushService.TServiceNames.GCM);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushStartupNotificationMessage, PushStartupNotificationMessageMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  FDeviceID := NSStrToStr(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).identifierForVendor.UUIDString);
  FDeviceToken := NSStrToStr(TiOSHelperEx.StandardUserDefaults.stringForKey(GetUserDefaultsTokenKey));
end;

destructor TFcmPushService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPushStartupNotificationMessage, PushStartupNotificationMessageMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  inherited;
end;

function TFcmPushService.GetUserDefaultsTokenKey: NSString;
begin
  Result := StrToNSStr(NSStrToStr(TiOSHelper.MainBundle.bundleIdentifier) + '.DeviceToken');
end;

procedure TFcmPushService.SetDeviceToken(const AToken: string);
begin
  FDeviceToken := AToken;
  TiOSHelperEx.StandardUserDefaults.setObject(NSObjectToID(StrToNSStr(FDeviceToken)), GetUserDefaultsTokenKey);
  DoChange([TPushService.TChange.Status, TPushService.TChange.DeviceToken]);
end;

procedure TFcmPushService.PushDeviceTokenMessageHandler(const Sender: TObject; const M: TMessage);
begin
  SetDeviceToken(TPushDeviceTokenMessage(M).Value.Token);
end;

procedure TFcmPushService.PushStartupNotificationMessageMessageHandler(const Sender: TObject; const M: TMessage);
begin
  FStartupNotification := TPushStartupNotificationMessage(M).Value.Notification;
  MessageReceived(FStartupNotification);
end;

procedure TFcmPushService.MessageReceived(const AJSON: string);
begin
  TMessageManager.DefaultManager.SendMessage(nil, TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(AJSON)));
end;

procedure TFcmPushService.Register;
begin
  TFIRApp.OCClass.configure;
  FFIRMessagingDelegate := TFIRMessagingDelegate.Create(Self);
  Messaging.setDelegate(FFIRMessagingDelegate.GetObjectID);
end;

procedure TFcmPushService.Unregister;
begin
  SetDeviceToken(string.Empty);
  Messaging.setDelegate(nil);
  FFIRMessagingDelegate := nil;
end;

procedure TFcmPushService.StartService;
begin
  Register;
  RequestAuthorization;
end;

procedure TFcmPushService.StopService;
begin
  if not FDeviceToken.IsEmpty then
  begin
    Unregister;
    FDeviceToken := string.Empty;
    FStatus := TPushService.TStatus.Stopped;
    FStartupError := string.Empty;
    FStatus := TStatus.Stopped;
    DoChange([TChange.Status]);
  end;
end;

function TFcmPushService.GetDeviceID: TPushService.TPropArray;
begin
  Result := TPushService.TPropArray.Create(TPushService.TPropPair.Create(TPushService.TDeviceIDNames.DeviceID, FDeviceID));
end;

function TFcmPushService.GetDeviceToken: TPushService.TPropArray;
begin
  Result := TPushService.TPropArray.Create(TPushService.TPropPair.Create(TPushService.TDeviceTokenNames.DeviceToken, FDeviceToken));
end;

function TFcmPushService.GetStartupError: string;
begin
  Result := FStartupError;
end;

function TFcmPushService.GetStartupNotifications: TArray<TPushServiceNotification>;
begin
  Result := TArray<TPushServiceNotification>.Create(TFcmPushServiceNotification.Create(FStartupNotification))
end;

function TFcmPushService.GetStatus: TPushService.TStatus;
begin
  Result := FStatus;
end;

function TFcmPushService.Messaging: FIRMessaging;
begin
  if FMessaging = nil then
    FMessaging := TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging);
  Result := FMessaging;
end;

procedure TFcmPushService.DoAuthorizationResult(const AGranted: Boolean);
begin
  //
end;

procedure TFcmPushService.RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin
  TThread.Queue(nil,
    procedure
    begin
      DoAuthorizationResult(granted);
    end
  );
end;

procedure TFcmPushService.RegisterRemoteNotificationsIOS10OrLater;
begin
  UserNotificationCenter.getNotificationSettingsWithCompletionHandler(CheckNotificationsAuthorizationHandler);
  if not TiOSHelperEx.SharedApplication.isRegisteredForRemoteNotifications then
    TiOSHelperEx.SharedApplication.registerForRemoteNotifications;
end;

procedure TFcmPushService.CheckNotificationsAuthorizationHandler(settings: UNNotificationSettings);
begin
  UserNotificationCenter.requestAuthorizationWithOptions(FAuthOptions, RequestAuthorizationWithOptionsCompletionHandler);
end;

procedure TFcmPushService.RegisterRemoteNotificationsIOS7OrEarlier;
begin
  TiOSHelper.SharedApplication.registerForRemoteNotificationTypes(Addr(FAuthOptions));
  DoAuthorizationResult(True);
end;

procedure TFcmPushService.RegisterRemoteNotificationsIOS8OrLater;
var
  LSettings: UIUserNotificationSettings;
begin
  LSettings := TUIUserNotificationSettings.Wrap(TUIUserNotificationSettings.OCClass.settingsForTypes(FAuthOptions, nil));
  TiOSHelper.SharedApplication.registerUserNotificationSettings(LSettings);
  if not TiOSHelperEx.SharedApplication.isRegisteredForRemoteNotifications then
    TiOSHelper.SharedApplication.registerForRemoteNotifications;
  DoAuthorizationResult(True);
end;

procedure TFcmPushService.RequestAuthorization;
begin
  FAuthOptions := UNAuthorizationOptionSound or UNAuthorizationOptionAlert or UNAuthorizationOptionBadge;
  if TOSVersion.Check(10) then
    RegisterRemoteNotificationsIOS10OrLater
  else if TOSVersion.Check(8) then
    RegisterRemoteNotificationsIOS8OrLater
  else
    RegisterRemoteNotificationsIOS7OrEarlier;
end;

initialization
  RegisterPushServices;

end.
