unit DW.Firebase.Messaging.iOS;

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
  System.TypInfo, System.Messaging,
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.Firebase.Messaging, DW.iOSapi.UserNotifications, DW.iOSapi.Firebase;

type
  TPlatformFirebaseMessaging = class;

  TUserNotificationCenterDelegate = class(TOCLocal, UNUserNotificationCenterDelegate)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenterWillPresentNotificationWithCompletionHandler(center: UNUserNotificationCenter;
      willPresentNotification: UNNotification; withCompletionHandler: Pointer); cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler(center: UNUserNotificationCenter;
      didReceiveNotificationResponse: UNNotificationResponse; withCompletionHandler: Pointer); cdecl;
  end;

  TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;
  end;

  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  private
    FFIRMessagingDelegate: TFIRMessagingDelegate;
    FUserNotificationCenterDelegate: TUserNotificationCenterDelegate;
    function Messaging: FIRMessaging;
    procedure FIRMessagingConnectCompletionHandler(error: NSError);
    procedure RegisterRemoteNotificationsIOS10OrLater(const AOptions: UNAuthorizationOptions);
    procedure RegisterRemoteNotificationsIOS8OrLater(const AOptions: UNAuthorizationOptions);
    procedure RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure SubscribeToTopic(const ATopicName: string); override;
    procedure UnsubscribeFromTopic(const ATopicName: string); override;
  public
    constructor Create(const AFirebaseMessaging: TFirebaseMessaging); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes,
  // Mac
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Helpers, iOSapi.UIKit,
  // FMX
  FMX.Platform,
  // DW
  DW.Macapi.ObjCRuntime, DW.iOSapi.Helpers;

{ TUserNotificationCenterDelegate }

constructor TUserNotificationCenterDelegate.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TUserNotificationCenterDelegate.userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler(center: UNUserNotificationCenter;
  didReceiveNotificationResponse: UNNotificationResponse; withCompletionHandler: Pointer);
var
  LBlockImp: procedure; cdecl;
  LJSON: string;
begin
  LJSON := NSDictionaryToJSON(didReceiveNotificationResponse.notification.request.content.userInfo);
  TMessageManager.DefaultManager.SendMessage(nil, TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJSON)));
  @LBlockImp := imp_implementationWithBlock(withCompletionHandler);
  LBlockImp;
  imp_removeBlock(@LBlockImp);
end;

procedure TUserNotificationCenterDelegate.userNotificationCenterWillPresentNotificationWithCompletionHandler(center: UNUserNotificationCenter;
  willPresentNotification: UNNotification; withCompletionHandler: Pointer);
var
  LBlockImp: procedure(options: UNNotificationPresentationOptions); cdecl;
  LOptions: UNNotificationPresentationOptions;
  LJSON: string;
begin
  LJSON := NSDictionaryToJSON(willPresentNotification.request.content.userInfo);
  TMessageManager.DefaultManager.SendMessage(nil, TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJSON)));
  @LBlockImp := imp_implementationWithBlock(withCompletionHandler);
  LOptions := UNNotificationPresentationOptionAlert;
  LBlockImp(LOptions);
  imp_removeBlock(@LBlockImp);
end;

{ TFIRMessagingDelegate }

constructor TFIRMessagingDelegate.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TFIRMessagingDelegate.applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage);
var
  LJSON: string;
begin
  LJSON := NSDictionaryToJSON(remoteMessage.appData);
  TMessageManager.DefaultManager.SendMessage(nil, TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJSON)));
end;

{ TPlatformFirebaseMessaging }

constructor TPlatformFirebaseMessaging.Create(const AFirebaseMessaging: TFirebaseMessaging);
var
  LOptions: UNAuthorizationOptions;
begin
  inherited;
  LOptions := UNAuthorizationOptionSound or UNAuthorizationOptionAlert or UNAuthorizationOptionBadge;
  if TOSVersion.Check(10) then
    RegisterRemoteNotificationsIOS10OrLater(LOptions)
  else if TOSVersion.Check(8) then
    RegisterRemoteNotificationsIOS8OrLater(LOptions)
  else
    TiOSHelper.SharedApplication.registerForRemoteNotificationTypes(Addr(LOptions));
end;

destructor TPlatformFirebaseMessaging.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformFirebaseMessaging.Connect;
begin
  Disconnect;
  Messaging.connectWithCompletion(FIRMessagingConnectCompletionHandler);
end;

procedure TPlatformFirebaseMessaging.Disconnect;
begin
  Messaging.disconnect;
  IsConnected := False;
end;

procedure TPlatformFirebaseMessaging.FIRMessagingConnectCompletionHandler(error: NSError);
begin
  IsConnected := True;
end;

procedure TPlatformFirebaseMessaging.RegisterRemoteNotificationsIOS10OrLater(const AOptions: UNAuthorizationOptions);
begin
  FUserNotificationCenterDelegate := TUserNotificationCenterDelegate.Create(self);
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.setdelegate(FUserNotificationCenterDelegate.GetObjectID);
  UserNotificationCenter.requestAuthorizationWithOptions(AOptions, RequestAuthorizationWithOptionsCompletionHandler);
  FFIRMessagingDelegate := TFIRMessagingDelegate.Create(self);
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).setRemoteMessageDelegate(FFIRMessagingDelegate.GetObjectID);
  TiOSHelper.SharedApplication.registerForRemoteNotifications;
end;

procedure TPlatformFirebaseMessaging.RegisterRemoteNotificationsIOS8OrLater(const AOptions: UNAuthorizationOptions);
var
  LSettings: UIUserNotificationSettings;
begin
  LSettings := TUIUserNotificationSettings.Wrap(TUIUserNotificationSettings.OCClass.settingsForTypes(AOptions, nil));
  TiOSHelper.SharedApplication.registerUserNotificationSettings(LSettings);
  TiOSHelper.SharedApplication.registerForRemoteNotifications;
end;

procedure TPlatformFirebaseMessaging.RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin
  if not granted then
  begin
    TThread.Queue(nil,
      procedure
      begin
        DoAuthorizationRefused;
      end
    );
  end;
end;

function TPlatformFirebaseMessaging.Messaging: FIRMessaging;
begin
  Result := TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging);
end;

procedure TPlatformFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  Messaging.subscribeToTopic(StrToNSStr(ATopicName));
end;

procedure TPlatformFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  Messaging.unsubscribeFromTopic(StrToNSStr(ATopicName));
end;

end.
