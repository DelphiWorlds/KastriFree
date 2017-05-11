unit DW.Firebase.Messaging;

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
  System.Classes, System.Messaging,
  // DW
  DW.Firebase.InstanceID;

type
  TFirebaseMessageReceivedEvent = procedure(Sender: TObject; const APayload: TStrings) of object;

  TFirebaseMessaging = class;

  TCustomPlatformFirebaseMessaging = class(TObject)
  private
    FFirebaseMessaging: TFirebaseMessaging;
    FIsConnected: Boolean;
    FIsForeground: Boolean;
    FWasConnected: Boolean;
  protected
    procedure ApplicationBecameActive;
    procedure ApplicationEnteredBackground;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure DoApplicationBecameActive; virtual;
    procedure DoApplicationEnteredBackground; virtual;
    procedure DoAuthorizationRefused;
    procedure DoMessageReceived(const APayload: TStrings);
    procedure SubscribeToTopic(const ATopicName: string); virtual; abstract;
    procedure UnsubscribeFromTopic(const ATopicName: string); virtual; abstract;
    property IsConnected: Boolean read FIsConnected write FIsConnected;
    property IsForeground: Boolean read FIsForeground write FIsForeground;
  public
    constructor Create(const AFirebaseMessaging: TFirebaseMessaging); virtual;
    destructor Destroy; override;
  end;

  TFirebaseMessaging = class(TObject)
  private
    FPlatformFirebaseMessaging: TCustomPlatformFirebaseMessaging;
    FOnAuthorizationRefused: TNotifyEvent;
    FOnMessageReceived: TFirebaseMessageReceivedEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    function GetIsConnected: Boolean;
    procedure PushFailToRegisterMessageHandler(const Sender: TObject; const M: TMessage);
    procedure PushRemoteNotificationMessageHandler(const Sender: TObject; const M: TMessage);
  protected
    procedure DoAuthorizationRefused;
    procedure DoMessageReceived(const APayload: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure SubscribeToTopic(const ATopicName: string);
    procedure UnsubscribeFromTopic(const ATopicName: string);
    property IsConnected: Boolean read GetIsConnected;
    property OnAuthorizationRefused: TNotifyEvent read FOnAuthorizationRefused write FOnAuthorizationRefused;
    property OnMessageReceived: TFirebaseMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
  end;

implementation

uses
  // FMX
  FMX.Platform,
  {$IF Defined(IOS)}
  DW.Firebase.Messaging.iOS;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.Firebase.Messaging.Android;
  {$ENDIF}

{ TCustomPlatformFirebaseMessaging }

constructor TCustomPlatformFirebaseMessaging.Create(const AFirebaseMessaging: TFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

destructor TCustomPlatformFirebaseMessaging.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformFirebaseMessaging.ApplicationBecameActive;
begin
  FIsForeground := True;
  if FWasConnected then
    Connect;
  DoApplicationBecameActive;
end;

procedure TCustomPlatformFirebaseMessaging.ApplicationEnteredBackground;
begin
  FIsForeground := False;
  FWasConnected := IsConnected;
  Disconnect;
end;

procedure TCustomPlatformFirebaseMessaging.DoApplicationBecameActive;
begin
  //
end;

procedure TCustomPlatformFirebaseMessaging.DoApplicationEnteredBackground;
begin
  //
end;

procedure TCustomPlatformFirebaseMessaging.DoAuthorizationRefused;
begin
  FFirebaseMessaging.DoAuthorizationRefused;
end;

procedure TCustomPlatformFirebaseMessaging.DoMessageReceived(const APayload: TStrings);
begin
  FFirebaseMessaging.DoMessageReceived(APayload);
end;

{ TFirebaseMessaging }

constructor TFirebaseMessaging.Create;
begin
  inherited;
  FPlatformFirebaseMessaging := TPlatformFirebaseMessaging.Create(Self);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushFailToRegisterMessage, PushFailToRegisterMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushRemoteNotificationMessage, PushRemoteNotificationMessageHandler);
end;

destructor TFirebaseMessaging.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushFailToRegisterMessage, PushFailToRegisterMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushRemoteNotificationMessage, PushRemoteNotificationMessageHandler);
  FPlatformFirebaseMessaging.Free;
  inherited;
end;

procedure TFirebaseMessaging.Connect;
begin
  FPlatformFirebaseMessaging.Connect;
end;

procedure TFirebaseMessaging.Disconnect;
begin
  FPlatformFirebaseMessaging.Disconnect;
end;

procedure TFirebaseMessaging.DoAuthorizationRefused;
begin
  if Assigned(FOnAuthorizationRefused) then
    FOnAuthorizationRefused(Self);
end;

procedure TFirebaseMessaging.DoMessageReceived(const APayload: TStrings);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, APayload);
end;

function TFirebaseMessaging.GetIsConnected: Boolean;
begin
  Result := FPlatformFirebaseMessaging.IsConnected;
end;

procedure TFirebaseMessaging.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
      FPlatformFirebaseMessaging.ApplicationBecameActive;
    TApplicationEvent.EnteredBackground:
      FPlatformFirebaseMessaging.ApplicationEnteredBackground;
  end;
end;

procedure TFirebaseMessaging.PushFailToRegisterMessageHandler(const Sender: TObject; const M: TMessage);
begin

end;

procedure TFirebaseMessaging.PushRemoteNotificationMessageHandler(const Sender: TObject; const M: TMessage);
var
  LPayload: TStrings;
  LJSON: string;
begin
  LPayload := TStringList.Create;
  try
    if (M is TPushRemoteNotificationMessage) then
      LJSON := (M as TPushRemoteNotificationMessage).Value.Notification
    else if (M is TPushStartupNotificationMessage) then
      LJSON := (M as TPushStartupNotificationMessage).Value.Notification
    else
      LJSON := '';
    if LJSON <> '' then
    begin
      LPayload.Text := LJSON; // TODO: Formatting?
      DoMessageReceived(LPayload);
    end;
  finally
    LPayload.Free;
  end;
end;

procedure TFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  FPlatformFirebaseMessaging.SubscribeToTopic(ATopicName);
end;

procedure TFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  FPlatformFirebaseMessaging.UnsubscribeFromTopic(ATopicName);
end;

end.
