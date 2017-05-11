unit DW.Firebase.Messaging.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  // DW
  DW.Firebase.Messaging;

type
  TPlatformFirebaseMessaging = class;

  TFirebaseMessagingReceiverListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
  public
    { JFMXBroadcastReceiverListener }
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
  end;

  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  private
    FFirebaseMessagingBroadcastReceiver: JFMXBroadcastReceiver;
    FFirebaseMessagingReceiverListener: TFirebaseMessagingReceiverListener;
    FStartupIntentHandled: Boolean;
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure DoApplicationBecameActive; override;
    procedure HandleMessageReceived(const data: JIntent);
    procedure SubscribeToTopic(const ATopicName: string); override;
    procedure UnsubscribeFromTopic(const ATopicName: string); override;
  public
    constructor Create(const AFirebaseMessaging: TFirebaseMessaging); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes, System.Threading,
  // Android
  Androidapi.JNI.Os, Androidapi.Helpers,
  // FMX
  FMX.Platform.Android,
  // DW
  DW.FirebaseApp.Android, DW.Androidapi.JNI.FirebaseServiceHelpers, DW.Androidapi.JNI.LocalBroadcastManager, DW.Androidapi.JNI.Firebase;

{ TFirebaseMessagingReceiverListener }

constructor TFirebaseMessagingReceiverListener.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TFirebaseMessagingReceiverListener.onReceive(context: JContext; intent: JIntent);
begin
  if (intent <> nil) and (intent.getAction.compareTo(TJDWFirebaseMessagingService.JavaClass.ACTION_MESSAGE_RECEIVED) = 0) then
    FFirebaseMessaging.HandleMessageReceived(intent);
end;

{ TPlatformFirebaseMessaging }

constructor TPlatformFirebaseMessaging.Create(const AFirebaseMessaging: TFirebaseMessaging);
var
  LIntentFilter: JIntentFilter;
begin
  inherited;
  TPlatformFirebaseApp.Start;
  FFirebaseMessagingReceiverListener := TFirebaseMessagingReceiverListener.Create(Self);
  FFirebaseMessagingBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FFirebaseMessagingReceiverListener);
  LIntentFilter := TJIntentFilter.JavaClass.init(TJDWFirebaseMessagingService.JavaClass.ACTION_MESSAGE_RECEIVED);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FFirebaseMessagingBroadcastReceiver, LIntentFilter);
end;

destructor TPlatformFirebaseMessaging.Destroy;
begin
  FFirebaseMessagingReceiverListener.Free;
  FFirebaseMessagingBroadcastReceiver := nil;
  inherited;
end;

procedure TPlatformFirebaseMessaging.Connect;
begin
  IsConnected := True;
end;

procedure TPlatformFirebaseMessaging.Disconnect;
begin
  IsConnected := False;
end;

procedure TPlatformFirebaseMessaging.DoApplicationBecameActive;
begin
  if not FStartupIntentHandled then
  begin
    HandleMessageReceived(MainActivity.getIntent);
    FStartupIntentHandled := True;
  end;
end;

procedure TPlatformFirebaseMessaging.HandleMessageReceived(const data: JIntent);
var
  LPayload: TStrings;
  LBundle: JBundle;
  LIterator: JIterator;
  LKeyObject: JObject;
  LValueObject: JObject;
  LValue: string;
begin
  if not IsForeground then
  begin
    TTask.Run(
      procedure
      begin
        TJDWNotificationPublisher.JavaClass.sendNotification(TAndroidHelper.Context, data, False);
      end
    );
  end;
  LPayload := TStringList.Create;
  try
    LBundle := data.getExtras;
    if LBundle <> nil then
    begin
      LIterator := LBundle.keySet.iterator;
      while LIterator.hasNext do
      begin
        LKeyObject := LIterator.next;
        if LKeyObject = nil then
          Continue;
        LValueObject := LBundle.&get(LKeyObject.toString);
        if LValueObject = nil then
          LValue := ''
        else
          LValue := JStringToString(LValueObject.toString);
        LPayload.Values[JStringToString(LKeyObject.toString)] := LValue;
      end;
    end;
    if LPayload.Count > 0 then
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          DoMessageReceived(LPayload);
        end
      );
    end;
  finally
    LPayload.Free;
  end;
end;

procedure TPlatformFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  TJFirebaseMessaging.JavaClass.getInstance.subscribeToTopic(StringToJString(ATopicName));
end;

procedure TPlatformFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopicName));
end;

end.
