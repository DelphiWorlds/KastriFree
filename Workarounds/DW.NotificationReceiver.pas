unit DW.NotificationReceiver;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

// Unit associated with workaround for: https://quality.embarcadero.com/browse/RSP-20565
// You should probably not use this unit for anything else

interface

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // DW
  DW.MultiReceiver.Android;

type
  TNotificationReceiver = class(TMultiReceiver)
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    class function ACTION_NOTIFICATION: JString;
    class function EXTRA_NOTIFICATION: JString;
    class function EXTRA_NOTIFICATION_ID: JString;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.App;

function NotificationManager: JNotificationManager;
var
  LObject: JObject;
begin
  LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  Result := TJNotificationManager.Wrap((LObject as ILocalObject).GetObjectID);
end;

{ TNotificationReceiver }

class function TNotificationReceiver.ACTION_NOTIFICATION: JString;
begin
  Result := StringToJString('ACTION_NOTIFICATION_EX');
end;

class function TNotificationReceiver.EXTRA_NOTIFICATION: JString;
begin
  Result := StringToJString('EXTRA_NOTIFICATION');
end;

class function TNotificationReceiver.EXTRA_NOTIFICATION_ID: JString;
begin
  Result := StringToJString('EXTRA_NOTIFICATION_ID');
end;

procedure TNotificationReceiver.ConfigureActions;
begin
  IntentFilter.addAction(ACTION_NOTIFICATION);
end;

procedure TNotificationReceiver.Receive(context: JContext; intent: JIntent);
var
  LNotification: JNotification;
begin
  LNotification := TJNotification.Wrap(intent.getParcelableExtra(EXTRA_NOTIFICATION));
  NotificationManager.notify(intent.getIntExtra(EXTRA_NOTIFICATION_ID, 0), LNotification);
end;

end.
