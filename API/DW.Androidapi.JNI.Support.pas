unit DW.Androidapi.JNI.Support;

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
  Androidapi.JNI.Support, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App,
  Androidapi.JNI.Widget, Androidapi.JNI.Net;

type
  JNotificationCompat_Builder = interface;

  JNotificationCompat_BuilderClass = interface(JObjectClass)
    ['{6EC74C2C-EBCC-4A55-98B6-6DD36DE3BA8C}']
    {class} function init(context: JContext): JNotificationCompat_Builder; cdecl;
  end;

  [JavaSignature('android/support/v4/app/NotificationCompat$Builder')]
  JNotificationCompat_Builder = interface(Androidapi.JNI.Support.JNotificationCompat_Builder)
    ['{81FD10B1-0D7F-4F6E-BF92-6A74F52C424C}']
    function addAction(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function build: JNotification; cdecl;
    function getNotification: JNotification; cdecl;//Deprecated
    function setAutoCancel(autoCancel: Boolean): JNotificationCompat_Builder; cdecl;
    function setChannelId(channelId: JString): JNotificationCompat_Builder; cdecl;
    function setContent(views: JRemoteViews): JNotificationCompat_Builder; cdecl;
    function setContentInfo(info: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setContentText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setContentTitle(title: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setDefaults(defaults: Integer): JNotificationCompat_Builder; cdecl;
    function setDeleteIntent(intent: JPendingIntent): JNotificationCompat_Builder; cdecl;
    function setFullScreenIntent(intent: JPendingIntent; highPriority: Boolean): JNotificationCompat_Builder; cdecl;
    function setLargeIcon(icon: JBitmap): JNotificationCompat_Builder; cdecl;
    function setLights(argb: Integer; onMs: Integer; offMs: Integer): JNotificationCompat_Builder; cdecl;
    function setNumber(number: Integer): JNotificationCompat_Builder; cdecl;
    function setOngoing(ongoing: Boolean): JNotificationCompat_Builder; cdecl;
    function setOnlyAlertOnce(onlyAlertOnce: Boolean): JNotificationCompat_Builder; cdecl;
    function setPriority(pri: Integer): JNotificationCompat_Builder; cdecl;
    function setProgress(max: Integer; progress: Integer; indeterminate: Boolean): JNotificationCompat_Builder; cdecl;
    function setSmallIcon(icon: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSmallIcon(icon: Integer; level: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setSound(sound: Jnet_Uri): JNotificationCompat_Builder; cdecl; overload;
    function setSound(sound: Jnet_Uri; streamType: Integer): JNotificationCompat_Builder; cdecl; overload;
    function setStyle(style: JNotificationCompat_Style): JNotificationCompat_Builder; cdecl;
    function setSubText(text: JCharSequence): JNotificationCompat_Builder; cdecl;
    function setTicker(tickerText: JCharSequence): JNotificationCompat_Builder; cdecl; overload;
    function setTicker(tickerText: JCharSequence; views: JRemoteViews): JNotificationCompat_Builder; cdecl; overload;
    function setUsesChronometer(b: Boolean): JNotificationCompat_Builder; cdecl;
    function setVibrate(pattern: TJavaArray<Int64>): JNotificationCompat_Builder; cdecl;
    function setWhen(when: Int64): JNotificationCompat_Builder; cdecl;
  end;
  TJNotificationCompat_Builder = class(TJavaGenericImport<JNotificationCompat_BuilderClass, JNotificationCompat_Builder>) end;

implementation

end.
