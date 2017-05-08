unit DW.Androidapi.JNI.FirebaseServiceHelpers;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JDWFirebaseInstanceIdService = interface;

  JDWFirebaseInstanceIdServiceClass = interface(JObjectClass)
    ['{A86BD0B7-320D-4773-B0CF-EBB7FF8F1328}']
    {class} function _GetACTION_TOKEN_REFRESHED: JString; cdecl;
    {class} property ACTION_TOKEN_REFRESHED: JString read _GetACTION_TOKEN_REFRESHED;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFirebaseInstanceIdService')]
  JDWFirebaseInstanceIdService = interface(JObject)
    ['{96CE8E1D-2697-4290-A16C-EF39F4827D92}']
  end;
  TJDWFirebaseInstanceIdService = class(TJavaGenericImport<JDWFirebaseInstanceIdServiceClass, JDWFirebaseInstanceIdService>) end;

  JDWFirebaseMessagingService = interface;

  JDWFirebaseMessagingServiceClass = interface(JObjectClass)
    ['{3111661E-03CE-45AB-9F60-90A0813EF914}']
    {class} function _GetACTION_MESSAGE_RECEIVED: JString; cdecl;
    {class} property ACTION_MESSAGE_RECEIVED: JString read _GetACTION_MESSAGE_RECEIVED;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFirebaseMessagingService')]
  JDWFirebaseMessagingService = interface(JObject)
    ['{D6C0E41A-3BD9-4477-A554-459A5916CE8F}']
  end;
  TJDWFirebaseMessagingService = class(TJavaGenericImport<JDWFirebaseMessagingServiceClass, JDWFirebaseMessagingService>) end;

implementation

end.
