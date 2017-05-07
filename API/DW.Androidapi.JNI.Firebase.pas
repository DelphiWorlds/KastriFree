unit DW.Androidapi.JNI.Firebase;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.App;

type
  JFirebaseOptions = interface;
  JFirebaseOptions_Builder = interface;
  JFirebaseApp = interface;
  JFirebaseInstanceId = interface;

  JFirebaseOptionsClass = interface(JObjectClass)
    ['{0B0D6931-3744-44B5-9909-4158E93EB585}']
    {class} function fromResource(context: JContext): JFirebaseOptions; cdecl;
  end;

  [JavaSignature('com/google/firebase/FirebaseOptions')]
  JFirebaseOptions = interface(JObject)
    ['{7F682156-DC17-414F-A537-358359AA01A7}']
    function equals(o: JObject): Boolean; cdecl;
    function getApiKey: JString; cdecl;
    function getApplicationId: JString; cdecl;
    function getDatabaseUrl: JString; cdecl;
    function getGcmSenderId: JString; cdecl;
    function getStorageBucket: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseOptions = class(TJavaGenericImport<JFirebaseOptionsClass, JFirebaseOptions>) end;

  JFirebaseOptions_BuilderClass = interface(JObjectClass)
    ['{745E3C1A-7F86-49A7-8377-CB3D7F953870}']
    {class} function init: JFirebaseOptions_Builder; cdecl; overload;
    {class} function init(options: JFirebaseOptions): JFirebaseOptions_Builder; cdecl; overload;
  end;

  [JavaSignature('com/google/firebase/FirebaseOptions$Builder')]
  JFirebaseOptions_Builder = interface(JObject)
    ['{71400595-7BCC-4AA2-BCDC-BA3F5FF3CD8F}']
    function build: JFirebaseOptions; cdecl;
    function setApiKey(apiKey: JString): JFirebaseOptions_Builder; cdecl;
    function setApplicationId(applicationId: JString): JFirebaseOptions_Builder; cdecl;
    function setDatabaseUrl(databaseUrl: JString): JFirebaseOptions_Builder; cdecl;
    function setGcmSenderId(gcmSenderId: JString): JFirebaseOptions_Builder; cdecl;
    function setStorageBucket(storageBucket: JString): JFirebaseOptions_Builder; cdecl;
  end;
  TJFirebaseOptions_Builder = class(TJavaGenericImport<JFirebaseOptions_BuilderClass, JFirebaseOptions_Builder>) end;

  JFirebaseAppClass = interface(JObjectClass)
    ['{DE33391C-BCF4-4718-A557-7703B772E23F}']
    {class} function _GetDEFAULT_APP_NAME: JString; cdecl;
    {class} function getApps(context: JContext): JList; cdecl;
    {class} function getInstance: JFirebaseApp; cdecl; overload;
    {class} function getInstance(name: JString): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; options: JFirebaseOptions): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; options: JFirebaseOptions; name: JString): JFirebaseApp; cdecl; overload;
    {class} property DEFAULT_APP_NAME: JString read _GetDEFAULT_APP_NAME;
  end;

  [JavaSignature('com/google/firebase/FirebaseApp')]
  JFirebaseApp = interface(JObject)
    ['{3612CB9C-FD82-4362-A93E-7D59FC068B9A}']
    function equals(o: JObject): Boolean; cdecl;
    function getApplicationContext: JContext; cdecl;
    function getName: JString; cdecl;
    function getOptions: JFirebaseOptions; cdecl;
    function hashCode: Integer; cdecl;
    procedure setAutomaticResourceManagementEnabled(enabled: Boolean); cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseApp = class(TJavaGenericImport<JFirebaseAppClass, JFirebaseApp>) end;

  JFirebaseInstanceIdClass = interface(JObjectClass)
    ['{E5387FB1-8757-45AF-B699-6781707A7D57}']
    {class} function getInstance: JFirebaseInstanceId; cdecl; overload;
    {class} function getInstance(app: JFirebaseApp): JFirebaseInstanceId; cdecl; overload;
  end;

  [JavaSignature('com/google/firebase/iid/FirebaseInstanceId')]
  JFirebaseInstanceId = interface(JObject)
    ['{F759878D-C386-4118-9578-A6FC6A4D6FBD}']
    procedure deleteInstanceId; cdecl;
    procedure deleteToken(authorizedEntity: JString; scope: JString); cdecl;
    function getCreationTime: Int64; cdecl;
    function getId: JString; cdecl;
    function getToken: JString; cdecl; overload;
    function getToken(authorizedEntity: JString; scope: JString): JString; cdecl; overload;
  end;
  TJFirebaseInstanceId = class(TJavaGenericImport<JFirebaseInstanceIdClass, JFirebaseInstanceId>) end;

  JFirebaseMessaging = interface;

  JFirebaseMessagingClass = interface(JObjectClass)
    ['{CAB4B525-5E1E-40E8-9A7C-CA3608FE26C6}']
    {class} function getInstance: JFirebaseMessaging; cdecl; overload;
  end;

  [JavaSignature('com/google/firebase/messaging/FirebaseMessaging')]
  JFirebaseMessaging = interface(JObject)
    ['{F33877E5-DAA2-4097-9527-CE1CDBD66A67}']
    procedure subscribeToTopic(topic: JString); cdecl;
    procedure unsubscribeFromTopic(topic: JString); cdecl;
  end;
  TJFirebaseMessaging = class(TJavaGenericImport<JFirebaseMessagingClass, JFirebaseMessaging>) end;

implementation

end.
