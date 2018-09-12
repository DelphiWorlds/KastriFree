unit DW.PermissionsRequester.Android;

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
  System.Messaging,
  // Android
  Androidapi.JNI,
  // DW
  DW.PermissionsRequester;

type
  TPlatformPermissionsRequester = class(TCustomPlatformPermissionsRequester)
  private
    FIsSubmitted: Boolean;
    FPermissions: array of string;
    FRequestCode: Integer;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CheckPermissionsResults;
  protected
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer); override;
  public
    constructor Create(const APermissionsRequester: TPermissionsRequester); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes,
  // FMX
  FMX.Platform,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App,
  // DW
  DW.PermissionsTypes, DW.OSDevice;

type
  TOpenPermissionsRequester = class(TPermissionsRequester);

{ TPlatformPermissionsRequester }

constructor TPlatformPermissionsRequester.Create(const APermissionsRequester: TPermissionsRequester);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TPlatformPermissionsRequester.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TPlatformPermissionsRequester.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      if FIsSubmitted then
        CheckPermissionsResults;
    end;
  end;
end;

procedure TPlatformPermissionsRequester.CheckPermissionsResults;
var
  LResults: TPermissionResults;
  LIndex, I, LCode: Integer;
begin
  SetLength(LResults, Length(FPermissions));
  for I := Low(FPermissions) to High(FPermissions) do
  begin
    LIndex := I - Low(FPermissions);
    LResults[LIndex].Permission := FPermissions[I];
    LResults[LIndex].Granted := TOSDevice.CheckPermission(FPermissions[I]);
  end;
  FIsSubmitted := False;
  TOpenPermissionsRequester(PermissionsRequester).DoPermissionsResult(FRequestCode, LResults);
end;

procedure TPlatformPermissionsRequester.RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
var
  LPermissions: TJavaObjectArray<JString>;
  I: Integer;
begin
  FRequestCode := ARequestCode;
  SetLength(FPermissions, Length(APermissions));
  for I := 0 to Length(APermissions) - 1 do
    FPermissions[I] := APermissions[I];
  LPermissions := TJavaObjectArray<JString>.Create(Length(FPermissions));
  for I := Low(FPermissions) to High(FPermissions) do
    LPermissions.Items[I] := StringToJString(FPermissions[I]);
  FIsSubmitted := True;
  TAndroidHelper.Activity.requestPermissions(LPermissions, ARequestCode);
end;

end.

