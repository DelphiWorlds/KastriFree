unit DW.Firebase.Default;

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
  DW.Firebase.InstanceId, DW.Firebase.Messaging;

type
  TPlatformFirebaseInstanceId = class(TCustomPlatformFirebaseInstanceId)
  protected
    function GetToken: string; override;
    procedure HandleTokenRefresh;
    function Start: Boolean; override;
  end;

  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure SubscribeToTopic(const ATopicName: string); override;
    procedure UnsubscribeFromTopic(const ATopicName: string); override;
  end;

implementation

{ TPlatformFirebaseInstanceId }

function TPlatformFirebaseInstanceId.GetToken: string;
begin
  Result := '';
end;

procedure TPlatformFirebaseInstanceId.HandleTokenRefresh;
begin
  //
end;

function TPlatformFirebaseInstanceId.Start: Boolean;
begin
  Result := False;
end;

{ TPlatformFirebaseMessaging }

procedure TPlatformFirebaseMessaging.Connect;
begin
  //
end;

procedure TPlatformFirebaseMessaging.Disconnect;
begin
  //
end;

procedure TPlatformFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  //
end;

procedure TPlatformFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  //
end;

end.
