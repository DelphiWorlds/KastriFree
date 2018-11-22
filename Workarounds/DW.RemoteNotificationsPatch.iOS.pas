unit DW.RemoteNotificationsPatch.iOS;

interface

implementation

uses
  System.SysUtils, iOSapi.Foundation, DW.iOSapi.UserNotifications;

type
  TRemoteNotificationsPatch = class(TObject)
  private
    class var FCurrent: TRemoteNotificationsPatch;
    class destructor DestroyClass;
  private
    procedure RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
    procedure InternalRequestAuthorization;
  protected
    class procedure RequestAuthorization;
  end;

{ TRemoteNotificationsPatch }

class destructor TRemoteNotificationsPatch.DestroyClass;
begin
  FCurrent.Free;
end;

procedure TRemoteNotificationsPatch.InternalRequestAuthorization;
var
  LOptions: UNAuthorizationOptions;
begin
  LOptions := UNAuthorizationOptionSound or UNAuthorizationOptionAlert or UNAuthorizationOptionBadge;
  UserNotificationCenter.requestAuthorizationWithOptions(LOptions, RequestAuthorizationWithOptionsCompletionHandler);
end;

procedure TRemoteNotificationsPatch.RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin
  //
end;

class procedure TRemoteNotificationsPatch.RequestAuthorization;
begin
  if TOSVersion.Check(10) then
  begin
    if FCurrent = nil then
      FCurrent := TRemoteNotificationsPatch.Create;
    FCurrent.InternalRequestAuthorization;
  end;
end;

initialization
  TRemoteNotificationsPatch.RequestAuthorization;

end.
