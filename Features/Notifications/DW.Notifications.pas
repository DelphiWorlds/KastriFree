unit DW.Notifications;

// ***************** NOTE **************************
//      THIS UNIT IS CURRENTLY EXPERIMENTAL
//           USE AT YOUR OWN RISK!
//
// It may or may not be removed from the Kastri Free library

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  TRepeatInterval = (None, Second, Minute, Hour, Day, Week, Weekday, Month, Quarter, Year, Era);

  TNotification = record
    AlertAction: string;
    AlertBody: string;
    EnableSound: Boolean;
    FireDate: TDateTime;
    HasAction: Boolean;
    Name: string;
    Number: Integer;
    RepeatInterval: TRepeatInterval;
    SoundName: string;
    Subtitle: string;
    Title: string;
  end;

  TNotifications = class;

  TCustomPlatformNotifications = class(TObject)
  private
    FNotifications: TNotifications;
  protected
    procedure CancelAll; virtual;
    procedure CancelNotification(const AName: string); virtual;
    procedure PresentNotification(const ANotification: TNotification); virtual;
    procedure ScheduleNotification(const ANotification: TNotification); virtual;
    property Notifications: TNotifications read FNotifications;
  public
    constructor Create(const ANotifications: TNotifications); virtual;
    destructor Destroy; override;
  end;

  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TNotification) of object;

  TNotifications = class(TObject)
  private
    FPlatformNotifications: TCustomPlatformNotifications;
    FOnNotificationReceived: TNotificationReceivedEvent;
  protected
    procedure DoNotificationReceived(const ANotification: TNotification);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CancelAll;
    procedure CancelNotification(const AName: string);
    procedure PresentNotification(const ANotification: TNotification);
    procedure ScheduleNotification(const ANotification: TNotification);
    property OnNotificationReceived: TNotificationReceivedEvent read FOnNotificationReceived write FOnNotificationReceived;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Notifications.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Notifications.Android;
  {$ELSE}
  DW.Notifications.Default;
  {$ENDIF}

{ TCustomPlatformNotifications }

constructor TCustomPlatformNotifications.Create(const ANotifications: TNotifications);
begin
  inherited Create;
  FNotifications := ANotifications;
end;

destructor TCustomPlatformNotifications.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformNotifications.CancelAll;
begin
  //
end;

procedure TCustomPlatformNotifications.CancelNotification(const AName: string);
begin
  //
end;

procedure TCustomPlatformNotifications.PresentNotification(const ANotification: TNotification);
begin
  //
end;

procedure TCustomPlatformNotifications.ScheduleNotification(const ANotification: TNotification);
begin
  //
end;

{ TNotifications }

constructor TNotifications.Create;
begin
  inherited;
  FPlatformNotifications := TPlatformNotifications.Create(Self);
end;

destructor TNotifications.Destroy;
begin
  FPlatformNotifications.Free;
  inherited;
end;

procedure TNotifications.DoNotificationReceived(const ANotification: TNotification);
begin
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, ANotification);
end;

procedure TNotifications.CancelAll;
begin
  FPlatformNotifications.CancelAll;
end;

procedure TNotifications.CancelNotification(const AName: string);
begin
  FPlatformNotifications.CancelNotification(AName);
end;

procedure TNotifications.PresentNotification(const ANotification: TNotification);
begin
  FPlatformNotifications.PresentNotification(ANotification);
end;

procedure TNotifications.ScheduleNotification(const ANotification: TNotification);
begin
  FPlatformNotifications.ScheduleNotification(ANotification);
end;

end.
