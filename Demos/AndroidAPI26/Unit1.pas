unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions, System.Notification,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, FMX.Objects, FMX.Layouts, FMX.TabControl,
  DW.PermissionsRequester, DW.PermissionsTypes, DW.MediaLibrary;

type
  TForm1 = class(TForm)
    TakePhotoButton: TButton;
    PhotoImage: TImage;
    BackgroundRectangle: TRectangle;
    BottomLayout: TLayout;
    TabControl: TTabControl;
    TakePhotoTab: TTabItem;
    NotificationsTab: TTabItem;
    ImmediateButton: TButton;
    Schedule10SecondsButton: TButton;
    CancelScheduled: TButton;
    NotificationCenter: TNotificationCenter;
    procedure TakePhotoButtonClick(Sender: TObject);
    procedure ImmediateButtonClick(Sender: TObject);
    procedure Schedule10SecondsButtonClick(Sender: TObject);
    procedure CancelScheduledClick(Sender: TObject);
  private
    FRequester: TPermissionsRequester;
    FMediaLibrary: TMediaLibrary;
    procedure MediaLibraryReceivedImageHandler(Sender: TObject; const AImagePath: string; const AImage: TBitmap);
    procedure PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
    procedure ImmediateNotification;
    procedure ScheduleNotification;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  cPermissionReadExternalStorage = 'android.permission.READ_EXTERNAL_STORAGE';
  cPermissionWriteExternalStorage = 'android.permission.WRITE_EXTERNAL_STORAGE';
  cPermissionCamera = 'android.permission.CAMERA';
  cPermissionsCodeExternalStorage = 1;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TabControl.ActiveTab := TakePhotoTab;
  FRequester := TPermissionsRequester.Create;
  FRequester.OnPermissionsResult := PermissionsResultHandler;
  FMediaLibrary := TMediaLibrary.Create;
  FMediaLibrary.OnReceivedImage := MediaLibraryReceivedImageHandler;
end;

destructor TForm1.Destroy;
begin
  FRequester.Free;
  FMediaLibrary.Free;
  inherited;
end;

procedure TForm1.PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
begin
  case ARequestCode of
    cPermissionsCodeExternalStorage:
    begin
      if AResults.AreAllGranted then
        FMediaLibrary.TakePhoto
      else
        ShowMessage('You need to grant all required permissions for the app to be able to take photos!');
    end;
  end;
end;

procedure TForm1.TakePhotoButtonClick(Sender: TObject);
begin
  FRequester.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage, cPermissionCamera], cPermissionsCodeExternalStorage);
end;

procedure TForm1.MediaLibraryReceivedImageHandler(Sender: TObject; const AImagePath: string; const AImage: TBitmap);
begin
  PhotoImage.Bitmap.Assign(AImage);
end;

procedure TForm1.CancelScheduledClick(Sender: TObject);
begin
  NotificationCenter.CancelNotification('ScheduledNotification');
end;

procedure TForm1.ImmediateButtonClick(Sender: TObject);
begin
  ImmediateNotification;
end;

procedure TForm1.Schedule10SecondsButtonClick(Sender: TObject);
begin
  ScheduleNotification;
end;

procedure TForm1.ImmediateNotification;
var
  LNotification: TNotification;
begin
  LNotification := NotificationCenter.CreateNotification;
  try
    LNotification.EnableSound := False;
    LNotification.AlertBody := 'Immediate Notification';
    NotificationCenter.PresentNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

procedure TForm1.ScheduleNotification;
var
  LNotification: TNotification;
begin
  LNotification := NotificationCenter.CreateNotification;
  try
    LNotification.Name := 'ScheduledNotification';
    LNotification.EnableSound := False;
    LNotification.AlertBody := 'Scheduled Notification';
    LNotification.FireDate := Now + EncodeTime(0, 0, 10, 0);
    NotificationCenter.ScheduleNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

end.
