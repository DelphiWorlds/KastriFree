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
    ExtraTab: TTabItem;
    ExtraButtonsLayout: TLayout;
    RequestWriteSettingsButton: TButton;
    RequestSMSPermissionsButton: TButton;
    OpenPDFButton: TButton;
    procedure TakePhotoButtonClick(Sender: TObject);
    procedure ImmediateButtonClick(Sender: TObject);
    procedure Schedule10SecondsButtonClick(Sender: TObject);
    procedure CancelScheduledClick(Sender: TObject);
    procedure RequestSMSPermissionsButtonClick(Sender: TObject);
    procedure OpenPDFButtonClick(Sender: TObject);
  private
    FRequester: TPermissionsRequester;
    FMediaLibrary: TMediaLibrary;
    procedure CopyPDFSample;
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

uses
  {$IF Defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.Net,
  DW.Android.Helpers,
  {$ENDIF}
  System.IOUtils;

const
  cPermissionReadExternalStorage = 'android.permission.READ_EXTERNAL_STORAGE';
  cPermissionWriteExternalStorage = 'android.permission.WRITE_EXTERNAL_STORAGE';
  cPermissionCamera = 'android.permission.CAMERA';
  cPermissionSendSMS = 'android.permission.SEND_SMS';
  cPermissionReceiveSMS = 'android.permission.RECEIVE_SMS';
  cPermissionReadSMS = 'android.permission.READ_SMS';
  cPermissionReceiveMMS = 'android.permission.RECEIVE_MMS';
  cPermissionReceiveWAPPush = 'android.permission.RECEIVE_WAP_PUSH';
  cPermissionsCodeExternalStorage = 1;
  cPermissionsCodeSMS = 2;

  cPDFSampleFileName = 'pdf-sample.pdf';

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  CopyPDFSample;
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

procedure TForm1.CopyPDFSample;
var
  LFileName, LCopyFileName: string;
begin
  LFileName := TPath.Combine(TPath.GetDocumentsPath, cPDFSampleFileName);
  LCopyFileName := TPath.Combine(TPath.GetPublicPath, cPDFSampleFileName);
  if TFile.Exists(LFileName) and not TFile.Exists(LCopyFileName) then
    TFile.Copy(LFileName, LCopyFileName);
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
    cPermissionsCodeSMS:
    begin
      if AResults.AreAllGranted then
        ShowMessage('SMS permissions granted')
      else
        ShowMessage('You need to grant all required permissions for the app to be able to handle SMS!');
    end;
  end;
end;

procedure TForm1.RequestSMSPermissionsButtonClick(Sender: TObject);
begin
  FRequester.RequestPermissions([cPermissionSendSMS, cPermissionReceiveSMS, cPermissionReadSMS, cPermissionReceiveMMS, cPermissionReceiveWAPPush], cPermissionsCodeSMS);
end;

procedure TForm1.TakePhotoButtonClick(Sender: TObject);
begin
  FRequester.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage, cPermissionCamera], cPermissionsCodeExternalStorage);
end;

procedure TForm1.MediaLibraryReceivedImageHandler(Sender: TObject; const AImagePath: string; const AImage: TBitmap);
begin
  PhotoImage.Bitmap.Assign(AImage);
end;

procedure TForm1.OpenPDFButtonClick(Sender: TObject);
{$IF Defined(ANDROID)}
var
  LIntent: JIntent;
  LFileName: string;
begin
  // NOTE: You will need a PDF viewer installed on your device in order for this to work
  LFileName := TPath.Combine(TPath.GetPublicPath, cPDFSampleFileName);
  LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  LIntent.setDataAndType(TAndroidHelperEx.UriFromFileName(LFileName), StringToJString('application/pdf'));
  LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(LIntent);
end;
{$ELSE}
begin

end;
{$ENDIF}

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
