unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  DW.Firebase.Messaging, DW.PermissionsRequester, DW.PermissionsTypes, FMX.Objects;

type
  TfrmMain = class(TForm)
    ContentLayout: TLayout;
    FirebaseCMLabel: TLabel;
    TokenLabel: TLabel;
    TokenMemo: TMemo;
    MessagesLabel: TLabel;
    MessagesMemo: TMemo;
    ClearMessagesButton: TButton;
    BackgroundRectangle: TRectangle;
    procedure ClearMessagesButtonClick(Sender: TObject);
  private
    FFCM: TFirebaseMessaging;
    FRequester: TPermissionsRequester;
    procedure FCMAuthorizationResultHandler(Sender: TObject; const AGranted: Boolean);
    procedure FCMTokenReceivedHandler(Sender: TObject; const AToken: string);
    procedure FCMMessageReceivedHandler(Sender: TObject; const APayload: TStrings);
    procedure RequesterPermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  DW.Classes.Helpers, DW.OSLog, DW.OSDevice;

const
  cDangerousPermissions: array[0..4] of string = (
    'android.permission.ACCESS_COARSE_LOCATION',
    'android.permission.ACCESS_FINE_LOCATION',
    'android.permission.CAMERA',
    'android.permission.READ_EXTERNAL_STORAGE',
    'android.permission.WRITE_EXTERNAL_STORAGE'
  );

{ TfrmMain }

procedure TfrmMain.ClearMessagesButtonClick(Sender: TObject);
begin
  MessagesMemo.Lines.Clear;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FFCM := TFirebaseMessaging.Create;
  FFCM.OnAuthorizationResult := FCMAuthorizationResultHandler;
  FFCM.OnTokenReceived := FCMTokenReceivedHandler;
  FFCM.OnMessageReceived := FCMMessageReceivedHandler;
  FRequester := TPermissionsRequester.Create;
  FRequester.OnPermissionsResult := RequesterPermissionsResultHandler;
  FRequester.RequestPermissions(cDangerousPermissions, 1);
end;

destructor TfrmMain.Destroy;
begin
  FRequester.Free;
  FFCM.Free;
  inherited;
end;

procedure TfrmMain.RequesterPermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
begin
  if (ARequestCode = 1) and AResults.AreAllGranted then
    FFCM.RequestAuthorization;
end;

procedure TfrmMain.Resize;
begin
  inherited;
  // Spacing for iPhoneX display
  ContentLayout.Margins.Rect := TOSDevice.GetOffsetRect;
end;

procedure TfrmMain.FCMAuthorizationResultHandler(Sender: TObject; const AGranted: Boolean);
begin
  if AGranted then
    FFCM.Connect;
end;

procedure TfrmMain.FCMTokenReceivedHandler(Sender: TObject; const AToken: string);
begin
  TokenMemo.Lines.Text := AToken;
  TOSLog.d('Token in FCMTokenReceivedHandler: %s', [TokenMemo.Lines.Text]);
end;

procedure TfrmMain.FCMMessageReceivedHandler(Sender: TObject; const APayload: TStrings);
begin
  MessagesMemo.Lines.AddStrings(APayload);
end;

end.
