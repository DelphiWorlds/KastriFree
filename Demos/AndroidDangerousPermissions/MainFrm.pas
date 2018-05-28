unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.SystemHelper, System.Sensors, System.Sensors.Components, FMX.Layouts, FMX.Objects;

type
  TfrmMain = class(TForm)
    CameraPermissionButton: TButton;
    RequestLocationPermissionsButton: TButton;
    RequestSMSPermissionsButton: TButton;
    LocationSensor: TLocationSensor;
    StatusBarRectangle: TRectangle;
    procedure CameraPermissionButtonClick(Sender: TObject);
    procedure RequestLocationPermissionsButtonClick(Sender: TObject);
    procedure RequestSMSPermissionsButtonClick(Sender: TObject);
  private
    FSystemHelper: TSystemHelper;
    procedure PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

const
  cRequestCodeCamera = 1;
  cRequestCodeLocation = 2;
  cRequestCodeContacts = 3;
  cRequestCodeSMS = 4;

  cPermissionCamera = 'android.permission.CAMERA';
  cPermissionAccessCoarseLocation = 'android.permission.ACCESS_COARSE_LOCATION';
  cPermissionAccessFineLocation = 'android.permission.ACCESS_FINE_LOCATION';
  cPermissionReadContacts = 'android.permission.READ_CONTACTS';
  cPermissionWriteContacts = 'android.permission.WRITE_CONTACTS';
  cPermissionSendSMS = 'android.permission.SEND_SMS';
  cPermissionReceiveSMS = 'android.permission.RECEIVE_SMS';
  cPermissionReadSMS = 'android.permission.READ_SMS';
  cPermissionReceiveWAPPush = 'android.permission.RECEIVE_WAP_PUSH';
  cPermissionReceiveMMS = 'android.permission.RECEIVE_MMS';

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FSystemHelper := TSystemHelper.Create;
  FSystemHelper.OnPermissionsResult := PermissionsResultHandler;
  StatusBarRectangle.Height := FSystemHelper.StatusBarHeight;
end;

procedure TfrmMain.PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
var
  I: Integer;
  LDeniedResults: TPermissionResults;
  LDeniedPermissions: string;
begin
  case ARequestCode of
    cRequestCodeCamera:
    begin
      if AResults.AreAllGranted then
        ShowMessage('You granted access to the camera')
      else
        ShowMessage('You denied access to the camera!');
    end;
    cRequestCodeLocation:
    begin
      if AResults.AreAllGranted then
      begin
        ShowMessage('You granted access to location');
        LocationSensor.Active := True;
      end
      else if AResults.DeniedResults.Count = 1 then
        ShowMessage('Both types of location access are required!')
      else if not AResults.AreAllGranted then
        ShowMessage('You denied access to location!');
    end;
    cRequestCodeSMS:
    begin
      if not AResults.AreAllGranted then
      begin
        LDeniedPermissions := '';
        LDeniedResults := AResults.DeniedResults;
        for I := 0 to LDeniedResults.Count - 1 do
          LDeniedPermissions := LDeniedPermissions + #13#10 + LDeniedResults[I].Permission;
        ShowMessage('You denied access to these SMS permissions: ' + LDeniedPermissions);
      end
      else
        ShowMessage('You granted access to all requested SMS permissions');
    end;
  end;
end;

procedure TfrmMain.CameraPermissionButtonClick(Sender: TObject);
begin
  FSystemHelper.RequestPermissions([cPermissionCamera], cRequestCodeCamera);
end;

procedure TfrmMain.RequestLocationPermissionsButtonClick(Sender: TObject);
begin
  FSystemHelper.RequestPermissions([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation], cRequestCodeLocation);
end;

procedure TfrmMain.RequestSMSPermissionsButtonClick(Sender: TObject);
begin
  FSystemHelper.RequestPermissions([cPermissionSendSMS, cPermissionReceiveSMS, cPermissionReadSMS, cPermissionReceiveWAPPush, cPermissionReceiveMMS],
    cRequestCodeSMS);
end;

end.
