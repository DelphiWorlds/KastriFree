unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, FMX.Objects, FMX.Layouts,
  DW.PermissionsRequester, DW.PermissionsTypes, DW.MediaLibrary;

type
  TForm1 = class(TForm)
    TakePhotoButton: TButton;
    PhotoImage: TImage;
    BackgroundRectangle: TRectangle;
    BottomLayout: TLayout;
    procedure TakePhotoButtonClick(Sender: TObject);
  private
    FRequester: TPermissionsRequester;
    FMediaLibrary: TMediaLibrary;
    procedure MediaLibraryReceivedImageHandler(Sender: TObject; const AImagePath: string; const AImage: TBitmap);
    procedure PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
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
  FRequester := TPermissionsRequester.Create;
  FRequester.OnPermissionsResult := PermissionsResultHandler;
  FMediaLibrary := TMediaLibrary.Create;
  FMediaLibrary.OnReceivedImage := MediaLibraryReceivedImageHandler;
end;

destructor TForm1.Destroy;
begin
  FRequester.Free;
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

end.
