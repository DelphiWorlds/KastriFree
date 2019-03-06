unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  {$IF CompilerVersion > 32}
  System.Permissions,
  {$ENDIF}
  Androidapi.JNI.JavaTypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdActns, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Layout1: TLayout;
    Image1: TImage;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FFileName: JString;
    procedure GetEXIF(const AFileName: JString);
    procedure ResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
    procedure TakePhoto;
    {$IF CompilerVersion > 32}
    procedure TakePhotoPermissionsResultHandler(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
   {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  Androidapi.Helpers, Androidapi.JNI.Media, Androidapi.JNIBridge, Androidapi.JNI.Provider,
  Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net,
  FMX.Platform.Android,
  DW.Androidapi.JNI.Os;

const
  cPermissionReadExternalStorage = 'android.permission.READ_EXTERNAL_STORAGE';
  cPermissionWriteExternalStorage = 'android.permission.WRITE_EXTERNAL_STORAGE';
  cPermissionCamera = 'android.permission.CAMERA';

{$IF CompilerVersion > 32}
type
  TGrantResults = TArray<TPermissionStatus>;

  TGrantResultsHelper = record helper for TGrantResults
  public
    function AreAllGranted: Boolean;
  end;

{ TGrantResultsHelper }

function TGrantResultsHelper.AreAllGranted: Boolean;
var
  LStatus: TPermissionStatus;
begin
  for LStatus in Self do
  begin
    if LStatus <> TPermissionStatus.Granted then
      Exit(False); // <======
  end;
  Result := True;
end;
{$ENDIF}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, ResultNotificationMessageHandler);
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, ResultNotificationMessageHandler);
  inherited;
end;

procedure TForm1.GetEXIF(const AFileName: JString);
var
  LEXIF: JExifInterface;
  LLatLong: TJavaArray<Single>;
  LStream: JFileInputStream;
begin
  LEXIF := TJExifInterface.JavaClass.init(AFileName);
  Memo1.Lines.Add('Date Taken: ' + JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_DATETIME)));
  Memo1.Lines.Add('Camera Make: ' + JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MAKE)));
  Memo1.Lines.Add('Camera Model: ' + JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MODEL)));
  LLatLong := TJavaArray<Single>.Create(2);
  try
    if LEXIF.getLatLong(LLatLong) then
    begin
      Memo1.Lines.Add('Latitude: ' +  LLatLong.Items[0].ToString);
      Memo1.Lines.Add('Longitude: ' +  LLatLong.Items[1].ToString);
    end;
  finally
    LLatLong.Free;
  end;
end;

procedure TForm1.ResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
var
  LMessage: TMessageResultNotification;
begin
  if M is TMessageResultNotification then
  begin
    LMessage := TMessageResultNotification(M);
    if LMessage.RequestCode = 1001 then
    begin
      GetEXIF(FFileName);
      Image1.Bitmap.LoadFromFile(JStringToString(FFileName));
    end;
  end;
end;

procedure TForm1.TakePhotoPermissionsResultHandler(Sender: TObject; const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
begin
  if TGrantResults(AGrantResults).AreAllGranted then
    TakePhoto
  else
    ShowMessage('Not all photo permissions granted!');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
{$IF CompilerVersion > 32}
  TPermissionsService.DefaultService.RequestPermissions([cPermissionReadExternalStorage, cPermissionWriteExternalStorage, cPermissionCamera],
    TakePhotoPermissionsResultHandler);
{$ELSE}
  TakePhoto;
{$ENDIF}
end;

// Based on: https://developer.android.com/training/camera/photobasics#java
procedure TForm1.TakePhoto;
var
  LIntent: JIntent;
  LFile, LDir: JFile;
  LUri: Jnet_Uri;
  LFileName: string;
begin
  LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_IMAGE_CAPTURE);
  if LIntent.resolveActivity(TAndroidHelper.Context.getPackageManager) <> nil then
  begin
    LFileName := Format('JPEG_%s', [FormatDateTime('yyyymmdd_hhnnss', Now)]);
    LDir := TAndroidHelper.Context.getExternalFilesDir(TJEnvironment.JavaClass.DIRECTORY_PICTURES);
    LFile := TJFile.JavaClass.createTempFile(StringToJString(LFileName), StringToJString('.jpg'), LDir);
    FFileName := LFile.getAbsolutePath;
    {$IF CompilerVersion > 32}
    LUri := TAndroidHelper.JFileToJURI(LFile);
    {$ELSE}
    LUri := TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.init(FFileName));
    {$ENDIF}
    LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_OUTPUT, TJParcelable.Wrap((LUri as ILocalObject).GetObjectID));
    MainActivity.startActivityForResult(LIntent, 1001);
  end
  else
    ShowMessage('Cannot take a photo!');
end;

end.

