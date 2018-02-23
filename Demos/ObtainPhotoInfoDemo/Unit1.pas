unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
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
    FFileName: string;
    procedure GetEXIF(const AFileName: string);
    procedure ResultNotificationMessageHandler(const Sender: TObject; const M: TMessage);
    procedure TakePhoto;
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
  Androidapi.Helpers, Androidapi.JNI.Media, Androidapi.JNIBridge, Androidapi.JNI.Provider, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText,
  FMX.Platform.Android;

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

procedure TForm1.GetEXIF(const AFileName: string);
var
  LEXIF: JExifInterface;
  LLatLong: TJavaArray<Single>;
begin
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(AFileName));
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
      Image1.Bitmap.LoadFromFile(FFileName);
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TakePhoto;
end;

procedure TForm1.TakePhoto;
var
  LIntent: JIntent;
  LUri: Jnet_Uri;
begin
  // Using code to do basically what EMBT's FMX java code does, however this code does not manipulate the resulting image, so EXIF data is retained
  FFileName := TPath.ChangeExtension(TPath.GetTempFileName, '.jpg');
  LUri := TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.init(StringToJString(FFileName)));
  LIntent := TJIntent.JavaClass.init(TJMediaStore.JavaClass.ACTION_IMAGE_CAPTURE);
  LIntent.putExtra(TJMediaStore.JavaClass.EXTRA_OUTPUT, TJParcelable.Wrap((LUri as ILocalObject).GetObjectID));
  MainActivity.startActivityForResult(LIntent, 1001);
end;

end.
