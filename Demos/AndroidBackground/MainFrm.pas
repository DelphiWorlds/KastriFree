unit MainFrm;

// Demo of ensuring that timers (and hopefully other code) will work when the app is in the background,
//   and (hopefully) also when the device is in "Doze" mode (yet to be tested for this)
//
// ***** THIS IS A WORK IN PROGRESS *****

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.Edit;

type
  TfrmMain = class(TForm)
    Timer1: TTimer;
    Edit1: TEdit;
    procedure Timer1Timer(Sender: TObject);
  private
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  FMX.Platform,
  DW.Android.Helpers;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  TAndroidHelperEx.RestartIfNotIgnoringBatteryOptimizations;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TfrmMain.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Tag := Timer1.Tag + 1;
  Edit1.Text := Timer1.Tag.ToString;
end;

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  if M is TApplicationEventMessage then
  begin
    case TApplicationEventMessage(M).Value.Event of
      TApplicationEvent.BecameActive:
        TAndroidHelperEx.EnableWakeLock(False);
      TApplicationEvent.WillBecomeInactive:
        TAndroidHelperEx.EnableWakeLock(True);
    end;
  end;
end;

end.
