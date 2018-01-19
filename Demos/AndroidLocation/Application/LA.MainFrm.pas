unit LA.MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Android.Service,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Location,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.TabControl,
  DW.MultiReceiver.Android,
  LS.ServiceModule;

type
  TMessageReceivedEvent = procedure(Sender: TObject; const Msg: string) of object;

  /// <summary>
  ///   Acts as a receiver of local broadcasts sent by the service
  /// </summary>
  TServiceMessageReceiver = class(TMultiReceiver)
  private
    FOnMessageReceived: TMessageReceivedEvent;
    procedure DoMessageReceived(const AMsg: string);
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
  end;

  TfrmMain = class(TForm)
    TitleLabel: TLabel;
    MessagesMemo: TMemo;
    BottomLayout: TLayout;
    ClearButton: TButton;
    TabControl: TTabControl;
    MessagesTab: TTabItem;
    LogTab: TTabItem;
    LogMemo: TMemo;
    RefreshLogButton: TButton;
    procedure TabControlChange(Sender: TObject);
    procedure RefreshLogButtonClick(Sender: TObject);
  private
    FReceiver: TServiceMessageReceiver;
    procedure RefreshLog;
    procedure ServiceMessageHandler(Sender: TObject; const AMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  DW.OSLog,
  LS.Consts;

{ TServiceMessageReceiver }

procedure TServiceMessageReceiver.ConfigureActions;
begin
  // Adds the appropriate action to the intent filter so that messages are received
  IntentFilter.addAction(StringToJString(cServiceMessageAction));
end;

procedure TServiceMessageReceiver.DoMessageReceived(const AMsg: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AMsg);
end;

procedure TServiceMessageReceiver.Receive(context: JContext; intent: JIntent);
begin
  // Retrieves the message from the intent
  DoMessageReceived(JStringToString(intent.getStringExtra(StringToJString(cServiceMessageParamMessage))));
end;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  TabControl.ActiveTab := MessagesTab;
  FReceiver := TServiceMessageReceiver.Create(True);
  FReceiver.OnMessageReceived := ServiceMessageHandler;
  TLocalServiceConnection.StartService('LocationService');
end;

destructor TfrmMain.Destroy;
begin
  FReceiver.Free;
  inherited;
end;

procedure TfrmMain.RefreshLog;
begin
  // The service writes certain messages to the log - this routine loads the log with those messages
  LogMemo.Lines.LoadFromFile(TPath.Combine(TPath.GetDocumentsPath, 'Location.log'));
end;

procedure TfrmMain.RefreshLogButtonClick(Sender: TObject);
begin
  RefreshLog;
end;

procedure TfrmMain.ServiceMessageHandler(Sender: TObject; const AMsg: string);
begin
  MessagesMemo.Lines.Add(AMsg);
end;

procedure TfrmMain.TabControlChange(Sender: TObject);
begin
  if TabControl.ActiveTab = LogTab then
    RefreshLog;
end;

end.
