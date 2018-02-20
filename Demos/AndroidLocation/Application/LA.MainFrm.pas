unit LA.MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Android.Service, System.Actions, System.Messaging,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Location,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.TabControl, FMX.ActnList,
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
    TabControl: TTabControl;
    MessagesTab: TTabItem;
    LogTab: TTabItem;
    LogMemo: TMemo;
    RefreshLogButton: TButton;
    PauseUpdatesButton: TButton;
    ActionList: TActionList;
    PauseUpdatesAction: TAction;
    RefreshLogAction: TAction;
    procedure TabControlChange(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure PauseUpdatesActionExecute(Sender: TObject);
    procedure RefreshLogActionExecute(Sender: TObject);
  private
    FReceiver: TServiceMessageReceiver;
    [Unsafe] FService: TServiceModule; // <--- This is a reference to the actual service datamodule
    FServiceConnection: TLocalServiceConnection;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure RefreshLog;
    procedure ServiceConnectedHandler(const ALocalService: TAndroidBaseService);  // <--- Event called once the service is connected
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
  FMX.Platform,
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
  FServiceConnection := TLocalServiceConnection.Create;
  FServiceConnection.OnConnected := ServiceConnectedHandler;
  FServiceConnection.StartService('LocationService');
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TfrmMain.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FReceiver.Free;
  inherited;
end;

procedure TfrmMain.PauseUpdatesActionExecute(Sender: TObject);
begin
  if FService <> nil then
  begin
    if FService.IsPaused then
      FService.Resume
    else
      FService.Pause;
  end;
end;

procedure TfrmMain.RefreshLog;
begin
  // The service writes certain messages to the log - this routine loads the log with those messages
  LogMemo.Lines.LoadFromFile(TPath.Combine(TPath.GetDocumentsPath, 'Location.log'));
end;

procedure TfrmMain.RefreshLogActionExecute(Sender: TObject);
begin
  RefreshLog;
end;

procedure TfrmMain.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  PauseUpdatesAction.Enabled := FService <> nil;
  if FService <> nil then
  begin
    if FService.IsPaused then
      PauseUpdatesAction.Text := 'Resume Updates'
    else
      PauseUpdatesAction.Text := 'Pause Updates';
  end;
  Handled := True;
end;

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
      // Bind the service, so an reference to the service datamodule can be obtained
      FServiceConnection.BindService('LocationService');
    TApplicationEvent.EnteredBackground:
    begin
      // Make sure to "unbind" when the app becomes inactive!
      FServiceConnection.UnbindService;
      FService := nil;
    end;
  end;
end;

procedure TfrmMain.ServiceConnectedHandler(const ALocalService: TAndroidBaseService);
begin
  // Obtain a reference to the service datamodule
  FService := TServiceModule(ALocalService);
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
