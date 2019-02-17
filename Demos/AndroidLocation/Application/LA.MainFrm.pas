unit LA.MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions, System.Messaging,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.JNI.Location,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.TabControl, FMX.ActnList, FMX.Objects,
  DW.MultiReceiver.Android, DW.PermissionsRequester, DW.PermissionsTypes;

type
  TMessageReceivedEvent = procedure(Sender: TObject; const Msg: string) of object;

  /// <summary>
  ///   Acts as a receiver of local broadcasts sent by the service
  /// </summary>
  TLocalReceiver = class(TMultiReceiver)
  private
    FOnMessageReceived: TMessageReceivedEvent;
    FOnStatus: TNotifyEvent;
    procedure DoMessageReceived(const AMsg: string);
    procedure DoStatus;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnStatus: TNotifyEvent read FOnStatus write FOnStatus;
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
    ActionList: TActionList;
    PauseUpdatesAction: TAction;
    RefreshLogAction: TAction;
    BackgroundRectangle: TRectangle;
    PauseUpdatesButton: TButton;
    procedure TabControlChange(Sender: TObject);
    procedure RefreshLogActionExecute(Sender: TObject);
    procedure PauseUpdatesActionExecute(Sender: TObject);
  private
    FPermissions: TPermissionsRequester;
    FReceiver: TLocalReceiver;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure EnablePauseUpdates;
    function IsPaused: Boolean;
    function IsServiceRunning: Boolean;
    procedure RefreshLog;
    procedure PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
    procedure ServiceMessageHandler(Sender: TObject; const AMsg: string);
    procedure ServiceStatusHandler(Sender: TObject);
    procedure SendCommand(const ACommand: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Android.Service,
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  FMX.Platform,
  DW.OSLog, DW.OSDevice, DW.Androidapi.JNI.LocalBroadcastManager, DW.Android.Helpers,
  LS.Consts, LS.Config;

{ TLocalReceiver }

procedure TLocalReceiver.ConfigureActions;
begin
  // Adds the appropriate actions to the intent filter..
  IntentFilter.addAction(StringToJString(cServiceStatusAction));
  IntentFilter.addAction(StringToJString(cServiceMessageAction));
end;

procedure TLocalReceiver.DoMessageReceived(const AMsg: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AMsg);
end;

procedure TLocalReceiver.DoStatus;
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self);
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  TOSLog.d('TLocalReceiver.Receive');
  if intent.getAction.equals(StringToJString(cServiceStatusAction)) then
    DoStatus
  else if intent.getAction.equals(StringToJString(cServiceMessageAction)) then
    DoMessageReceived(JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamMessage))));
end;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  TabControl.ActiveTab := MessagesTab;
  FPermissions := TPermissionsRequester.Create;
  FPermissions.OnPermissionsResult := PermissionsResultHandler;
  FReceiver := TLocalReceiver.Create(True);
  FReceiver.OnMessageReceived := ServiceMessageHandler;
  FReceiver.OnStatus := ServiceStatusHandler;
  if not IsServiceRunning then
    TLocalServiceConnection.StartService('LocationService');
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TfrmMain.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FPermissions.Free;
  FReceiver.Free;
  inherited;
end;

function TfrmMain.IsPaused: Boolean;
var
  LConfig: TLocationConfig;
begin
  Result := True;
  LConfig := TLocationConfig.GetConfig;
  if LConfig <> nil then
  try
    Result := LConfig.IsPaused;
  finally
    LConfig.Free;
  end;
end;

function TfrmMain.IsServiceRunning: Boolean;
begin
  Result := TAndroidHelperEx.IsServiceRunning('com.embarcadero.services.LocationService');
end;

procedure TfrmMain.EnablePauseUpdates;
begin
  PauseUpdatesAction.Enabled := True;
  if IsPaused then
    PauseUpdatesAction.Text := 'Resume Updates'
  else
    PauseUpdatesAction.Text := 'Pause Updates';
end;

procedure TfrmMain.SendCommand(const ACommand: Integer);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceCommandAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamCommand), ACommand);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TfrmMain.PauseUpdatesActionExecute(Sender: TObject);
begin
  if IsPaused then
    FPermissions.RequestPermissions([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation], cRequestCodeLocation)
  else
    SendCommand(cServiceCommandPause);
end;

procedure TfrmMain.PermissionsResultHandler(Sender: TObject; const ARequestCode: Integer; const AResults: TPermissionResults);
begin
  case ARequestCode of
    cRequestCodeLocation:
    begin
      if AResults.AreAllGranted then
        SendCommand(cServiceCommandResume);
    end;
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

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      // Assume the service is not running until the app "hears" from it
      PauseUpdatesAction.Enabled := False;
      SendCommand(cServiceCommandAppBecameActive);
    end;
    TApplicationEvent.EnteredBackground:
    begin
      SendCommand(cServiceCommandAppEnteredBackground);
    end;
  end;
end;

procedure TfrmMain.ServiceMessageHandler(Sender: TObject; const AMsg: string);
begin
  MessagesMemo.Lines.Add(AMsg);
end;

procedure TfrmMain.ServiceStatusHandler(Sender: TObject);
begin
  // If a status message is received from the service it *is* obviously running (IsServiceRunning may return false if it is still starting)
  EnablePauseUpdates;
end;

procedure TfrmMain.TabControlChange(Sender: TObject);
begin
  if TabControl.ActiveTab = LogTab then
    RefreshLog;
end;

end.
