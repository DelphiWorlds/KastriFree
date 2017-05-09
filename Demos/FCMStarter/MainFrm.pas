unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  DW.Firebase.InstanceId, DW.Firebase.Messaging;

type
  TfrmMain = class(TForm)
    FirebaseCMLabel: TLabel;
    TokenLabel: TLabel;
    TokenMemo: TMemo;
    MessagesLabel: TLabel;
    MessagesMemo: TMemo;
  private
    FInstanceId: TFirebaseInstanceId;
    FMessaging: TFirebaseMessaging;
    FToken: string;
    procedure InstanceIdTokenRefreshHandler(Sender: TObject; const AToken: string);
    procedure MessagingMessageReceivedHandler(Sender: TObject; const APayload: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  // DW
  DW.OSLog;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceId := TFirebaseInstanceId.Create;
  FInstanceId.OnTokenRefresh := InstanceIdTokenRefreshHandler;
  FMessaging := TFirebaseMessaging.Create;
  FMessaging.OnMessageReceived := MessagingMessageReceivedHandler;
  // The first time the app is run, token will be blank at this point, however the OnTokenRefreshHandler will be called
  TokenMemo.Lines.Text :=  FInstanceId.Token;
  TOSLog.d('Token at startup: %s', [TokenMemo.Lines.Text]);
  FMessaging.Connect;
end;

destructor TfrmMain.Destroy;
begin
  FInstanceId.Free;
  FMessaging.Free;
  inherited;
end;

procedure TfrmMain.InstanceIdTokenRefreshHandler(Sender: TObject; const AToken: string);
begin
  TokenMemo.Lines.Text := AToken;
  TOSLog.d('Token in OnTokenRefresh: %s', [TokenMemo.Lines.Text]);
end;

procedure TfrmMain.MessagingMessageReceivedHandler(Sender: TObject; const APayload: TStrings);
begin
  MessagesMemo.Lines.AddStrings(APayload);
end;

end.
