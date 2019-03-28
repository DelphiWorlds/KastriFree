unit DW.ThreadedTCPClient;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// THIS IS A WORK IN PROGRESS - Apologies for the lack of documentation - coming soon

interface

uses
  // RTL
  System.Classes, System.SysUtils, System.SyncObjs,
  // Indy
  IdTCPClient;

type
  // Based partly on: https://forums.embarcadero.com/message.jspa?messageID=773729&tstart=0
  // PROBLEM!!!! This code can be Windows-only because of WaitForMultiple - check http://seanbdurkin.id.au/pascaliburnus2/archives/230
  TErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TExceptionEvent = procedure(Sender: TObject; const E: Exception) of object;
  TResponseEvent = procedure(Sender: TObject; const Code: Integer; const Response: string) of object;
  TReceiveDataEvent = procedure(Sender: TObject; const Data: TBytes) of object;

  TClientState = (None, Connecting, Disconnecting, Sending, Receiving);

  TThreadedTCPClient = class(TThread)
  private
    FClientState: TClientState;
    FCommand: string;
    FConnectEvent: TEvent;
    FData: TBytes;
    FDisconnectEvent: TEvent;
    FEvents: THandleObjectArray;
    FSendCmdEvent: TEvent;
    FTCPClient: TIdTCPClient;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnReceiveData: TReceiveDataEvent;
    FOnResponse: TResponseEvent;
    function ConnectClient: Boolean;
    procedure DoConnected;
    procedure DoDisconnected;
    procedure DoException(const AException: Exception);
    procedure DoReceiveData;
    procedure DoResponse(const ACode: Integer; const AResponse: string);
    function GetConnectTimeout: Integer;
    function GetHost: string;
    function GetIsConnected: Boolean;
    function GetPort: Integer;
    function GetReadTimeout: Integer;
    procedure HandleException(const AException: Exception);
    function InternalConnect: Boolean;
    procedure InternalDisconnect;
    procedure InternalSendCmd;
    procedure ReadData;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetPort(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
    procedure SetHost(const Value: string);
    procedure TCPClientDisconnectedHandler(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure SendCmd(const ACmd: string);
    property ClientState: TClientState read FClientState;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property Host: string read GetHost write SetHost;
    property IsConnected: Boolean read GetIsConnected;
    property Port: Integer read GetPort write SetPort;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    property OnResponse: TResponseEvent read FOnResponse write FOnResponse;
  end;

implementation

uses
  // Indy
  IdGlobal,
  // DW
  DW.OSLog;

{ TThreadedTCPClient }

constructor TThreadedTCPClient.Create;
begin
  inherited Create;
  FTCPClient := TIdTCPClient.Create(nil);
  FTCPClient.ConnectTimeout := 5000;
  FTCPClient.ReadTimeout := 5000;
  FTCPClient.OnDisconnected := TCPClientDisconnectedHandler;
  FConnectEvent := TEvent.Create(nil, True, False, '');
  FDisconnectEvent := TEvent.Create(nil, True, False, '');
  FSendCmdEvent := TEvent.Create(nil, True, False, '');
  FEvents := [FConnectEvent, FDisconnectEvent, FSendCmdEvent];
end;

destructor TThreadedTCPClient.Destroy;
begin
  Terminate;
  WaitFor;
  FTCPClient.Free;
  FConnectEvent.Free;
  FDisconnectEvent.Free;
  FSendCmdEvent.Free;
  inherited;
end;

function TThreadedTCPClient.GetConnectTimeout: Integer;
begin
  Result := FTCPClient.ConnectTimeout;
end;

function TThreadedTCPClient.GetHost: string;
begin
  Result := FTCPClient.Host;
end;

function TThreadedTCPClient.GetIsConnected: Boolean;
begin
  Result := FTCPClient.Connected;
end;

function TThreadedTCPClient.GetPort: Integer;
begin
  Result := FTCPClient.Port;
end;

function TThreadedTCPClient.GetReadTimeout: Integer;
begin
  Result := FTCPClient.ReadTimeout;
end;

procedure TThreadedTCPClient.SetConnectTimeout(const Value: Integer);
begin
  FTCPClient.ConnectTimeout := Value;
end;

procedure TThreadedTCPClient.SetHost(const Value: string);
begin
  FTCPClient.Host := Value;
end;

procedure TThreadedTCPClient.SetPort(const Value: Integer);
begin
  FTCPClient.Port := Value;
end;

procedure TThreadedTCPClient.SetReadTimeout(const Value: Integer);
begin
  FTCPClient.ReadTimeout := Value;
end;

procedure TThreadedTCPClient.TCPClientDisconnectedHandler(Sender: TObject);
begin
  if Assigned(FOnDisconnected) then
    DoDisconnected;
end;

function TThreadedTCPClient.InternalConnect: Boolean;
var
  LIsConnected: Boolean;
begin
  LIsConnected := FTCPClient.Connected;
  FConnectEvent.ResetEvent;
  Result := ConnectClient;
  if not LIsConnected and Result and Assigned(FOnConnected) then
    DoConnected;
end;

function TThreadedTCPClient.ConnectClient: Boolean;
begin
  Result := False;
  try
    if not FTCPClient.Connected then
    begin
      FClientState := TClientState.Connecting;
      FTCPClient.Connect;
    end;
    Result := FTCPClient.Connected;
    FClientState := TClientState.None;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TThreadedTCPClient.InternalDisconnect;
begin
  FDisconnectEvent.ResetEvent;
  FClientState := TClientState.Connecting;
  try
    FTCPClient.Disconnect;
    FClientState := TClientState.None;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TThreadedTCPClient.InternalSendCmd;
begin
  FSendCmdEvent.ResetEvent;
  if InternalConnect then
  begin
    TOSLog.d('FTCPClient.SendCmd(%s)', [FCommand]);
    FClientState := TClientState.Sending;
    try
      FTCPClient.SendCmd(FCommand);
      FClientState := TClientState.None;
    except
      on E: Exception do
        HandleException(E);
    end;
    if Assigned(FOnResponse) then
      DoResponse(FTCPClient.LastCmdResult.NumericCode, FTCPClient.LastCmdResult.Text.Text);
  end;
end;

procedure TThreadedTCPClient.ReadData;
begin
  SetLength(FData, 0);
  FClientState := TClientState.Receiving;
  try
    FTCPClient.IOHandler.ReadBytes(TIdBytes(FData), -1);
    FClientState := TClientState.None;
    if (Length(FData) > 0) and Assigned(FOnReceiveData) then
      DoReceiveData;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TThreadedTCPClient.DoConnected;
begin
  Queue(Self,
    procedure
    begin
      FOnConnected(Self);
    end
  );
end;

procedure TThreadedTCPClient.DoDisconnected;
begin
  Queue(Self,
    procedure
    begin
      FOnDisconnected(Self);
    end
  );
end;

procedure TThreadedTCPClient.HandleException(const AException: Exception);
begin
  try
    if Assigned(FOnException) then
      DoException(AException);
  finally
    FClientState := TClientState.None;
  end;
end;

procedure TThreadedTCPClient.DoException(const AException: Exception);
begin
  Synchronize(Self,
    procedure
    begin
      FOnException(Self, AException);
    end
  );
end;

procedure TThreadedTCPClient.DoReceiveData;
begin
  Queue(Self,
    procedure
    begin
      FOnReceiveData(Self, FData);
    end
  );
end;

procedure TThreadedTCPClient.DoResponse(const ACode: Integer; const AResponse: string);
begin
  Queue(Self,
    procedure
    begin
      FOnResponse(Self, ACode, AResponse);
    end
  );
end;

procedure TThreadedTCPClient.Execute;
var
  LSignaledEvent: THandleObject;
begin
  while not Terminated do
  begin
    LSignaledEvent := nil;
    TEvent.WaitForMultiple(FEvents, 20, False, LSignaledEvent);
    if LSignaledEvent = FDisconnectEvent then
      InternalDisconnect
    else if LSignaledEvent = FConnectEvent then
      InternalConnect
    else if LSignaledEvent = FSendCmdEvent then
      InternalSendCmd;
    if not Terminated and FTCPClient.Connected then
      ReadData;
  end;
end;

procedure TThreadedTCPClient.Connect;
begin
  FConnectEvent.SetEvent;
end;

procedure TThreadedTCPClient.Disconnect;
begin
  FDisconnectEvent.SetEvent;
end;

procedure TThreadedTCPClient.SendCmd(const ACmd: string);
begin
  FCommand := ACmd;
  FSendCmdEvent.SetEvent;
end;

end.
