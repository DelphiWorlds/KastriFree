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
  System.Classes, System.SysUtils, System.SyncObjs, System.Generics.Collections,
  // Indy
  IdTCPClient;

type
  // Based partly on: https://forums.embarcadero.com/message.jspa?messageID=773729&tstart=0
  // PROBLEM!!!! This code can be Windows-only because of WaitForMultiple - check http://seanbdurkin.id.au/pascaliburnus2/archives/230
  TErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TExceptionEvent = procedure(Sender: TObject; const E: Exception) of object;
  TResponseEvent = procedure(Sender: TObject; const Code: Integer; const Response: string) of object;
  TReceiveDataEvent = procedure(Sender: TObject; const Data: TBytes) of object;
  TCmdEvent = procedure(Sender: TObject; const Cmd: string) of object;

  TClientState = (None, Connecting, Disconnecting, Sending, Receiving, ConnectError);

  TSendQueue = class(TThreadList<string>)
  protected
    function Count: Integer;
    procedure Push(const ACmd: string);
    function Pop: string;
  end;

  TCustomThreadedTCPClient = class(TThread)
  private
    FClientState: TClientState;
    FConnectEvent: TEvent;
    FDisconnectEvent: TEvent;
    FEvents: THandleObjectArray;
    FIsConnected: Boolean;
    FIsSynchronized: Boolean;
    FSendQueue: TSendQueue;
    FTCPClient: TIdTCPClient;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnReceiveData: TReceiveDataEvent;
    FOnResponse: TResponseEvent;
    FOnSendingCmd: TCmdEvent;
    procedure ConnectClient;
    procedure DisconnectClient;
    procedure DoConnected;
    procedure DoDisconnected;
    procedure DoException(const AException: Exception);
    procedure DoReceiveData(const AData: TBytes);
    procedure DoResponse(const ACode: Integer; const AResponse: string);
    procedure DoSendingCommand(const ACmd: string);
    function GetConnectTimeout: Integer;
    function GetHost: string;
    function GetIsConnected: Boolean;
    function GetPort: Integer;
    function GetReadTimeout: Integer;
    procedure HandleException(const AException: Exception);
    function InternalConnect: Boolean;
    procedure InternalDisconnect;
    procedure InternalSendCmd;
    function IsMainThread: Boolean;
    procedure ReadData;
    procedure ReadDataFromBuffer;
    procedure SendCmdFromClient(const ACmd: string);
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetPort(const Value: Integer);
    procedure SetReadTimeout(const Value: Integer);
    procedure SetHost(const Value: string);
    procedure TCPClientConnectedHandler(Sender: TObject);
    procedure TCPClientDisconnectedHandler(Sender: TObject);
  protected
    procedure Execute; override;
    procedure InternalDoConnected; virtual;
    procedure InternalDoDisconnected; virtual;
    procedure InternalDoException(const AException: Exception); virtual;
    procedure InternalDoReceiveData(const AData: TBytes); virtual;
    procedure InternalDoResponse(const ACode: Integer; const AResponse: string); virtual;
    procedure InternalDoSendingCmd(const ACmd: string); virtual;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    property OnResponse: TResponseEvent read FOnResponse write FOnResponse;
    property OnSendingCmd: TCmdEvent read FOnSendingCmd write FOnSendingCmd;
  public
    constructor Create;
    destructor Destroy; override;
    function CanConnect: Boolean;
    procedure Connect;
    procedure Disconnect;
    procedure SendCmd(const ACmd: string); virtual;
    property ClientState: TClientState read FClientState;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property Host: string read GetHost write SetHost;
    property IsConnected: Boolean read GetIsConnected;
    property IsSynchronized: Boolean read FIsSynchronized write FIsSynchronized;
    property Port: Integer read GetPort write SetPort;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  end;

  TThreadedTCPClient = class(TCustomThreadedTCPClient)
  public
    property OnConnected;
    property OnDisconnected;
    property OnException;
    property OnReceiveData;
    property OnResponse;
    property OnSendingCmd;
  end;

implementation

uses
  // Indy
  IdGlobal, IdExceptionCore,
  // DW
  DW.OSLog;

  { TSendQueue }

function TSendQueue.Count: Integer;
var
  LList: TList<string>;
begin
  LList := LockList;
  try
    Result := LList.Count;
  finally
    UnlockList;
  end;
end;

function TSendQueue.Pop: string;
var
  LList: TList<string>;
begin
  LList := LockList;
  try
    Result := LList[0];
    LList.Delete(0);
  finally
    UnlockList;
  end;
end;

procedure TSendQueue.Push(const ACmd: string);
var
  LList: TList<string>;
begin
  LList := LockList;
  try
    LList.Add(ACmd);
  finally
    UnlockList;
  end;
end;

{ TCustomThreadedTCPClient }

constructor TCustomThreadedTCPClient.Create;
begin
  inherited Create;
  // FIsSynchronized := True;
  FSendQueue := TSendQueue.Create;
  FTCPClient := TIdTCPClient.Create(nil);
  FTCPClient.ConnectTimeout := 5000;
  FTCPClient.ReadTimeout := 5000;
  FTCPClient.OnConnected := TCPClientConnectedHandler;
  FTCPClient.OnDisconnected := TCPClientDisconnectedHandler;
  FConnectEvent := TEvent.Create(nil, True, False, '');
  FDisconnectEvent := TEvent.Create(nil, True, False, '');
  FEvents := [FConnectEvent, FDisconnectEvent];
end;

destructor TCustomThreadedTCPClient.Destroy;
begin
  FConnectEvent.Free;
  FDisconnectEvent.Free;
  FTCPClient.Free;
  FSendQueue.Free;
  inherited;
end;

function TCustomThreadedTCPClient.GetConnectTimeout: Integer;
begin
  Result := FTCPClient.ConnectTimeout;
end;

function TCustomThreadedTCPClient.GetHost: string;
begin
  Result := FTCPClient.Host;
end;

function TCustomThreadedTCPClient.GetIsConnected: Boolean;
begin
  Result := FIsConnected;
end;

function TCustomThreadedTCPClient.GetPort: Integer;
begin
  Result := FTCPClient.Port;
end;

function TCustomThreadedTCPClient.GetReadTimeout: Integer;
begin
  Result := FTCPClient.ReadTimeout;
end;

procedure TCustomThreadedTCPClient.SetConnectTimeout(const Value: Integer);
begin
  FTCPClient.ConnectTimeout := Value;
end;

procedure TCustomThreadedTCPClient.SetHost(const Value: string);
begin
  FTCPClient.Host := Value;
end;

procedure TCustomThreadedTCPClient.SetPort(const Value: Integer);
begin
  FTCPClient.Port := Value;
end;

procedure TCustomThreadedTCPClient.SetReadTimeout(const Value: Integer);
begin
  FTCPClient.ReadTimeout := Value;
end;

function TCustomThreadedTCPClient.IsMainThread: Boolean;
begin
  Result := TThread.CurrentThread.ThreadID = MainThreadID;
end;

procedure TCustomThreadedTCPClient.TCPClientConnectedHandler(Sender: TObject);
begin
  FIsConnected := True;
  DoConnected;
end;

procedure TCustomThreadedTCPClient.TCPClientDisconnectedHandler(Sender: TObject);
begin
  TOSLog.d('TCustomThreadedTCPClient.TCPClientDisconnectedHandler');
  DoDisconnected;
end;

function TCustomThreadedTCPClient.InternalConnect: Boolean;
begin
  FConnectEvent.ResetEvent;
  Result := FIsConnected;
  if not Result then
  begin
    FClientState := TClientState.Connecting;
    try
      ConnectClient;
      Result := FIsConnected;
    finally
      FClientState := TClientState.None;
    end;
  end;
end;

procedure TCustomThreadedTCPClient.DoConnected;
begin
  if FIsSynchronized and not IsMainThread then
    Synchronize(InternalDoConnected)
  else
    InternalDoConnected;
end;

procedure TCustomThreadedTCPClient.InternalDoConnected;
begin
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TCustomThreadedTCPClient.ConnectClient;
begin
  try
    FTCPClient.Connect;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TCustomThreadedTCPClient.InternalDisconnect;
begin
  FDisconnectEvent.ResetEvent;
  FClientState := TClientState.Disconnecting;
  try
    DisconnectClient;
  finally
    FClientState := TClientState.None;
  end;
end;

procedure TCustomThreadedTCPClient.DisconnectClient;
begin
  try
    FTCPClient.Disconnect;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TCustomThreadedTCPClient.DoDisconnected;
begin
  FIsConnected := False;
  if FIsSynchronized and not IsMainThread then
    Synchronize(InternalDoDisconnected)
  else
    InternalDoDisconnected;
end;

procedure TCustomThreadedTCPClient.InternalDoDisconnected;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TCustomThreadedTCPClient.InternalSendCmd;
var
  LCmd: string;
begin
  LCmd := FSendQueue.Pop;
  if InternalConnect then
  begin
    FClientState := TClientState.Sending;
    try
      SendCmdFromClient(LCmd);
    finally
      FClientState := TClientState.None;
    end;
  end;
end;

procedure TCustomThreadedTCPClient.SendCmdFromClient(const ACmd: string);
begin
  try
    DoSendingCommand(ACmd);
  except
    on E: Exception do
      HandleException(E);
  end;
  try
    FTCPClient.SendCmd(ACmd);
    if not Terminated then
      DoResponse(FTCPClient.LastCmdResult.NumericCode, FTCPClient.LastCmdResult.Text.Text);
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TCustomThreadedTCPClient.ReadData;
begin
  if FTCPClient.IOHandler <> nil then
  begin
    if not FTCPClient.IOHandler.ClosedGracefully then
    begin
      try
        ReadDataFromBuffer;
      except
        on E: Exception do
          HandleException(E);
      end;
    end
    else
      DoDisconnected;
  end;
end;

procedure TCustomThreadedTCPClient.ReadDataFromBuffer;
var
  LData: TBytes;
begin
  if FTCPClient.IOHandler.CheckForDataOnSource(10) then
  begin
    // TOSLog.d('TCustomThreadedTCPClient.ReadDataFromBuffer - has data');
    FClientState := TClientState.Receiving;
    try
      FTCPClient.IOHandler.InputBuffer.ExtractToBytes(TIdBytes(LData));
    finally
      FClientState := TClientState.None;
    end;
    if not Terminated and (Length(LData) > 0) then
      DoReceiveData(LData)
    else if not Terminated then
      TOSLog.d('TCustomThreadedTCPClient.ReadDataFromBuffer - had data, but nothing there!');
  end;
end;

procedure TCustomThreadedTCPClient.HandleException(const AException: Exception);
begin
  TOSLog.d('TCustomThreadedTCPClient.HandleException: %s - %s', [AException.ClassName, AException.Message]);
  if AException.InheritsFrom(EIdSocksServerConnectionRefusedError) then
    FClientState := TClientState.ConnectError;
  DoException(AException);
end;

procedure TCustomThreadedTCPClient.DoException(const AException: Exception);
begin
  if FIsSynchronized and not IsMainThread then
  begin
    Synchronize(
      procedure
      begin
        InternalDoException(AException);
      end
    );
  end
  else
    InternalDoException(AException);
end;

procedure TCustomThreadedTCPClient.InternalDoException(const AException: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Self, AException);
end;

procedure TCustomThreadedTCPClient.DoResponse(const ACode: Integer; const AResponse: string);
begin
  if FIsSynchronized and not IsMainThread then
  begin
    Synchronize(
      procedure
      begin
        InternalDoResponse(ACode, AResponse);
      end
    );
  end
  else
    InternalDoResponse(ACode, AResponse);
end;

procedure TCustomThreadedTCPClient.DoSendingCommand(const ACmd: string);
begin
  if FIsSynchronized and not IsMainThread then
  begin
    Synchronize(
      procedure
      begin
        InternalDoSendingCmd(ACmd);
      end
    );
  end
  else
    InternalDoSendingCmd(ACmd);
end;

procedure TCustomThreadedTCPClient.InternalDoResponse(const ACode: Integer; const AResponse: string);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, ACode, AResponse);
end;

procedure TCustomThreadedTCPClient.InternalDoSendingCmd(const ACmd: string);
begin
  if Assigned(FOnSendingCmd) then
    FOnSendingCmd(Self, ACmd);
end;

procedure TCustomThreadedTCPClient.DoReceiveData(const AData: TBytes);
begin
  if FIsSynchronized and not IsMainThread then
    Synchronize(
      procedure
      begin
        InternalDoReceiveData(AData)
      end
    )
  else
    InternalDoReceiveData(AData);
end;

procedure TCustomThreadedTCPClient.InternalDoReceiveData(const AData: TBytes);
begin
  if Assigned(FOnReceiveData) then
    FOnReceiveData(Self, AData);
end;

procedure TCustomThreadedTCPClient.Execute;
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
    else if not Terminated then
    begin
      if FIsConnected then
        ReadData;
      if FSendQueue.Count > 0 then
        InternalSendCmd;
    end;
  end;
end;

function TCustomThreadedTCPClient.CanConnect: Boolean;
begin
  Result := not Host.IsEmpty and (Port > 0);
end;

procedure TCustomThreadedTCPClient.Connect;
begin
  FConnectEvent.SetEvent;
end;

procedure TCustomThreadedTCPClient.Disconnect;
begin
  FDisconnectEvent.SetEvent;
end;

procedure TCustomThreadedTCPClient.SendCmd(const ACmd: string);
begin
  FSendQueue.Push(ACmd);
end;

end.
