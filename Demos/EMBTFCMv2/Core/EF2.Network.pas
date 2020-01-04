unit EF2.Network;

interface

uses
  System.SysUtils, System.Classes, IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient, FMX.Types;

type
  TNetwork = class(TDataModule)
    UDPClient: TIdUDPClient;
    UDPTimer: TTimer;
    procedure UDPTimerTimer(Sender: TObject);
  private
    FDeviceID: string;
    FToken: string;
    procedure SendDeviceInfo;
  public
    procedure UpdateDeviceInfo(const ADeviceID, AToken: string);
  end;

var
  Network: TNetwork;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.JSON;

procedure TNetwork.UDPTimerTimer(Sender: TObject);
begin
  if not FToken.IsEmpty and not FDeviceID.IsEmpty then
    SendDeviceInfo;
end;

procedure TNetwork.SendDeviceInfo;
var
  LJSON: TJSONObject;
  LOS: string;
begin
  if TOSVersion.Platform = TOSVersion.TPlatform.pfiOS then
    LOS := 'IOS'
  else if TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid then
    LOS := 'Android'
  else
    LOS := 'Unknown';
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('deviceid', FDeviceID);
    LJSON.AddPair('token', FToken);
    LJSON.AddPair('os', LOS);
    UDPClient.Broadcast(LJSON.ToJSON, UDPClient.Port);
  finally
    LJSON.Free;
  end;
end;

procedure TNetwork.UpdateDeviceInfo(const ADeviceID, AToken: string);
begin
  FDeviceID := ADeviceID;
  FToken := AToken;
end;

end.
