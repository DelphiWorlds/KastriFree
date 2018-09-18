unit LS.Config;

interface

type
  TLocationConfig = class(TObject)
  private
    class function GetFileName: string;
    class function GetJSON: string;
  private
    FIsPaused: Boolean;
  public
    class function GetConfig: TLocationConfig;
  public
    procedure Save;
    property IsPaused: Boolean read FIsPaused write FIsPaused;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  REST.Json;

{ TLocationConfig }

class function TLocationConfig.GetConfig: TLocationConfig;
var
  LJSON: string;
begin
  Result := nil;
  LJSON := GetJSON;
  if not LJSON.IsEmpty then
    Result := TJson.JsonToObject<TLocationConfig>(LJSON);
  if Result = nil then
    Result := TLocationConfig.Create;
end;

class function TLocationConfig.GetFileName: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'LocationConfig.json');
end;

class function TLocationConfig.GetJSON: string;
begin
  if TFile.Exists(GetFileName) then
    Result := TFile.ReadAllText(GetFileName)
  else
    Result := '';
end;

procedure TLocationConfig.Save;
begin
  TFile.WriteAllText(GetFileName, TJson.ObjectToJsonString(Self));
end;

end.
