unit DW.Services;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Common
  System.Generics.Collections;

type
  TServiceList = TDictionary<TGUID, IInterface>;

  /// <summary>
  ///   Registry of services used by an application. Similar to FMX.Platform.TPlatformServices
  /// </summary>
  TServices = class(TObject)
  private
    class var FServices: TServices;
    class constructor Initialise;
    class destructor Finalise;
  private
    FList: TServiceList;
  protected
    property List: TServiceList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Adds a service to the list. First checks if the service has already been added
    /// </summary>
    class procedure Add(const AServiceGUID: TGUID; const AService: IInterface);
    /// <summary>
    ///   Retrieves a service from the list
    /// </summary>
    class function Get(const AServiceGUID: TGUID; out AService): Boolean;
    /// <summary>
    ///   Removes a service from the list
    /// </summary>
    class procedure Remove(const AServiceGUID: TGUID);
  end;

implementation

uses
  // Common
  System.SysUtils;

{ TServices }

constructor TServices.Create;
begin
  inherited;
  FList := TDictionary<TGUID, IInterface>.Create;
end;

destructor TServices.Destroy;
begin
  FList.Free;
  inherited;
end;

class constructor TServices.Initialise;
begin
  FServices := TServices.Create;
end;

class destructor TServices.Finalise;
begin
  FServices.Free;
end;

class procedure TServices.Add(const AServiceGUID: TGUID; const AService: IInterface);
begin
  if not FServices.List.ContainsKey(AServiceGUID) then
    FServices.List.Add(AServiceGUID, AService);
end;

class function TServices.Get(const AServiceGUID: TGUID; out AService): Boolean;
begin
  if not FServices.List.ContainsKey(AServiceGUID) then
  begin
    Pointer(AService) := nil;
    Result := False;
  end
  else
    Result := Supports(FServices.List.Items[AServiceGUID], AServiceGUID, AService);
end;

class procedure TServices.Remove(const AServiceGUID: TGUID);
begin
  FServices.List.Remove(AServiceGUID);
end;

end.
