unit DW.PermissionsTypes {$IF CompilerVersion > 32} deprecated 'use System.Permissions'{$ENDIF};

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  TPermissionResult = record
    Permission: string;
    Granted: Boolean
  end;

  TPermissionResults = TArray<TPermissionResult>;

  TPermissionResultsHelper = record helper for TPermissionResults
  public
    function Add(const AValue: TPermissionResult): Integer;
    function AreAllGranted: Boolean;
    procedure Clear;
    function Count: Integer;
    function DeniedResults: TPermissionResults;
  end;

implementation

{ TPermissionResultsHelper }

function TPermissionResultsHelper.Add(const AValue: TPermissionResult): Integer;
begin
  SetLength(Self, Count + 1);
  Self[Count - 1] := AValue;
  Result := Count - 1;
end;

function TPermissionResultsHelper.AreAllGranted: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    if not Self[I].Granted then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TPermissionResultsHelper.Clear;
begin
  SetLength(Self, 0);
end;

function TPermissionResultsHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TPermissionResultsHelper.DeniedResults: TPermissionResults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if not Self[I].Granted then
      Result.Add(Self[I]);
  end;
end;

end.
