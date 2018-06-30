unit DW.PermissionsRequester.Default;

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
  // DW
  DW.PermissionsRequester;

type
  TPlatformPermissionsRequester = class(TCustomPlatformPermissionsRequester)
  protected
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer); override;
  end;

implementation

{ TPlatformPermissionsRequester }

procedure TPlatformPermissionsRequester.RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
begin
  // Default implementation does nothing
end;

end.
