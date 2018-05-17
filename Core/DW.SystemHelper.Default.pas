unit DW.SystemHelper.Default;

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
  DW.SystemHelper;

type
  TPlatformSystemHelper = class(TCustomPlatformSystemHelper)
  protected
    procedure RequestPermissions(const APermissions: array of string; const ARequestCode: Integer); override;
  end;

implementation

{ TPlatformSystemHelper }

procedure TPlatformSystemHelper.RequestPermissions(const APermissions: array of string; const ARequestCode: Integer);
begin
  // Default implementation does nothing
end;

end.
