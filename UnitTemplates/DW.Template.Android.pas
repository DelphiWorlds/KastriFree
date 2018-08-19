unit DW.Template.Android;

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
  DW.Template.CustomPlatform;

type
  TPlatformTemplate = class(TCustomPlatformTemplate)
  private
    //
  protected
    //
  public
    constructor Create(const ATemplate: TTemplate); override;
    destructor Destroy; override;
  end;

implementation

{ TPlatformTemplate }

constructor TPlatformTemplate.Create(const ATemplate: TTemplate);
begin
  inherited;
  //
end;

destructor TPlatformTemplate.Destroy;
begin
  //
  inherited;
end;

end.
