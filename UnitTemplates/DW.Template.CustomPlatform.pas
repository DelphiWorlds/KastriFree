unit DW.Template.CustomPlatform;

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
  TTemplate = class;

  TCustomPlatformTemplate = class(TObject)
  private
    FTemplate: TTemplate;
  protected
    property Template: TTemplate read FTemplate;
  public
    constructor Create(const ATemplate: TTemplate); virtual;
    destructor Destroy; override;
  end;

  TTemplate = class(TObject)
  private
    FPlatformTemplate: TCustomPlatformTemplate;
  protected
    //
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Template.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Template.Android;
  {$ELSE}
  DW.Template.Default;
  {$ENDIF}

{ TCustomPlatformTemplate }

constructor TCustomPlatformTemplate.Create(const ATemplate: TTemplate);
begin
  inherited Create;
  FTemplate := ATemplate;
end;

destructor TCustomPlatformTemplate.Destroy;
begin
  //
  inherited;
end;

{ TTemplate }

constructor TTemplate.Create;
begin
  inherited;
  FPlatformTemplate := TPlatformTemplate.Create(Self);
end;

destructor TTemplate.Destroy;
begin
  FPlatformTemplate.Free;
  inherited;
end;

end.
