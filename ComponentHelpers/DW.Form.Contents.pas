unit DW.Form.Contents;

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
  // RTL
  System.Classes,
  // FMX
  FMX.Forms;

type
  /// <summary>
  ///   Interposer class for TForm that allows the contents to be displayed in a control (such as TTabItem)
  /// </summary>
  /// <remarks>
  ///   Expects a TLayout to exist on the form called RootLayout
  /// </remarks>
  TForm = class(FMX.Forms.TForm)
  private
    FIsShown: Boolean;
    procedure ReparentRootLayout(AOwner: TComponent);
  protected
    procedure FirstShow; virtual;
    property IsShown: Boolean read FIsShown;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Sets IsShown to true. Used internally in Delphi Worlds projects
    /// </summary>
    procedure Shown;
  end;

implementation

uses
  FMX.Types, FMX.Controls;

{ TForm }

constructor TForm.Create(AOwner: TComponent);
begin
  // Passing Application to the inherited Create works around a problem on (at least) Android
  inherited Create(Application);
  ReparentRootLayout(AOwner);
end;

procedure TForm.ReparentRootLayout(AOwner: TComponent);
var
  LControl: TControl;
begin
  LControl := TControl(FindComponent('RootLayout'));
  if LControl <> nil then
  begin
    LControl.Align := TAlignLayout.Contents;
    LControl.Parent := TFmxObject(AOwner);
  end;
end;

procedure TForm.Shown;
begin
  if not FIsShown then
    FirstShow;
  FIsShown := True;
end;

procedure TForm.FirstShow;
begin
  //
end;

end.
