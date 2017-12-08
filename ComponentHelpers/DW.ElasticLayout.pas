unit DW.ElasticLayout;

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
  // FMX
  FMX.Types, FMX.Controls, FMX.Layouts;

type
  /// <summary>
  ///   Layout that adjusts its size to accomodate the *commonly aligned* controls within it
  /// </summary>
  /// <remarks>
  ///   Place the unit name *after* FMX.Layouts in the uses clause of the unit where you have layouts that you want to be elastic
  /// </remarks>
  TLayout = class(FMX.Layouts.TLayout)
  private
    FIsElastic: Boolean;
    procedure AdjustSize(const AAdjustWidth: Boolean);
    function GetCommonAlign: TAlignLayout;
    function GetControlSize(const AControl: TControl; const AGetWidth: Boolean): Single;
    procedure SetIsElastic(const Value: Boolean);
  protected
    procedure DoRealign; override;
  public
    /// <summary>
    ///   Set this property to True when you want the layout to adjust its size automatically
    /// </summary>
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
  end;

implementation

{ TLayout }

procedure TLayout.AdjustSize(const AAdjustWidth: Boolean);
var
  LSize: Single;
  I: Integer;
begin
  LSize := 0;
  for I := 0 to ControlsCount - 1 do
    LSize := LSize + GetControlSize(Controls[I], AAdjustWidth);
  if AAdjustWidth then
    Width := LSize
  else
    Height := LSize;
end;

procedure TLayout.DoRealign;
begin
  case GetCommonAlign of
    TAlignLayout.Left, TAlignLayout.Right:
      AdjustSize(True);
    TAlignLayout.Top, TAlignLayout.Bottom:
      AdjustSize(False);
  end;
  inherited;
end;

function TLayout.GetCommonAlign: TAlignLayout;
var
  I: Integer;
begin
  Result := TAlignLayout.None;
  for I := 0 to ControlsCount - 1 do
  begin
    if not Controls[I].Visible or (Controls[I].Align = TAlignLayout.Contents) then
      Continue;
    if (Result = TAlignLayout.None) and (Controls[I].Align <> TAlignLayout.None) then
      Result := Controls[I].Align;
    if (Result <> TAlignLayout.None) and (Controls[I].Align <> Result)  then
      Exit(TAlignLayout.None);
  end;
end;

function TLayout.GetControlSize(const AControl: TControl; const AGetWidth: Boolean): Single;
begin
  if not AControl.Visible or (AControl.Align = TAlignLayout.Contents) then
    Exit(0); // <======
  if AGetWidth then
    Result := AControl.Width + AControl.Margins.Left + AControl.Margins.Right
  else
    Result := AControl.Height + AControl.Margins.Top + AControl.Margins.Bottom;
end;

procedure TLayout.SetIsElastic(const Value: Boolean);
begin
  if Value = FIsElastic then
    Exit; // <======
  FIsElastic := Value;
  Realign;
end;

end.
