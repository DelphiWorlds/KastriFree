unit DW.FaderRectangle;

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
  FMX.Objects, FMX.Controls;

type
  /// <summary>
  ///   Interposer class for fade in/out and integration with busy indicator (Enabled property should turn the indicator on/off)
  /// </summary>
  TRectangle = class(FMX.Objects.TRectangle)
  protected
    procedure Click; override;
  public
    /// <summary>
    ///  Shows and enables, or disables and hides the indicator, and fades the rectangle in/out if required
    /// </summary>
    procedure Busy(const AIsBusy: Boolean; const AIndicator: TControl = nil; const AFade: Boolean = True; const AFadeInOpacity: Single = 0.4);
    /// <summary>
    ///  Fades the rectangle in/out and enables the hit test so that OnClick can be used (when faded in)
    /// </summary>
    procedure Fade(const AFadeIn: Boolean; const AFadeInOpacity: Single = 0.4); overload;
    procedure Fade(const AFadeIn: Boolean; const AClickHandler: TNotifyEvent; const AFadeInOpacity: Single = 0.4); overload;
  end;

implementation

uses
  // RTL
  System.UITypes,
  // FMX
  FMX.Ani, FMX.StdCtrls;

{ TRectangle }

procedure TRectangle.Busy(const AIsBusy: Boolean; const AIndicator: TControl = nil; const AFade: Boolean = True; const AFadeInOpacity: Single = 0.4);
begin
  Enabled := not AIsBusy;
  if AIsBusy and AFade then
    Fade(True, AFadeInOpacity);
  if AIndicator <> nil then
  begin
    AIndicator.Enabled := AIsBusy;
    AIndicator.Visible := AIsBusy;
    AIndicator.BringToFront;
  end;
  if not AIsBusy and (Opacity > 0) then
    Fade(False);
end;

procedure TRectangle.Fade(const AFadeIn: Boolean; const AFadeInOpacity: Single = 0.4);
var
  LFinalValue: Single;
begin
  Visible := True;
  BringToFront;
  HitTest := AFadeIn;
  Fill.Color := TAlphaColorRec.Black;
  if AFadeIn then
    LFinalValue := AFadeInOpacity
  else
    LFinalValue := 0;
  TAnimator.AnimateFloat(Self, 'Opacity', LFinalValue);
end;

procedure TRectangle.Click;
begin
  Fade(False);
  inherited;
end;

procedure TRectangle.Fade(const AFadeIn: Boolean; const AClickHandler: TNotifyEvent; const AFadeInOpacity: Single = 0.4);
begin
  OnClick := AClickHandler;
  Fade(AFadeIn, AFadeInOpacity);
end;

end.
