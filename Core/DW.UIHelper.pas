unit DW.UIHelper;

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
  System.Types,
  // FMX
  FMX.Forms;

type
  /// <summary>
  ///   Helper functions specific to UI
  /// </summary>
  TUIHelper = record
  public
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; static;
    /// <summary>
    ///   Force a repaint of the form
    /// </summary>
    class procedure Render(const AForm: TForm); static;
  end;

implementation

{$IF Defined(ANDROID)}
uses
  DW.UIHelper.Android;
{$ELSEIF Defined(IOS)}
uses
  DW.UIHelper.iOS;
{$ENDIF}

{ TUIHelper }

class function TUIHelper.GetOffsetRect: TRectF;
begin
  {$IF Defined(IOS) or Defined(Android)}
  Result := TPlatformUIHelper.GetOffsetRect;
  {$ELSE}
  Result := RectF(0, 0, 0, 0);
  {$ENDIF}
end;

class procedure TUIHelper.Render(const AForm: TForm);
begin
  // Should not be required in other OS's
  {$IF Defined(Android)}
  TPlatformUIHelper.Render(AForm);
  {$ENDIF}
end;

end.
