unit DW.UIHelper.Android;

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
  FMX.Types, FMX.Forms;

type
  /// <summary>
  ///   Helper functions specific to UI
  /// </summary>
  TPlatformUIHelper = record
  public
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; overload; static;
    class function GetOffsetRect(const AHandle: TWindowHandle): TRectF; overload; static;
    class procedure Render(const AForm: TForm); static;
  end;

implementation

uses
  FMX.Platform.UI.Android;

{ TPlatformUIHelper }

class function TPlatformUIHelper.GetOffsetRect: TRectF;
begin
  // Yet to be implemented. Work is in progress
  Result := TRectF.Empty;
end;

class function TPlatformUIHelper.GetOffsetRect(const AHandle: TWindowHandle): TRectF;
begin
  // Yet to be implemented. Work is in progress
  Result := TRectF.Empty;
end;

class procedure TPlatformUIHelper.Render(const AForm: TForm);
begin
  TAndroidWindowHandle(AForm.Handle).Render.Render;
end;

end.
