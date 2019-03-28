unit DW.UIHelper.iOS;

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
  System.Types;

type
  /// <summary>
  ///   Helper functions specific to UI
  /// </summary>
  TPlatformUIHelper = record
  public
    /// <summary>
    ///   Special function for handling of "notch" based devices
    /// </summary>
    class function GetOffsetRect: TRectF; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.ObjectiveC, Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation,
  // FMX
  FMX.Forms, FMX.Platform.iOS;

type
  UIView = interface(iOSapi.UIKit.UIView)
    ['{9E246E80-7773-400C-8027-9FF0FA6FFA3E}']
    function safeAreaInsets: UIEdgeInsets; cdecl;
  end;
  TUIView = class(TOCGenericImport<UIViewClass, UIView>)  end;

class function TPlatformUIHelper.GetOffsetRect: TRectF;
var
  LInsets: UIEdgeInsets;
begin
  Result := TRectF.Empty;
  if TOSVersion.Check(11) and (Application.MainForm <> nil) then
  begin
    LInsets := TUIView.Wrap(NSObjectToID(WindowHandleToPlatform(Application.MainForm.Handle).View)).safeAreaInsets;
    Result := RectF(LInsets.left, LInsets.top, LInsets.right, LInsets.bottom);
  end;
end;

end.
