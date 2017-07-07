unit DW.VirtualKeyboard.Helpers;

interface

uses
  FMX.Types;

type
  TVirtualKeyboard = record
  public
    class procedure Hide; static;
    class procedure Show(const AObject: TFmxObject); static;
  end;

implementation

uses
  FMX.Platform, FMX.VirtualKeyboard;

{ TVirtualKeyboard }

class procedure TVirtualKeyboard.Hide;
var
  LService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, LService) then
    LService.HideVirtualKeyboard;
end;

class procedure TVirtualKeyboard.Show(const AObject: TFmxObject);
var
  LService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, LService) then
    LService.ShowVirtualKeyboard(AObject);
end;

end.
