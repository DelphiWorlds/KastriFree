unit DW.VirtualKeyboardRect.Android;

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
  System.Types, System.Messaging,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge;

type
  TVirtualKeyboardRect = class;

  TGlobalLayoutListener = class(TJavaLocal, JViewTreeObserver_OnGlobalLayoutListener)
  private
    FVirtualKeyboardRect: TVirtualKeyboardRect;
  public
    { JViewTreeObserver_OnGlobalLayoutListener }
    procedure onGlobalLayout; cdecl;
  public
    constructor Create(const AVirtualKeyboardRect: TVirtualKeyboardRect);
  end;

  TVirtualKeyboardRect = class(TInterfacedObject)
  private
    FDecorView: JView;
    FListener: JViewTreeObserver_OnGlobalLayoutListener;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure InstallListener;
    procedure RemoveListener;
    procedure UpdateDecorView;
  protected
    procedure GlobalLayoutChange;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.JNI.Util,
  // FMX
  FMX.Platform, FMX.Platform.Android,
  // DW
  DW.Messaging;

{ TGlobalLayoutListener }

constructor TGlobalLayoutListener.Create(const AVirtualKeyboardRect: TVirtualKeyboardRect);
begin
  inherited Create;
  FVirtualKeyboardRect := AVirtualKeyboardRect;
end;

procedure TGlobalLayoutListener.onGlobalLayout;
begin
  FVirtualKeyboardRect.GlobalLayoutChange;
end;

{ TVirtualKeyboardRect }

constructor TVirtualKeyboardRect.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TVirtualKeyboardRect.Destroy;
begin
  if FListener <> nil then
    RemoveListener;
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TVirtualKeyboardRect.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      if FListener = nil then
        InstallListener;
    end;
  end;
end;

procedure TVirtualKeyboardRect.GlobalLayoutChange;
var
  LVisibleRect: JRect;
  LMetrics: JDisplayMetrics;
  LKeyboardRect: TRect;
begin
  UpdateDecorView;
  LVisibleRect := TJRect.Create;
  FDecorView.getWindowVisibleDisplayFrame(LVisibleRect);
  LMetrics := FDecorView.getContext.getResources.getDisplayMetrics;
  LKeyboardRect := TRectF.Create(ConvertPixelToPoint(TPointF.Create(0, LVisibleRect.height)),
    ConvertPixelToPoint(TPointF.Create(LMetrics.widthPixels, LMetrics.heightPixels - LVisibleRect.top))).Truncate;
  TMessageManager.DefaultManager.SendMessage(Self, TVirtualKeyboardRectChangeMessage.Create(LKeyboardRect));
end;

procedure TVirtualKeyboardRect.InstallListener;
begin
  UpdateDecorView;
  FListener := TGlobalLayoutListener.Create(Self);
  FDecorView.getViewTreeObserver.addOnGlobalLayoutListener(FListener);
end;

procedure TVirtualKeyboardRect.RemoveListener;
begin
  UpdateDecorView;
  FDecorView.getViewTreeObserver.removeGlobalOnLayoutListener(FListener);
end;

procedure TVirtualKeyboardRect.UpdateDecorView;
begin
  if FDecorView = nil then
    FDecorView := TAndroidHelper.Activity.getWindow.getDecorView;
end;

var
  VirtualKeyboardRect: TVirtualKeyboardRect;

initialization
  VirtualKeyboardRect := TVirtualKeyboardRect.Create;

end.
