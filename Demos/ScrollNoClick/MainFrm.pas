unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TMouseInfo = record
    Down: Boolean;
    DownPt: TPointF;
    Moved: Boolean;
    procedure MouseDown(const X, Y: Single);
    procedure MouseMove(const X, Y: Single);
    procedure MouseUp;
  end;

  TButton = class(FMX.StdCtrls.TButton)
  private
    FMouseInfo: TMouseInfo;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

  TfrmMain = class(TForm)
    MessagesMemo: TMemo;
    VertScrollBox: TVertScrollBox;
  private
    procedure ControlClickHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{ TMouseInfo }

procedure TMouseInfo.MouseDown(const X, Y: Single);
begin
  Down := True;
  Moved := False;
  DownPt := PointF(X, Y);
end;

procedure TMouseInfo.MouseMove(const X, Y: Single);
begin
  if Down and not Moved then
    Moved := (Abs(X - DownPt.X) > 10) or (Abs(Y - DownPt.Y) > 10);
end;

procedure TMouseInfo.MouseUp;
begin
  Down := False;
end;

{ TButton }

procedure TButton.Click;
begin
  if not FMouseInfo.Moved then
    inherited;
end;

procedure TButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseInfo.MouseDown(X, Y);
end;

procedure TButton.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseInfo.MouseMove(X, Y);
end;

procedure TButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseInfo.MouseUp;
end;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
var
  I: Integer;
  LButton: TButton;
begin
  inherited;
  for I := 0 to 29 do
  begin
    LButton := TButton.Create(Self);
    LButton.Name := 'Button' + (I + 1).ToString;
    LButton.Width := 120;
    LButton.Height := 32;
    LButton.Position.X := (Width - LButton.Width) / 2;
    LButton.Position.Y := I * 80;
    LButton.OnClick := ControlClickHandler;
    LButton.Parent := VertScrollBox;
  end;
end;

procedure TfrmMain.ControlClickHandler(Sender: TObject);
begin
  MessagesMemo.Lines.Add(TComponent(Sender).Name + ' was clicked');
end;

end.
