unit DialogFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Objects;

type
  TfrmDialog = class(TForm)
    DialogLayout: TLayout;
    FaderRectangle: TRectangle;
    CenterLayout: TPanel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    ButtonsLayout: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FResultMethod: TProc<TModalResult>;
    procedure CloseDialog(const AResult: TModalResult);
  public
    procedure ShowDialog(const AResultMethod: TProc<TModalResult> = nil);
  end;

var
  frmDialog: TfrmDialog;

implementation

{$R *.fmx}

uses
  FMX.Ani;

{ TfrmDialog }

procedure TfrmDialog.ShowDialog(const AResultMethod: TProc<TModalResult> = nil);
begin
  // A fader is used to convey to the user that the controls under it are not accessible
  FaderRectangle.Opacity := 0;
  FResultMethod := AResultMethod;
  // Instead of showing the whole form, DialogLayout is just parented to the active form..
  DialogLayout.Parent := Screen.ActiveForm;
  // ..then the fader is "faded in"
  TAnimator.AnimateFloat(FaderRectangle, 'Opacity', 0.3);
end;

procedure TfrmDialog.CloseDialog(const AResult: TModalResult);
begin
  // The result method is called if one was passed in..
  if Assigned(FResultMethod) then
    FResultMethod(AResult);
  // ..and the DialogLayout is "unparented"
  DialogLayout.Parent := nil;
end;

procedure TfrmDialog.OKButtonClick(Sender: TObject);
begin
  CloseDialog(mrOk);
end;

procedure TfrmDialog.CancelButtonClick(Sender: TObject);
begin
  CloseDialog(mrCancel);
end;

end.
