unit MainFrm;

// Simple demo of showing a "pseudo modal" that does not cover the entire screen

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  DialogFrm;

type
  TfrmMain = class(TForm)
    ShowDialogButton: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure ShowDialogButtonClick(Sender: TObject);
  private
    // Because of the asynchronous nature of the dialog, a reference to the dialog instance needs to be held
    FDialog: TfrmDialog;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FDialog := TfrmDialog.Create(Application);
end;

procedure TfrmMain.ShowDialogButtonClick(Sender: TObject);
begin
  // The ShowDialog method is declared on the TfrmDialog class
  FDialog.ShowDialog(
    procedure(AResult: TModalResult)
    begin
      if AResult = mrOK then
        Edit1.Text := 'OK clicked'
      else
        Edit1.Text := 'Cancel clicked';
    end
  );
end;

end.
