unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus, System.ImageList, FMX.ImgList,
  DW.StatusBarMenu.Mac;

type
  TfrmMain = class(TForm)
    ImageList: TImageList;
    StatusBarPopupMenu: TPopupMenu;
    TestMenuItem: TMenuItem;
    StatusBarMenuItem: TMenuItem;
    SeparatorMenuItem1: TMenuItem;
    QuitMenuItem: TMenuItem;
    procedure QuitMenuItemClick(Sender: TObject);
    procedure TestMenuItemClick(Sender: TObject);
  private
    FStatusBarMenu: TStatusBarMenu;
  protected
    function CanShow: Boolean; override;
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
  FStatusBarMenu := TStatusBarMenu.Create(Self);
  FStatusBarMenu.PopupMenu := StatusBarPopupMenu;
end;

function TfrmMain.CanShow: Boolean;
begin
  Result := False; // Prevent this form from showing
end;

procedure TfrmMain.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.TestMenuItemClick(Sender: TObject);
begin
  ShowMessage('Test');
end;

end.
