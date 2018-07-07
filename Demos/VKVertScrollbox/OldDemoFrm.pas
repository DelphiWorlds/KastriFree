unit OldDemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.TabControl,
  FMX.Layouts, FMX.Memo, FMX.Edit, FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox,
  DW.VKVertScrollbox;

type
  TfrmOldDemo = class(TForm)
    tbaMain: TToolBar;
    tabMain: TTabControl;
    tiConfig: TTabItem;
    tiMain: TTabItem;
    lblTitle: TLabel;
    lsbInfo: TListBox;
    lbghInfo: TListBoxItem;
    lbiInfoCode: TListBoxItem;
    edtInfoCode: TEdit;
    lbiProjectDescription: TListBoxItem;
    mmoInfoDescription: TMemo;
    lblInfoDescription: TLabel;
    vsbConfig: TVertScrollBox;
    layConfig: TLayout;
    lsbConfig: TListBox;
    lbghSomeCombos: TListBoxItem;
    lbiCombo1: TListBoxItem;
    ComboBox1: TComboBox;
    lbiCombo2: TListBoxItem;
    ComboBox2: TComboBox;
    lbiCombo3: TListBoxItem;
    ComboBox3: TComboBox;
    lbghSwitch: TListBoxItem;
    lbiSwitch1: TListBoxItem;
    Switch1: TSwitch;
    lbghEditAndSwitch: TListBoxItem;
    lbiEdit1: TListBoxItem;
    Edit1: TEdit;
    lbiSwitch2: TListBoxItem;
    Switch2: TSwitch;
    MainVertScrollBox: TVertScrollBox;
    MainLayout: TLayout;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    procedure mmoInfoDescriptionChangeTracking(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmOldDemo: TfrmOldDemo;

implementation

{$R *.fmx}

constructor TfrmOldDemo.Create(AOwner: TComponent);
begin
  inherited;
  MainVertScrollBox.ControlsLayout := MainLayout;
  vsbConfig.ControlsLayout := layConfig;
end;

destructor TfrmOldDemo.Destroy;
begin
  //
  inherited;
end;

procedure TfrmOldDemo.mmoInfoDescriptionChangeTracking(Sender: TObject);
begin
  MainVertScrollBox.ControlsChanged;
end;

end.
