unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.Patch;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Text := 'Button1 Clicked';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := ApplicationIdleHandler;
end;

procedure TForm1.ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
begin
  Label1.Text := 'OnIdle Called';
end;

end.
