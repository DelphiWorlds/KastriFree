program RichEditFMXDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DW.RichEdit in '..\..\Controls\DW.RichEdit.pas',
  DW.RichEdit.Win in '..\..\Controls\DW.RichEdit.Win.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
