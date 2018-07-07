program VKVertScrollboxDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {frmMain},
  DW.VKVertScrollbox in '..\..\ComponentHelpers\DW.VKVertScrollbox.pas',
  OldDemoFrm in 'OldDemoFrm.pas' {frmOldDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  // Application.CreateForm(TfrmOldDemo, frmOldDemo);
  Application.Run;
end.
