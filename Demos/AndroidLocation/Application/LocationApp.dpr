program LocationApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  LA.MainFrm in 'LA.MainFrm.pas' {frmMain},
  LS.ServiceModule in '..\Service\LS.ServiceModule.pas' {ServiceModule: TAndroidService},
  LS.Config in '..\Common\LS.Config.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
