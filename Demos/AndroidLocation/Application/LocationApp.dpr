program LocationApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  LA.MainFrm in 'LA.MainFrm.pas' {frmMain},
  LS.ServiceModule in '..\Service\LS.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
