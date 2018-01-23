program LocationService;

uses
  System.Android.ServiceApplication,
  LS.ServiceModule in 'LS.ServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
