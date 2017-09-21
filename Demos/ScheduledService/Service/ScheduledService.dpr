program ScheduledService;

uses
  System.Android.ServiceApplication,
  ScheduledServiceModule in 'ScheduledServiceModule.pas' {ServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
