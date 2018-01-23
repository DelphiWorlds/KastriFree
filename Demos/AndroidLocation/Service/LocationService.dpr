program LocationService;

uses
  System.Android.ServiceApplication,
  LS.ServiceModule in 'LS.ServiceModule.pas' {ServiceModule: TAndroidService},
  DW.AppLog in '..\..\..\..\..\Private\Kastri\Core\DW.AppLog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
