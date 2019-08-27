program LocationService;

uses
  System.Android.ServiceApplication,
  LS.ServiceModule in 'LS.ServiceModule.pas' {ServiceModule: TAndroidService},
  DW.Consts.Android in '..\..\..\Core\DW.Consts.Android.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
