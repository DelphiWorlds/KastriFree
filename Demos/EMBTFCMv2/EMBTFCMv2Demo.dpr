program EMBTFCMv2Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  DW.PushUDP,
  EF2.View.Main in 'Views\EF2.View.Main.pas' {MainView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPushUDP, PushUDP);
  Application.CreateForm(TMainView, MainView);
  Application.Run;
end.
