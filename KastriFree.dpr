program KastriFree;

uses
  System.StartUpCopy,
  FMX.Forms,
  DW.Firebase.InstanceId in 'Features\Firebase\DW.Firebase.InstanceId.pas',
  DW.Firebase.Messaging in 'Features\Firebase\DW.Firebase.Messaging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
