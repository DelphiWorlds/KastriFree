program KastriFree;

uses
  System.StartUpCopy,
  FMX.Forms,
  DW.OSLog in 'Core\DW.OSLog.pas',
  DW.REST.Json.Helpers in 'Core\DW.REST.Json.Helpers.pas',
  DW.Services in 'Core\DW.Services.pas',
  DW.ElasticLayout in 'ComponentHelpers\DW.ElasticLayout.pas',
  DW.VirtualKeyboard.Helpers in 'Core\DW.VirtualKeyboard.Helpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
