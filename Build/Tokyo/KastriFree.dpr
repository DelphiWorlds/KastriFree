program KastriFree;

uses
  System.StartUpCopy,
  FMX.Forms,
  DW.ElasticLayout in '..\..\ComponentHelpers\DW.ElasticLayout.pas',
  DW.Canvas.GPU.Helpers in '..\..\Core\DW.Canvas.GPU.Helpers.pas',
  DW.Geodetic in '..\..\Core\DW.Geodetic.pas',
  DW.Messaging in '..\..\Core\DW.Messaging.pas',
  DW.OSLog in '..\..\Core\DW.OSLog.pas',
  DW.Patch in '..\..\Core\DW.Patch.pas',
  DW.Precompile in '..\..\Core\DW.Precompile.pas',
  DW.REST.Json.Helpers in '..\..\Core\DW.REST.Json.Helpers.pas',
  DW.Services in '..\..\Core\DW.Services.pas',
  DW.VirtualKeyboard.Helpers in '..\..\Core\DW.VirtualKeyboard.Helpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
