program KastriFree;

uses
  System.StartUpCopy,
  FMX.Forms,
  DW.DragDropLayout in '..\..\ComponentHelpers\DW.DragDropLayout.pas',
  DW.DragListenerListView in '..\..\ComponentHelpers\DW.DragListenerListView.pas',
  DW.ElasticLayout in '..\..\ComponentHelpers\DW.ElasticLayout.pas',
  DW.Base64.Helpers in '..\..\Core\DW.Base64.Helpers.pas',
  DW.Forms.Helpers in '..\..\Core\DW.Forms.Helpers.pas',
  DW.Geodetic in '..\..\Core\DW.Geodetic.pas',
  DW.Listeners in '..\..\Core\DW.Listeners.pas',
  DW.Messaging in '..\..\Core\DW.Messaging.pas',
  DW.Patch in '..\..\Core\DW.Patch.pas',
  DW.Precompile in '..\..\Core\DW.Precompile.pas',
  DW.REST.Json.Helpers in '..\..\Core\DW.REST.Json.Helpers.pas',
  DW.Services in '..\..\Core\DW.Services.pas',
  DW.VirtualKeyboard.Helpers in '..\..\Core\DW.VirtualKeyboard.Helpers.pas',
  DW.OSLog in '..\..\Core\DW.OSLog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
