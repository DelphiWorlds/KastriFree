unit DW.FileWriter;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.Classes;

type
  /// <summary>
  ///   Specialised TStreamWriter that writes to a file
  /// </summary>
  /// <remarks>
  ///   Set the AutoFlush property (of TStreamWriter) to True for immediate writes
  /// </remarks>
  TFileWriter = class(TStreamWriter)
  private
    FStream: TStream;
  public
    constructor Create(const Filename: string; Append: Boolean = False); overload; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TFileWriter }

constructor TFileWriter.Create(const Filename: string; Append: Boolean);
var
  LShareMode: Word;
begin
  if TOSVersion.Platform <> TOSVersion.TPlatform.pfiOS then
    LShareMode := fmShareDenyWrite
  else
    LShareMode := 0;
  if (not TFile.Exists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate or LShareMode)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite or LShareMode);
    FStream.Seek(0, soEnd);
  end;
  inherited Create(FStream);
end;

destructor TFileWriter.Destroy;
begin
  FStream.Free;
  inherited;
end;

end.
