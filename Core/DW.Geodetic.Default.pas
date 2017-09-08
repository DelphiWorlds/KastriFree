unit DW.Geodetic.Default;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  TPlatformGeodetic = record
  public
    class function DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double; static;
  end;

implementation

uses
  // RTL
  System.Math;

{ TPlatformGeodetic }

class function TPlatformGeodetic.DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double;
const
  cDiameter = 2 * 6372.8; // Might need to check the radius value
var
  LDX, LDY, LDZ: double;
  LLatRadFrom, LLatRadTo, LLongRadDiff: Double;
begin
  LLongRadDiff := DegToRad(ALongitudeFrom - ALongitudeTo);
  LLatRadFrom := DegToRad(ALatitudeFrom);
  LLatRadTo := DegToRad(ALatitudeTo);
  LDZ := Sin(LLatRadFrom) - Sin(LLatRadTo);
  LDX := Cos(LLongRadDiff) * Cos(LLatRadFrom) - Cos(LLatRadTo);
  LDY := Sin(LLongRadDiff) * Cos(LLatRadFrom);
  Result := ArcSin(Sqrt(Sqr(LDX) + Sqr(LDY) + Sqr(LDZ)) / 2) * cDiameter;
end;

end.
