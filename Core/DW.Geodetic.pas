unit DW.Geodetic;

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
  TGeodetic = record
  public
    /// <summary>
    ///   Calculates distance between two locations, in metres
    /// </summary>
    /// <remarks>
    ///   At present, untested. Result may vary between OS's, and might change in future if their algorithm is changed
    /// </remarks>
    class function DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double; static;
  end;

implementation

uses
  {$IF Defined(ANDROID)}
  DW.Geodetic.Android;
  {$ELSEIF Defined(IOS)}
  DW.Geodetic.iOS;
  {$ELSE}
  DW.Geodetic.Default;
  {$ENDIF}

{ TGeodetic }

class function TGeodetic.DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double;
begin
  Result := TPlatformGeodetic.DistanceBetween(ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo);
end;

end.
