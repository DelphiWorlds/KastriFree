unit DW.FirebaseApp.Android;

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
  TPlatformFirebaseApp = class(TObject)
  private
    class var FStarted: Boolean;
  public
    class procedure Start;
  end;

implementation

uses
  // Android
  Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.Firebase;

{ TPlatformFirebaseApp }

class procedure TPlatformFirebaseApp.Start;
begin
  if not FStarted then
  begin
    TJFirebaseApp.JavaClass.initializeApp(TAndroidHelper.Context);
    FStarted := True;
  end;
end;

end.
