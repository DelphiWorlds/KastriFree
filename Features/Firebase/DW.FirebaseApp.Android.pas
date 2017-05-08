unit DW.FirebaseApp.Android;

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
  Androidapi.Helpers,
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
