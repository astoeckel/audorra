program custom_audio;

{$APPTYPE CONSOLE}

uses
  Main in 'Main.pas';

{$R ..\icon.RES}

var
  app: TAuApp;
begin
  app := TAuApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
end.
