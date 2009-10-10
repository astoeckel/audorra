program Simple;

{$APPTYPE CONSOLE}

uses
  Forms,
  Main in 'Main.pas' {Form1},
  wnd_OpenURL in 'wnd_OpenURL.pas' {OpenURLWnd};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOpenURLWnd, OpenURLWnd);
  Application.Run;
end.

