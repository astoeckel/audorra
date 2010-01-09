program simple;

{$APPTYPE CONSOLE}

uses
  Forms,
  Main in 'Main.pas' {frmPlayer},
  wnd_OpenURL in 'wnd_OpenURL.pas' {OpenURLWnd};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPlayer, frmPlayer);
  Application.CreateForm(TOpenURLWnd, OpenURLWnd);
  Application.Run;
end.

