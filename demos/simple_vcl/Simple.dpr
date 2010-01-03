program Simple;

{$APPTYPE CONSOLE}

uses
  Forms,
  Main in 'Main.pas' {frmPlayer},
  wnd_OpenURL in 'wnd_OpenURL.pas' {OpenURLWnd},
  AuCDAudio in '..\..\src\AuCDAudio.pas',
  win_cdrom in '..\..\lib\win_cdrom.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPlayer, frmPlayer);
  Application.CreateForm(TOpenURLWnd, OpenURLWnd);
  Application.Run;
end.

