program soundscape;

uses
  Forms,
  umain in 'umain.pas' {frmmain},
  uplaygroundclasses in 'uplaygroundclasses.pas',
  AuWin32Common in '..\..\src\drivers\AuWin32Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Audorra Soundscape';
  Application.CreateForm(Tfrmmain, frmmain);
  Application.Run;
end.
