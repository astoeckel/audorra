program soundscape;

uses
  Windows, Forms,
  umain in 'umain.pas' {frmmain},
  uplaygroundclasses in 'uplaygroundclasses.pas';

{$R *.res}

begin
  AllocConsole;
  
  Application.Initialize;
  Application.Title := 'Audorra Soundscape';
  Application.CreateForm(Tfrmmain, frmmain);
  Application.Run;
end.
