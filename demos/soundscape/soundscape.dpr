program soundscape;

uses
  Forms,
  umain in 'umain.pas' {frmmain},
  uplaygroundclasses in 'uplaygroundclasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Audorra Soundscape';
  Application.CreateForm(Tfrmmain, frmmain);
  Application.Run;
end.
