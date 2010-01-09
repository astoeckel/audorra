program soundlist;

uses
  Windows,
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Audorra Soundlist Demo';
  Application.CreateForm(TForm1, Form1);
  
  Application.Run;
end.
