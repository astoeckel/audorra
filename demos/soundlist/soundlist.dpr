program soundlist;

uses
  Windows,
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  AllocConsole;

  Writeln('Test');

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);


  Application.Run;
end.
