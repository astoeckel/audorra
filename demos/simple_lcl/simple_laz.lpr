program simple_laz;

{$mode objfpc}{$H+}

{$IFDEF WIN32}
  {$APPTYPE CONSOLE}
  {$R ..\ICON.RES}
{$ENDIF}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, LResources
  { you can add units after this }, main_laz;

{$IFDEF WINDOWS}{$R simple_laz.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

