program simple_laz;

{$mode objfpc}{$H+}

{$IFDEF WIN32}
  {$APPTYPE CONSOLE}
  {$R ..\ICON.RES}
{$ENDIF}

uses
  (*{$IFDEF UNIX}
  cthreads,
  {$ENDIF}*)
  Interfaces, // this includes the LCL widgetset
  Forms, LResources
  { you can add units after this }, main_laz;

{$IFDEF WINDOWS}{$R simple_laz.rc}{$ENDIF}

begin
  {$I simple_laz.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

