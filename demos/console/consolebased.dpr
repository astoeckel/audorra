{*******************************************************}
{                                                       }
{       Audorra Digital Audio Library                   }
{       Copyright (c) Andreas Stöckel, 2009             }
{       Audorra is an "Andorra Suite" Project           }
{                                                       }
{*******************************************************}

{The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is
Andreas Stöckel. All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License license (the “GPL License”), in which case the provisions of
GPL License are applicable instead of those above. If you wish to allow use
of your version of this file only under the terms of the GPL License and not
to allow others to use your version of this file under the MPL, indicate your
decision by deleting the provisions above and replace them with the notice and
other provisions required by the GPL License. If you do not delete the
provisions above, a recipient may use your version of this file under either the
MPL or the GPL License.

File: console.dpr
Author: Andreas Stöckel
}

{Simple test program for using Audorra in a console. The compiler switch
 'DO_NOT_USE_VCL' has to be activated in order to make the program work properly.}
program consolebased;

{$IFDEF WIN32}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I audorra_conf.inc}

{$IFDEF WIN32}
  {$R ..\icon.RES}
{$ENDIF}

uses
  {$IFDEF LINUX}
  cthreads,
  {$ENDIF}
  SysUtils,
  AuAudio,
  AuTypes,
  AuAnalyzers,
  AuAcinerella,
  AuALSA,
  AcNotify;

var
  AuAudio: TAuAudio;
  AuPlayer: TAuPlayer;
  AuPeaks: TAuPeakMeter;
  i, j: integer;
  peaks: TAuPeaks;
  time: int64;
  tops: array of Integer;
  pos: Double;

const
  Day = 24 * 60 * 60 * 1000;
  PeakSize = 30;

{$IFDEF WINDOWS}{$R consolebased.rc}{$ENDIF}

begin
  //Use the manual notify mode
  AcNotifyManualInit();

  Writeln('Audorra Digital Audio Library - Console based application');
  Writeln('---------------------------------------------------------');
  Writeln;

  //Make sure that a file is specified and this file exists
  if (ParamCount < 1) or (not FileExists(ParamStr(1))) then
  begin
    WriteLn('No (existing) file specified!');
    exit;
  end;

  //Create the TAuAudio component
  AuAudio := TAuAudio.Create;

  //Initialize it
  if AuAudio.Initialize then
  begin
    AuPlayer := TAuPlayer.Create(AuAudio);

    //Create an peak meter analyzer
    AuPeaks := TAuPeakMeter.Create;
    AuPeaks.Active := true;
    AuPlayer.AddAnalyzer(AuPeaks);

    //Load...
    AuPlayer.LoadFromFile(ParamStr(1));
    //...open...
    if AuPlayer.Open then
    begin
      //...and play the file.
      AuPlayer.Play;

      //Prepare fore screen output
      WriteLn('Driver: ', AuAudio.Driver.ClassName);
      WriteLn('Device: ', AuAudio.Devices.ItemById[AuAudio.StandardDeviceID].Name);
      WriteLn('File: ', ExtractFileName(ParamStr(1)));
      WriteLn;

      SetLength(tops, AuPeaks.Parameters.Channels);
      for i := 0 to High(tops) do
        tops[i] := 0;
      time := 0;

      while not (AuPlayer.State = aupsOpened) do
      begin
        Write('|');
        for i := 0 to 20 do
        begin
          pos := AuPlayer.Position / AuPlayer.Len;
          if (i = trunc(pos * 20)) then
            Write('x')
          else
            Write('-');
        end;
        Write('|   ');
        Write('Pos: ', FormatDateTime('hh:mm:ss:zzz', AuPlayer.Position / Day ),'/', FormatDateTime('hh:mm:ss:zzz', AuPlayer.Len / Day ), #9);
        AuPeaks.GetPeaks(peaks);
        for i := 0 to AuPeaks.Parameters.Channels - 1 do
        begin
          if (time mod 100 = 0) and (tops[i] > 0) then
            tops[i] := tops[i] - 1;

          Write(' |');
          for j := 0 to PeakSize do
          begin
            if (peaks.ChannelPeaks[i] >= (j / PeakSize)) or (tops[i] = j) then
            begin
              Write('-');
              if j > tops[i] then
                tops[i] := j;
            end else
              Write(' ');
          end;
        end;
        Write(#13);
        Sleep(10);
        time := time + 10;

        AcNotifyManualProcessQueue;
      end;

    end else
      Writeln('The specified file could not be opened!');

    AuPeaks.Free;
    AuPlayer.Free;
  end else
    Writeln('Audorra could not be initialized!');
  AuAudio.Free;

  Writeln;
end.

