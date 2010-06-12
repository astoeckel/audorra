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
GNU General Public License license (the GPL License), in which case the provisions of
GPL License are applicable instead of those above. If you wish to allow use
of your version of this file only under the terms of the GPL License and not
to allow others to use your version of this file under the MPL, indicate your
decision by deleting the provisions above and replace them with the notice and
other provisions required by the GPL License. If you do not delete the
provisions above, a recipient may use your version of this file under either the
MPL or the GPL License.

File: AuNULL.pas
Author: Andreas Stöckel
}

{Contains a NULL driver for Audorra.}
unit AuNULL;

interface

uses
  SysUtils, Classes,
  AcPersistent, AcSysUtils,
  AuTypes, AuUtils, AuDriverClasses;

type
  TAuNULLStreamDriver = class(TAuStreamDriver)
    private
      FFmt: TAuDriverParameters;
      FMem: PByte;
      FSize: Cardinal;
      FTC: Double;
      FFill: Boolean;
      FWait: Boolean;
      FThread: TAuStreamDriverIdleThread;
      FSmpls: Cardinal;
      FSmpl: Int64;
      FActive: Boolean;
      function Idle(AReadCallback: TAuReadCallback): boolean;
    public
      constructor Create(AWait: Boolean = true);
      destructor Destroy;override;

      {Openes the audio stream driver and makes it ready for playback. The driver
       will initially be opened in the inactive state.
       @param(AParameters is used to set the audio format information.)
       @param(ACallback is the function the data should be read from.)
       @returns(True if opening the audio device was successful, false if an error
         occured.)}
      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;override;
      {Closes the driver, if it had been opened. If the driver is not in a opened
       state, "Close" will do nothing.}
      procedure Close;override;

      {Sets the driver active or inactive. In the active state, the driver pulls
       audio data from the callback and plays it back, int the inactive state
       the driver just idles and waits for getting active immediately.}
      procedure SetActive(AActive: Boolean);override;

      {Swithes to the innactive state and flushes the audio buffer.}
      procedure FlushBuffer;override;
  end;

  TAuNULLDriver = class(TAuDriver)
    public
      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;
      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;override;
  end;

const
  AuNULLBufferTime = 20;

implementation

{ TAuNULLStreamDriver }

constructor TAuNULLStreamDriver.Create(AWait: Boolean);
begin
  inherited Create;
  
  FWait := AWait;
end;

destructor TAuNULLStreamDriver.Destroy;
begin
  inherited;
end;

function TAuNULLStreamDriver.Open(ADriverParams: TAuDriverParameters;
  ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;
begin
  FFill := true;
  FSmpl := 0;

  FFmt := ADriverParams;

  FSmpls := AuNullBufferTime * FFmt.Frequency div 1000;
  FSize := FFmt.BitDepth * FFmt.Channels * FSmpls div 8;

  FMem := GetMemory(FSize);

  AWriteFormat := AuBitdepth(ADriverParams.BitDepth);

  FActive := false;

  //Create the idle thread handler
  FThread := TAuStreamDriverIdleThread.Create(ACallback, Idle);

  result := true;
end;

procedure TAuNULLStreamDriver.Close;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  if FMem <> nil then
  begin
    FreeMem(FMem);
    FSize := 0;
    FMem := nil;
  end;
end;

procedure TAuNULLStreamDriver.FlushBuffer;
begin
  SetActive(false);
  FSmpl := 0;
end;

function TAuNULLStreamDriver.Idle(AReadCallback: TAuReadCallback): boolean;
var
  tc2: Double;
begin
  result := false;
  if FActive then
  begin
    tc2 := AcGetTickCount;
    if (not FWait) or FFill or (tc2 - FTC > AuNULLBufferTime) then
    begin
      FSmpl := FSmpl + FSmpls;
      AReadCallback(FMem, FSize, FSmpl);

      FFill := false;
      FTC := tc2;
    end;
  end;
end;

procedure TAuNULLStreamDriver.SetActive(AActive: Boolean);
begin
  FActive := AActive;
end;

(*function TAuNULLStreamDriver.Open: boolean;
begin
  result := FMem <> nil;
  FState := audsOpened;
end;

procedure TAuNULLStreamDriver.Close;
begin
  FState := audsClosed;
end;

procedure TAuNULLStreamDriver.Pause;
begin
  if FState = audsPlaying then
    FState := audsPaused;
end;

procedure TAuNULLStreamDriver.Play;
begin
  if FState >= audsOpened then
    FState := audsPlaying;
end;

procedure TAuNULLStreamDriver.Stop;
begin
  if FState >= audsOpened then
  begin
    FState := audsOpened;
    FFill := true;
  end;
end;     *)

{ TAuNULLDriver }

function TAuNULLDriver.CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;
begin
  if (ADeviceID >= 0) and (ADeviceID <= 1) then
    result := TAuNULLStreamDriver.Create(ADeviceID = 1)
  else
    result := nil;
end;

procedure TAuNULLDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  dev: TAuDevice;
begin
  //Return the null device
  dev.Name := 'NULL Sink';
  dev.ID := 0;
  dev.Priority := 1;
  dev.UserData := nil;
  ACallback(dev);

  //The null stream device simulates a real stream driver by reading the data in
  //realtime instead of reading with infinite speed 
  dev.Name := 'NULL Stream';
  dev.ID := 1;
  dev.Priority := 0;
  dev.UserData := nil;
  ACallback(dev);
end;

function CreateNULLDriver: TAuNULLDriver;
begin
  result := TAuNULLDriver.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuNULLDriver, @CreateNULLDriver);

end.
