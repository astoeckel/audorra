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

File: AuWaveOut32Driver.pas
Author: Andreas Stöckel
}

{Software output driver for the low level windows "WaveOut" interface.}
unit AuWaveOut32;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Windows, MMSystem,
  AcPersistent, AcSyncObjs,
  AuTypes, AuUtils, AuDriverClasses, AuWin32Common;

type
  TAuWaveOutDriver = class(TAuDriver)
    public
      constructor Create;

      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;

      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;override;
  end;

  TAuWaveOutStreamDriver = class(TAuStreamDriver)
    private
      FHWO: HWAVEOUT;
      FDevId: integer;
      FFormat: TWaveFormatExtensible;
      FBlocks: array of TWaveHdr;
      FBuffer: PByte;
      FBlockCount: Cardinal;
      FBlockSize: Cardinal;
      FCurrentblock: Cardinal;
      FFreeblocks: Cardinal;
      FTimecode: Int64;
      FThread: TAuStreamDriverIdleThread;
      FCallback: TAuStreamDriverProc;
      FParameters: TAuDriverParameters;
      FMutex: TAcMutex;
      FActive: Boolean;
      function Idle(ACallback: TAuReadCallback):boolean;
      procedure AllocBlocks(ABlockCount, ABlockSize: Cardinal);
      procedure DestroyBlocks;
    public
      {Creates a new instance of TAuWaveOutStaticSoundDriver.
       @param(AID is the device id.)
       @param(AFmt is the windows wave format descriptor.)}
      constructor Create(AID: Cardinal);

      {Destroys the instance of TAuWaveOutStaticSoundDriver.}
      destructor Destroy;override;

      procedure SetActive(AActive: Boolean);override;
      procedure FlushBuffer;override;

      {Openes the audio object. And prepares it for playback. When using the
       TAuStaticSoundDriver, data can now be written into the object.}
      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): boolean;override;
      {Closes the audio object.}
      procedure Close;override;
  end;

var
  WaveOut_StdBlockCount: integer = 8;
  WaveOut_SamplesPerBlock: integer = 1024;

implementation

function CreateWaveOutDriver: TAuDriver;
begin
  result := TAuWaveOutDriver.Create;
end;

{ TAuWaveOutDriver }

constructor TAuWaveOutDriver.Create;
begin
  inherited;

  FPriority := 10;
end;

procedure TAuWaveOutDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  device: TAuDevice;
  num: Cardinal;
  i: Cardinal;
  caps: TWaveOutCapsA;  
begin
  device.UserData := nil;

  //Add the default device
  device.Name := 'Default Wave Mapper';
  device.ID := Integer(WAVE_MAPPER);
  device.Priority := 1;
  ACallback(device);

  //Enumerate all other devices
  device.Priority := 0;

  num := waveOutGetNumDevs;
  for i := 0 to num - 1 do
  begin
    if waveOutGetDevCaps(i, @caps, SizeOf(Caps)) = MMSYSERR_NOERROR then
    begin
      device.ID := i;
      device.Name := caps.szPname;

      ACallback(device);
    end;
  end;    
end;

function TAuWaveOutDriver.CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;
begin
  result := TAuWaveOutStreamDriver.Create(Cardinal(ADeviceID));
end;

{ TAuWaveOutStreamDriver }

procedure stream_callback(hwo: HWAVEOUT; uMsg: Cardinal; dwInstance, dwParam1, dwParam2: DWORD);stdcall;
begin
  if (uMsg = WOM_DONE) then
  begin
    with TAuWaveOutStreamDriver(Pointer(dwInstance)) do
      FFreeblocks := FFreeblocks + 1;
  end;
end;

constructor TAuWaveOutStreamDriver.Create(AID: Cardinal);
begin
  inherited Create;

  FDevId := Integer(AID);
  FMutex := TAcMutex.Create;
end;

destructor TAuWaveOutStreamDriver.Destroy;
begin
  Close;
  DestroyBlocks;
  FMutex.Free;

  inherited;
end;

function TAuWaveOutStreamDriver.Idle(ACallback: TAuReadCallback): boolean;
begin
  result := false;
  if (FFreeblocks > 0) and (FHwo <> 0) and (FBlockCount > 0) then
  begin
    FMutex.Acquire;
    try
      if FActive then
      begin
        with FBlocks[FCurrentBlock] do
        begin
          if dwFlags = WHDR_PREPARED then
            waveOutUnprepareHeader(Fhwo, @FBlocks[FCurrentBlock], SizeOf(FBlocks[FCurrentBlock]));

          //Set the sync data
          dwBufferLength := ACallback(PByte(lpData), FBlockSize, FTimecode);

          if dwBufferLength > 0 then
          begin
            waveOutPrepareHeader(Fhwo, @FBlocks[FCurrentBlock], SizeOf(FBlocks[FCurrentBlock]));
            waveOutWrite(Fhwo, @FBlocks[FCurrentBlock], SizeOf(FBlocks[FCurrentBlock]));

            dec(FFreeblocks);

            FCurrentBlock := FCurrentBlock + 1;
            if FBlockCount > 0 then
              FCurrentBlock := FCurrentBlock mod FBlockcount;
            FTimecode := FTimecode + WaveOut_SamplesPerBlock;

            result := true;
          end;
        end;
      end;
    finally
      FMutex.Release;
    end;
  end;
end;

function TAuWaveOutStreamDriver.Open(ADriverParams: TAuDriverParameters;
  ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): boolean;
begin
  result := false;

  FFormat := GetWaveFormatEx(ADriverParams);
  FCallback := ACallback;
  AWriteFormat := AuBitdepth(ADriverParams.BitDepth);
  FParameters := ADriverParams;
  FActive := false;

  //Try to open wave out for streaming purposes
  if waveOutOpen(@Fhwo, Cardinal(FDevId), @FFormat, Cardinal(@stream_callback), Cardinal(self),
    CALLBACK_FUNCTION) = MMSYSERR_NOERROR then
  begin
    result := true;

    //Allocate buffer blocks
    AllocBlocks(
      WaveOut_StdBlockCount,
      WaveOut_SamplesPerBlock * FFormat.Format.nChannels * FFormat.Format.wBitsPerSample div 8);

    FThread := TAuStreamDriverIdleThread.Create(FCallback, Idle);

    //Pause the playback
    SetActive(false);
  end;
end;

procedure TAuWaveOutStreamDriver.Close;
var
  i: integer;
begin
  if FHWO <> 0 then
  begin
    FlushBuffer;
    
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);

    waveOutReset(FHWO);
    
    for i := 0 to FBlockCount - 1 do
      if FBlocks[i].dwFlags = WHDR_PREPARED then
        while (waveOutUnprepareHeader(FHWO, @FBlocks[i], SizeOf(FBlocks[i])) = WAVERR_STILLPLAYING) do
          Sleep(1);

    while waveOutClose(FHWO) = WAVERR_STILLPLAYING do
      Sleep(1);

    DestroyBlocks;

    waveOutClose(FHWO);
    DestroyBlocks;
  end;

  FHWO := 0;
end;

procedure TAuWaveOutStreamDriver.SetActive(AActive: Boolean);
begin
  if FHWO <> 0 then
  begin
    FMutex.Acquire;
    try
      if AActive then
        waveOutRestart(FHWO)
      else
        waveOutPause(FHWO);

      FActive := true;
    finally
      FMutex.Release;
    end;
  end;
end;

procedure TAuWaveOutStreamDriver.FlushBuffer;
begin
  if FHWO <> 0 then
  begin
    FMutex.Acquire;
    try
      waveOutReset(FHWO);
      waveOutPause(FHWO);

      FFreeblocks := FBlockCount;
      FCurrentblock := 0;
      FTimecode := 0;

      FActive := false;
    finally
      FMutex.Release;
    end;
  end;
end;

procedure TAuWaveOutStreamDriver.AllocBlocks(ABlockCount, ABlockSize: Cardinal);
var
  ptr: PByte;
  i: integer;
begin
  DestroyBlocks;

  FBlockCount := ABlockCount;
  FBlockSize := ABlockSize;

  //Reserve memory for the buffers
  GetMem(FBuffer, FBlocksize * FBlockcount);

  //Reserve memory for the FBuffer headers (FBlocks)
  SetLength(FBlocks, FBlockcount);

  //Reserve memory for the sync data array
  ptr := FBuffer;
  for i := 0 to FBlockcount - 1 do
  begin
    FBlocks[i].lpData := PAnsiChar(ptr);
    FBlocks[i].dwBufferLength := FBlocksize;
    inc(ptr, FBlocksize);
  end;

  FFreeblocks := FBlockcount;
  FCurrentblock := 0;
end;

procedure TAuWaveOutStreamDriver.DestroyBlocks;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);

  FBuffer := nil;
  SetLength(FBlocks, 0);
  
  FBlockCount := 0;
  FBlockSize := 0;
end;

initialization
  AcRegSrv.RegisterClass(TAuWaveOutDriver, @CreateWaveOutDriver);

end.
