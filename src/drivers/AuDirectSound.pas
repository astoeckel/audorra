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

File: AuDirectSound.pas
Author: Andreas Stöckel
}

{Contains a DirectSound audio driver for windows. As it has a very low latency,
 it should be used as a default.}
unit AuDirectSound;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, AuSyncUtils,
  Windows, MMSystem, DirectSound,
  AcPersistent, AcSyncObjs,
  AuTypes, AuDriverClasses;

type
  TWaveFormatExtensible = record
    Format : tWAVEFORMATEX;

    wValidBitsPerSample : Word;
    dwChannelMask : DWord;
    SubFormat : TGuid;
  end;

  PWaveFormatExtensible = ^TWaveFormatExtensible;

  PWaveHdr = ^TWaveHdr;
  
  TAuDirectSoundDevice = record
    GUID: PGUID;
    Description: string;
    Module: string;
    DSS8: IDirectSound8;
  end;
  PAuDirectSoundDevice = ^TAuDirectSoundDevice;

  TAuDirectSoundStreamDriver = class(TAuStreamDriver)
    private
      FDSS8: IDirectSound8;
      FFmt: TWaveFormatExtensible;
      FDesc: TDSBufferDesc;
      FBuf: IDirectSoundBuffer;
      FLastCursor: Cardinal;
      FFragsize: Cardinal;
      FFramesize: Cardinal;
      FMutex: TAcMutex;
      FBufMem: PByte;
      FBufMemSize: Cardinal;
    public
      constructor Create(ADSS8: IDirectSound8; AFmt: TWaveFormatExtensible);
      destructor Destroy;override;

      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;
      function Open: boolean;override;
      procedure Close;override;
      function Idle(AReadCallback: TAuReadCallback): boolean;override;
  end;
  
  TAuDirectSoundDriver = class(TAuDriver)
    private
      FDevices: TList;
      procedure EnumDev;
      function GetDevice(AID: integer): IDirectSound8;
    public
      constructor Create;
      destructor Destroy;override;
      
      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;
      function CreateStreamDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx): TAuStreamDriver;override;
  end;

const
  //GUID needed for multi channel audio output
  KSDATAFORMAT_SUBTYPE_PCM: TGUID = '{00000001-0000-0010-8000-00aa00389b71}';
  WAVE_FORMAT_EXTENSIBLE = $FFFE;
  SPEAKER_ALL = $FFFFFFFF;

  //Buffer size in ms
  DS_BufferSize = 100;

implementation

function DSEnumCallback(lpGuid: PGUID; lpcstrDescription, lpcstrModule: PAnsiChar;
    lpContext: Pointer): BOOL; stdcall;
var
  pdsd: PAuDirectSoundDevice;
begin
  result := true;
  
  New(pdsd);
  pdsd^.GUID := lpGuid;
  pdsd^.Description := lpcstrDescription;
  pdsd^.Module := lpcstrModule;

  TAuDirectSoundDriver(lpContext).FDevices.Add(pdsd);
end;

function GetWaveFormatEx(AParameters: TAuAudioParametersEx): TWaveFormatExtensible;
begin
  //Fill the result record with zeros
  FillChar(result, SizeOf(result), #0);

  with AParameters do
  begin           
    //Copy wave format description into the wav_fmt buffer

    //Set channel count, sample rate and bit depth
    result.Format.nChannels := Channels;
    result.Format.nSamplesPerSec := Frequency;
    result.Format.wBitsPerSample := BitDepth;

    //Calculate needed "Bytes Per Second" value
    result.Format.nAvgBytesPerSec := (BitDepth div 8) * (Channels * Frequency);

    //Set the size of a single block
    result.Format.nBlockAlign := (BitDepth div 8 * Channels);

    if Channels > 2 then
    begin
      //As we have more than two audio channels, we have to use another wave format
      //descriptor
      result.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
      result.Format.cbSize := 22;

      //Set the bit depth mask
      result.wValidBitsPerSample := BitDepth;

      //Set the speakers that should be used
      result.dwChannelMask := SPEAKER_ALL;

      //We're still sending PCM data to the driver
      result.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
    end else
      //We only have two or one channels, so we're using the simple WaveFormatPCM
      //format descriptor
      result.Format.wFormatTag := WAVE_FORMAT_PCM;
  end;
end; 

{ TAuDriver }

constructor TAuDirectSoundDriver.Create;
begin
  inherited Create;

  //Enumerate all direct sound devices
  FDevices := TList.Create;
  EnumDev;
end;

procedure TAuDirectSoundDriver.EnumDev;
begin
  DirectSoundEnumerate(DSEnumCallback, self);
end;

procedure TAuDirectSoundDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  i: Integer;
  dev: TAuDevice;
  pdsd: PAuDirectSoundDevice;
begin
  for i := 0 to FDevices.Count - 1 do
  begin
    pdsd := PAuDirectSoundDevice(FDevices[i]);
    dev.ID := i;
    dev.Priority := 0;
    if pdsd^.GUID = nil then
      dev.Priority := 1;    
    dev.Name := pdsd^.Description;
    ACallback(dev);
  end;
end;

function TAuDirectSoundDriver.GetDevice(AID: integer): IDirectSound8;
var
  pdsd: PAuDirectSoundDevice;
  res: HRESULT;
begin
  result := nil;
  if (AID >= 0) and (AID < FDevices.Count) then
  begin
    pdsd := PAuDirectSoundDevice(FDevices[AID]);
    if pdsd^.DSS8 = nil then
    begin
      res := DirectSoundCreate8(pdsd^.GUID, pdsd^.DSS8, nil);
      if res = DS_OK then
        pdsd^.DSS8.SetCooperativeLevel(GetForegroundWindow, DSSCL_PRIORITY);
    end;

    result := pdsd^.DSS8;
  end;
end;

function TAuDirectSoundDriver.CreateStreamDriver(ADeviceID: integer;
  AParameters: TAuAudioParametersEx): TAuStreamDriver;
var
  dev: IDirectSound8;
begin
  result := nil;

  dev := GetDevice(ADeviceID);
  if dev <> nil then
    result := TAuDirectSoundStreamDriver.Create(dev, GetWaveFormatEx(AParameters));
end;

destructor TAuDirectSoundDriver.Destroy;
var
  i: Integer;
begin
  if FDevices <> nil then
  begin
    for i := 0 to FDevices.Count - 1 do
      Dispose(PAuDirectSoundDevice(FDevices[i]));
    FreeAndNil(FDevices);
  end;

  inherited;
end;

{ TAuDirectSoundStreamDriver }

constructor TAuDirectSoundStreamDriver.Create(ADSS8: IDirectSound8;
  AFmt: TWaveFormatExtensible);
var
  smpls: Cardinal;
begin
  inherited Create;

  FMutex := TAcMutex.Create;

  FDelay := DS_BufferSize;

  FDSS8 := ADSS8;
  FFmt := AFmt;

  //Setup the description buffer
  FFramesize := AFmt.Format.wBitsPerSample * AFmt.Format.nChannels div 8;
  FillChar(FDesc, SizeOf(FDesc), 0);
  smpls := round(DS_BufferSize / 1000 * AFmt.Format.nSamplesPerSec);
  FDesc.dwSize := SizeOf(FDesc);
  FDesc.dwFlags := DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2;
  FDesc.dwBufferBytes := smpls * FFramesize;     
  Pointer(FDesc.lpwfxFormat) := @FFmt;

  FFragsize := round(smpls * 0.25) * FFramesize;
  FParameters.Frequency := AFmt.Format.nSamplesPerSec;
  FParameters.Channels := AFmt.Format.nChannels;
  FParameters.BitDepth := AFmt.Format.wBitsPerSample;
end;

destructor TAuDirectSoundStreamDriver.Destroy;
begin
  FBuf := nil;
  FDSS8 := nil;
  FMutex.Free;

  if FBufMem <> nil then
    FreeMem(FBufMem, FBufMemSize);
  FBufMem := nil;
  FBufMemSize := 0;

  inherited;
end;

var
  time: Double;

procedure CreateSine(ABuf: PByte; ASize: Cardinal);
var
  i, j: integer;
  ps: PSmallInt;
begin
  ps := PSmallInt(ABuf);
  for i := 0 to (ASize div 4) - 1 do
  begin
    time := time + 1/20;
    for j := 0 to 1 do
    begin
      ps^ := Round(sin(time + PI) * High(SmallInt) * 0.25);
      inc(ps);
    end;
  end;
end;

function TAuDirectSoundStreamDriver.Idle(AReadCallback: TAuReadCallback): boolean;
var
  avail: Cardinal;
  playcur: Cardinal;
  res: HRESULT;
  wrtcnt1, wrtcnt2: Cardinal;
  wrtptr1, wrtptr2: Pointer;
  read: integer;
  pb: PByte;
begin
  result := false;
  
  FMutex.Acquire;
  try
    if FBuf <> nil then
    begin
      FBuf.GetCurrentPosition(@playcur, nil);
      avail := (playcur - FLastcursor + FDesc.dwBufferBytes) mod FDesc.dwBufferBytes;
      if avail > FFragsize then
      begin
        avail := avail - avail mod FFragsize;

        //Read audio data from the callback
        if (avail <> FBufMemSize) or (FBufMem = nil) then
          ReallocMem(FBufMem, avail);
        FBufMemSize := avail;
        read := AReadCallback(FBufMem, avail, FSyncData);

        //Initialize some varibles
        wrtcnt1 := 0; wrtcnt2 := 0; wrtptr1 := nil; wrtptr2 := nil;

        //Lock the direct sound buffer
        res := FBuf.Lock(FLastcursor, read, @wrtptr1, @wrtcnt1, @wrtptr2, @wrtcnt2, 0);
        if res = DSERR_BUFFERLOST then
        begin
          res := FBuf.Restore;
          if res = DS_OK then
            res := FBuf.Play(0, 0, DSBPLAY_LOOPING);
          if res = DS_OK then
            res := FBuf.Lock(FLastcursor, read, @wrtptr1, @wrtcnt1, @wrtptr2, @wrtcnt2, 0);
        end;

        if res = DS_OK then
        begin
          try
            pb := FBufMem;
            Move(pb^, wrtptr1^, wrtcnt1);
            inc(pb, wrtcnt1);
            Move(pb^, wrtptr2^, wrtcnt2);
            result := true;
          finally
            FBuf.Unlock(wrtptr1, wrtcnt1, wrtptr2, wrtcnt2);
          end;
        end;

        FLastCursor := (FLastCursor + wrtcnt1 + wrtcnt2) mod FDesc.dwBufferBytes;
      end;
    end;
  finally
    FMutex.Release;
  end;
end;

function TAuDirectSoundStreamDriver.Open: boolean;
var
  res: HRESULT;
begin
  FMutex.Acquire;
  try
    result := false;
    if FBuf = nil then
    begin
      res := FDSS8.CreateSoundBuffer(FDesc, FBuf, nil);
      if res = DS_OK then
      begin
        FState := audsOpened;
        result := true;
      end;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TAuDirectSoundStreamDriver.Close;
begin
  FMutex.Acquire;
  try
    if (FState = audsOpened) and (FBuf <> nil) then
    begin
      Stop;
      FBuf := nil;
    end;
  finally
    FMutex.Release;
  end;
  inherited;     
end;

procedure TAuDirectSoundStreamDriver.Pause;
begin
  FMutex.Acquire;
  try
    if (FState = audsPlaying) and (FBuf <> nil) then
    begin
      FBuf.Stop;
      FState := audsPaused;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TAuDirectSoundStreamDriver.Play;
var
  res: HRESULT;
begin
  FMutex.Acquire;
  try
    if (FState >= audsOpened) then
    begin
      if FBuf = nil then
        Open;
    
      res := FBuf.Play(0, 0, DSBPLAY_LOOPING);
      if res = DS_OK then
      begin
        if FState = audsOpened then
          FBuf.GetCurrentPosition(@FLastCursor, nil);

        FState := audsPlaying;
      end;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TAuDirectSoundStreamDriver.Stop;
begin
  FMutex.Acquire;
  try
    if (FState >= audsOpened) and (FBuf <> nil) then
    begin
      FLastCursor := 0;
      FBuf.SetCurrentPosition(0);
      FBuf.Stop;
      FBuf := nil;

      FState := audsOpened;
    end;
  finally
    FMutex.Release;
  end;
end;

function CreateDirectSoundDriver: TAuDirectSoundDriver;
begin
  result := TAuDirectSoundDriver.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuDirectSoundDriver, @CreateDirectSoundDriver);


end.
