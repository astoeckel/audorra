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
  SysUtils, Classes,
  Windows, DirectSound, ActiveX,
  AcSysUtils, AcPersistent, AcSyncObjs,

  AuTypes, AuDriverClasses, AuWin32Common;

type
  TAuDirectSoundDevice = record
    GUID: PGUID;
    Description: string;
    Module: string;
    DSS8: IDirectSound8;
  end;
  PAuDirectSoundDevice = ^TAuDirectSoundDevice;

  TAuDirectSoundStreamDriver = class(TAuStreamDriver)
    private
      FThread: TAuDriverIdleThread;
      FMutex: TAcMutex;
      FActive: Boolean;
      FPlaybackPosition: Int64;
      FCallback: TAuStreamDriverProc;
      
      FDSS8: IDirectSound8;
      FBuf: IDirectSoundBuffer;

      FFmt: TWaveFormatExtensible;
      FDesc: TDSBufferDesc;
      FLastCursor: Cardinal;
      FFragsize: Cardinal;
      FFramesize: Cardinal;
      FBufMem: PByte;
      FBufMemSize: Cardinal;

      function DriverIdle: Boolean;
    public
      constructor Create(ADSS8: IDirectSound8);
      destructor Destroy;override;

      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;override;
      procedure Close;override;

      procedure SetActive(AActive: Boolean);override;
      procedure FlushBuffer;override;
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
      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;override;
  end;

const
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

{ TAuDriver }

constructor TAuDirectSoundDriver.Create;
begin
  inherited Create;

  CoInitialize(nil);

  FPriority := 50;

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

function TAuDirectSoundDriver.CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;
var
  dev: IDirectSound8;
begin
  result := nil;

  dev := GetDevice(ADeviceID);
  if dev <> nil then
    result := TAuDirectSoundStreamDriver.Create(dev);
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

constructor TAuDirectSoundStreamDriver.Create(ADSS8: IDirectSound8);
begin
  inherited Create;
  
  FDSS8 := ADSS8;
  FMutex := TAcMutex.Create;
end;

destructor TAuDirectSoundStreamDriver.Destroy;
begin
  Close;

  FDSS8 := nil;
  FreeAndNil(FMutex);

  if FBufMem <> nil then
    FreeMem(FBufMem, FBufMemSize);
  FBufMem := nil;
  FBufMemSize := 0;

  inherited;
end;

function TAuDirectSoundStreamDriver.Open(ADriverParams: TAuDriverParameters;
  ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;
var
  res: HRESULT;
  smpls: Integer;
begin
  result := false;

  if FBuf = nil then
  begin
    //Setup the description buffer
    FFmt := GetWaveFormatEx(ADriverParams);
    FFramesize := FFmt.Format.wBitsPerSample * FFmt.Format.nChannels div 8;
    smpls := round(DS_BufferSize / 1000 * FFmt.Format.nSamplesPerSec);
    FFragsize := round(smpls / 10) * FFramesize;

    AWriteFormat := AuBitdepth(ADriverParams.BitDepth);

    FillChar(FDesc, SizeOf(FDesc), 0);
    FDesc.dwSize := SizeOf(FDesc);
    FDesc.dwFlags := DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2;
    FDesc.dwBufferBytes := Cardinal(smpls) * Cardinal(FFramesize);
    Pointer(FDesc.lpwfxFormat) := @FFmt;

    FCallback := ACallback;
    FPlaybackPosition := 0;
    res := FDSS8.CreateSoundBuffer(FDesc, FBuf, nil);
    if res = DS_OK then
    begin
      FBuf.GetCurrentPosition(@FLastCursor, nil);

      FThread := TAuDriverIdleThread.Create(DriverIdle);

      result := true;
    end else
      FBuf := nil;
  end;
end;

procedure TAuDirectSoundStreamDriver.Close;
begin
  if FBuf <> nil then
  begin
    //Deactivate playback
    FlushBuffer;

    //Wait for the thread to terminate
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);

    FActive := false;
  end;
  FBuf := nil;
end;

procedure TAuDirectSoundStreamDriver.FlushBuffer;
var
  pb1, pb2: PByte;
  s1, s2: Cardinal;
begin
  if FBuf <> nil then
  begin
    FMutex.Acquire;
    try
      //Deactivate playback and seek back
      SetActive(false);
      FBuf.SetCurrentPosition(0);
      FLastCursor := 0;
      FPlaybackPosition := 0;

      //Perform the actual flushing
      if FBuf.Lock(0, FDesc.dwBufferBytes, @pb1, @s1, @pb2, @s2, 0) = DS_OK then
      begin
        FillChar(pb1^, s1, 0);
        FillChar(pb2^, s2, 0);
        FBuf.Unlock(pb1, s1, pb2, s2);
      end;
    finally
      FMutex.Release;
    end;
  end;
end;

procedure TAuDirectSoundStreamDriver.SetActive(AActive: Boolean);
begin
  if FBuf <> nil then
  begin
    FMutex.Acquire;
    try
      if AActive then
        FBuf.Play(0, 0, DSBPLAY_LOOPING)
      else
        FBuf.Stop;
      FActive := AActive;
    finally
      FMutex.Release;
    end;
  end;
end;

function TAuDirectSoundStreamDriver.DriverIdle: Boolean;
var
  avail: Cardinal;
  playcur: Cardinal;
  res: HRESULT;
  wrtcnt1, wrtcnt2: Cardinal;
  wrtptr1, wrtptr2: Pointer;
  read: integer;
  pb: PByte;
  sp: Int64;
begin
  result := false;
  if FBuf <> nil then
  begin
    FMutex.Acquire;
    try
      if FActive then
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

          sp := FPlaybackPosition - (DS_BufferSize * FFmt.Format.nSamplesPerSec div 1000);
          if sp < 0 then
            sp := 0;

          read := FCallback(FBufMem, avail, sp);
          FPlaybackPosition :=
            FPlaybackPosition + read div FFmt.Format.nBlockAlign;

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
              AcMove(pb^, wrtptr1^, wrtcnt1);
              inc(pb, wrtcnt1);
              AcMove(pb^, wrtptr2^, wrtcnt2);
              result := wrtcnt1 + wrtcnt2 > 0;
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
end;

function CreateDirectSoundDriver: TAuDirectSoundDriver;
begin
  result := TAuDirectSoundDriver.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuDirectSoundDriver, @CreateDirectSoundDriver);


end.
