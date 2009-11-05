unit AuDirectSound;

interface

uses
  SysUtils, Classes,
  Windows, MMSystem, DirectSound,
  AcPersistent,
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
//      FCaps: TDSBcaps;
      FLastCursor: Cardinal;
      FFragsize: Cardinal;
      FFramesize: Cardinal;
    public
      constructor Create(ADSS8: IDirectSound8; AFmt: TWaveFormatExtensible);
      destructor Destroy;override;

      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;
      function Open: boolean;override;
      procedure Close;override;
      procedure Idle(AReadCallback: TAuReadCallback);override;
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
      function CreateStaticSoundDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx): TAuStaticSoundDriver;override;
      function CreateStreamDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx): TAuStreamDriver;override;
  end;

const
  //GUID needed for multi channel audio output
  KSDATAFORMAT_SUBTYPE_PCM: TGUID = '{00000001-0000-0010-8000-00aa00389b71}';
  WAVE_FORMAT_EXTENSIBLE = $FFFE;
  SPEAKER_ALL = $FFFFFFFF;

  //Buffer size in ms
  DS_BufferSize = 50;

implementation

function DSEnumCallback(lpGuid: PGUID; lpcstrDescription, lpcstrModule: PAnsiChar;
    lpContext: Pointer): BOOL; stdcall;
var
  pdsd: PAuDirectSoundDevice;
begin
  result := false;
  
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

function TAuDirectSoundDriver.CreateStaticSoundDriver(ADeviceID: integer;
  AParameters: TAuAudioParametersEx): TAuStaticSoundDriver;
begin
  result := nil;
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

  inherited;
end;

procedure TAuDirectSoundStreamDriver.Idle(AReadCallback: TAuReadCallback);
var
  avail: Cardinal;
  playcur: Cardinal;
  res: HRESULT;
  wrtcnt1, wrtcnt2: Cardinal;
  wrtptr1, wrtptr2: Pointer;
begin
  if FBuf = nil then
    exit;
    
  FBuf.GetCurrentPosition(@playcur, nil);
  avail := (playcur - FLastcursor + FDesc.dwBufferBytes) mod FDesc.dwBufferBytes;
  if avail > FFragsize then
  begin
    avail := avail - avail mod FFragsize;

    wrtcnt1 := 0;
    wrtcnt2 := 0;
    wrtptr1 := nil;
    wrtptr2 := nil;
    res := FBuf.Lock(FLastcursor, avail, @wrtptr1, @wrtcnt1, @wrtptr2, @wrtcnt2, 0);
    if res = DSERR_BUFFERLOST then
    begin
      res := FBuf.Restore;
      if res = DS_OK then
        res := FBuf.Play(0, 0, DSBPLAY_LOOPING);
      if res = DS_OK then
        res := FBuf.Lock(FLastcursor, avail, @wrtptr1, @wrtcnt1, @wrtptr2, @wrtcnt2, 0);
    end;

    if res = DS_OK then
    begin
      AReadCallback(wrtptr1, wrtcnt1, FSyncData);
      if wrtptr2 <> nil then      
        AReadCallback(wrtptr2, wrtcnt2, FSyncData);

      FBuf.Unlock(wrtptr1, wrtcnt1, wrtptr2, wrtcnt2);
    end;

    FLastCursor := (FLastCursor + wrtcnt1 + wrtcnt2) mod FDesc.dwBufferBytes;
  end;  
end;

function TAuDirectSoundStreamDriver.Open: boolean;
var
  res: HRESULT;
begin
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
end;

procedure TAuDirectSoundStreamDriver.Close;
begin
  if (FState = audsOpened) and (FBuf <> nil) then
  begin
    Stop;
    FBuf := nil;
  end;
    
  inherited;     
end;

procedure TAuDirectSoundStreamDriver.Pause;
begin
  if (FState = audsPlaying) and (FBuf <> nil) then
  begin
    FBuf.Stop;
    FState := audsPaused;
  end;
end;

procedure TAuDirectSoundStreamDriver.Play;
var
  res: HRESULT;
begin
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
end;

procedure TAuDirectSoundStreamDriver.Stop;
var
  wrtptr1, wrtptr2: Pointer;
  wrtcnt1, wrtcnt2: Cardinal;
  res: HRESULT;
  buf: PByte;
begin
  if (FState >= audsOpened) and (FBuf <> nil) then
  begin
    FLastCursor := 0;
    FBuf.SetCurrentPosition(0);
    FBuf.Stop;
    FBuf := nil;

    FState := audsOpened;
  end;
end;

function CreateDirectSoundDriver: TAuDirectSoundDriver;
begin
  result := TAuDirectSoundDriver.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuDirectSoundDriver, @CreateDirectSoundDriver);


end.
