unit AuWin32Common;

interface



uses
  Classes, Windows, MMSystem,
  AuTypes;

type
  TAuDriverIdleEvent = function: boolean of object;

  //Only a helper class - does not need to be used by any driver plugin
  TAuDriverIdleThread = class(TThread)
  private
    FCallback: TAuDriverIdleEvent;
    FEvent:    THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(ACallback: TAuDriverIdleEvent; AEvent: THandle = 0);
  end;

  TWaveFormatExtensible = record
    Format: tWAVEFORMATEX;

    wValidBitsPerSample: word;
    dwChannelMask: DWord;
    SubFormat:     TGuid;
  end;

  PWaveFormatExtensible = ^TWaveFormatExtensible;

  PWaveHdr = ^TWaveHdr;

const
  //GUID needed for multi channel audio output
  KSDATAFORMAT_SUBTYPE_PCM: TGUID = '{00000001-0000-0010-8000-00aa00389b71}';
  WAVE_FORMAT_EXTENSIBLE = $FFFE;

function GetWaveFormatEx(AParameters: TAuDriverParameters): TWaveFormatExtensible;

implementation

function GetWaveFormatEx(AParameters: TAuDriverParameters): TWaveFormatExtensible;
var
  i: integer;
begin
  //Fill the result record with zeros
  FillChar(Result, SizeOf(Result), #0);

  with AParameters do
  begin
    //Copy wave format description into the wav_fmt buffer

    //Set channel count, sample rate and bit depth
    Result.Format.nChannels      := Channels;
    Result.Format.nSamplesPerSec := Frequency;
    Result.Format.wBitsPerSample := BitDepth;

    //Calculate needed "Bytes Per Second" value
    Result.Format.nAvgBytesPerSec := (BitDepth div 8) * (Channels * Frequency);

    //Set the size of a single block
    Result.Format.nBlockAlign := (BitDepth div 8 * Channels);

    if (Channels > 2) or (BitDepth > 16) then
    begin
      //As we have more than two audio channels, we have to use another wave format
      //descriptor
      Result.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
      Result.Format.cbSize     := 22;

      //Set the bit depth mask
      Result.wValidBitsPerSample := BitDepth;

      //Set the speakers that should be used
      for i := 0 to Channels - 1 do
        Result.dwChannelMask := Result.dwChannelMask or (1 shl i);

      //We're still sending PCM data to the driver
      Result.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
    end
    else
      //We only have two or one channels, so we're using the simple WaveFormatPCM
      //format descriptor
      Result.Format.wFormatTag := WAVE_FORMAT_PCM;
  end;
end;

{ TAuDriverIdleThread }

constructor TAuDriverIdleThread.Create(ACallback: TAuDriverIdleEvent;
  AEvent: THandle = 0);
begin
  inherited Create(False);

  FCallback := ACallback;
  FEvent    := AEvent;
end;

procedure TAuDriverIdleThread.Execute;
begin
  while not Terminated do
  begin
    if FEvent = 0 then
      //Problematic, as sleep may give to much time slices to other processes
      Sleep(1)
    else
      WaitForSingleObject(FEvent, 1);

    while FCallback() do ;
  end;
end;


end.

