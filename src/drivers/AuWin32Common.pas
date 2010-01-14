unit AuWin32Common;

interface

uses
  Windows, MMSystem,

  AuTypes;

type
  TWaveFormatExtensible = record
    Format : tWAVEFORMATEX;

    wValidBitsPerSample : Word;
    dwChannelMask : DWord;
    SubFormat : TGuid;
  end;

  PWaveFormatExtensible = ^TWaveFormatExtensible;

  PWaveHdr = ^TWaveHdr;

const
  //GUID needed for multi channel audio output
  KSDATAFORMAT_SUBTYPE_PCM: TGUID = '{00000001-0000-0010-8000-00aa00389b71}';
  WAVE_FORMAT_EXTENSIBLE = $FFFE;

function GetWaveFormatEx(AParameters: TAuAudioParametersEx): TWaveFormatExtensible;

implementation

function GetWaveFormatEx(AParameters: TAuAudioParametersEx): TWaveFormatExtensible;
var
  i: integer;
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
      for i := 0 to Channels - 1 do
        result.dwChannelMask := result.dwChannelMask or (1 shl i);

      //We're still sending PCM data to the driver
      result.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
    end else
      //We only have two or one channels, so we're using the simple WaveFormatPCM
      //format descriptor
      result.Format.wFormatTag := WAVE_FORMAT_PCM;
  end;
end; 

end.
