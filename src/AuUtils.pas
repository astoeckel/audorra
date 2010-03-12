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

File: AuUtils.pas
Author: Andreas Stöckel
}

{Contains functions, which are useful for audio programming.}
unit AuUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$INCLUDE andorra.inc}

uses
  Math,
  AuTypes;

{Reads a sample from the given memory pointer at the given bit depth and returns
 a normalized single value (-1 to 1).
 @param(AMem is the pointer to the sample that should be read. The pointer
   is automatically incremented.)
 @param(ABitDepth is the bit depth the sample is in. Currently 8 and 16 bits are
   supported).}
function AuReadSample(var AMem: PByte; ABitDepth: Cardinal): Single;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{Writes a normalized single value AVal as a audio sample with the bit depth defined in
 ABitDepth to AMem. The single value is clipped to the range from -1 to 1. AMem
 is automatically incremented.}
procedure AuWriteSample(var AMem: PByte; AVal: Single; ABitDepth: Cardinal);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{Convertes a normalized, positive single sample value to dB.}
function AuToDezibel(AVal: Single): Single;
{Convertes a dB value to the corresponding normalized sample value.}
function AuFromDezibel(ADez: Single): Single;
{Limits a sample to a range from -1 to 1.}
function AuLimit(AVal: Single): Single;
{Calculates the bytes per second the given audio format has.}
function AuBytesPerSecond(const AParameters: TAuAudioParametersEx): Cardinal;overload;
{Calculates the bytes per second the given audio format (32-Bit single values are assumed)
 has.}
function AuBytesPerSecond(const AParameters: TAuAudioParameters): Cardinal;overload;
{Calculates the bytes per sample the given audio format (32-Bit single values are assumed)
 has.}
function AuBytesPerSample(const AParameters: TAuAudioParametersEx): Cardinal;overload;
{Calculates the bytes per sample the given audio format.}
function AuBytesPerSample(const AParameters: TAuAudioParameters): Cardinal;overload;

{Converts the given byte count to the number of samples that fits in this count.}
function AuBytesToSamples(AByteCount: integer; const ASrcParameters: TAuAudioParameters): integer;overload;
{Converts the given byte count to the number of samples that fits in this count.}
function AuBytesToSamples(AByteCount: integer; const ASrcParameters: TAuAudioParametersEx): integer;overload;
{Converts the count of bytes between to audio formats.}
function AuConvertByteCount(AByteCount: integer; const ASrcParameters, ATarParameters: TAuAudioParametersEx): integer;

{Converts ASamples from the memory ASrc points on to single float values.
 @param(AParameters defines the format the samples are stored in.)
 @param(ASrc defines the source memory)
 @param(ATar defines the target memory)
 @param(ASampleCount defines the count of samples that should be copied.)}
procedure AuPCMIntToFloat(const AParameters: TAuAudioParametersEx;
  ASrc, ATar: PByte; ASampleCount: Integer);
{Converts ASamples from the memory ASrc points on to int values in the given
 bit depth.
 @param(AParameters defines the format the samples should be stored in.)
 @param(ASrc defines the source memory)
 @param(ATar defines the target memory)
 @param(ASampleCount defines the count of samples that should be copied.)}
procedure AuPCMFloatToInt(const AParameters: TAuAudioParametersEx;
  ASrc, ATar: PByte; ASampleCount: Integer);

implementation

function AuToDezibel(AVal: Single): Single;
begin
  //
  //dB = 10 * log10(val)
  //

  try
    if AVal > 0.0001 then
      result := 10 * Log10(AVal)
    else
      result := -1000;
  except
    result := -1000;
  end;

  if result < -1000 then
    result := -1000;
end;

function AuFromDezibel(ADez: Single): Single;
begin
  //
  //val = 10 ^ (dB / 10)
  //

  result := Power(10, ADez / 10);
end;

function AuReadSample(var AMem: PByte; ABitDepth: Cardinal): Single;
begin
  result := 0;

  case ABitDepth of
    8:
    begin
      result := (PByte(AMem)^ - 127) / 127;
      Inc(AMem, 1);
    end;
    16:
    begin
      result := PSmallInt(AMem)^ / High(SmallInt);
      Inc(AMem, 2);
    end;
    32:
    begin
      result := PSingle(AMem)^;
      Inc(AMem, 4);
    end;
  end;
end;

function AuLimit(AVal: Single): Single;
begin
  //Clamp the value to a range from 1 to -1
  if AVal > 1 then
    result := 1
  else if AVal < -1 then
    result := -1
  else
    result := AVal;
end;


procedure AuWriteSample(var AMem: PByte; AVal: Single; ABitDepth: Cardinal);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  case ABitDepth of
    8:
    begin
      PByte(AMem)^ := trunc((AuLimit(AVal) + 1) / 2 * High(Byte));
      Inc(AMem, 1);
    end;
    16:
    begin
      PSmallInt(AMem)^ := trunc(AuLimit(AVal) * High(SmallInt));
      Inc(AMem, 2);
    end;
    32:
    begin
      PSingle(AMem)^ := AuLimit(AVal);
      Inc(AMem, 4);
    end;
  end;
end;

function AuBytesPerSecond(const AParameters: TAuAudioParametersEx): Cardinal;
begin
  result := AParameters.Frequency * AParameters.Channels * AParameters.BitDepth div 8;
end;

function AuBytesPerSecond(const AParameters: TAuAudioParameters): Cardinal;
begin
  result := AParameters.Frequency * AParameters.Channels * SizeOf(Single);
end;

function AuBytesPerSample(const AParameters: TAuAudioParametersEx): Cardinal;
begin
  result := AParameters.Channels * AParameters.BitDepth div 8;
end;

function AuBytesPerSample(const AParameters: TAuAudioParameters): Cardinal;
begin
  result := AParameters.Channels * SizeOf(Single)
end;

procedure AuPCMIntToFloat(const AParameters: TAuAudioParametersEx;
  ASrc, ATar: PByte; ASampleCount: Integer);
var
  i, j: integer;
  psrc: PByte;
  ptar: PSingle;
begin
  //Define source and target
  psrc := ASrc;
  ptar := PSingle(ATar);

  for i := 0 to ASampleCount - 1 do
  begin
    //Read a sample and convert it
    for j := 0 to AParameters.Channels - 1 do
    begin
      ptar^ := AuReadSample(psrc, AParameters.BitDepth);

      //Increment the target pointer (psrc is automatically incremented by the
      //AuReadSample function)
      inc(ptar);
    end;
  end;         
end;

procedure AuPCMFloatToInt(const AParameters: TAuAudioParametersEx;
  ASrc, ATar: PByte; ASampleCount: Integer);
var
  i, j: integer;
  psrc: PSingle;
  ptar: PByte;
begin
  //Define source and target
  psrc := PSingle(ASrc);
  ptar := ATar;

  for i := 0 to ASampleCount - 1 do
  begin
    //Read a sample and convert it
    for j := 0 to AParameters.Channels - 1 do
    begin
      AuWriteSample(ptar, psrc^, AParameters.BitDepth);

      //Increment the source pointer (ptar is automatically incremented by the
      //AuReadSample function)
      inc(psrc);
    end;
  end;
end;

function AuBytesToSamples(AByteCount: integer; const ASrcParameters: TAuAudioParameters): integer;overload;
begin
  result := AByteCount div Integer(AuBytesPerSample(ASrcParameters));
end;

function AuBytesToSamples(AByteCount: integer; const ASrcParameters: TAuAudioParametersEx): integer;overload;
begin
  result := AByteCount div Integer(AuBytesPerSample(ASrcParameters));
end;

function AuConvertByteCount(AByteCount: integer; const ASrcParameters, ATarParameters: TAuAudioParametersEx): integer;
begin
  result :=
    (AByteCount div Integer(AuBytesPerSample(ASrcParameters))) *
    Integer(AuBytesPerSample(ATarParameters));
end;



end.
