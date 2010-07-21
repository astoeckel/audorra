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
  SysUtils, Classes,
  Math,
  AcSysUtils, AcTypes,
  AuTypes;

type
  TAuStreamDriverIdleFunc = function(
    AReadCallback: TAuReadCallback): boolean of object;
  
  TAuStreamDriverIdleThread = class(TThread)
    private
      FIdleFunc: TAuStreamDriverIdleFunc;
      FReadCallback: TAuStreamDriverProc;
    protected
      procedure Execute;override;
    public
      constructor Create(AReadCallback: TAuStreamDriverProc;
        AIdleFunc: TAuStreamDriverIdleFunc);
      destructor Destroy;override;
  end;

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
procedure AuReadSamples(const AParams: TAuAudioParametersEx; ASrc, ATar: PByte; ASampleCount: Cardinal);

{Converts ASamples from the memory ASrc points on to single float values.
 @param(AParameters defines the format the samples are stored in.)
 @param(ASrc defines the source memory)
 @param(ATar defines the target memory)
 @param(ASampleCount defines the count of samples that should be copied.)}
procedure AuWriteSamples(const AParams: TAuAudioParametersEx; ASrc, ATar: PByte; ASampleCount: Cardinal);

implementation

procedure _AuReadSamples8(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PSingle(t)^ := PAcInt8(s)^ * mul;
    Inc(s, 1); Inc(t, 4);
  end;
end;

procedure _AuReadSamples8U(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PSingle(t)^ := PAcUInt8(s)^ * mul - 1;
    Inc(s, 1); Inc(t, 4);
  end;
end;

procedure _AuReadSamples16(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PSingle(t)^ := PAcInt16(s)^ * mul;
    Inc(s, 2); Inc(t, 4);
  end;
end;

procedure _AuReadSamples16U(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PSingle(t)^ := PAcUInt16(s)^ * mul - 1;
    Inc(s, 2); Inc(t, 4);
  end;
end;

procedure _AuReadSamples32(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PSingle(t)^ := PAcInt32(s)^ * mul;
    Inc(s, 4); Inc(t, 4);
  end;
end;

procedure _AuReadSamples32U(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PSingle(t)^ := PAcUInt32(s)^ * mul - 1;
    Inc(s, 4); Inc(t, 4);
  end;
end;


procedure AuReadSamples(const AParams: TAuAudioParametersEx; ASrc, ATar: PByte;
  ASampleCount: Cardinal);
var
  iterations: integer;
  mul: Single;
begin
  if AParams.BitDepth.sample_type < austFloat then
  begin
    iterations := AParams.Channels * ASampleCount - 1;

    mul := 1 / AcUInt32(1 shl (AParams.BitDepth.bits - 1));

    case AParams.BitDepth.align of
      8:
        if AParams.BitDepth.sample_type = austInt then
          _AuReadSamples8(ASrc, ATar, iterations, mul)
        else
          _AuReadSamples8U(ASrc, ATar, iterations, mul);

      16:
        if AParams.BitDepth.sample_type = austInt then
          _AuReadSamples16(ASrc, ATar, iterations, mul)
        else
          _AuReadSamples16U(ASrc, ATar, iterations, mul);

(*      24:
        if AParams.BitDepth.sample_type = austInt then
          _AuReadSamples24(ASrc, ATar, iterations, mul)
        else
          _AuReadSamples24U(ASrc, ATar, iterations, mul); *)

      32:
        if AParams.BitDepth.sample_type = austInt then
          _AuReadSamples32(ASrc, ATar, iterations, mul)
        else
          _AuReadSamples32U(ASrc, ATar, iterations, mul);
    end;
  end else
    AcMove(ASrc^, ATar^,
      AParams.Channels * ASampleCount * AParams.BitDepth.align div 8);
end;

procedure _AuWriteSamples8(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PAcInt8(t)^ := trunc(AuLimit(PSingle(s)^) * mul);
    Inc(t, 1); Inc(s, 4);
  end;
end;

procedure _AuWriteSamples8U(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PAcUInt8(t)^ := trunc((AuLimit(PSingle(s)^ + 1)) * mul);
    Inc(t, 1); Inc(s, 4);
  end;
end;

procedure _AuWriteSamples16(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PAcInt16(t)^ := trunc(AuLimit(PSingle(s)^) * mul);
    Inc(t, 2); Inc(s, 4);
  end;
end;

procedure _AuWriteSamples16U(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PAcUInt16(t)^ := trunc((AuLimit(PSingle(s)^) + 1) * mul);
    Inc(t, 2); Inc(s, 4);
  end;
end;

procedure _AuWriteSamples32(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PAcInt32(t)^ := trunc(AuLimit(PSingle(s)^) * mul);
    Inc(t, 4); Inc(s, 4);
  end;
end;

procedure _AuWriteSamples32U(s, t: PByte; it: Cardinal; mul: Single);
var
  i: integer;
begin
  for i := 0 to it do
  begin
    PAcUInt32(t)^ := trunc((AuLimit(PSingle(s)^) + 1) * mul);
    Inc(t, 4); Inc(s, 4);
  end;
end;

procedure AuWriteSamples(const AParams: TAuAudioParametersEx; ASrc, ATar: PByte;
  ASampleCount: Cardinal);
var
  iterations: integer;
  mul: Single;
begin
  if AParams.BitDepth.sample_type < austFloat then
  begin
    iterations := AParams.Channels * ASampleCount - 1;

    mul := AcUInt32(1 shl (AParams.BitDepth.bits - 1)) - 1;

    case AParams.BitDepth.align of
      8:
        if AParams.BitDepth.sample_type = austInt then
          _AuWriteSamples8(ASrc, ATar, iterations, mul)
        else
          _AuWriteSamples8U(ASrc, ATar, iterations, mul);

      16:
        if AParams.BitDepth.sample_type = austInt then
          _AuWriteSamples16(ASrc, ATar, iterations, mul)
        else
          _AuWriteSamples16U(ASrc, ATar, iterations, mul);

(*      24:
        if AParams.BitDepth.sample_type = austInt then
          _AuWriteSamples24(ASrc, ATar, iterations, mul)
        else
          _AuWriteSamples24U(ASrc, ATar, iterations, mul); *)

      32:
        if AParams.BitDepth.sample_type = austInt then
          _AuWriteSamples32(ASrc, ATar, iterations, mul)
        else
          _AuWriteSamples32U(ASrc, ATar, iterations, mul);
    end;
  end else
    AcMove(ASrc^, ATar^,
      AParams.Channels * ASampleCount * AParams.BitDepth.align div 8);
end;

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

function AuLimit(AVal: Single): Single;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  //Clamp the value to a range from 1 to -1
  if AVal > 1 then
    result := 1
  else if AVal < -1 then
    result := -1
  else
    result := AVal;
end;

function AuBytesPerSecond(const AParameters: TAuAudioParametersEx): Cardinal;
begin
  result := AParameters.Frequency * AParameters.Channels * AParameters.BitDepth.align div 8;
end;

function AuBytesPerSecond(const AParameters: TAuAudioParameters): Cardinal;
begin
  result := AParameters.Frequency * AParameters.Channels * SizeOf(Single);
end;

function AuBytesPerSample(const AParameters: TAuAudioParametersEx): Cardinal;
begin
  result := AParameters.Channels * AParameters.BitDepth.align div 8;
end;

function AuBytesPerSample(const AParameters: TAuAudioParameters): Cardinal;
begin
  result := AParameters.Channels * SizeOf(Single)
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

{ TAuStreamDriverIdleThread }

constructor TAuStreamDriverIdleThread.Create(
  AReadCallback: TAuStreamDriverProc; AIdleFunc: TAuStreamDriverIdleFunc);
begin
  inherited Create(false);

  FReadCallback := AReadCallback;
  FIdleFunc := AIdleFunc;
end;

destructor TAuStreamDriverIdleThread.Destroy;
begin
  inherited;
end;

procedure TAuStreamDriverIdleThread.Execute;
begin
  try
    while not Terminated do
    begin
      while FIdleFunc(FReadCallback) do;

      Sleep(1);
    end;
  except
    //
  end;
end;

end.
