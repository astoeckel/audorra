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

File: AuEffects.pas
Author: Andreas Stöckel
}

{Unit which contains some easy to use sound effects.}
unit AuEffects;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I andorra.inc}

uses
  AcSysUtils,
  Math;

type
  TAuWaveform = (
    auwfSine,
    auwfTriangle,
    auwfSawtooth,
    auwfRectangle
  );

  TAuLVArr = array of Single;

const
  e = 2.718281828459;

function AuWaveformSine(t: Single): Single;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function AuWaveformTriangle(t: Single): Single;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function AuWaveformSawtooth(t: Single): Single;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function AuWaveformRectangle(t: Single): Single;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function AuGargleFilter(ABuf: PSingle; ABufSize: Integer; ASampleRate: Integer;
  AChannels: integer; AFreq: Single; APhase: Single;
  AWaveform: TAuWaveform): Single;
function AuWaveform(ABuf: PSingle; ABufSize: Integer; ASampleRate: Integer;
  AChannels: integer; AFreq: Single; APhase: Single;
  AWaveform: TAuWaveform): Single;

type
  TSingleArray = array[0..0] of Single;
  PSingleArray = ^TSingleArray;

  TAuIIRWorkingset = record
    XV: array of Single;
    YV: array of Single;
  end;
  
  TAuIIRFilter = class
    private
      FKernelAlpha: PSingle;
      FKernelBeta: PSingle;
      FKernelAlphaSize: integer;
      FKernelBetaSize: integer;
      FChannels: integer;
      FWorkingset: array of TAuIIRWorkingset;
      FWPTR, FWPTR2: integer;
      FGain: Single;
    public
      constructor Create(AKernelAlpha, AKernelBeta: PSingle; AAlphaSize,
        ABetaSize: integer; AChannels: integer; AGain: Single);
      destructor Destroy;override;

      procedure ClearWorkingset;
      procedure Apply(AIn, AOut: PSingle; ASamples: integer);
      procedure Apply2(AIn, AOut: PSingle; ASamples: integer);
  end;

implementation

function AuWaveformSine(t: Single): Single;
begin
  result := Sin(t * 2 * PI);
end;

function AuWaveformTriangle(t: Single): Single;
var
  ft: Single;
begin
  ft := frac(t);
  if ft < 0.5 then
    result := ft * 4 - 1
  else
    result := 1 - (ft-0.5) * 4;
end;

function AuWaveformSawtooth(t: Single): Single;
begin
  result := frac(t) * 2 - 1;
end;

function AuWaveformRectangle(t: Single): Single;
begin
  if frac(t) < 0.5 then
    result := 1
  else
    result := -1;  
end;

function AuGargleFilter(ABuf: PSingle; ABufSize: Integer; ASampleRate: integer;
  AChannels: integer; AFreq: Single; APhase: Single;
  AWaveform: TAuWaveform): Single;
var
  i: integer;
  pos: Single;
begin
  for i := 0 to ABufSize div SizeOf(Single) - 1 do
  begin
    pos := APhase + AFreq / ASampleRate * (i div AChannels);

    case AWaveform of
      auwfSine:
        ABuf^ := ABuf^ * (AuWaveformSine(pos) + 1) * 0.5;
      auwfTriangle:
        ABuf^ := ABuf^ * (AuWaveformTriangle(pos) + 1) * 0.5;
      auwfSawtooth:
        ABuf^ := ABuf^ * (AuWaveformSawtooth(pos) + 1) * 0.5;
      auwfRectangle:
        ABuf^ := ABuf^ * (AuWaveformRectangle(pos) + 1) * 0.5;
    end;

    inc(ABuf);
  end;

  //Prevent loosing percision with increasing time position values
  result := Frac(APhase + AFreq / ASampleRate * (ABufSize div (SizeOf(Single) * AChannels)));
end;

function AuWaveform(ABuf: PSingle; ABufSize: Integer; ASampleRate: integer;
  AChannels: integer; AFreq: Single; APhase: Single;
  AWaveform: TAuWaveform): Single;
var
  i: integer;
  pos: Single;
begin
  for i := 0 to ABufSize div SizeOf(Single) - 1 do
  begin
    pos := APhase + AFreq / ASampleRate * (i div AChannels);

    case AWaveform of
      auwfSine:
        ABuf^ := AuWaveformSine(pos);
      auwfTriangle:
        ABuf^ := AuWaveformTriangle(pos);
      auwfSawtooth:
        ABuf^ := AuWaveformSawtooth(pos);
      auwfRectangle:
        ABuf^ := AuWaveformRectangle(pos);
    end;

    inc(ABuf);
  end;

  //Prevent loosing percision with increasing time position values
  result := Frac(APhase + AFreq / ASampleRate * (ABufSize div (SizeOf(Single) * AChannels)));
end;


{ TAuIIRFilter }

constructor TAuIIRFilter.Create(AKernelAlpha, AKernelBeta: PSingle; AAlphaSize,
  ABetaSize: integer; AChannels: integer; AGain: Single);
var
  i: integer;
begin
  inherited Create;

  //Copy the alpha kernel
  FKernelAlphaSize := AAlphaSize;
  GetMem(FKernelAlpha, FKernelAlphaSize * SizeOf(Single));
  AcMove(AKernelAlpha^, FKernelAlpha^, FKernelAlphaSize * SizeOf(Single));

  //Copy the beta kernel
  FKernelBetaSize := ABetaSize;
  GetMem(FKernelBeta, FKernelBetaSize * SizeOf(Single));
  AcMove(AKernelBeta^, FKernelBeta^, FKernelBetaSize * SizeOf(Single));

  //Reserve the workingset memory
  FChannels := AChannels;
  SetLength(FWorkingset, FChannels);
  for i := 0 to AChannels - 1 do
  begin
    SetLength(FWorkingset[i].XV, FKernelAlphaSize);
    SetLength(FWorkingset[i].YV, FKernelBetaSize + 1);
  end;

  FGain := 1 / AGain;
end;

destructor TAuIIRFilter.Destroy;
begin
  FreeMem(FKernelAlpha, FKernelAlphaSize * SizeOf(Single));
  FreeMem(FKernelBeta, (FKernelBetaSize + 1) * SizeOf(Single));

  inherited Destroy;
end;

{procedure TAuIIRFilter.Apply(AIn, AOut: PSingle; ASamples: integer);

function GetOldSample(AChan, AIndex: integer): Single;
begin
  result := PSingleArray(FWorkingset[AChan])[(FWorkingsetPtr - AIndex) mod FKernelBetaSize];
end;

var
  i, j, k: integer;
  np: integer;
  pin, pout: PSingleArray;
begin
  pin := PSingleArray(AIn);
  pout := PSingleArray(AOut);

  for i := 0 to ASamples - 1 do
  begin
    np := (FWorkingsetPtr + 1) mod FKernelBetaSize;
    for j := 0 to FChannels - 1 do
    begin
      pout[i * FChannels + j] := 0;

      //Apply the alpha path
      for k := 0 to FKernelAlphaSize - 1 do
        pout[i * FChannels + j] := pout[i * FChannels + j] + pin[(i - k) * FChannels] * PSingleArray(FKernelAlpha)[k] / 3.608416605e+01;

      //Apply the beta path
      for k := 0 to FKernelBetaSize - 1 do
        pout[i * FChannels + j] := pout[i * FChannels + j] + GetOldSample(j, k) * PSingleArray(FKernelBeta)[k];

      //Copy the new output sample to the workingset
      PSingleArray(FWorkingset[j])[np] := pout[i * FChannels + j];
    end;

    //Increment the workingset pointer
    FWorkingsetPtr := np;
  end;
end;   }

procedure TAuIIRFilter.Apply(AIn, AOut: PSingle; ASamples: integer);
var
  i, j, k: integer;
  pin, pout: PSingleArray;
begin
  pin := PSingleArray(AIn);
  pout := PSingleArray(AOut);

  for i := 0 to ASamples - 1 do
  begin
    for j := 0 to FChannels - 1 do
    begin
      with FWorkingset[j] do
      begin
        //Shuffle the X-Values
        for k := 1 to High(XV) do
          XV[k - 1] := XV[k];
        XV[High(XV)] := pin[i * FChannels + j] * FGain;

        //Shuffle the Y-Values
        for k := 1 to High(YV) do
          YV[k - 1] := YV[k];
        YV[High(YV)] := 0;

        //Apply the alpha path
        for k := 0 to FKernelAlphaSize - 1 do
          YV[High(YV)] := YV[High(YV)] + XV[k] * PSingleArray(FKernelAlpha)[k];

        //Apply the beta path
        for k := 0 to FKernelBetaSize - 1 do
          YV[High(YV)] := YV[High(YV)] + YV[k] * PSingleArray(FKernelBeta)[k];

        pout[i * FChannels + j] := YV[High(YV)];
      end;
    end;
  end;
end;

procedure TAuIIRFilter.Apply2(AIn, AOut: PSingle; ASamples: integer);
var
  i, j, k: integer;
  pin, pout: PSingleArray;
  elm: integer;
begin
  pin := PSingleArray(AIn);
  pout := PSingleArray(AOut);
  
  for i := 0 to ASamples - 1 do
  begin
    for j := 0 to FChannels - 1 do
    begin
      with FWorkingset[j] do
      begin
        XV[FWPTR] := pin[i * FChannels + j] * FGain;

        //Shuffle the Y-Values
        for k := 1 to High(YV) do
          YV[k - 1] := YV[k];
        YV[High(YV)] := 0;

        //Apply the alpha path
        for k := 0 to FKernelAlphaSize - 1 do
        begin
          elm := (FWPTR - k) mod FKernelAlphaSize;
          if elm < 0 then
            elm := FKernelAlphaSize + elm;
          YV[High(YV)] := YV[High(YV)] + XV[elm] * PSingleArray(FKernelAlpha)[k];
        end;

        //Apply the beta path
        for k := 0 to FKernelBetaSize - 1 do
          YV[High(YV)] := YV[High(YV)] + YV[k] * PSingleArray(FKernelBeta)[k];

        pout[i * FChannels + j] := YV[High(YV)];
      end;
    end;
    FWPTR := (FWPTR + 1) mod FKernelAlphaSize;
    FWPTR2 := (FWPTR2 + 1) mod (FKernelBetaSize + 1);
  end;
end;

procedure TAuIIRFilter.ClearWorkingset;
var
  i, j: integer;
begin
  for i := 0 to FChannels - 1 do
  begin
    for j := 0 to FKernelAlphaSize - 1 do
      FWorkingset[i].XV[j] := 0.0;

    for j := 0 to FKernelBetaSize do
      FWorkingset[i].YV[j] := 0.0;
  end;
  FWPTR := 0;
  FWPTR2 := 0;
end;

end.
