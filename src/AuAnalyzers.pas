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

File: AuAnalyzers.pas
Author: Andreas Stöckel
}

{Contains simple audio analyzers (e.g. for visualisations)} 
unit AuAnalyzers;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils, Classes, SyncObjs,

  AuUtils, AuTypes, AuAnalyzerClasses, AuFFT, AuComplex;

type
  TAuPeakMeter = class(TAuAnalyzer)
    private
      FChannelPeaks: TAuChannelPeaks;
      FTmpChannelPeaks: TAuChannelPeaks;
      FPasses: Cardinal;
      ptr: PSingle;
      procedure ClearValues;
    protected
      procedure DoAnalyze(ASamples: PSingle; ACount: Cardinal);override;
      procedure DoSetParameters;override;
    public
      constructor Create;
      destructor Destroy;override;

      function GetPeaks(var APeaks: TAuPeaks): boolean;
  end;

  TAuOscilloscope = class(TAuAnalyzer)
    private
      FCurrentData: PByte;
      FDataSize: integer;
      FTimeSize: Single;
    protected
      procedure DoAnalyze(ASamples: PSingle; ACount: Cardinal);override;
      procedure DoSetParameters;override;
    public
      constructor Create(ATime: Single);
      destructor Destroy;override;

      function GetData(ATar: PByte): boolean;
      function GetDataSize: integer;
  end;

  TAuFFT = class(TAuAnalyzer)
    private
      FCurrentData: PByte;
      FDataSize: integer;
      FFFTDataSize: integer;
      FSampleCount: integer;
      FLog2Count: integer;
      FFFTData: array of TAuComplexArray;
    protected
      procedure DoAnalyze(ASamples: PSingle; ACount: Cardinal);override;
      procedure DoSetParameters;override;
    public
      constructor Create(ASampleCount: Integer);
      destructor Destroy;override;

      procedure GetChannelData(AChannel: integer; var AData: TAuComplexArray);
      function GetDataSize: integer;
  end;

implementation

{ TAuPeakMeter }

constructor TAuPeakMeter.Create;
begin
  inherited Create;
end;

destructor TAuPeakMeter.Destroy;
begin
  inherited;
end;

function TAuPeakMeter.GetPeaks(var APeaks: TAuPeaks): boolean;
var
  i: integer;
begin
  result := false;

  SetLength(APeaks.ChannelPeaks, Parameters.Channels);

  CritSect.Enter;
  try
    if FPasses > 0 then
    begin
      with APeaks do
      begin
        MixedValue := 0;

        for i := 0 to Parameters.Channels - 1 do
        begin
          ChannelPeaks[i] := FChannelPeaks[i] / FPasses;
          MixedValue := MixedValue + ChannelPeaks[i];
        end;

        MixedValue := MixedValue / Parameters.Channels;
      end;
      ClearValues;

      result := true;
    end;
  finally
    CritSect.Leave;
  end;      
end;

procedure TAuPeakMeter.ClearValues;
var
  i: integer;
begin
  FPasses := 0;
  for i := 0 to High(FChannelPeaks) do
    FChannelPeaks[i] := 0; 
end;

procedure TAuPeakMeter.DoAnalyze(ASamples: PSingle; ACount: Cardinal);
var
  i, j: integer;
  smpl: Single;
  data: PSingle;
begin
  data := ASamples;
  
  ptr := ASamples;
  for i := 0 to ACount - 1 do
  begin
    for j := 0 to Parameters.Channels - 1 do
    begin
      smpl := data^;
      if (i = 0) or (abs(smpl) > FTmpChannelPeaks[j]) then
        FTmpChannelPeaks[j] := abs(smpl);
      inc(data);
    end;
  end;

  for i := 0 to High(FChannelPeaks) do
    FChannelPeaks[i] := FChannelPeaks[i] + FTmpChannelPeaks[i];

  FPasses := FPasses + 1;
end;

procedure TAuPeakMeter.DoSetParameters;
begin
  SetLength(FChannelPeaks, Parameters.Channels);
  SetLength(FTmpChannelPeaks, Parameters.Channels);
end;

{ TAuOscilloscope }

constructor TAuOscilloscope.Create(ATime: Single);
begin
  inherited Create;

  FTimeSize := ATime;
end;

destructor TAuOscilloscope.Destroy;
begin
  if FCurrentData <> nil then
    FreeMem(FCurrentData, FDataSize);
    
  inherited;
end;

procedure TAuOscilloscope.DoAnalyze(ASamples: PSingle; ACount: Cardinal);
var
  ds: Integer;
  psrc: PByte;
begin
  //Calculate the data size
  ds := ACount * Cardinal(AuBytesPerSample(Parameters));
  if ds > FDataSize then
    ds := FDataSize;

  //Move the current data backward
  psrc := FCurrentData;
  inc(psrc, ds);
  Move(psrc^, FCurrentData^, FDataSize - ds);

  //Append the new data
  psrc := FCurrentData;
  inc(psrc, FDataSize - ds);
  Move(ASamples^, psrc^, ds);
end;

procedure TAuOscilloscope.DoSetParameters;
var
  smpls: Cardinal;
begin
  if FCurrentData <> nil then
    FreeMem(FCurrentData);

  smpls := round(FTimeSize * Parameters.Frequency);
  FDataSize := smpls * AuBytesPerSample(Parameters);
  GetMem(FCurrentData, FDataSize);

  //Zero the reserved memory
  FillChar(FCurrentData^, FDataSize, 0);
end;

function TAuOscilloscope.GetData(ATar: PByte): boolean;
begin
  result := false;

  if FDataSize > 0 then
  begin
    CritSect.Enter;
    try
      Move(FCurrentData^, ATar^, FDataSize);
      result := true;
    finally
      CritSect.Leave;
    end;
  end;
end;

function TAuOscilloscope.GetDataSize: integer;
begin
  result := FDataSize;
end;

{ TAuFFT }

function IsPowerOfTwo(Value: Cardinal): Boolean;
begin
  Result := (Value > 0) and (Value and (Value -1) = 0);
end;

function IntLog2(AVal: integer): integer;
var
  i: Integer;
begin
  result := 0;
  if AVal > 1 then
    for i := 0 to 31 do
    begin
      AVal := AVal shr 1;
      result := result + 1;
      if AVal = 1 then
        break;
    end;
end;

constructor TAuFFT.Create(ASampleCount: integer);
begin
  inherited Create;

  if not IsPowerOfTwo(ASampleCount) then
    raise Exception.Create('Must be power of two');//!

  FSampleCount := ASampleCount;
  FLog2Count := IntLog2(ASampleCount); 
end;

destructor TAuFFT.Destroy;
begin
  if FCurrentData <> nil then
    FreeMem(FCurrentData, FDataSize);

  inherited;
end;

procedure TAuFFT.DoAnalyze(ASamples: PSingle; ACount: Cardinal);
var
  ds: Integer;
  psrc: PByte;
  ps: PSingle;
  val: Single;
  i, j: integer;
begin
  //Calculate the data size
  ds := ACount * Cardinal(AuBytesPerSample(Parameters));
  if ds > FDataSize then
    ds := FDataSize;

  //Move the current data backward
  psrc := FCurrentData;
  inc(psrc, ds);
  Move(psrc^, FCurrentData^, FDataSize - ds);

  //Append the new data
  psrc := FCurrentData;
  inc(psrc, FDataSize - ds);
  Move(ASamples^, psrc^, ds);

  //Copy the channel data into the channel arrays
  ps := PSingle(FCurrentData);
  for i := 0 to FSampleCount - 1 do
  begin
    for j := 0 to Parameters.Channels - 1 do
    begin
      //Apply an hanning filter to the sample data
      val := ps^ * 0.5* (1+cos(2*pi*((i / FSampleCount)+0.5)));
      FFFTData[j][i] := Complex(val, 0);
      inc(ps);
    end;
  end;

  //Analyze the data
  for i := 0 to Parameters.Channels - 1 do
    ForwardFFT(FFFTData[i], FFFTData[i], Length(FFFTData[i]));
end;

procedure TAuFFT.DoSetParameters;
var
  i: integer;
begin
  if FCurrentData <> nil then
    FreeMem(FCurrentData);

  FDataSize := FSampleCount * Integer(AuBytesPerSample(Parameters));
  FFFTDataSize := FSampleCount * SizeOf(TAuComplex);

  //Reserve memory for the current data
  GetMem(FCurrentData, FDataSize);
  FillChar(FCurrentData^, FDataSize, 0);

  //Reserve space for the fft data and channel splitted data
  SetLength(FFFTData, Parameters.Channels, FSampleCount);
  for i := 0 to Parameters.Channels - 1 do
    FillChar(FFFTData[i][0], FFFTDataSize, 0);
end;

procedure TAuFFT.GetChannelData(AChannel: integer; var AData: TAuComplexArray);
begin
  CritSect.Enter;
  try
    if (AChannel >= 0) and (AChannel < Length(FFFTData)) then
      AData := Copy(FFFTData[AChannel], 0, FSampleCount);
  finally
    CritSect.Leave;
  end;
end;

function TAuFFT.GetDataSize: integer;
begin
  result := FSampleCount;
end;

end.
