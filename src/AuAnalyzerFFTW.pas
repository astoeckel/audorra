unit AuAnalyzerFFTW;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fftw,

  AcSysUtils,
  AuUtils, AuTypes, AuAnalyzerClasses, AuFFT, AuComplex, syncobjs;

type

  { TAuAnalyzerFFTW }

  TAuAnalyzerFFTW = class (TAuAnalyzer)
  private
    FSampleCount: integer;
    FOutSampleCount: Integer;
    FFFTPlan: array of fftw_plan;
    FFFTBuffer: array of PDouble;
    FFFTIn: array of PDouble;
    FFFTOut: array of Pcomplex_double;
    FLock: TCriticalSection;
  protected
    procedure Burn;
    procedure DoAnalyze(ASamples: PSingle; ACount: Cardinal);override;
    procedure DoSetParameters;override;
  public
    constructor Create(ASampleCount: Integer);
    destructor Destroy;override;

    procedure GetChannelData(AChannel: integer; var AData: TAuComplexArray);
    function GetDataSize: integer;
    procedure SetSampleCount(const ASampleCount: Integer);
  end;

implementation

{ TAuAnalyzerFFTW }

procedure TAuAnalyzerFFTW.Burn;
var
  I: Integer;
begin
  if Length(FFFTPlan) > 0 then
  begin
    for I := 0 to High(FFFTIn) do
    begin
      FreeMem(FFFTBuffer[I]);
      fftw_freemem(FFFTIn[I]);
      fftw_freemem(FFFTOut[I]);
      fftw_destroy_plan(FFFTPlan[I]);
    end;
  end;
end;

procedure TAuAnalyzerFFTW.DoAnalyze(ASamples: PSingle; ACount: Cardinal);
var
  I, J: Integer;
  CC, BlockSize, Remaining: Integer;
  PIn: PSingle;
begin
  FLock.Enter;

  CC := Parameters.Channels - 1;
  if ACount >= FSampleCount then
  begin
    {Inc(ASamples, (ACount - FSampleCount) * (CC+1));
    for I := 0 to ACount - 1 do
    begin
      for J := 0 to CC do
      begin
        FFFTIn[J][I] := ASamples^;
        Inc(ASamples);
      end;
    end;}
    ACount := FSampleCount;
  end;
  //else
  begin
    BlockSize := ACount * SizeOf(Double);
    Remaining := FSampleCount - ACount;
    for I := 0 to CC do
    begin
      Move(FFFTBuffer[I][ACount], FFFTBuffer[I][0], Remaining * SizeOf(Double));
    end;
    PIn := ASamples;
    for J := 0 to ACount - 1 do
      for I := 0 to CC do
      begin
        FFFTBuffer[I][J + Remaining] := PIn^;
        Inc(PIn);
      end;
  end;

  for I := 0 to CC do
  begin
    for J := 0 to FSampleCount - 1 do
    begin
      //FFFTBuffer[I][J] := Sin((J / 100.0) * 2 * pi) + Sin((J / 1000.0) * 2 * pi) + Sin((J / 10.0) * 2 * pi);
      FFFTIn[I][J] := FFFTBuffer[I][J] * 0.5 * (1+cos(2*pi*((J / FSampleCount)+0.5)));
    end;
    fftw_execute(FFFTPlan[I]);
  end;

  FLock.Leave;
end;

procedure TAuAnalyzerFFTW.DoSetParameters;
var
  i: integer;
  ISize, OSize: SizeInt;
begin
  FLock.Enter;
  try
    Burn;

    ISize := SizeOf(Double) * FSampleCount;
    OSize := SizeOf(complex_double) * FOutSampleCount;
    SetLength(FFFTIn, Parameters.Channels);
    SetLength(FFFTBuffer, Parameters.Channels);
    SetLength(FFFTOut, Parameters.Channels);
    SetLength(FFFTPlan, Parameters.Channels);
    for I := 0 to Parameters.Channels - 1 do
    begin
      fftw_getmem(FFFTIn[I], ISize);
      fftw_getmem(FFFTOut[I], OSize);
      FFFTBuffer[I] := GetMem(ISize);

      FillByte(FFFTIn[I]^, ISize, 0);
      FillByte(FFFTOut[I]^, OSize, 0);
      FillByte(FFFTBuffer[I]^, ISize, 0);

      FFFTPlan[I] := fftw_plan_dft_1d(FSampleCount, FFFTIn[I], FFFTOut[I], []);
    end;
  finally
    FLock.Leave;
  end;
end;

constructor TAuAnalyzerFFTW.Create(ASampleCount: Integer);
begin
  inherited Create;

  FSampleCount := ASampleCount;
  FOutSampleCount := ASampleCount div 2 + 1;
  FLock := TCriticalSection.Create;
end;

destructor TAuAnalyzerFFTW.Destroy;
begin
  FLock.Enter;
  Burn;
  FLock.Free;
  inherited Destroy;
end;

procedure TAuAnalyzerFFTW.GetChannelData(AChannel: integer;
  var AData: TAuComplexArray);
begin
  CritSect.Enter;
  try
    if (AChannel >= 0) and (AChannel < Length(FFFTPlan)) then
    begin
      SetLength(AData, FOutSampleCount);
      AcMove(FFFTOut[AChannel]^, AData[0], FOutSampleCount * SizeOf(TAuComplex));
    end;
  finally
    CritSect.Leave;
  end;
end;

function TAuAnalyzerFFTW.GetDataSize: integer;
begin
  result := FOutSampleCount;
end;

procedure TAuAnalyzerFFTW.SetSampleCount(const ASampleCount: Integer);
begin
  FSampleCount := ASampleCount;
  FOutSampleCount := ASampleCount div 2 + 1;
  DoSetParameters;
end;

end.

