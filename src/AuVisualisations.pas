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

File: AuVisualisations.pas
Author: Andreas Stöckel
}

{Contains classes for the graphical output of visualisations.}
unit AuVisualisations;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF WIN32}Windows,{$ELSE}LCLIntf,{$ENDIF}
  SysUtils, Classes, Graphics,
  AuTypes, AuUtils, AuAnalyzerClasses, AuAnalyzers, AuComplex;

type
  TAuVisualisation = class
  private
    FWidth, FHeight: integer;
  protected
    FAnalyzer: TAuAnalyzer;
  public
    constructor Create;
    destructor Destroy; override;

    function Update: boolean; virtual;
    procedure Draw(ATar: TCanvas); virtual;
    procedure Resize(AWidth, AHeight: integer); virtual;

    procedure Activate;
    procedure Deactivate;

    property Analyzer: TAuAnalyzer Read FAnalyzer;
    property Width: integer Read FWidth;
    property Height: integer Read FHeight;
  end;

  TAuCompositingVisualisation = class(TAuVisualisation)
  private
    FBG:   TBitmap;
    FFG:   TBitmap;
    FComp: TBitmap;
    FTransparent: boolean;
  protected
    procedure DrawBG(ACanvas: TCanvas); virtual; abstract;
    procedure DrawFG(ACanvas: TCanvas); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw(ATar: TCanvas); override;
    procedure Resize(AWidth, AHeight: integer); override;

    property Transparent: boolean Read FTransparent Write FTransparent;
  end;

  TAuPeakMeterVisualisation = class(TAuCompositingVisualisation)
  private
    FPeaks: TAuPeaks;
    FGradientColor1: TColor;
    FGradientColor2: TColor;
  protected
    procedure DrawBG(ACanvas: TCanvas); override;
    procedure DrawFG(ACanvas: TCanvas); override;
  public
    constructor Create;
    destructor Destroy; override;

    function Update: boolean; override;

    property GradientColor1: TColor Read FGradientColor1 Write FGradientColor1;
    property GradientColor2: TColor Read FGradientColor2 Write FGradientColor2;
  end;

  TAuOsciloscopeVisualisation = class(TAuCompositingVisualisation)
  private
    FOszMem:     PByte;
    FOszMemSize: integer;
    FGradientColor1: TColor;
    FGradientColor2: TColor;
  protected
    procedure DrawBG(ACanvas: TCanvas); override;
    procedure DrawFG(ACanvas: TCanvas); override;
  public
    constructor Create(ATime: single);
    destructor Destroy; override;

    function Update: boolean; override;

    property GradientColor1: TColor Read FGradientColor1 Write FGradientColor1;
    property GradientColor2: TColor Read FGradientColor2 Write FGradientColor2;
  end;

  TAuFFTVisualisation = class(TAuCompositingVisualisation)
  private
    FFFTData: TAuComplexArray;
    FGradientColor1: TColor;
    FGradientColor2: TColor;
  protected
    procedure DrawBG(ACanvas: TCanvas); override;
    procedure DrawFG(ACanvas: TCanvas); override;
  public
    constructor Create(AFFTSize: integer);
    destructor Destroy; override;

    function Update: boolean; override;

    property GradientColor1: TColor Read FGradientColor1 Write FGradientColor1;
    property GradientColor2: TColor Read FGradientColor2 Write FGradientColor2;
  end;


implementation

{ TAuVisualisation }

constructor TAuVisualisation.Create;
begin
  inherited Create;

  Resize(100, 100);
end;

destructor TAuVisualisation.Destroy;
begin
  inherited;
end;

procedure TAuVisualisation.Activate;
begin
  FAnalyzer.Active := True;
end;

procedure TAuVisualisation.Deactivate;
begin
  FAnalyzer.Active := False;
end;

procedure TAuVisualisation.Resize(AWidth, AHeight: integer);
begin
  FWidth  := AWidth;
  FHeight := AHeight;
end;

function TAuVisualisation.Update: boolean;
begin
  Result := False;
end;

procedure TAuVisualisation.Draw(ATar: TCanvas);
begin
  //Nothing to do here
end;


{ TAuCompositingVisualisation }

constructor TAuCompositingVisualisation.Create;
begin
  FBG   := TBitmap.Create;
  FFG   := TBitmap.Create;
  FComp := TBitmap.Create;
  FTransparent := False;
  inherited;
end;

destructor TAuCompositingVisualisation.Destroy;
begin
  FreeAndNil(FBG);
  FreeAndNil(FFG);
  FreeAndNil(FComp);

  inherited;
end;

procedure TAuCompositingVisualisation.Draw(ATar: TCanvas);
begin
  FComp.Transparent := False;

  //Redraw the foreground
  FFG.Canvas.Brush.Color := clBlack;
  FFG.Canvas.FillRect(Rect(0, 0, Width, Height));
  DrawFG(FFG.Canvas);

  //Composite the background and the foreground

  //1. Draw the background bitmap
  BitBlt(FComp.Canvas.Handle, 0, 0, Width, Height,
    FBG.Canvas.Handle, 0, 0, SRCCOPY);

  //2. Draw the foreground bitmap with the "AND" operator
  BitBlt(FComp.Canvas.Handle, 0, 0, Width, Height,
    FFG.Canvas.Handle, 0, 0, SRCAND);

  //3. Draw the comp bitmap to the target
  if FComp.Transparent then
  begin
    FComp.TransparentColor := clBlack;
    FComp.Transparent      := True;
  end;
  ATar.Draw(0, 0, FComp);
end;

procedure TAuCompositingVisualisation.Resize(AWidth, AHeight: integer);
begin
  inherited;

  //Redraw the background
  with FBG do
  begin
    Width  := AWidth;
    Height := AHeight;
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
    DrawBG(FBg.Canvas);
  end;

  //Resize the foreground bitmap
  FFG.Width  := AWidth;
  FFG.Height := AHeight;

  //Resize the compositing bitmap
  FComp.Width  := AWidth;
  FComp.Height := AHeight;
end;

function RGB(r, g, b: byte): TColor;
begin
  Result := (r) or (g shl 8) or (b shl 16);
end;

function GetRValue(col: TColor): byte;
begin
  Result := col and 255;
end;

function GetGValue(col: TColor): byte;
begin
  Result := (col shr 8) and 255;
end;

function GetBValue(col: TColor): byte;
begin
  Result := (col shr 16) and 255;
end;

const
  clWebWhite     = clWhite;
  clWebOrangeRed = $0045FF;


function ColorBetween(AC1, AC2: TColor; v: single): TColor;
begin
  result := RGB(
    round(GetRValue(AC1) * (1-v) + GetRValue(AC2) * (v)),
    round(GetGValue(AC1) * (1-v) + GetGValue(AC2) * (v)),
    round(GetBValue(AC1) * (1-v) + GetBValue(AC2) * (v)));
end;

procedure DrawHGradient(ATar: TCanvas; AW, AH: integer; AC1, AC2: TColor;
  AX, AY: integer);
var
  i: integer;
begin
  ATar.Pen.Style := psSolid;
  for i := 0 to AW do
  begin
    ATar.Pen.Color := ColorBetween(AC1, AC2, i / AW);
    ATar.MoveTo(i + AX, 0 + AY);
    ATar.LineTo(i + AX, AH + AY);
  end;
end;

procedure DrawVGradient(ATar: TCanvas; AW, AH: integer; AC1, AC2: TColor;
  AX, AY: integer);
var
  i: integer;
begin
  ATar.Pen.Style := psSolid;
  for i := 0 to AH do
  begin
    ATar.Pen.Color := ColorBetween(AC1, AC2, i / AH);
    ATar.MoveTo(0 + AX, i + AY);
    ATar.LineTo(AW + AX, i + AY);
  end;
end;

{ TAuPeakMeterVisualisation }

constructor TAuPeakMeterVisualisation.Create;
begin
  inherited Create;

  FAnalyzer := TAuPeakMeter.Create;
  FGradientColor1 := clWebOrangeRed;
  FGradientColor2 := clWhite;
end;

destructor TAuPeakMeterVisualisation.Destroy;
begin
  FAnalyzer.Free;
  inherited;
end;

procedure TAuPeakMeterVisualisation.DrawBG(ACanvas: TCanvas);
begin
  DrawHGradient(ACanvas, Width, Height, FGradientColor1, FGradientColor2, 0, 0);
end;

procedure TAuPeakMeterVisualisation.DrawFG(ACanvas: TCanvas);
var
  i: integer;
  h: integer;
  w: integer;
begin
  with ACanvas do
  begin
    if Width <= 0 then
      exit;

    h := Height div Length(FPeaks.ChannelPeaks);

    Brush.Color := clWhite;
    Pen.Color   := clWhite;

    for i := 0 to High(FPeaks.ChannelPeaks) do
    begin
      w := Round((AuToDezibel(FPeaks.ChannelPeaks[i]) + 30) * 0.0333 * Width);
      if w < 0 then
        w := 0;

      Rectangle(0, i * h + 1, w, (i + 1) * h - 1);
    end;
  end;
end;

function TAuPeakMeterVisualisation.Update: boolean;
begin
  Result := TAuPeakMeter(FAnalyzer).GetPeaks(FPeaks);
end;

{ TAuOsciloscopeVisualisation }

constructor TAuOsciloscopeVisualisation.Create(ATime: single);
begin
  inherited Create;

  FAnalyzer := TAuOscilloscope.Create(ATime);
  FGradientColor1 := clWebOrangeRed;
  FGradientColor2 := clWebWhite;
end;

destructor TAuOsciloscopeVisualisation.Destroy;
begin
  if FOszMem <> nil then
    FreeMem(FOszMem, FOszMemSize);
  FAnalyzer.Free;
  inherited;
end;

procedure TAuOsciloscopeVisualisation.DrawBG(ACanvas: TCanvas);
begin
  DrawVGradient(ACanvas, Width, Height div 2, FGradientColor2, FGradientColor1, 0, 0);
  DrawVGradient(ACanvas, Width, Height div 2 + 1, FGradientColor1,
    FGradientColor2, 0, Height div 2);
end;

procedure TAuOsciloscopeVisualisation.DrawFG(ACanvas: TCanvas);
var
  i, j:   integer;
  ps:     PSingle;
  val:    single;
  cc, sc: integer;
  lx, x, y: integer;
  c_n, c_p: integer;
  max_n, max_p: single;
begin
  with ACanvas do
  begin
    if Width <= 0 then
      exit;

    if FOszMem <> nil then
    begin
      Pen.Color := clWhite;
      Pen.Style := psSolid;

      ps := PSingle(FOszmem);
      cc := TAuOscilloscope(FAnalyzer).Parameters.Channels;
      sc := FOszMemSize div integer(
        AuBytesPerSample(TAuOscilloscope(FAnalyzer).Parameters));

      lx    := 0;
      max_n := 0;
      max_p := 0;
      c_p   := 0;
      c_n   := 0;

      for i := 0 to sc - 1 do
      begin
        x := Round(Width / (sc - 1) * i);

        //Read  a sample
        val := 0;
        for j := 0 to cc - 1 do
        begin
          val := val + ps^;
          Inc(ps);
        end;
        val := val / cc;

        if (val > max_p) then
        begin
          max_p := val;
          c_p   := c_p + 1;
        end;
        if (val < max_n) then
        begin
          max_n := val;
          c_n   := c_n + 1;
        end;

        if (x <> lx) or (i = sc - 1) then
        begin
          if c_n > c_p then
            val := max_n
          else
            val := max_p;

          y := round((Height / 2) * (1 + val));
          if lx = 0 then
            MoveTo(0, y)
          else
            LineTo(lx, y);

          lx    := x;
          max_n := 0;
          max_p := 0;
          c_n   := 0;
          c_p   := 0;
        end;
      end;
    end;
  end;
end;

function TAuOsciloscopeVisualisation.Update: boolean;
begin
  FOszMemSize := TAuOscilloscope(FAnalyzer).GetDataSize;
  ReallocMem(FOszMem, FOszMemSize);
  Result := TAuOscilloscope(FAnalyzer).GetData(FOszMem);
end;

{ TAuFFTVisualisation }

constructor TAuFFTVisualisation.Create(AFFTSize: integer);
begin
  inherited Create;

  FAnalyzer := TAuFFT.Create(AFFTSize);

  FGradientColor1 := clWhite;
  FGradientColor2 := clWebOrangeRed;
end;

destructor TAuFFTVisualisation.Destroy;
begin
  FAnalyzer.Free;
  inherited;
end;

procedure TAuFFTVisualisation.DrawBG(ACanvas: TCanvas);
begin
  DrawVGradient(ACanvas, Width, Height, FGradientColor1, FGradientColor2, 0, 0);
end;

procedure TAuFFTVisualisation.DrawFG(ACanvas: TCanvas);
const
  barwidth = 3;
  maxval   = 330.0;
  bargap   = 1;

var
  i, c:     integer;
  val:      extended;
  posx:     integer;
  posy:     integer;
  barcount: integer;
  fpb:      single;
  bar:      integer;
  bars:     array of single;
begin
  with ACanvas do
  begin
    if Width = 0 then
      exit;

    barcount := Width div (barwidth + bargap);
    if barcount = 0 then
      exit;

    c   := Round(Length(FFFTData) * 0.3);
    fpb := c / barcount;
    if fpb < 1 then
      fpb := 1;

    SetLength(bars, barcount);
    for i := 0 to High(bars) do
      bars[i] := 0;

    for i := 0 to c - 1 do
    begin
      bar := Round(i / fpb);
      if bar >= barcount then
        break;

      //Calculate the volume value
      val := Sqrt(Sqr(FFFTData[i].Re) + Sqr(FFFTData[i].Im));

      if val > bars[bar] then
        bars[bar] := val;
    end;

    Brush.Color := clWhite;
    Pen.Color   := clWhite;
    for i := 0 to barcount - 1 do
    begin
      //Calculate the x position of the bar
      posx := i * (barwidth + bargap);

      //Calculate the height of the bar
      posy := Round((AuToDezibel(bars[i] / maxval) + 30) * 0.033333 * Height);
      if posy > 0 then
        Rectangle(posx, Height, posx + barwidth, Height - posy);
    end;
  end;
end;

function TAuFFTVisualisation.Update: boolean;
begin
  SetLength(FFFTData, TAuFFT(FAnalyzer).GetDataSize);
  TAuFFT(FAnalyzer).GetChannelData(0, FFFTdata);
  Result := Length(FFFTData) > 0;
end;

end.

