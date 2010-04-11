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

--------------------------------------------------------------------------------

Based on the original source code "mkfilter":
mkfilter -- given n, compute recurrence relation
to implement Butterworth, Bessel or Chebyshev filter of order n
A.J. Fisher, University of York   <fisher@minster.york.ac.uk>
September 1992

http://www-users.cs.york.ac.uk/~fisher/mkfilter

The source code has been translated and adapted for use in the Audorra 3D Audio
library by Andreas Stöckel.

--------------------------------------------------------------------------------
}

unit mkfilter;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Math;

type
  TmkFilterCharacteristic = (
    mkfcBessel,
    mkfcButterworth,
    mkfcChebyshev
  );

  TmkFilterType = (
    mkftLowpass,
    mkftHighpass,
    mkftBandpass,
    mkftBandstop
  );

  TmkFilterCoeffs = record
    gain: Double;
    xcoeffs: array of Single;
    ycoeffs: array of Single;
  end;
  PmkFilterCoeffs = ^TmkFilterCoeffs;

const
  CC_OK = 0;
  CC_ERR_INVALID_PARAM = -1;
  CC_ERR_CALC = -2;

function mkCalcCoefficients(ACharacteristic: TmkFilterCharacteristic;
  AFilterType: TmkFilterType; AOrder: Integer; ASampleFreq: Double;
  AFreq1, AFreq2: Double; var ACoeffs: TmkFilterCoeffs): Integer;
  
implementation

//
// Complex number types and functions
//

type
  TComplex = record
    re, im: Double;
  end;
  PComplex = ^TComplex;

  TComplexArray = array[0..0] of TComplex;
  PComplexArray = ^TComplexArray;

function Complex(r: Double; i: Double = 0): TComplex;overload;
begin
  with result do
  begin
    re := r;
    im := i;
  end;
end;

function Complex: TComplex;overload;
begin
  with result do
  begin
    re := 0.0;
    im := 0.0;
  end;
end;

function cconj(const z: TComplex): TComplex;
begin
  result := z;
  result.im := -result.im;
end;

function cmul(const z1, z2: TComplex): TComplex;
begin
  result.re := z1.re * z2.re - z1.im * z2.im;
  result.im := z1.re * z2.im + z1.im * z2.re;
end;

function cmuls(const z: TComplex; const a: Double): TComplex;
begin
  result.re := z.re * a;
  result.im := z.im * a;
end;

function cdiv(const z1, z2: TComplex): TComplex;
var
  mag: Double;
begin
  mag := (z2.re * z2.re) + (z2.im * z2.im);
  result.re := ((z1.re * z2.re) + (z1.im * z2.im)) / mag;
  result.im := ((z1.im * z2.re) - (z1.re * z2.im)) / mag;
end;  

function cdivs(const z: TComplex; const a: Double): TComplex;
begin
  result.re := z.re / a;
  result.im := z.im / a;
end;

function cadd(const z1, z2: TComplex): TComplex;
begin
  result.re := z1.re + z2.re;
  result.im := z1.im + z2.im;
end;

function csub(const z1, z2: TComplex): TComplex;
begin
  result.re := z1.re - z2.re;
  result.im := z1.im - z2.im;
end;

function ceq(const z1, z2: TComplex): Boolean;
begin
  result :=
    (z1.re = z2.re) and
    (z1.im = z2.im);
end;

function csqr(const z: TComplex): TComplex;
begin
  result := cmul(z, z);
end;

function csqrt(const x: TComplex): TComplex;
var
  r: Double;
begin
  r := hypot(x.re, x.im);
  result.re := sqrt(0.5 * (r + x.re));
  result.im := sqrt(0.5 * (r - x.re));
  if x.im < 0 then
    result.im := -result.im;
end;

function cexpj(const theta: Double): TComplex;
begin
  result := Complex(cos(theta), sin(theta));
end;

function cexp(const z: TComplex): TComplex;
begin
  result := cmuls(cexpj(z.im), exp(z.re));
end;

function ceval(coeffs: PComplexArray; n: integer; z: TComplex): TComplex;
var
  i: integer;
begin
  result := Complex;
  for i := n downto 0 do
    result := cadd(cmul(result, z), coeffs[i]);
end;

function cevaluate(topco: PComplexArray; nz: Integer; botco: PComplexArray;
  np: Integer; z: TComplex): TComplex;
begin
  result := cdiv(ceval(topco, nz, z), ceval(botco, np, z));
end;

//
//.......
//

const
   bessel_poles: array[0..29] of TComplex = (
    (re: -1.00000000000e+00; im: 0.00000000000e+00),
    (re: -1.10160133059e+00; im: 6.36009824757e-01),
    (re: -1.32267579991e+00; im: 0.00000000000e+00),
    (re: -1.04740916101e+00; im: 9.99264436281e-01),
    (re: -1.37006783055e+00; im: 4.10249717494e-01),
    (re: -9.95208764350e-01; im: 1.25710573945e+00),
    (re: -1.50231627145e+00; im: 0.00000000000e+00),
    (re: -1.38087732586e+00; im: 7.17909587627e-01),
    (re: -9.57676548563e-01; im: 1.47112432073e+00),
    (re: -1.57149040362e+00; im: 3.20896374221e-01),
    (re: -1.38185809760e+00; im: 9.71471890712e-01),
    (re: -9.30656522947e-01; im: 1.66186326894e+00),
    (re: -1.68436817927e+00; im: 0.00000000000e+00),
    (re: -1.61203876622e+00; im: 5.89244506931e-01),
    (re: -1.37890321680e+00; im: 1.19156677780e+00),
    (re: -9.09867780623e-01; im: 1.83645135304e+00),
    (re: -1.75740840040e+00; im: 2.72867575103e-01),
    (re: -1.63693941813e+00; im: 8.22795625139e-01),
    (re: -1.37384121764e+00; im: 1.38835657588e+00),
    (re: -8.92869718847e-01; im: 1.99832584364e+00),
    (re: -1.85660050123e+00; im: 0.00000000000e+00),
    (re: -1.80717053496e+00; im: 5.12383730575e-01),
    (re: -1.65239648458e+00; im: 1.03138956698e+00),
    (re: -1.36758830979e+00; im: 1.56773371224e+00),
    (re: -8.78399276161e-01; im: 2.14980052431e+00),
    (re: -1.92761969145e+00; im: 2.41623471082e-01),
    (re: -1.84219624443e+00; im: 7.27257597722e-01),
    (re: -1.66181024140e+00; im: 1.22110021857e+00),
    (re: -1.36069227838e+00; im: 1.73350574267e+00),
    (re: -8.65756901707e-01; im: 2.29260483098e+00)
  );

const
  MAXPZ = 512;

type
  TPlane = record
    poles: array[0..MAXPZ] of TComplex;
    zeros: array[0..MAXPZ] of TComplex;
    numpoles: integer;
    numzeros: integer;
  end;

  TmkFilterCalc = record
    chara: TmkFilterCharacteristic;
    atype: TmkFilterType;
    order: Integer;
    alpha1: Double;
    alpha2: Double;
    walpha1: Double;
    walpha2: Double;
    splane: TPlane;
    zplane: TPlane;

    coeffs: PmkFilterCoeffs;
  end;
  PmkFilterCalc = ^TmkFilterCalc;

function computesplane(c: PmkFilterCalc): Boolean;
var
  p, i: Integer;
  theta: double;
  z: TComplex;
begin
  result := false;
  
  //Calculate the s-plane for a bessel filter
  if c^.chara = mkfcBessel then
  begin
    p := sqr(c^.order) div 4;

    //For an uneven order (or one), add an additional bessel pole
    if c^.order mod 2 = 1 then
    begin
      c^.splane.poles[c^.splane.numpoles] := bessel_poles[p];
      inc(c^.splane.numpoles);
      p := p + 1;
    end;

    //Add the bessel poles
    for i := 0 to c^.order div 2 - 1 do
    begin
      c^.splane.poles[c^.splane.numpoles] := bessel_poles[p];
      inc(c^.splane.numpoles);

      c^.splane.poles[c^.splane.numpoles] := cconj(bessel_poles[p]);
      inc(c^.splane.numpoles);

      p := p + 1;
    end;

    result := true;      
  end else

  //Calculate the s-plane for a Butterworth filter
  if c^.chara = mkfcButterworth then
  begin
    for i := 0 to 2 * c^.order - 1 do
    begin
      if c^.order mod 2 = 1 then
        theta := i * PI / c^.order
      else
        theta := ((i + 0.5) * PI) / c^.order;

      z := cexpj(theta);
      if z.re < 0 then
      begin
        c^.splane.poles[c^.splane.numpoles] := z;
        inc(c^.splane.numpoles);
      end;
    end;

    result := true;
  end;  
end;

function normalizefilter(c: PmkFilterCalc): Boolean;
var
  w0, w1, w2, bw: Double;
  i: integer;
  hba, temp: TComplex;
begin
  result := true;
  
  w1 := 2 * PI * c^.walpha1;
  w2 := 2 * PI * c^.walpha2;

  case c^.atype of
    mkftLowpass:
    begin
      for i := 0 to c^.splane.numpoles - 1 do
        c^.splane.poles[i] := cmul(c^.splane.poles[i], Complex(w1));
      c^.splane.numzeros := 0;
    end;

    mkftHighpass:
    begin
      for i := 0 to c^.splane.numpoles - 1 do
        c^.splane.poles[i] := cdiv(Complex(w1), c^.splane.poles[i]);
      for i := 0 to c^.splane.numpoles - 1 do
        c^.splane.zeros[i] := Complex(0.0);
	    c^.splane.numzeros := c^.splane.numpoles;
    end;

    mkftBandpass:
    begin
      w0 := sqrt(w1 * w2);
      bw := w2 - w1;
      for i := 0 to c^.splane.numpoles - 1 do
      begin
        hba := cmuls(cmul(c^.splane.poles[i], Complex(bw)), 0.5);
        temp := csqrt(csub(Complex(1.0), csqr(cdiv(Complex(w0), hba))));
        c^.splane.poles[i] := cmul(hba, (cadd(Complex(1.0), temp)));
        c^.splane.poles[c^.splane.numpoles + i] := cmul(hba, (csub(Complex(1.0), temp)));
      end;

      for i := 0 to c^.splane.numpoles - 1 do
        c^.splane.zeros[i] := Complex(0.0);

      c^.splane.numzeros := c^.splane.numpoles;
      c^.splane.numpoles := c^.splane.numpoles * 2;
    end;

    mkftBandstop:
    begin
      w0 := sqrt(w1 * w2);
      bw := w2 - w1;

      for i := 0 to c^.splane.numpoles - 1 do
      begin
        hba := cmul(Complex(0.5), cdiv(Complex(bw), c^.splane.poles[i]));
        temp := csqrt(csub(Complex(1.0), csqr(cdiv(Complex(w0), hba))));
        c^.splane.poles[i] := cmul(hba, (cadd(Complex(1.0), temp)));
        c^.splane.poles[c^.splane.numpoles + i] := cmul(hba, (csub(Complex(1.0), temp)));
      end;

      for i := 0 to c^.splane.numpoles - 1 do
      begin
        c^.splane.zeros[i] := Complex(0.0, w0);
        c^.splane.zeros[c^.splane.numpoles + i] := Complex(0.0, - w0);
      end;

      c^.splane.numpoles := c^.splane.numpoles * 2;
      c^.splane.numzeros := c^.splane.numpoles;
    end;
  end;
end;

function computezblt(c: PmkFilterCalc): Boolean;

  function blt(const z: TComplex): TComplex;
  begin
    result :=
      cdiv(cadd(Complex(2.0), z),
           csub(Complex(2.0), z));
  end;

var
  i: integer;
begin
  result := true;

  //Copy the count of poles and zeros
  c^.zplane.numpoles := c^.splane.numpoles;
  c^.zplane.numzeros := c^.splane.numzeros;

  //Transform the poles to the z plane using Bilineartransform (blt)
  for i := 0 to c^.zplane.numpoles - 1 do
    c^.zplane.poles[i] := blt(c^.splane.poles[i]);
           
  //Transform the zeros to the z plane using Bilineartransform (blt)
  for i := 0 to c^.zplane.numzeros - 1 do
    c^.zplane.zeros[i] := blt(c^.splane.zeros[i]); 

  //Fill the zplane zeros
  while (c^.zplane.numzeros < c^.zplane.numpoles) do
  begin
    c^.zplane.zeros[c^.zplane.numzeros] := Complex(-1.0);
    c^.zplane.numzeros := c^.zplane.numzeros + 1;
  end;
end;

function expandpoly(c: PmkFilterCalc): Boolean;

  procedure multin(const w: TComplex; n: Integer; coeffs: PComplexArray);
  var
    nw: TComplex;
    i: integer;
  begin
    nw := csub(Complex(0.0), w);
    for i := n downto 1 do
      coeffs^[i] := cadd(cmul(nw, coeffs^[i]), coeffs^[i - 1]);
    coeffs^[0] := cmul(nw, coeffs^[0]);
  end;

  function expand(pz: PComplexArray; n: integer;
    coeffs: PComplexArray): Boolean;
  var
    i: integer;
  begin
    result := true;

    coeffs^[0] := Complex(1.0);
    for i := 0 to n - 1 do
      coeffs^[i + 1] := Complex(0.0);

    for i := 0 to n - 1 do
      multin(pz^[i], n, coeffs);

    for i := 0 to n do
    begin
      if Abs(coeffs^[i].im) > 0.0000000001 then
      begin
        result := false;
        exit;
      end;
    end;
  end;  

var
  topcoeffs: array[0..MAXPZ] of TComplex;
  botcoeffs: array[0..MAXPZ] of TComplex;
  i: integer;
  theta: double;
  gain: TComplex;
  dcgain: TComplex;
  hfgain: TComplex;
  fcgain: TComplex;
begin
  result := false;

  if expand(PComplexArray(@(c^.zplane.zeros[0])), c^.zplane.numzeros,
       PComplexArray(@topcoeffs[0])) and
     expand(PComplexArray(@(c^.zplane.poles[0])), c^.zplane.numpoles,
       PComplexArray(@botcoeffs[0])) then
  begin
    //Calculate the center frequency "theta"
    theta := PI * (c^.alpha1 + c^.alpha2); //2 * PI * 0.5 * (FFreq1 + FFreq2);

    //Calculate gain at dc
    dcgain := cevaluate(PComplexArray(@topcoeffs[0]), c^.zplane.numzeros,
      PComplexArray(@botcoeffs[0]), c^.zplane.numpoles, Complex(1.0));

    //Calculate gain at center
    fcgain := cevaluate(@topcoeffs, c^.zplane.numzeros, @botcoeffs,
      c^.zplane.numpoles, cexpj(theta));

    //Calculate gain at hf
    hfgain := cevaluate(@topcoeffs, c^.zplane.numzeros, @botcoeffs,
      c^.zplane.numpoles, Complex(-1.0));

    //Write the results
    //.................

    //Calculate the gain value
    case c^.atype of
      mkftLowpass:
        gain := dcgain;
      mkftHighpass:
        gain := hfgain;
      mkftBandpass:
        gain := fcgain;
      mkftBandstop:
        gain := csqrt(cmul(dcgain, hfgain));
    end;
    c^.coeffs^.gain := Hypot(gain.re, gain.im);

    //Calculate the X coeffs
    SetLength(c^.coeffs^.xcoeffs, c^.zplane.numzeros + 1);
    for i := 0 to c^.zplane.numzeros do
      c^.coeffs^.xcoeffs[i] :=  (topcoeffs[i].re / botcoeffs[c^.zplane.numpoles].re);

    //Calculate the Y coeffs
    SetLength(c^.coeffs^.ycoeffs, c^.zplane.numpoles);
    for i := 0 to c^.zplane.numpoles - 1 do
      c^.coeffs^.ycoeffs[i] := -(botcoeffs[i].re / botcoeffs[c^.zplane.numpoles].re);

    result := true;
  end;
end;

function mkCalcCoefficients(ACharacteristic: TmkFilterCharacteristic;
  AFilterType: TmkFilterType; AOrder: Integer; ASampleFreq: Double;
  AFreq1, AFreq2: Double; var ACoeffs: TmkFilterCoeffs): Integer;
var
  calc: PmkFilterCalc;
begin
  result := CC_ERR_CALC;

  New(calc); //As "calc" is quite large, I don't want to put it on the stack.
  try
    FillChar(calc^, SizeOf(calc^), 0);

    //Check the parameters for validity
    if (AOrder < 1) or (ASampleFreq < 0) or (AFreq1 < 0) or
       ((AFreq2 < 0) and (AFilterType >= mkftBandpass)) or
       ((AFreq2 < AFreq1) and (AFilterType >= mkftBandpass)) then
      //Return "CC_ERR_INVALID_PARAM" in order to inform the caller of the
      //function that the supplied parameters were wrong.
      result := CC_ERR_INVALID_PARAM
    else begin
      //Parameters are ok, fill the workingset
      with calc^ do
      begin
        chara := ACharacteristic;
        atype := AFilterType;
        order := AOrder;

        //Calculate the alpha values from the given frequencies
        alpha1 := AFreq1 / ASampleFreq;
        if (AFilterType >= mkftBandpass) then
          alpha2 := AFreq2 / ASampleFreq
        else
          alpha2 := alpha1;
          
        walpha1 := tan(PI * alpha1) / PI;
        walpha2 := tan(PI * alpha2) / PI;

        coeffs := @ACoeffs;
      end;

      if computesplane(calc) and normalizefilter(calc) and computezblt(calc) and
         expandpoly(calc) then
        result := CC_OK
      else
        result := CC_ERR_CALC;
    end;
  finally
    Dispose(calc);
  end;
end;

end.

