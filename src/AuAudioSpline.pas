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

File: AuAudioSpline.pas
Author: Andreas Stöckel
}

{Contains functions and types for a cubic spline interpolation for audio purposes.} 
unit AuAudioSpline;

interface

type
  {This record contains the cubic spline factors and can be used to interpolate
   between two values v1 and v2. The spline function is f(t) = a*t^3 + b*t^2 +
   c*t + d, wheras t should be a value between 0 and 1.
   @seealso(PAuSplineData)
   @seealso(TAuSplineProcessor)
   @seealso(AuSplineCalcValue)
   @seealso(AuSplineStart)
   @seealso(AuSplineStop)
   @seealsp(AuSplineFeed)}
  TAuSplineData = record
    a, //< "a" factor for the cubic spline interpolation
    b, //< "b" factor for the cubic spline interpolation
    c, //< "c" factor for the cubic spline interpolation
    d: Single; //< "d" factor for the cubic spline interpolation
  end;
  {Pointer on the TAuSplineData structure.
   @seealso(TAuSplineData)}
  PAuSplineData = ^TAuSplineData;

  {Record which holds the current data needed for efficient spline interpolation.
   A new spline processor can be created using the AuSplineStart function. A spline
   processor should always be freed using the AuSplineStop function.
   @seealso(PAuSplineProcessor)
   @seealso(AuSplineStart)
   @seealso(AuSplineStop)}
  TAuSplineProcessor = record
    lv1, //< "lv1" is the first value of the piece which has to be interpolated
    lv2, //< "lv2" is the second value of the piece which has to be interpolated
    ldv: Single; //< "ldv" contains the gradient at "lv1".
  end;
  {Pointer to a TAuSplineProcessor structure.}
  PAuSplineProcessor = ^TAuSplineProcessor;

{Evaluates the cubic spline function f(t) = a*t^3 + b*t^2 + c*t + d. The constant
 factors are written in the "data" record. "t" should be a value between 0 and 1.
 A "data" record can be created using the AuSplineFeed function. 
 @seealso(AuSplineFeed)}
function AuSplineCalcValue(const t: Single; const data: PAuSplineData): Single;

{Creates a new TAuSplineProcessor. v1 and v2 are the first two values of the spline.
 Every spline processor should be destroyed using the AuSplineStop function.
@seealso(AuSplineStop)
@seealso(AuSplineFeed)}
function AuSplineStart(const v1, v2: Single): PAuSplineProcessor;
{Destroys a spline processor created by AuSplineStart.
@seealso(AuSplineStart)}
procedure AuSplineStop(const processor: PAuSplineProcessor);
{Adds a new value "v" to the spline defined by the "processor" value. The data writen
 to the "data" parameter contains the interpolation factors for the last two spline factors:
 So if v1 and v2 are already written to the spline, "v" represents "v3". Interpolation
 is done between v1 and v2 using the gradient created by "v". In a next step, "v1" is set
 to the value of "v2" and "v2" is set to the value of "v".
 @seealso(AuSplineStart)
 @seealso(AuSplineStop)
 @seealso(TAuSplineData)
 @seealso(TAuSplineProcessor)}
procedure AuSplineFeed(const processor: PAuSplineProcessor;
  const v: Single; const data: PAuSplineData);

implementation

var
  i: integer;

function AuSplineCalcValue(const t: Single; const data: PAuSplineData): Single;
var
  ft: single;
begin
  //f(t) = at³ + bt² + ct + d
  ft := t;
  result := data^.d + data^.c * ft;
  ft := ft * t;
  result := result  + data^.b * ft;
  ft := ft * t;
  result := result  + data^.a * ft;

//  result := data^.a * (1-t) + data^.b * t;
end;

procedure AuSplineStop(const processor: PAuSplineProcessor);
begin
  Dispose(processor);
end;

function AuSplineStart(const v1, v2: Single): PAuSplineProcessor;
begin
  //Create a new spline processor
  New(Result);

  //Initialize the spline data
  Result^.lv1 := v1;
  Result^.lv2 := v2;
  Result^.ldv := 0;
end;

procedure AuSplineFeed(const processor: PAuSplineProcessor;
  const v: Single; const data: PAuSplineData);
var
  l1, l2, d: Single;
begin
  //CUSTOM CUBIC SPLINE INTERPOLATION

  //f(t ) = at³ + bt² + ct + d
  //f'(t) = 3at² + 2bt + c
  //f(0)  = d = y0                  ( = processor.lv1)
  //f'(0) = c = y'0                 ( = processor.lvd)
  //f(1)  = a + b + c + d = y1      ( = processor.lv2)
  //f'(1) = 3a + 2b + c = y'1       ( = d)

  //l1 = y'1 - y'0
  //l2 = y1 - y'0 - y0

  //a = l2 - 2 * l1
  //b = 3 * l1 - l2
  //c = y'0
  //d = y0

  //Gradient
  //d = (y2 - y0) / 2
  //If y1 is smaller than y2 and y0 or it is grater than both, d = 0  


  data^.d := processor^.lv1;
  data^.c := processor^.ldv;

  //Caculate the spline gradient at lv2
  if ((processor^.lv1 < processor^.lv2) and (v < processor^.lv2)) or
     ((processor^.lv1 > processor^.lv2) and (v > processor^.lv2)) then
    d := 0
  else
    d := (v - processor^.lv1) / 2;

  l2 := d - processor^.ldv;
  l1 := processor^.lv2 - processor^.ldv - processor^.lv1;

  data^.a := l2 - 2 * l1;
  data^.b := 3 * l1 - l2;

{  data^.a := processor^.lv1;
  data^.b := processor^.lv2; }

  //Store the new data in the processor
  processor^.lv1 := processor^.lv2;
  processor^.lv2 := v;
  processor^.ldv := d;
end;

end.
