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

File: Au3DRingBuffer.pas
Author: Andreas Stöckel
}

{Contains a ring buffer class for accessing audio data on a time and not a sample
 basis.}
unit Au3DRingBuffer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AuTypes, AuUtils, AuAudioSpline;

type
  //Thread secure ring buffer
  TAuRingBuffer = class
    private
      FMem: PByte;
      FSize: Cardinal;
      FWrtPtr: Integer;
      FWrtPos: Int64;
    public
      constructor Create(ASize: Cardinal);
      destructor Destroy;override;

      //Returns the count of bytes which totaly have been written to the buffer
      function WriteData(ACount: Cardinal; AData: PByte): Int64;

      function GetData(APos: Int64): PByte;// inline;

      procedure Clear;

      property Size: Cardinal read FSize;
  end;

  TAu3DAudioRingBuffer = class
    private
      FRings: array of TAuRingBuffer;
      FProcessors: array of PAuSplineProcessor;
      FInitializedProcessors: boolean;
      FChannels: Integer;

      FSize: Integer;
      FFilled: Integer;
      FPos: TAuSamplestamp;
      FSmplSize: integer;
      FLoop: Boolean;
      FLocked: Boolean;
    public
      constructor Create(ASampleCount: integer; AChannels: integer);
      destructor Destroy;override;

      function GetSample(ATime: TAuSamplestamp; AChannel: Integer): Single; inline;

      procedure WriteSamples(ACount: Cardinal; ABuf: PSingle);

      procedure Clear;

      property Size: Integer read FSize;
      property Filled: Integer read FFilled;
      property SmplPos: TAuSamplestamp read FPos;
      property SmplSize: Integer read FSmplSize;

      property Locked: Boolean read FLocked write FLocked;
      property Loop: Boolean read FLoop write FLoop;
  end;

const
  Au3DRingBuffer_TblEntries = (1 shl 16);

var
  Au3DRingBuffer_Tbl: array[0..Au3DRingBuffer_TblEntries-1] of Single;
  
implementation

{ TAuRingBuffer }

constructor TAuRingBuffer.Create(ASize: Cardinal);
begin
  inherited Create;

  //Reserve memory for the ring buffer
  FSize := ASize;
  GetMem(FMem, FSize);

  Clear;
end;

destructor TAuRingBuffer.Destroy;
begin
  //Free the ring memory
  if FMem <> nil then
    FreeMem(FMem, FSize);
  FMem := nil;
  FSize := 0;

  inherited;
end;

function TAuRingBuffer.GetData(APos: Int64): PByte;
var
  min: Int64;
begin
  //Calculate the start position
  min := FWrtPos - FSize;
  if min < 0 then
    min := 0;

  if (APos >= min) and (APos < FWrtPos) then
  begin
    result := FMem;
    inc(result, Integer(APos mod FSize));
  end else
    result := nil;
end;

procedure TAuRingBuffer.Clear;
begin
  //Reset the write position and the write pointer
  FWrtPos := 0;
  FWrtPtr := 0;
end;

function TAuRingBuffer.WriteData(ACount: Cardinal; AData: PByte): Int64;
var
  psrc, ptar: PByte;
  rs: Integer;
begin
  result := -1;

  if ACount <= FSize then
  begin
    //Calculate the remaining space in the ring buffer
    rs := Integer(FSize) - FWrtPtr;

    ptar := FMem;
    inc(ptar, FWrtPtr);
    //The data which has to be written perfectly fits into the remaining space
    //at the end of the ring buffer
    if Integer(ACount) < rs then
    begin
      //Writeln(Integer(ptar):10, #9, Integer(FMem):10, #9, rs:10, #9, ACount:10, #9, FWrtPtr:10);
      Move(AData^, ptar^, ACount)
    end
    else begin
      //Copy the first part to the ring
      psrc := AData;
      Move(psrc^, ptar^, rs);
      inc(psrc, rs);

      //Copy the second part to the ring
      ptar := FMem;
      rs := Integer(ACount) - rs;
      Move(psrc^, ptar^, rs);
    end;

    //Calculate the position of the new write pointer
    FWrtPtr := (FWrtPtr + Integer(ACount)) mod Integer(FSize);

    //Calculate the new byte count position and return it
    FWrtPos := FWrtPos + ACount;
    result := FWrtPos;
  end;
end;

{ TAu3DAudioRingBuffer }

procedure TAu3DAudioRingBuffer.Clear;
var
  i: integer;
begin
  FPos := 0;
  FFilled := 0;
  
  for i := 0 to High(FRings) do
    FRings[i].Clear;  
end;

constructor TAu3DAudioRingBuffer.Create(ASampleCount: integer;
  AChannels: integer);
var
  i: integer;
begin
  inherited Create;

  FLoop := false;
  FChannels := AChannels;
  FSmplSize := ASampleCount;
  FSize := ASampleCount * SizeOf(TAuSplineData);

  SetLength(FRings, FChannels);
  SetLength(FProcessors, FChannels);

  FInitializedProcessors := false;

  for i := 0 to FChannels - 1 do
  begin
    //Create the ring buffers
    FRings[i] := TAuRingBuffer.Create(FSize);

    //Initialize the processors
    FProcessors[i] := nil;
  end;
end;

destructor TAu3DAudioRingBuffer.Destroy;
var
  i: integer;
begin
  for i := 0 to FChannels - 1 do
  begin
    //Destroy the ring buffers
    FreeAndNil(FRings[i]);

    //Destroy the spline processors
    if FProcessors[i] <> nil then
      AuSplineStop(FProcessors[i]);
    FProcessors[i] := nil;
  end;
    
  inherited;
end;

function TAu3DAudioRingBuffer.GetSample(ATime: TAuSampleStamp;
  AChannel: Integer): Single;
var
  bytepos: Int64;
  data: PAuSplineData;
  f: Single;
begin
  //Return zero
  result := 0;

  if not FLocked then
  begin
    //Calculate the position of the sample in the byte stream
    bytepos := (ATime div (1 shl 16)) * SizeOf(TAuSplineData);

    if FLoop then
    begin
      if bytepos < 0 then
        bytepos := bytepos + FRings[AChannel].Size
      else if bytepos > FRings[AChannel].Size then
        bytepos := bytepos - FRings[AChannel].Size;
    end;

    //Obtain the pointer to the spline data structure
    data := PAuSplineData(FRings[AChannel].GetData(bytepos));

    //If the data is in the ring buffer, interpolate it to the given time
    if data <> nil then
    begin
      f := Au3DRingBuffer_Tbl[(ATime and $FFFF)];
      result := AuSplineCalcValue(f, data);
    end;
  end;
end;

procedure TAu3DAudioRingBuffer.WriteSamples(ACount: Cardinal; ABuf: PSingle);
var
  i, smploffs, c, w: Integer;
  ps: PSingle;
  pt: PAuSplineData;
  v1, v2: Single;
  buf: array of PAuSplineData;
  bufsize: Integer;
begin
  //Reserve memory for the target values
  SetLength(buf, FChannels);
  bufsize := ACount * SizeOf(TAuSplineData);
  for i := 0 to FChannels - 1 do
    GetMem(buf[i], bufsize);
    
  try
    //Increase the current max position pointer
    FPos := FPos + ACount;

    //Increase the filled counter
    FFilled := FFilled + Integer(ACount * SizeOf(TAuSplineData));
    if FFilled > FSize then
      FFilled := FSize;

    smploffs := 0;

    //Initialize the processors, if this hasn't been done now
    if (not FInitializedProcessors) and (ACount >= 2) then
    begin
      FInitializedProcessors := true;
      smploffs := 2;
      for i := 0 to FChannels - 1 do
      begin
        ps := ABuf;
        //Get the first start value
        inc(ps, i);
        v1 := ps^;

        //Get the second start value
        inc(ps, FChannels);
        v2 := ps^;

        //Create a new spline
        FProcessors[i] := AuSplineStart(v1, v2);
      end;
    end;

    if FInitializedProcessors then
    begin
      c := Integer(ACount) - smploffs;
      w := 0;
      ps := ABuf;
      inc(ps, smploffs * Integer(FChannels));

      //Calculate the data values
      while c > 0 do
      begin
        for i := 0 to FChannels - 1 do
        begin
          //Get the position in the target buffer
          pt := buf[i];
          inc(pt, w);

          //Add the value to the spline
          AuSplineFeed(FProcessors[i], ps^, pt);

          //Increment the source position
          inc(ps);
        end;
        
        w := w + 1;
        c := c - 1;
      end;

      //Write  the result data into the ring buffers
      for i := 0 to FChannels - 1 do
        FRings[i].WriteData(w * SizeOf(TAuSplineData), PByte(buf[i]));
    end;
  finally
    //Free the memory reserved for the target values
    for i := 0 to FChannels - 1 do
      FreeMem(buf[i], bufsize);
  end;                    
end;

var
  i: integer;

initialization
  for i := 0 to Au3DRingBuffer_TblEntries - 1 do
      Au3DRingBuffer_Tbl[i] := i / Au3DRingBuffer_TblEntries;

end.

