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

File: AuWAVFormat.pas
Author: Andreas Stöckel
}

{Contains a native pascal WAVE file reader.}
unit AuWAVFormat;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AuUtils, AuTypes, AuProtocolClasses, AuDecoderClasses;

type
  TAuWAVFmt = packed record
    HeadSize   : LongInt;
    FormatTag  : Word;                { Format Tag}
    Channels   : Word;                { Numer of channels}
    SampleRate : LongInt;             { Sample rate in kHz}
    BytesPSec  : LongInt;             { Bytes per second }
    BlockAlign : Word;                { Block align}
    BitsPSmp   : Word;                { Bits per sample}  
  end;
  
  TAuWAVFile = class
    private
      FAudioParameters: TAuAudioParametersEx;
      FProtocol: TAuProtocol;
      FOpened: boolean;
      FFmt: TAuWAVFmt;
      FFileSize: LongInt;
      FDataSize: LongInt;
      FFilePos: LongInt;
      FBPSec: Cardinal;
      FBPSmp: Cardinal;
      function SearchHeader(AName: LongWord; AMaxCount: integer): boolean;
    public
      function Open(AProtocol: TAuProtocol): boolean;
      procedure Close;

      function Seek(APos: Cardinal): boolean;
      function Read(ABuf: PByte; ASampleCount: Cardinal; var ATimeCode: Cardinal): integer;
      function StreamLength: integer;

      property Parameters: TAuAudioParametersEx read FAudioParameters;
      property BytesPerSample: Cardinal read FBPSmp;
  end;

const
  RIFF_HDR =  $46464952; //'RIFF'
  RIFF_WAVE = $45564157; //'WAVE'
  RIFF_FMT =  $20746D66; //'fmt '
  RIFF_DATA = $61746164; //'data'

implementation

{ TAuWAVFile }

function TAuWAVFile.Open(AProtocol: TAuProtocol): boolean;
begin
  Close;

  FProtocol := AProtocol;

  FFilePos := 0;

  if SearchHeader(RIFF_HDR, 20) then
  begin
    //Read the file size
    FProtocol.Read(PByte(@FFileSize), SizeOf(FFileSize));

    //Search the "WAVE" and the "fmt " header
    if SearchHeader(RIFF_WAVE, 20) and SearchHeader(RIFF_FMT, 20) then
    begin
      //Read the file format
      FProtocol.Read(PByte(@FFmt), SizeOf(TAuWAVFmt));

      //Check whether the format is supported
      if FFmt.FormatTag = 1 then
      begin
        if SearchHeader(RIFF_DATA, 20) then
        begin
          //Read data size
          FProtocol.Read(PByte(@FDataSize), SizeOf(FDataSize));

          //Set audio parameters record
          FAudioParameters.Frequency := FFmt.SampleRate;
          FAudioParameters.BitDepth := FFmt.BitsPSmp;
          FAudioParameters.Channels := FFmt.Channels;

          //Calculate a few constants that will be needed lateron
          FBPSec := AuBytesPerSecond(FAudioParameters);
          FBPSmp := AuBytesPerSample(FAudioParameters);

          FOpened := true;
        end;
      end;
    end;
  end;

  result := FOpened;
end;

procedure TAuWAVFile.Close;
begin
  FProtocol := nil;
  FOpened := false;
end;

function TAuWAVFile.Read(ABuf: PByte; ASampleCount: Cardinal; var ATimeCode: Cardinal): integer;
var
  c: integer;
begin
  result := 0;
  if FOpened then
  begin
    //Calculate the count of bytes that should be read
    c := ASampleCount * FBPSmp;
    if c + FFilePos > FDataSize then
      c := FDataSize - FFilePos;

    //Calculate time code
    ATimeCode := round(FFilePos / FBPSec * 1000);

    result := FProtocol.Read(ABuf, c);
    FFilePos := FFilePos + result;
  end;
end;

function TAuWAVFile.SearchHeader(AName: LongWord; AMaxCount: integer): boolean;
var
  read: integer;
  read_count: integer;
  head: LongWord;
  b: byte;
begin
  read_count := FProtocol.Read(PByte(@head), SizeOf(LongWord));
  while (head <> AName) and (read_count < AMaxCount) do
  begin
    read := FProtocol.Read(@b, 1);
    head := (head shr 8) or (b shl 24);
    read_count := read_count + read;

    if read = 0 then
      break;
  end;

  result := head = AName;
end;

function TAuWAVFile.Seek(APos: Cardinal): boolean;
var
  bpos: Int64;
begin
  result := false;
  
  if FProtocol.Seekable then
  begin
    //Calculate byte position
    bpos := Round(Int64(APos) * Int64(FBPSec) / 1000);
    bpos := (bpos div Integer(FBPSmp)) * Integer(FBPSmp);

    //Check bounds
    if bpos < 0 then
      bpos := 0
    else if bpos > FDataSize then
      bpos := FDataSize;

    //Calculate relative movement
    bpos := bpos - FFilePos;

    //Set the new file position
    FFilePos := FFilePos + bpos;    

    FProtocol.Seek(aupsFromCurrent, bpos);

    result := true;
  end;
end;

function TAuWAVFile.StreamLength: integer;
begin
  result := round(FDataSize / FBPSec * 1000);
end;

end.
