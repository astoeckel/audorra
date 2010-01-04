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

File: AuMPG123.pas
Author: Andreas Stöckel
}

{This unit allows Audorra to use the MPG123 decoder.}
unit AuMPG123;

interface

uses
  mpg123,
  AcPersistent,
  AuTypes, AuProtocolClasses, AuDecoderClasses;

type
  TAuMPG123Decoder = class(TAuDecoder)
    private
      FDec: PMPG123Handle;
      FInBuf: PByte;
      FInBufSize: integer;
      FOutBuf: PByte;
      FOutBufSize: integer;
      FOutBufCount: Cardinal;
      FFrequency: integer;
      FBitDepth: integer;
      FChannels: integer;
      FTimecode: Cardinal;
      FMustRead: boolean;
      FProtocol: TAuProtocol;
      function OpenFeedDecoder: boolean;
      function OpenStreamDecoder: boolean;
      function DecodeFeed: TAuDecoderState;
      function DecodeStream: TAuDecoderState;
    protected
      function GetInfo: TAuAudioParametersEx;override;
    public
      constructor Create(AProtocol: TAuProtocol);override;
      destructor Destroy;override;

      function OpenDecoder: boolean;override;
      procedure CloseDecoder;override;

      function Decode: TAuDecoderState;override;

      function SeekTo(ACur, ATar: integer): boolean;override;
      function StreamLength: integer;override;

      procedure GetPacket(var APacket: TAuPacket);override;
  end;

implementation

{ TAuMPG123Decoder }

constructor TAuMPG123Decoder.Create(AProtocol: TAuProtocol);
begin
  inherited;
  FProtocol := AProtocol;
end;

destructor TAuMPG123Decoder.Destroy;
begin
  CloseDecoder;
  inherited;
end;


procedure TAuMPG123Decoder.CloseDecoder;
begin
  if FDec <> nil then
  begin
    mpg123_close(FDec);
    mpg123_delete(FDec);

    FDec := nil;
  end;

  if FInBuf <> nil then  
    FreeMem(FInBuf, FInBufSize);
  FInBuf := nil;
  FInBufSize := 0;

  if FOutBuf <> nil then
    FreeMem(FOutBuf, FOutBufSize);
  FOutBuf := nil;
  FOutBufSize := 0;
  FOutBufCount := 0;

  FTimecode := 0;
end;

function TAuMPG123Decoder.Decode: TAuDecoderState;
begin
  result := audsEnd;
  
  if FDec <> nil then
  begin
    if FProtocol.Seekable then
      result := DecodeStream
    else
      result := DecodeFeed;
  end;
end;

function TAuMPG123Decoder.DecodeFeed: TAuDecoderState;
var
  read: Integer;
  res: integer;
begin
  result := audsEnd;
  
  if FMustRead then
  begin
    read := FProtocol.Read(FInBuf, FInBufSize);
    FMustRead := false;
    if read = 0 then
    begin
      result := audsEnd;
      exit;
    end;
  end else
    read := 0;

  FTimecode := mpg123_tell(FDec);
  res := mpg123_decode(FDec, FInBuf, read, FOutBuf, FOutBufSize, @FOutBufCount);

  case res of
    MPG123_NEED_MORE:
    begin
      if FOutBufCount <= 0 then
        result := audsIncomplete
      else
        result := audsHasFrame;
      FMustRead := true;
    end;
    MPG123_OK:
      result := audsHasFrame;
  end;
end;

function TAuMPG123Decoder.DecodeStream: TAuDecoderState;
var
  res: integer;
begin
  FTimecode := mpg123_tell(FDec);
  res := mpg123_read(FDec, FOutBuf, FOutBufSize, @FOutBufCount);
  if res = MPG123_OK then
    result := audsHasFrame
  else
    result := audsEnd;
end;

function TAuMPG123Decoder.GetInfo: TAuAudioParametersEx;
begin
  with result do
  begin
    Frequency := FFrequency;
    Channels := FChannels;
    BitDepth := FBitDepth;
  end;
end;

procedure TAuMPG123Decoder.GetPacket(var APacket: TAuPacket);
begin
  with APacket do
  begin
    Buffer := FOutBuf;
    BufferSize := FOutBufCount;
    Timecode := round(FTimecode / FFrequency * 1000);
  end;
end;

function TAuMPG123Decoder.OpenDecoder: boolean;
begin
  CloseDecoder;

  result := false;

  //Create a new decoder
  FDec := mpg123_new(nil, nil);
  if FDec <> nil then
  begin
    //Reserve memory for the output
    FOutBufSize := 32768;
    GetMem(FOutBuf, FOutBufSize);

    if FProtocol.Seekable then
      result := OpenStreamDecoder
    else
      result := OpenFeedDecoder;
  end;
end;

function TAuMPG123Decoder.OpenFeedDecoder: boolean;
var
  read, c: integer;
  enc: integer;
  res: integer;
  atend: boolean;
begin
  result := false;

  //Open the decoder
  mpg123_open_feed(FDec);

  //Reserve memory for the input
  FInBufSize := 16384;
  GetMem(FInBuf, FInBufSize);

  //Read some data from the stream
  atend := false;
  c := 0;
  repeat
    inc(c);
    read := FProtocol.Read(FInBuf, FInBufSize);
    if read <> 0 then
    begin
      atend := true;
      res := mpg123_decode(FDec, FInBuf, FInBufSize, nil, 0, nil);
      case res of
        MPG123_NEW_FORMAT:
        begin
          mpg123_getformat(FDec, @FFrequency, @FChannels, @enc);
          FBitDepth := 16;
          result := true;
        end;
        MPG123_NEED_MORE:
          atend := false;
      end;
    end;
  until atend or (c > 5);
end;

function read_proc(fd: integer; buf: PByte; count: integer): integer;cdecl;
begin
  result := TAuMPG123Decoder(Pointer(fd)).FProtocol.Read(buf, count);
end;

function seek_proc(fd: integer; count: integer; offset: integer): integer;cdecl;
begin
  if offset in [0, 1, 2] then
    result := TAuMPG123Decoder(Pointer(fd)).FProtocol.Seek(TAuProtocolSeekMode(offset), count)
  else
    result := -1;
end;

function TAuMPG123Decoder.OpenStreamDecoder: boolean;
var
  enc: integer;
begin
  result := false;
  
  if (mpg123_replace_reader(FDec, @read_proc, @seek_proc) < 0) or
     (mpg123_open_fd(FDec, Integer(self)) < 0) or
     (mpg123_getformat(FDec, @FFrequency, @FChannels, @enc) < 0) then
    exit;

  if (FFrequency = 0) or (FChannels = 0) then
    exit;  
    
  FBitDepth := 16;

  result := true;
end;

function TAuMPG123Decoder.SeekTo(ACur, ATar: integer): boolean;
begin
  result :=
    (FDec <> nil) and
    (mpg123_seek(FDec, trunc(ATar / 1000 * FFrequency), 0) >= 0);
end;

function TAuMPG123Decoder.StreamLength: integer;
begin
  result := -1;
  if FDec <> nil then
    result := round(mpg123_length(FDec) / FFrequency * 1000);
end;

function CreateMPG123Decoder(AProtocol: TAuProtocol): TAuMPG123Decoder;
begin
  result := TAuMPG123Decoder.Create(AProtocol);
end;

initialization
  if InitMPG123 then
    AcRegSrv.RegisterClass(TAuMPG123Decoder, @CreateMPG123Decoder);

finalization
  FinalizeMPG123;

end.
