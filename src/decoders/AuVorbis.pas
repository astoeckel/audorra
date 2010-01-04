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

File: AuVorbis.pas
Author: Andreas Stöckel
}

{Adds vorbis support to Audorra.}
unit AuVorbis;

interface

uses
  SysUtils, Classes,
  ogg, vorbis,
  AcPersistent,
  AuDecoderClasses, AuProtocolClasses,
  AuTypes, AuUtils;

type
  TAuVorbisDecoder = class(TAuDecoder)
    protected
      FVorbisFile: OggVorbis_File;
      FCallbacks: ov_callbacks;
      FOpened: boolean;
      FBuf: PByte;
      FBufSize: integer;
      FCurrent_Section: integer;
      FVI: pvorbis_info;
      FLen: Integer;
      function GetInfo: TAuAudioParametersEx;override;
    public
      constructor Create(AProtocol: TAuProtocol);override;
      destructor Destroy;override;

      function OpenDecoder: boolean;override;
      procedure CloseDecoder;override;

      function Decode: TAuDecoderState;override;

      procedure GetPacket(var APacket: TAuPacket);override;
      function SeekTo(ACur, ATar: integer): boolean;override;
      function StreamLength: integer;override;
  end;

implementation

function ogg_read_func(ptr: pointer; size, nmemb: csize_t; datasource: pointer): csize_t; cdecl;
begin
  with TAuVorbisDecoder(datasource) do
    result := FProtocol.Read(ptr, size * nmemb) div size;
end;

function ogg_seek_func(datasource: pointer; offset: ogg_int64_t; whence: cint): cint; cdecl;
begin
  with TAuVorbisDecoder(datasource) do
  begin
    result := -1;
    if FProtocol.Seekable then
    begin
      case whence of
        0: result := FProtocol.Seek(aupsFromBeginning, offset);
        1: result := FProtocol.Seek(aupsFromCurrent, offset);
        2: result := FProtocol.Seek(aupsFromEnd, offset);
      end;
    end;
  end;
end;

function ogg_close_func(datasource: pointer): cint; cdecl;
begin
  result := 0;
end;

function ogg_tell_func(datasource: pointer): clong; cdecl;
begin
  with TAuVorbisDecoder(datasource) do
  begin
    result := -1;
    if FProtocol.Seekable then
      result := FProtocol.Seek(aupsFromCurrent, 0);
  end;
end;

{ TAuVorbisDecoder }

constructor TAuVorbisDecoder.Create(AProtocol: TAuProtocol);
begin
  inherited;

  //Setup the callbacks record
  FCallbacks.read := ogg_read_func;
  FCallbacks.seek := ogg_seek_func;
  FCallbacks.close := ogg_close_func;
  FCallbacks.tell := ogg_tell_func;

  //Reserve some buffer memory
  GetMem(FBuf, 4096);

  FOpened := false;
end;

destructor TAuVorbisDecoder.Destroy;
begin
  FreeMem(FBuf, 4096); 

  CloseDecoder;
  inherited;
end;

function TAuVorbisDecoder.OpenDecoder: boolean;
begin
  result := false;
  if ov_open_callbacks(self, FVorbisFile, nil, 0, FCallbacks) >= 0 then
  begin
    //Get some info about the file
    FVI := ov_info(FVorbisFile, -1);
    FLen := ov_pcm_total(FVorbisFile, -1);

    FOpened := true;
    result := true;
  end;
end;

procedure TAuVorbisDecoder.CloseDecoder;
begin
  if FOpened then
  begin
    ov_clear(FVorbisFile);
    FOpened := false;
  end;
end;

function TAuVorbisDecoder.Decode: TAuDecoderState;
begin
  result := audsIncomplete;
  if FOpened then
  begin
    FBufSize := ov_read(FVorbisFile, FBuf, 4096, false, 2, true, @FCurrent_section);
    if FBufSize = 0 then
      result := audsEnd
    else if FBufSize > 0 then
      result := audsHasFrame;
  end;
end;

function TAuVorbisDecoder.GetInfo: TAuAudioParametersEx;
begin
  FillChar(result, SizeOf(result), 0);
  if FOpened then
  begin
    with result do
    begin
      Frequency := FVI.rate;
      Channels := FVI.channels;
      BitDepth := 16;
    end;
  end;
end;

procedure TAuVorbisDecoder.GetPacket(var APacket: TAuPacket);
begin
  if FBufSize > 0 then
  begin
    with APacket do
    begin
      Timecode := round(ov_time_tell(FVorbisFile) * 1000);
      BufferSize := FBufSize;
      Buffer := FBuf;      
    end;
  end;
end;

function TAuVorbisDecoder.SeekTo(ACur, ATar: integer): boolean;
begin
  result := false;
  if FOpened then
  begin
    if ov_seekable(FVorbisFile) <> 0 then
    begin
      result := ov_time_seek(FVorbisFile, ATar / 1000) = 0;
    end;
  end;
end;

function TAuVorbisDecoder.StreamLength: integer;
begin
  result := -1;
  if FOpened then
    result := trunc(FLen / FVI.rate * 1000);
end;

function CreateVorbisDecoder(AProtocol: TAuProtocol): TAuVorbisDecoder;
begin
  result := TAuVorbisDecoder.Create(AProtocol);
end;

initialization
  AcRegSrv.RegisterClass(TAuVorbisDecoder, @CreateVorbisDecoder);

end.
