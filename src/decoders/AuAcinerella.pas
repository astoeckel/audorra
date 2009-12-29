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

File: AuAcinerella.pas
Author: Andreas Stöckel
}

{Contains a Acinerella binding for Audorra. See http://acinerella.sourceforge.net/ for more info.}
unit AuAcinerella;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  acinerella,
  AcPersistent,
  AuUtils, AuTypes, AuProtocolClasses, AuDecoderClasses;

type
  TAuAcinerellaDecoder = class(TAuDecoder)
    private
      pInstance: PAc_instance;
      pDecoder: PAc_decoder;

      lt1, lt2: Cardinal;

      seek_req: boolean;
      seek_pos: int64;
      seek_stream: integer;
      seek_dir: integer;
    protected
      function GetInfo: TAuAudioParametersEx;override;
    public
      constructor Create(AProtocol: TAuProtocol);override;

      function OpenDecoder: boolean;override;
      procedure CloseDecoder;override;

      function Decode: TAuDecoderState;override;

      function SeekTo(ACur, ATar: integer): boolean;override;
      function StreamLength: integer;override;

      procedure GetPacket(var APacket: TAuPacket);override;

      property Protocol: TAuProtocol read FProtocol;
  end;

implementation

function read_proc(sender: Pointer; buf: PByte; size: integer): integer; cdecl;
begin
  result := TAuAcinerellaDecoder(sender).Protocol.Read(buf, size);
end;

function seek_proc(sender: Pointer; pos: int64; whence: integer): int64; cdecl;
begin
  //Writeln('Seek: ', pos, ' ', whence, '; Sender: ', Integer(Sender));
  if whence in [0, 1, 2] then
    result := TAuAcinerellaDecoder(sender).Protocol.Seek(TAuProtocolSeekMode(whence), pos)
  else
    result := -1;
end;

{ TAuAcinerellaDecoder }

constructor TAuAcinerellaDecoder.Create(AProtocol: TAuProtocol);
begin
  inherited;
  lt1 := 0;
  lt2 := 0;
end;

function TAuAcinerellaDecoder.OpenDecoder: boolean;
var
  i: integer;
  info: TAc_stream_info;
begin
  CloseDecoder;

  //Init Acinerella
  pInstance := ac_init;
  pDecoder := nil;

  if FProtocol.Seekable then
    ac_open(pInstance, self, nil, @read_proc, @seek_proc, nil)
  else
    ac_open(pInstance, self, nil, @read_proc, nil, nil);

  //Search audio decoder
  for i := 0 to pInstance^.stream_count - 1 do
  begin
    //Fetch info about the current stream
    ac_get_stream_info(pInstance, i, @info);

    if (info.stream_type = AC_STREAM_TYPE_AUDIO) then
    begin
      pDecoder := ac_create_decoder(pInstance, i);
      if pDecoder <> nil then      
        break;
    end;
  end;

  result := pDecoder <> nil;
end;

function TAuAcinerellaDecoder.SeekTo(ACur, ATar: integer): boolean;
begin
  result := false;

  if (not seek_req) and (pInstance <> nil) and (pDecoder <> nil) and (ATar >= 0) then
  begin
    seek_pos := ATar;
    if ATar < ACur then
      seek_dir := -1
    else
      seek_dir := 0;
    seek_stream := pDecoder^.stream_index;
    seek_req := true;
    result := true;
  end;
end;

function TAuAcinerellaDecoder.StreamLength: integer;
begin
  if pInstance <> nil then
    result := pInstance^.info.duration
  else
    result := -1;
end;

procedure TAuAcinerellaDecoder.CloseDecoder;
begin
  //Free the audio decoder
  if pDecoder <> nil then
    ac_free_decoder(pDecoder);
  pDecoder := nil;

  //Free the acinerella instance
  if pInstance <> nil then
    ac_free(pInstance);
  pInstance := nil;  
end;

function TAuAcinerellaDecoder.Decode: TAuDecoderState;
var
  pckg: PAc_package;
begin
  result := audsIncomplete;

  if (pInstance <> nil) and (pInstance^.opened) then
  begin
    //Seek if required
    if seek_req then
    begin
      ac_seek(pDecoder, seek_dir, seek_pos);
      seek_req := false;
    end;

    //Read a package from data stream.
    pckg := ac_read_package(pInstance);
    if pckg <> nil then
    begin
      //Decode package if it belongs to the audio decoder
      if (pckg^.stream_index = pDecoder^.stream_index) and
         (ac_decode_package(pckg, pDecoder) > 0) then
        result := audsHasFrame;

      ac_free_package(pckg);
    end else
      //No package had been found, we are at the end of the stream
      result := audsEnd;
  end else
    result := audsEnd;
end;

function TAuAcinerellaDecoder.GetInfo: TAuAudioParametersEx;
begin
  //Return the information from the Acinerella stream object
  with pDecoder^.stream_info.additional_info.audio_info do
  begin
    result.Frequency := samples_per_second;
    result.BitDepth := bit_depth;
    result.Channels := channel_count;
  end;
end;

procedure TAuAcinerellaDecoder.GetPacket(var APacket: TAuPacket);
var
  t: Cardinal;
  td: Double;
begin
{  if (pDecoder = nil) or (pInstance = nil) or (not pInstance^.opened) then
    exit;}


  //Calculate the timecode in ms
  with pDecoder^.stream_info.additional_info.audio_info do
  begin
    t := round(pDecoder^.timecode * 1000);

    //Many file formats do not contain exact timecodes. This code works around
    //this issue and increases the timecode steadily.
    if t = lt1 then
    begin
      td := AuBytesToSamples(pDecoder^.buffer_size, Info) / Info.Frequency * 1000;
      lt2 := lt2 + round(td);
      t := lt2;
    end else
    begin
      lt1 := t;
      lt2 := t;
    end;
  end;

  APacket.Timecode := t;
  APacket.BufferSize := pDecoder^.buffer_size;
  APacket.Buffer := pDecoder^.buffer;
end;

function CreateAcinerellaDecoder(AProtocol: TAuProtocol): TAuDecoder;
begin
  result := TAuAcinerellaDecoder.Create(AProtocol);
end;

initialization
  AcRegSrv.RegisterClass(TAuAcinerellaDecoder, @CreateAcinerellaDecoder);

end.
