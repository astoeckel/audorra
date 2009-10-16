{*******************************************************}
{                                                       }
{       Audorra Digital Audio Library                   }
{       Copyright (c) Andreas St�ckel, 2009             }
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
Andreas St�ckel. All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License license (the �GPL License�), in which case the provisions of
GPL License are applicable instead of those above. If you wish to allow use
of your version of this file only under the terms of the GPL License and not
to allow others to use your version of this file under the MPL, indicate your
decision by deleting the provisions above and replace them with the notice and
other provisions required by the GPL License. If you do not delete the
provisions above, a recipient may use your version of this file under either the
MPL or the GPL License.

File: AuMPG123.pas
Author: Andreas St�ckel
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
var
  read: Integer;
  res: integer;
begin
  result := audsEnd;
  if FDec <> nil then
  begin
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
var
  read: integer;
  enc: integer;
  res: integer;
  atend: boolean;
begin
  CloseDecoder;

  result := false;

  //Create a new decoder
  FDec := mpg123_new(nil, nil);
  if FDec <> nil then
  begin
    //Open the decoder
    mpg123_open_feed(FDec);

    //Reserve memory for the input
    FInBufSize := 16384;
    GetMem(FInBuf, FInBufSize);

    //Reserve memory for the output
    FOutBufSize := 32768;
    GetMem(FOutBuf, FOutBufSize);

    //Read some data from the stream
    atend := false;
    repeat
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
    until atend;
  end;
end;

function TAuMPG123Decoder.SeekTo(ACur, ATar: integer): boolean;
begin
  result := false;
end;

function TAuMPG123Decoder.StreamLength: integer;
begin
  result := -1;
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
