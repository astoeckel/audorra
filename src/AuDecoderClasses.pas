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

File: AuDecoderClasses.pas
Author: Andreas Stöckel
}

{Contains the audorra decoder class interface definition.} 
unit AuDecoderClasses;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,
  AcPersistent,
  AuTypes, AuProtocolClasses;

type
  TAuPacket = record
    {The position of the audio stream in milliseconds. Should be High(Cardinal)
     if the position is unknown.}
    Timecode: Cardinal;
    {Size of the data buffer}
    BufferSize: integer;
    {Pointer to the data buffer.}
    Buffer: PByte;
  end;
  PAuPacket = ^TAuPacket;

  {Current video decoding state of the video decoder plugin.}
  TAuDecoderState = (
    audsIncomplete, {< The frame data was incomplete, we have to transfer more
      data to the audio decoder.}
    audsHasFrame, {< The audio decoder found a frame in the data we provided.
      It can be received by calling the "GetPacket" method.}
    audsEnd {< There was a fatal error in the data stream or it indicated that
      the stream has come to an end.}
  );

  TAuDecoder = class(TAcPersistent)
    private
      FProtocol: TAuProtocol;
      FOpened: Boolean;
    protected
      property Protocol: TAuProtocol read FProtocol write FProtocol;

      function GetInfo: TAuAudioParametersEx;virtual;abstract;
      function DoOpenDecoder(AProbeResult: Pointer = nil): Boolean;virtual;abstract;
      procedure DoCloseDecoder;virtual;abstract;
    public
      destructor Destroy;override;

      function OpenDecoder(AProtocol: TAuProtocol; AProbeResult: Pointer = nil): boolean;
      procedure CloseDecoder;

      function Decode: TAuDecoderState;virtual;abstract;

      {The probe function returns whether the decoder might be able to decode
       the given data set. A score between 0 and 100 is returned, wheras 0
       means that the decoder is dead sure that it can't decode the data. 100 is
       the exact oposite. The default implementation wrapps the given buffer
       into a "memory protocol" and tries to open it. If opening is succesfull,
       probe returns 100, else 0 is returned. Do not call inherited when overriding
       the "probe" function.}
      function Probe(ABuf: PByte; ASize: Integer; AUrl: PAnsiChar; var AResult: Pointer): Integer;virtual;

      procedure GetPacket(var APacket: TAuPacket);virtual;abstract;
      function SeekTo(ACur, ATar: integer): boolean;virtual;abstract;
      function StreamLength: integer;virtual;abstract;

      property Info: TAuAudioParametersEx read GetInfo;
      property Opened: Boolean read FOpened;
  end;

  TAuCreateDecoderProc = function: TAuDecoder;

{AuFindDecoder tries to find a decoder which is able to decode the data provided
by AProtocol. AuFindDecoder reads snippets with increasing size from AProtocol an
asks all registered decoders whether they are able to handle the data in this snippet.
This is done using the TAuDecoder.Probe function. AuFindDecoder reads a maximum
of 1MB from the stream (//! Add a parameter). If AuFindDecoder found a working
decoder, a instance of this deocder is created, opened and returned. Seekable
input protocols are sought back, non seekable streams are adapted with TAuInitialMemAdapter.
@seealso(TAuInitialMemAdapter)}
function AuFindDecoder(AProtocol: TAuProtocol): TAuDecoder;

implementation

{ TAuDecoder }

destructor TAuDecoder.Destroy;
begin          
  CloseDecoder;

  inherited;
end;

function TAuDecoder.OpenDecoder(AProtocol: TAuProtocol; AProbeResult: Pointer = nil): boolean;
begin
  result := false;
  
  //Force closing the decoder
  if FOpened = false then
  begin
    FProtocol := AProtocol;

    //Perform the actual openning process
    FOpened := DoOpenDecoder(AProbeResult);
    result := FOpened;
  end else ; //! RAISE EXCEPTION
end;

procedure TAuDecoder.CloseDecoder;
begin
  //Perform the close operation and reset the protocol to nil
  if FOpened then
    DoCloseDecoder;

  FProtocol := nil;
  FOpened := false;
end;

function TAuDecoder.Probe(ABuf: PByte; ASize: Integer; AUrl: PAnsiChar;
  var AResult: Pointer): Integer;
var
  memprot: TAuMemoryProtocol;
begin
  //Assume that opening the decoder didn't work
  result := 0;
  AResult := nil;

  memprot := TAuMemoryProtocol.Create(ABuf, ASize, false);
  memprot.URL := AUrl;
  try
    if OpenDecoder(memprot) then
    begin
      result := 75;
      CloseDecoder;
    end;
  finally
    memprot.Free;
  end;
end;

{ AuFindDecoder }

type
  TEnumDecoderOpaque = record
    probedata: PByte;
    probesize: Integer;
    proberesult: Pointer;
    decoder: PAcRegisteredClassEntry;
    maxscore: Integer;
    url: string;
  end;
  PEnumDecoderOpaque = ^TEnumDecoderOpaque;

const
  maxprobesize = 1024 * 1024 * 1; //1 MiB

procedure AuFindDecoder_Callback(ASender: Pointer; AEntry: PAcRegisteredClassEntry);
var
  opaque: PEnumDecoderOpaque;
  dec: TAuDecoder;
  score: Integer;
  proberes: Pointer;
begin
  opaque := PEnumDecoderOpaque(ASender);

  //Create the decoder
  dec := TAuCreateDecoderProc(AEntry^.ClassConstructor)();
  try
    //Performe the probe and check whether the decoder performs better than
    //the other decoders
    score := dec.Probe(opaque^.probedata, opaque^.probesize, PAnsiChar(opaque^.url), proberes);
    if (score > opaque^.maxscore) then
    begin
      opaque^.decoder := AEntry;
      opaque^.maxscore := score;
      opaque^.proberesult := proberes;
    end;
  finally
    dec.Free;
  end;
end;

function AuFindDecoder(AProtocol: TAuProtocol): TAuDecoder;
var
  opaque: PEnumDecoderOpaque;
  ms: TMemoryStream;
  mem: PByte;
  ops: Integer;
  read: Integer;
  prot: TAuProtocol;
begin
  result := nil;
  
  New(opaque);
  try
    FillChar(opaque^, SizeOf(opaque^), 0);

    ms := TMemoryStream.Create;
    try
      ops := 0;
      opaque^.probesize := 1024; //1 KB
      opaque^.url := AProtocol.URL;
      repeat
        //Read n bytes to the memory stream wheras n is the difference to the last readsize value
        GetMem(mem, opaque^.probesize - ops);
        try
          read := AProtocol.Read(mem, opaque^.probesize - ops);
          ms.Position := ms.Size;
          ms.Write(mem^, read);
        finally
          FreeMem(mem);
        end;

        ms.Position := 0;
        opaque^.probedata := ms.Memory;

        //Perform the read action
        AcRegSrv.EnumClasses(TAuDecoder, AuFindDecoder_Callback, opaque);

        //Set the old probe size value and calculate the next readsize
        ops := opaque^.probesize;
        opaque^.probesize := opaque^.probesize shl 1;
      until (opaque^.probesize > maxprobesize) or (opaque^.maxscore >= 75);

      //Seek to the beginning of the stream if possible
      if AProtocol.Seekable then
        AProtocol.Seek(aupsFromBeginning, 0);

      //Return an instance of the best decoder
      if (opaque^.decoder <> nil) and (opaque^.maxscore > 50) then
      begin
        if not AProtocol.Seekable then
          prot := TAuInitialMemAdapter.Create(AProtocol, opaque^.probedata,
            ops)
        else
          prot := AProtocol;
            
        //Create the result decoder and open it
        result := TAuCreateDecoderProc(opaque^.decoder^.ClassConstructor)();
        if not result.OpenDecoder(prot, opaque^.proberesult) then
        begin
          result.Free;
          result := nil;
        end;
      end;
    finally
      ms.Free;
    end;
  finally
    Dispose(opaque);
  end;            
end;

end.
