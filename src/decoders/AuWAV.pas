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

File: AuWAV.pas
Author: Andreas Stöckel
}

{Contains a native pascal WAVE file binding for Audorra.}
unit AuWAV;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AcPersistent,
  AuUtils, AuTypes, AuProtocolClasses, AuDecoderClasses, AuWAVFormat;

type
  TAuWAVDecoder = class(TAuDecoder)
    private
      FWAVFile: TAuWAVFile;
      FBuf: PByte;
      FBufSize: integer;
      FPacket: TAuPacket;
      procedure FreeBuf;
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

{ TAuWAVDecoder }

constructor TAuWAVDecoder.Create(AProtocol: TAuProtocol);
begin
  inherited;

  FWAVFile := TAuWAVFile.Create;
end;

destructor TAuWAVDecoder.Destroy;
begin
  FWAVFile.Free;
  FreeBuf;
  inherited;
end;

procedure TAuWAVDecoder.CloseDecoder;
begin
  FWAVFile.Close;
end;

function TAuWAVDecoder.Decode: TAuDecoderState;
begin
  result := audsEnd;

  if (FBuf <> nil) then
  begin
    //Read 512 samples from the wav file
    FPacket.BufferSize := FWAVFile.Read(FBuf, 512, FPacket.Timecode);

    if FPacket.BufferSize > 0 then
      result := audsHasFrame;
  end;
end;

procedure TAuWAVDecoder.FreeBuf;
begin
  if FBuf <> nil then
    FreeMem(FBuf, FBufSize);

  FBuf := nil;
end;

function TAuWAVDecoder.GetInfo: TAuAudioParametersEx;
begin
  result := FWAVFile.Parameters; 
end;

procedure TAuWAVDecoder.GetPacket(var APacket: TAuPacket);
begin
  APacket:= FPacket;
end;

function TAuWAVDecoder.OpenDecoder: boolean;
begin
  //Close the decoder
  CloseDecoder;

  result := false;

  if FWAVFile.Open(FProtocol) then
  begin
    //Reserve space for 512 samples
    FBufSize := 512 * AuBytesPerSample(FWAVFile.Parameters);
    FBuf := GetMemory(FBufSize);

    FPacket.Buffer := FBuf;
    
    result := true;
  end;  
end;

function TAuWAVDecoder.SeekTo(ACur, ATar: integer): boolean;
begin
  result := false;

  if FProtocol.Seekable then
    result := FWAVFile.Seek(ATar);
end;

function TAuWAVDecoder.StreamLength: integer;
begin
  result := FWAVFile.StreamLength;
end;

function CreateWAVDecoder(AProtocol: TAuProtocol): TAuDecoder;
begin
  result := TAuWAVDecoder.Create(AProtocol);
end;

initialization
  AcRegSrv.RegisterClass(TAuWAVDecoder, @CreateWAVDecoder);

end.
