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
    protected
      FProtocol: TAuProtocol;
      function GetInfo: TAuAudioParametersEx;virtual;abstract;
    public
      constructor Create(AProtocol: TAuProtocol);virtual;
      destructor Destroy;override;

      function OpenDecoder: boolean;virtual;abstract;
      procedure CloseDecoder;virtual;abstract;

      function Decode: TAuDecoderState;virtual;abstract;

      procedure GetPacket(var APacket: TAuPacket);virtual;abstract;
      function SeekTo(ACur, ATar: integer): boolean;virtual;abstract;
      function StreamLength: integer;virtual;abstract;

      property Info: TAuAudioParametersEx read GetInfo;
  end;

  TAuCreateDecoderProc = function(AProtocol: TAuProtocol): TAuDecoder;

implementation

{ TAuDecoder }

constructor TAuDecoder.Create(AProtocol: TAuProtocol);
begin
  inherited Create;
  
  FProtocol := AProtocol;
end;

destructor TAuDecoder.Destroy;
begin          
  CloseDecoder;

  inherited;
end; 

end.
