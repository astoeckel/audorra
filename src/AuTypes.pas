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

File: AuTypes.pas
Author: Andreas Stöckel
}

{Contains Audorra type definitions.} 
unit AuTypes;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AcTypes;

type
  TAuVector1 = TAcVector1;
  PAuVector1 = PAcVector1;

  TAuVector2 = TAcVector2;
  PAuVector2 = PAcVector2;

  TAuVector3 = TAcVector3;
  PAuVector3 = PAcVector3;

  TAuVector4 = TAcVector4;
  PAuVector4 = PAcVector4;

  TAuOrientation = packed record
    at: TAuVector3;
    up: TAuVector3;
  end;

  TAuSampletype = (
    austInt,
    austUInt,
    austFloat
  );

  TAuBitdepth = record
    bits: Byte;
    align: Byte;
    sample_type: TAuSampletype;
  end;

  {TAuAudioParameters represents the internal audio data structure. The bit depth
   is always 32 bit.}
  TAuAudioParameters = packed record
    {The playback frequency of the audio data. Common values are 44100 or 48000 Hz.}
    Frequency: Cardinal;
    {The channel count of the audio data. Mono: 1 Channel, Stereo 2 Channels, 5.1 6 Channels, 7.1 8 Channels}
    Channels: Cardinal;
    {Not used now - You may use this value in your Audorra audio drive implementations for
     any purpose. The standard Audorra implementations don't use this value.}
    UserData: Pointer;
  end;
  PAuAudioParameters = ^TAuAudioParameters;

  {TAuAudioParameters represents the internal audio data structure. This structure
   contains (in difference to TAuAudioParameters) the BitDepth of the audio data.
   Audorra normally represents PCM data as 32 bit float numbers.}
  TAuAudioParametersEx = packed record
    case integer of
      0:(
       {The playback frequency of the audio data. Common values are 44100 or 48000 Hz.}
       Frequency: Cardinal;
       {The channel count of the audio data. Mono: 1 Channel, Stereo 2 Channels, 5.1 6 Channels, 7.1 8 Channels}
       Channels: Cardinal;
       {Not used now - You may use this value in your Audorra audio drive implementations for
        any purpose. The standard Audorra implementations don't use this value.}
       UserData: Pointer;
       {The bit depth of a single audio sub-sample. Common values are 16, 24 or 8 Bits.}
       BitDepth: TAuBitdepth; );
      1:(
       {Data without the bit depth.}
       Parameters: TAuAudioParameters);
  end;
  PAuAudioParametersEx = ^TAuAudioParametersEx;

  TAuDriverParameters = record
    Frequency: Cardinal;
    Channels: Cardinal;
    BitDepth: Cardinal;
  end;

  {The TAuAudioDriverState represents the current state of an audio driver.
   @seealso(TAuAudioDriver)}
  TAuAudioDriverState = (
    {The output audio driver is currently closed.}
    audsClosed,
    {The output audio driver is currently opened and ready for playback.}
    audsOpened,
    {The output audio driver already played something but is now paused.}
    audsPaused,
    {The output audio driver is currently in the playback mode.}
    audsPlaying
  );

  TAuFrameType = (
    auftBeginning,
    auftNormal,
    auftEnding
  );

  TAuSyncData = record
    Timecode: Cardinal;
    FrameType: TAuFrameType;
  end;
  PAuSyncData = ^TAuSyncData;

  TAuNotifyEvent = procedure(Sender: TObject) of object;

  {TAuDriverReadCallback is used by the stream driver to gain new audio data information.
   @param(ABuf is a pointer to the first byte of the buffer)
   @param(ASize is the size of the buffer.)
   @returns(Count of bytes that has been read.)}
  TAuReadCallback = function(ABuf: PByte; ASize: Cardinal;
    APlaybackSample: Int64): Cardinal of object;

  TAuStreamDriverProc = function(ABuf: PByte; ASize: Cardinal;
    APlaybackSample: Int64): Cardinal of object;
    
  TAuChannelPeaks = array of Single;
  TAuChannelVolumes = array of Single;

  TAuPeaks = record
    ChannelPeaks: TAuChannelPeaks;
    MixedValue: Single;
  end;

  TAuSamplestamp = Int64;

{Produces a TAuAudioParameters record.
 @seealso(TAuAudioParametersEx)}
function AuAudioParametersEx(const AFrequency, AChannels: Cardinal;
  const ABitDepth: TAuBitdepth; const AUserData: Pointer = nil): TAuAudioParametersEx;overload;
{Produces a TAuAudioParameters record.
 @seealso(TAuAudioParametersEx)}
function AuAudioParametersEx(const AParams: TAuAudioParameters;
  const ABitDepth: TAuBitdepth): TAuAudioParametersEx;overload;
{Produces a TAuAudioParametersEx record.
 @seealso(TAuAudioParameters)}
function AuAudioParameters(const AFrequency, AChannels: Cardinal;
  const AUserData: Pointer = nil): TAuAudioParameters;

function AuCompSyncData(AData1, AData2: TAuSyncData): boolean;

function AuOrientation(AAt, AUp: TAuVector3): TAuOrientation;

function AuBitdepth(ABitdepth: Byte): TAuBitdepth;overload;
function AuBitdepth(ABits, AAlign: Byte; ASampleType: TAuSampletype): TAuBitdepth;overload;
function AuCheckBitdepth(const ABitdepth: TAuBitdepth): Boolean;
function AuDriverParameters(AFrequency, AChannels, ABitdepth: Cardinal): TAuDriverParameters;overload;
function AuDriverParameters(AParametersEx: TAuAudioParametersEx): TAuDriverParameters;overload;
function AuDriverParameters(AParameters: TAuAudioParameters; ABitdepth: Cardinal): TAuDriverParameters;overload;

const
  au8Bit: TAuBitdepth = (
    bits: 8;
    align: 8;
    sample_type: austUInt;
  );

  au16Bit: TAuBitdepth = (
    bits: 16;
    align: 16;
    sample_type: austInt;
  );

  au24Bit: TAuBitdepth = (
    bits: 24;
    align: 24;
    sample_type: austInt;
  );

  au32Bit: TAuBitdepth = (
    bits: 32;
    align: 32;
    sample_type: austInt;
  );

  auFloat32Bit: TAuBitdepth = (
    bits: 32;
    align: 32;
    sample_type: austFloat;
  );

implementation

function AuDriverParameters(AFrequency, AChannels, ABitdepth: Cardinal): TAuDriverParameters;overload;
begin
  with result do
  begin
    Frequency := AFrequency;
    Channels := AChannels;
    BitDepth := ABitdepth;
  end;
end;

function AuDriverParameters(AParametersEx: TAuAudioParametersEx): TAuDriverParameters;overload;
begin
  with result do
  begin
    Frequency := AParametersEx.Frequency;
    Channels := AParametersEx.Channels;
    BitDepth := AParametersEx.BitDepth.bits;
  end;
end;

function AuDriverParameters(AParameters: TAuAudioParameters; ABitdepth: Cardinal): TAuDriverParameters;overload;
begin
  with result do
  begin
    Frequency := AParameters.Frequency;
    Channels := AParameters.Channels;
    BitDepth := ABitdepth;
  end;
end;

function AuBitdepth(ABitdepth: Byte): TAuBitdepth;overload;
begin
  with result do
  begin
    //Set the bitdepth accordingly
    bits := ABitdepth;

    //Set the align properly. It has to be a multiple of eight.
    if ABitdepth mod 8 = 0 then    
      align := ABitdepth
    else
      align := (ABitdepth div 8 + 1) * 8;

    //Set sample type according to the given bitdepth
    if ABitdepth <= 8 then    
      sample_type := austUInt
    else
      sample_type := austInt;
  end;
end;

function AuBitdepth(ABits, AAlign: Byte; ASampleType: TAuSampletype): TAuBitdepth;overload;
begin
  with result do
  begin
    bits := ABits;
    align := AAlign;
    sample_type := ASampleType;
  end;
end;

function AuCheckBitdepth(const ABitdepth: TAuBitdepth): Boolean;
begin
  result :=
     //Align must always be greater than bits
    (ABitdepth.bits <= ABitdepth.align) and
    //The align must be a multiple of eight
    (ABitdepth.align mod 8 = 0) and
    //There actually have to be some bits
    (ABitdepth.bits > 0) and
    //Bitdepths greater than 32 bits are not supported
    (ABitdepth.bits <= 32) and
    //Only single percission floats are currently supported
    ((ABitdepth.sample_type < austFloat) or ((ABitDepth.bits = 32) and (ABitdepth.align = 32)));
end;                  

function AuOrientation(AAt, AUp: TAuVector3): TAuOrientation;
begin
  with result do
  begin
    at := AAt;
    up := AUp;
  end;
end;

function AuCompSyncData(AData1, AData2: TAuSyncData): boolean;
begin
  result :=
    (AData1.Timecode = AData2.Timecode) and
    (AData1.FrameType = AData2.FrameType);
end;

function AuAudioParameters(const AFrequency, AChannels: Cardinal;
  const AUserData: Pointer = nil): TAuAudioParameters;
begin
  //Set all values and return the result record
  with result do
  begin
    Frequency := AFrequency;
    Channels := AChannels;
    UserData := AUserData;
  end;
end;

function AuAudioParametersEx(const AFrequency, AChannels: Cardinal;
  const ABitDepth: TAuBitdepth; const AUserData: Pointer = nil): TAuAudioParametersEx;
begin
  //Set all values and return the result record
  with result do
  begin
    Frequency := AFrequency;
    Channels := AChannels;
    UserData := AUserData;
    BitDepth := ABitDepth;
  end;
end;

function AuAudioParametersEx(const AParams: TAuAudioParameters;
  const ABitDepth: TAuBitdepth): TAuAudioParametersEx;overload;
begin
  with result do
  begin
    Parameters := AParams;
    BitDepth := ABitDepth;
  end;
end;


end.
