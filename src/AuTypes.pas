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

type
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
       BitDepth: Cardinal; );
      1:(
       {Data without the bit depth.}
       Parameters: TAuAudioParameters);
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
   @param(ASyncData may be used for you own needs. For example you may write the
     timecode information you've got from an audio decoder into it. Lateron you
     may read the current timecode from the stream driver.)
   @returns(Count of bytes that has been read.)}
  TAuReadCallback = function(
    ABuf: PByte; ASize: Cardinal; var ASyncData: TAuSyncData): Cardinal of object;

  TAuChannelPeaks = array of Single;
  TAuChannelVolumes = array of Single;

  TAuPeaks = record
    ChannelPeaks: TAuChannelPeaks;
    MixedValue: Single;
  end;

{Produces a TAuAudioParameters record.
 @seealso(TAuAudioParametersEx)}
function AuAudioParametersEx(const AFrequency, AChannels, ABitDepth: Cardinal;
  const AUserData: Pointer = nil): TAuAudioParametersEx;overload;
{Produces a TAuAudioParameters record.
 @seealso(TAuAudioParametersEx)}
function AuAudioParametersEx(const AParams: TAuAudioParameters;
  ABitDepth: Cardinal): TAuAudioParametersEx;overload;
{Produces a TAuAudioParametersEx record.
 @seealso(TAuAudioParameters)}
function AuAudioParameters(const AFrequency, AChannels: Cardinal;
  const AUserData: Pointer = nil): TAuAudioParameters;

function AuCompSyncData(AData1, AData2: TAuSyncData): boolean;

implementation

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

function AuAudioParametersEx(const AFrequency, AChannels, ABitDepth: Cardinal;
  const AUserData: Pointer = nil): TAuAudioParametersEx;
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
  ABitDepth: Cardinal): TAuAudioParametersEx;overload;
begin
  with result do
  begin
    Parameters := AParams;
    BitDepth := ABitDepth;
  end;
end;


end.
