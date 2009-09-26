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

File: AuDriverClasses.pas
Author: Andreas Stöckel
}

{Contains the audorra driver class interface defintitions.}
unit AuDriverClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AcPersistent,
  AuTypes;

type
  {The TAuDevice record contains information about a (logical) sound output device.
   It is used to be able to use multiple sound cards or audio outputs. Its ID property
   is for internal use.}
  TAuDevice = record
    {The name string contains the name of this audio output device.}
    Name: string[255];
    {The ID is an internal audio device inventory number of the used audio device.}
    ID: integer;
    {The audio device should sort the audio devices by their latency using the
     priority value. The device with the highes priority will be the default
     device.}
    Priority: integer;
    {Not used now - You may use this value in your Audorra audio drive implementations for
     any purpose. The standard Audorra implementations don't use this value.}
    UserData: Pointer; 
  end;

  PAuDevice = ^TAuDevice;

  {TAuEnumDeviceProc is a callback funktion type declaration used by the driver
   to send all devices to the host appliaction.}
  TAuEnumDeviceProc = procedure(ADevice: TAuDevice) of object;

  {TAuAudioDriver is the base abstract class for audio output objects. Remember
   that many hardware devices have a limitation in how many output objects can
   be opened at once. So mix down channels together in software if possible.
   Descendands of this class are TAuStaticSoundDriver and TAuStreamDriver. The
   StaticSound driver is used for the playback of single sound effects wheras
   the stream driver streams its audio data while playing. This is necessary for
   the playback of music.}
  TAuAudioDriver = class(TAcPersistent)
    protected
      FParameters: TAuAudioParametersEx;
      FState: TAuAudioDriverState;
    public
      {Starts the audio playback.}
      procedure Play;virtual;abstract;
      {Pauses the audio playback.}
      procedure Pause;virtual;abstract;
      {Stops audio playback: All loaded audio buffers are cleaned up.}
      procedure Stop;virtual;abstract;
      {Openes the audio object. And prepares it for playback. When using the
       TAuStaticSoundDriver, data can now be written into the object.}
      function Open: boolean;virtual;abstract;
      {Closes the audio object.}
      procedure Close;virtual;abstract;

      {Represents the parameters this audio object was created with.}
      property Parameters: TAuAudioParametersEx read FParameters;
      {Represents the state of the audio object. @seealso(TAuAudioDriverState)}
      property State: TAuAudioDriverState read FState;
  end;

  {TAuStaticSoundDriver is a descendand of TAuAudioDriver. It is used for the
   playback of single, static sound effects. If you want to stream data to it,
   use the TAuStreamDriverClass. An instance of this class may be created by
   using the corresponding TAuDriver.CreateStaticSoundDriver function.
   @seealso(TAuAudioDriver)
   @seealso(TAuDriver)}
  TAuStaticSoundDriver = class(TAuAudioDriver)
    protected
      FLoop: boolean;
      FStopProc: TAuNotifyEvent;
      procedure SetLoop(AValue: boolean);virtual;
    public
      {After the audio object has been opened, the WriteData function can be used
       to write data into its sound buffer. Remember that the length of this audio
       data shouldn't be too long. A justifiable value is 10 seconds, an absolute
       maximum should be one minute.
       The data is not copied to the driver object, the data has to be available
       until the sound object is freed.
       You can't write new data to the buffer once you've started playback. You
       have to close the device then.}
      procedure WriteData(ABuf: PByte; ASize: Cardinal);virtual;abstract;

      {If thid property is true, the sound will repeat playing after each playback.}
      property Loop: boolean read FLoop write SetLoop;

      property OnStop: TAuNotifyEvent read FStopProc write FStopProc;
  end;

  TAuStreamDriver = class(TAuAudioDriver)
    protected
      FSyncData: TAuSyncData;
      FDelay: Cardinal;
    public
      procedure Idle(AReadCallback: TAuReadCallback);virtual;abstract;

      property SyncData: TAuSyncData read FSyncData;
      property Delay: Cardinal read FDelay;
  end;

  TAu3DScene = class
    //
  end;

  {TAuDriver is the base abstract audio output managing class. If you want to implement
   a new audio output system, you have to derive a class from TAuDriver. TAuDriver
   copes with creating the corresponding audio output objects and enumerating
   available output devices.}
  TAuDriver = class(TAcPersistent)
    public
      {Calls the given callback function for each device and returns information
       about them.}
      procedure EnumDevices(ACallback: TAuEnumDeviceProc);virtual;abstract;
      {Creates a stream driver.
       @param(ADeviceID is the device the sound should be output to. You get valid
         device IDs by calling the EnumDevices method.)
       @param(AParameters is used to set the audio format information.)
       @param(AScene is used to put the output object in a 3D environment.)
       @seealso(EnumDevices)
       @seealso(TAuAudioParameters)
       @seealso(TAuStaticSoundDriver)}
      function CreateStaticSoundDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx;
        AScene: TAu3DScene = nil): TAuStaticSoundDriver;virtual;abstract;
      {Creates a stream driver.
       @param(ADeviceID is the device the sound should be output to. You get valid
         device IDs by calling the EnumDevices method.)
       @param(AParameters is used to set the audio format information.)
       @param(AScene is used to put the output object in a 3D environment.)
       @seealso(EnumDevices)
       @seealso(TAuAudioParameters)
       @seealso(TAuStreamDriver)}
      function CreateStreamDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx;
        AScene: TAu3DScene = nil): TAuStreamDriver;virtual;abstract;
      {Creates a new 3D scene. If the driver doesn't support 3D sound, nil will
       be returned.
       @seealso(TAu3DScene)}
      function Create3DScene: TAu3DScene;virtual;abstract;
  end;

  TAuCreateDriverProc = function: TAuDriver;

implementation

{ TAuStaticSoundDriver }

procedure TAuStaticSoundDriver.SetLoop(AValue: boolean);
begin
  FLoop := AValue;
end;

end.
