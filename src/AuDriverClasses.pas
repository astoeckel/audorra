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

  TAuStreamDriver = class
    public
      {Openes the audio stream driver and makes it ready for playback. The driver
       will initially be opened in the inactive state.
       @param(AParameters is used to set the audio format information.)
       @param(ACallback is the function the data should be read from.)
       @returns(True if opening the audio device was successful, false if an error
         occured.)}
      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;virtual;abstract;
      {Closes the driver, if it had been opened. If the driver is not in a opened
       state, "Close" will do nothing.}
      procedure Close;virtual;abstract;

      {Sets the driver active or inactive. In the active state, the driver pulls
       audio data from the callback and plays it back, int the inactive state
       the driver just idles and waits for getting active immediately.}
      procedure SetActive(AActive: Boolean);virtual;abstract;

      {Swithes to the innactive state and flushes the audio buffer.}
      procedure FlushBuffer;virtual;abstract;
  end;

  //TAuDeviceShareMode = (audsExclusive, audsShared);
  //TAuDeviceShareModes = set of TAuDeviceShare;

  {TAuDriver is the base abstract audio output managing class. If you want to implement
   a new audio output system, you have to derive a class from TAuDriver. TAuDriver
   copes with creating the corresponding audio output objects and enumerating
   available output devices.}
  TAuDriver = class(TAcPersistent)
    protected
      FPriority: integer;
    public
      {Calls the given callback function for each device and returns information
       about them.}
      procedure EnumDevices(ACallback: TAuEnumDeviceProc);virtual;abstract;

      {Creates a stream driver.
       @param(ADeviceID is the device the sound should be output to. You get valid
         device IDs by calling the EnumDevices method.)
       @param(AScene is used to put the output object in a 3D environment.)
       @seealso(EnumDevices)
       @seealso(TAuAudioParameters)
       @seealso(TAuStreamDriver)}
      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;virtual;abstract;

      property Priority: integer read FPriority;

      {TODO: Share mode settings}
      //function GetAvailableDeviceShareModes(ADevice: integer): TAuDeviceShareModes;
      //procedure SetDeviceShareMode(ADevice: integer; AMode: TAuDeviceShareMode);
      //function GetDefaultFormat: TAuAudioPrametersEx;
  end;

  TAuCreateDriverProc = function: TAuDriver;

implementation

end.
