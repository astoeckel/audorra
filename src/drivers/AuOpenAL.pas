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

File: AuOpenAL.pas
Author: Andreas Stöckel
}

{Contains the OpenAL driver for Audorra.}
unit AuOpenAL;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils, Classes,
  openal,
  AcPersistent, AcSyncObjs,
  AuTypes, AuDriverClasses, AuUtils;

type
  TAuOpenALDevice = class
    private
      FName: string;
      FID: integer;
      FDevice: TALCdevice;
      FContext: TALCcontext;
    public
      constructor Create(AName: string; AID: integer);
      destructor Destroy;override;

      procedure AllocDevice;
      procedure FreeDevice;
      procedure AllocContext;
      procedure FreeContext;

      property Name: string read FName;
      property ID: integer read FID;
      property Context: TALCcontext read FContext;
  end;

  TAuOpenALDeviceList = class(TList)
    private
      function GetItem(AIndex: integer): TAuOpenALDevice;
      procedure SetItem(AIndex: integer; ACont: TAuOpenALDevice);
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      property Items[Index: integer]: TAuOpenALDevice read GetItem write SetItem; default;
  end;

  TAuOpenALDriver = class(TAuDriver)
    private
      FDevices: TAuOpenALDeviceList;
      FOpenDefault: boolean;
      FDefaultDevice: string;
      procedure FillDevicesList;
      function GetDeviceFromId(AID: integer): TAuOpenALDevice;
      function GetContext(ADeviceID: integer): TALCcontext;
    public
      constructor Create;
      destructor Destroy;override;

      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;

      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;override;
  end;

  TAuOpenALStreamDriver = class(TAuStreamDriver)
    private
      FContext: TALCContext;
      FSource: TALuint;
      FBuffers: array of TALuint;
      FOpened: boolean;
      FMem: PByte;
      FMemSize: integer;
      FBufSize: integer;
      FFmt: integer;
      FParameters: TAuDriverParameters;
      FCurrentBlock: integer;
      FFreeBlocks: integer;
      FThread: TAuStreamDriverIdleThread;
      FCallback: TAuStreamDriverProc;
      FTimecode: Int64;
      FMutex: TAcMutex;
      FActive: Boolean;
      function AllocBuffers: boolean;
      procedure DestroyBuffers;
      function Idle(ACallback: TAuReadCallback): Boolean;
    public
      constructor Create(AContext: TALCcontext);
      destructor Destroy;override;

      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;override;
      procedure Close;override;

      procedure SetActive(AActive: Boolean);override;
      procedure FlushBuffer;override;
  end;


const
  AuOpenALDefaultDeviceName = 'Default Device';
  AuOpenALBufferCount = 8;
  AuOpenALBufferSize = 1024;

implementation

var
  OALMutex: TAcLock;

function GetALFormat(AFormat: TAuDriverParameters): integer;
begin
  result := 0;
  if (AFormat.Channels = 1) and (AFormat.BitDepth = 8) then
    result := AL_FORMAT_MONO8;
  if (AFormat.Channels = 1) and (AFormat.BitDepth = 16) then
    result := AL_FORMAT_MONO16;
  if (AFormat.Channels = 2) and (AFormat.BitDepth = 8) then
    result := AL_FORMAT_STEREO8;
  if (AFormat.Channels = 2) and (AFormat.BitDepth = 16) then
    result := AL_FORMAT_STEREO16;
end;

procedure ActivateContext(AContext: TALCcontext);
begin
  //Activate the context
  alcMakeContextCurrent(AContext);
  //Reset the error id
  alGetError();
end;

{ TAuOpenALDriver }

constructor TAuOpenALDriver.Create;
begin
  inherited;

  FPriority := 20;

  FDevices := TAuOpenALDeviceList.Create;

  FillDevicesList;
end;

destructor TAuOpenALDriver.Destroy;
begin
  FDevices.Free;
  inherited;
end;

function TAuOpenALDriver.CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;
var
  cont: TALCcontext;
begin
  //Initialize variables
  result := nil;

  cont := GetContext(ADeviceID);
  if cont <> nil then
    result := TAuOpenALStreamDriver.Create(cont)
end;

function TAuOpenALDriver.GetContext(ADeviceID: integer): TALCcontext;
var
  dev: TAuOpenALDevice;
begin
  result := nil;
  dev := GetDeviceFromId(ADeviceID);
  if dev <> nil then
  begin
    dev.AllocContext;
    result := dev.Context;
  end;
end;

procedure TAuOpenALDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  i: integer;
  dev: TAuDevice;
begin
  //Initialize the device record
  FillChar(dev, SizeOf(dev), 0);

  if FOpenDefault then
  begin
    dev.Name := AuOpenALDefaultDeviceName;
    dev.ID := -1;
    ACallback(dev);
  end else
  begin
    for i := 0 to FDevices.Count - 1 do
    begin
      dev.Name := FDevices[i].Name;
      dev.ID := i;
      if FDevices[i].Name = FDefaultDevice then
        dev.Priority := 1
      else
        dev.Priority := 0;

      ACallback(dev);
    end;
  end;
end;

procedure TAuOpenALDriver.FillDevicesList;
var
  deviceList: PAnsiChar;
  s: string;
  abort: boolean;
  i: integer;
  device: TAuOpenALDevice;
begin
  //Fill the devices string list
  FOpenDefault := true;
  FDevices.Clear;

  if alcIsExtensionPresent(nil, 'ALC_ENUMERATE_ALL_EXT') then
  begin
    FOpenDefault := false;
    FDefaultDevice := PAnsiChar(alcGetString(nil, ALC_DEFAULT_ALL_DEVICES_SPECIFIER));
    deviceList := PAnsiChar(alcGetString(nil, ALC_ALL_DEVICES_SPECIFIER)); //Zero separated list

    //Worst case length
    SetLength(s, Length(deviceList));
    s := '';
    abort := false;
    i := 0;

    //Seperate values
    repeat
      if deviceList^ = #0 then
      begin
        if Length(s) <> 0 then
        begin
          device := TAuOpenALDevice.Create(s, i);
          FDevices.Add(device);
          i := i + 1;
          s := '';
        end else
          abort := true;
      end else
        s := s + deviceList^;
      inc(deviceList);
    until abort;
  end else
  begin
    device := TAuOpenALDevice.Create(AuOpenALDefaultDeviceName, -1);
    FDevices.Add(device);
  end;
end;

function TAuOpenALDriver.GetDeviceFromId(AID: integer): TAuOpenALDevice;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FDevices.Count - 1 do
  begin
    if FDevices[i].ID = AID then
    begin
      result := FDevices[i];
      break;
    end;
  end;
end;

{ TAuDeviceList }

function TAuOpenALDeviceList.GetItem(AIndex: integer): TAuOpenALDevice;
begin
  result := inherited Items[AIndex];
end;

procedure TAuOpenALDeviceList.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    TAuOpenALDevice(ptr).Free;
end;

procedure TAuOpenALDeviceList.SetItem(AIndex: integer; ACont: TAuOpenALDevice);
begin
  inherited Items[AIndex] := ACont;
end;

{ TAuOpenALDevice }

constructor TAuOpenALDevice.Create(AName: string; AID: integer);
begin
  inherited Create;

  FName := AName;
  FID := AID;

  FDevice := nil;
  FContext := nil;
end;

destructor TAuOpenALDevice.Destroy;
begin
  FreeContext;
  FreeDevice;
  inherited;
end;

procedure TAuOpenALDevice.AllocContext;
var
  args: PALCint;
begin
  if FContext = nil then
  begin
    //Be sure to have a device opened
    AllocDevice;

    OALMutex.Acquire;
    try
      args := nil;
      FContext := alcCreateContext(FDevice, args);
    finally
      OALMutex.Release;
    end;
  end;
end;

procedure TAuOpenALDevice.AllocDevice;
begin
  if FDevice = nil then
  begin
    OALMutex.Acquire;
    try
      if FName = AuOpenALDefaultDeviceName then
        FDevice := alcOpenDevice(nil)
      else
        FDevice := PAnsiChar(alcOpenDevice(PByte(FName)));
    finally
      OALMutex.Release;
    end;
  end;
end;

procedure TAuOpenALDevice.FreeContext;
begin
  OALMutex.Acquire;
  try
    if FContext <> nil then
      alcDestroyContext(FContext);
    FContext := nil;
  finally
    OALMutex.Release;
  end;
end;

procedure TAuOpenALDevice.FreeDevice;
begin
  OALMutex.Acquire;
  try
    if FDevice <> nil then
      alcCloseDevice(FDevice);
    FDevice := nil;
  finally
    OALMutex.Release;
  end;
end;

{ TAuOpenALStreamDriver }

constructor TAuOpenALStreamDriver.Create(AContext: TALCcontext);
begin
  inherited Create;

  FContext := AContext;
  FMutex := TAcMutex.Create;
end;                 

destructor TAuOpenALStreamDriver.Destroy;
begin
  Close;

  FreeAndNil(FMutex);

  inherited;
end;

function TAuOpenALStreamDriver.Open(ADriverParams: TAuDriverParameters;
  ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;
begin
  result := false;

  //Get the OpenAL-Format and copy the driver parameters
  FFmt := GetALFormat(ADriverParams);
  FParameters := ADriverParams;
  FCallback := ACallback;
  
  if (FContext <> nil) and (FFmt <> 0) then
  begin
    //Set the write format
    AWriteFormat := AuBitdepth(ADriverParams.BitDepth);

    //Reset the timecode
    FTimecode := 0;
    
    OALMutex.Acquire;
    try
      ActivateContext(FContext);

      //Setup the source
      alGenSources(1, @FSource);
      alSource3f(FSource, AL_POSITION, 0, 0, 0);
      alSource3f(FSource, AL_VELOCITY, 0, 0, 0);
      alSource3f(FSource, AL_DIRECTION, 0, 0, 0);

      alDistanceModel(AL_EXPONENT_DISTANCE);

      result := (alGetError() = AL_NO_ERROR) and AllocBuffers;

      if result then
        FOpened := true;

      FThread := TAuStreamDriverIdleThread.Create(FCallback, Idle);
    finally
      OALMutex.Release;
    end;
  end;
end;

procedure TAuOpenALStreamDriver.Close;
begin
  OALMutex.Acquire;
  try
    if FThread <> nil then
    begin
      FThread.Terminate;
      FThread.WaitFor;
      FreeAndNil(FThread);
    end;
    
    DestroyBuffers;

    alDeleteSources(1, @FSource);
  finally
    OALMutex.Release;
  end;
end;

function TAuOpenALStreamDriver.AllocBuffers: boolean;
begin
  result := false;
  
  OALMutex.Acquire;
  try
    //Reserve memory for the buffers
    FBufSize := FParameters.Channels * FParameters.BitDepth div 8 * AuOpenALBufferSize;
    FMemSize := FParameters.Channels * FParameters.BitDepth div 8 * AuOpenALBufferCount * AuOpenALBufferSize;
    GetMem(FMem, FMemSize);

    //Generate the buffers
    SetLength(FBuffers, AuOpenALBufferCount);
    alGenBuffers(AuOpenALBufferCount, @FBuffers[0]);
    if alGetError() <> AL_NO_ERROR then
      exit;

    FCurrentBlock := 0;
    FFreeBlocks := Length(FBuffers);

    result := true;
  finally
    OALMutex.Release;
  end;
end;

procedure TAuOpenALStreamDriver.DestroyBuffers;
begin
  OALMutex.Acquire;
  try
    ActivateContext(FContext);

    //Free the buffer memory
    if Length(FBuffers) > 0 then
    begin
      alDeleteBuffers(AuOpenALBufferCount, @FBuffers[0]);
      SetLength(FBuffers, 0);
    end;

    if FMem <> nil then
      FreeMem(FMem);
    FMem := nil;
    FMemSize := 0;

    FCurrentBlock := 0;
    FFreeBlocks := 0;
    FTimecode := 0;
  finally
    OALMutex.Release;
  end;
end;

function TAuOpenALStreamDriver.Idle(ACallback: TAuReadCallback): boolean;
var
  processed: integer;
  i: integer;
  pmem: PByte;
  size: integer;
  mustplay: boolean;
begin
  result := false;

  FMutex.Acquire;
  try
    if FActive then
    begin
      OALMutex.Acquire;
      try
        ActivateContext(FContext);

        Processed := 0;
        alGetSourcei(FSource, AL_BUFFERS_PROCESSED, @processed);
        FFreeBlocks := FFreeBlocks + processed;
        FTimecode := FTimecode + processed * AuOpenALBufferSize;

        mustplay := FFreeBlocks = AuOpenALBufferCount;

        for i := 0 to FFreeBlocks - 1 do
        begin
          alSourceUnqueueBuffers(FSource, 1, @FBuffers[FCurrentBlock]);

          //Calculate the memory position of the buffer
          pmem := FMem;
          inc(pmem, FBufSize * FCurrentBlock);

          //Read the wave data from the decoder
          size := ACallback(pmem, FBufSize, FTimecode);

          //Write the data to the block and queue it
          alBufferData(FBuffers[FCurrentBlock], FFmt, pmem, size,
            FParameters.Frequency);
          alSourceQueueBuffers(FSource, 1, @FBuffers[FCurrentBlock]);

          //Increment block counters
          FCurrentBlock := (FCurrentBlock + 1) mod AuOpenALBufferCount;
          FFreeBlocks := FFreeBlocks - 1;
          result := true;
        end;

        if mustplay then
          alSourcePlay(FSource);
      finally
        OALMutex.Release;
      end;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TAuOpenALStreamDriver.SetActive(AActive: Boolean);
begin
  OALMutex.Acquire;
  try
    FMutex.Acquire;
    try
      ActivateContext(FContext);
      if AActive then
        alSourcePlay(FSource)
      else
        alSourcePause(FSource);

      FActive := AActive;
    finally
      FMutex.Release;
    end;
  finally
    OALMutex.Release;
  end;
end;

procedure TAuOpenALStreamDriver.FlushBuffer;
begin
  OALMutex.Acquire;
  try
      FMutex.Acquire;
      try
        ActivateContext(FContext);
        alSourceUnqueueBuffers(FSource, AuOpenALBufferCount, @FBuffers[0]);
        alSourceStop(FSource);

        FCurrentBlock := 0;
        FFreeBlocks := Length(FBuffers);
        FTimecode := 0;
        FActive := false;
      finally
        FMutex.Release;
      end;
  finally
    OALMutex.Release;
  end;
end;

function CreateOpenALDriver: TAuDriver;
begin
  result := TAuOpenALDriver.Create;
end;

initialization
  if {$IFDEF WIN32}InitOpenAL('soft_oal.dll') or {$ENDIF} InitOpenAL then
    AcRegSrv.RegisterClass(TAuOpenALDriver, @CreateOpenALDriver);

  OALMutex := TAcLock.Create;

finalization
  OALMutex.Free;
  OALMutex := nil;

end.
