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
GNU General Public License license (the GPL License), in which case the provisions of
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
unit AuLibao;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AcSyncObjs, AcBuffer, AcPersistent,
  AuTypes, AuDriverClasses,
  libao;

type

  { TAuLibaoDriver }

  TAuLibaoDriver = class(TAuDriver)
    private
    public
      constructor Create;
      destructor Destroy;override;

      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;

      function CreateStreamDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx): TAuStreamDriver;override;
  end;

  { TAuLibaoStreamDriver }

  { TAuLibaoStreamingThread }

  TAuLibaoStreamingThread = class(TThread)
    private
      FPaused: boolean;
      FBuffer: TAcBuffer;
      FCritSect: TAcCriticalSection;
      FFormat: Tao_sample_format;
      FDevice: Pao_device;
    protected
      procedure Execute;override;
    public
      constructor Create(ADevice: Pao_device; ABuffer: TAcBuffer;
        AFormat: Tao_sample_format; ACritSect: TAcCriticalSection);

      procedure Play;
      procedure Pause;
  end;

  TAuLibaoStreamDriver = class(TAuStreamDriver)
    private
      FDeviceID: integer;
      FDevice: Pao_device;
      FBuffer: TAcBuffer;
      FCritSect: TAcCriticalSection;
      FThread: TAuLibaoStreamingThread;
      FBuf: PByte;
      FBufSize: integer;
    public
      constructor Create(ADeviceID: integer; AFmt: TAuAudioParametersEx);
      destructor Destroy;override;

      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;
      function Open: boolean;override;
      procedure Close;override;

      function Idle(ACallback: TAuReadCallback): boolean;override;
  end;

const
  LIBAO_BUFSIZE = 10; //10ms * 10 --> 100ms Latency
  LIBAO_BUFCOUNT = 10;


implementation

function GetBufByteCount(format: Tao_sample_format): integer;
begin
  result := format.channels * (format.bits div 8) * round((LIBAO_BUFSIZE / 1000) *
    format.rate);
end;

function CreateLibaoDriver: TAuDriver;
begin
  result := TAuLibaoDriver.Create;
end;

{ TAuLibaoDriver }

constructor TAuLibaoDriver.Create;
begin
  inherited Create;


end;

destructor TAuLibaoDriver.Destroy;
begin
  inherited Destroy;
end;

procedure TAuLibaoDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  i, c: integer;
  dev: TAuDevice;
  lst: PPao_info;
begin
  c := 0;

  //Add the standard device driver to the list
  dev.Name := 'libao default device';
  dev.ID := 0;
  dev.Priority := 100;
  dev.UserData := nil;
  ACallback(dev);

  //Get the driver info list
  lst := ao_driver_info_list(@c);
  for i := 0 to c - 1 do
  begin
    //Send the next device item via the callback
    dev.Name := lst^^.name;
    dev.ID := i + 1;
    dev.Priority := lst^^.priority;
    ACallback(dev);

    //Fetch the next list element
    inc(lst);
  end;
end;

function TAuLibaoDriver.CreateStreamDriver(ADeviceID: integer;
  AParameters: TAuAudioParametersEx): TAuStreamDriver;
begin
  //Create a stream driver
  result := TAuLibaoStreamDriver.Create(ADeviceID, AParameters);
end;

{ TAuLibaoStreamDriver }

constructor TAuLibaoStreamDriver.Create(ADeviceID: integer;
  AFmt: TAuAudioParametersEx);
begin
  inherited Create;

  FDeviceID := ADeviceID;
  FParameters := AFmt;
  FDelay := LIBAO_BUFSIZE * LIBAO_BUFCOUNT;

  FBuffer := TAcBuffer.Create;
  FCritSect := TAcCriticalSection.Create;
end;

destructor TAuLibaoStreamDriver.Destroy;
begin
  Close;

  FCritSect.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TAuLibaoStreamDriver.Play;
begin
  if FThread <> nil then
    FThread.Play;
end;

procedure TAuLibaoStreamDriver.Pause;
begin
  if FThread <> nil then
    FThread.Pause;
end;

procedure TAuLibaoStreamDriver.Stop;
begin
  if FThread <> nil then
  begin
    Pause;
    FCritSect.Enter;
    try
      FBuffer.Clear;
    finally
      FCritSect.Leave;
    end;
  end;
end;

function TAuLibaoStreamDriver.Open: boolean;
var
  fmt: Tao_sample_format;
begin
  result := false;
  if FDevice = nil then
  begin
    //Convert the sample format
    fmt := ao_sample_format(FParameters.BitDepth, FParameters.Frequency,
      FParameters.Channels);

    //Try to open the driver
    FDevice := ao_open_live(FDeviceID, @fmt, nil);
    if FDevice <> nil then
    begin
      result := true;

      //Create the streaming thread
      FThread := TAuLibaoStreamingThread.Create(FDevice, FBuffer, fmt,
        FCritSect);

      FBufSize := GetBufByteCount(fmt);
      GetMem(FBuf, FBufSize);
    end;
  end;
end;

procedure TAuLibaoStreamDriver.Close;
begin
  if FDevice <> nil then
  begin
    //Stop the output and clear the buffer
    Stop;

    //Terminate the streaming thread
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);

    FreeMem(FBuf, FBufSize);
    FBuf := nil;

    //Close the device
    ao_close(FDevice);
    FDevice := nil;
  end;
end;

function TAuLibaoStreamDriver.Idle(ACallback: TAuReadCallback): boolean;
var
  read: integer;
begin
  result := false;
  if FBuffer.Filled < FBufSize * LIBAO_BUFCOUNT then
  begin
    result := true;

    read := ACallback(FBuf, FBufSize, FSyncData);
    FCritSect.Enter;
    try
      FBuffer.Write(FBuf, read);
    finally
      FCritSect.Leave;
    end;
  end;
end;

{ TAuLibaoStreamingThread }

constructor TAuLibaoStreamingThread.Create(ADevice: Pao_device;
  ABuffer: TAcBuffer; AFormat: Tao_sample_format;
  ACritSect: TAcCriticalSection);
begin
  FBuffer := ABuffer;
  FFormat := AFormat;
  FCritSect := ACritSect;
  FDevice := ADevice;
  FPaused := true;

  inherited Create(false);
end;


procedure TAuLibaoStreamingThread.Execute;
var
  buf: PByte;
  buf_size: Integer;
begin
  buf_size := GetBufByteCount(FFormat);
  GetMem(buf, buf_size);
  try
    while not Terminated do
    begin
      //Output silence
      FillChar(buf^, buf_size, 0);

      //Fetch the audio data if not paused
      if not FPaused then
      begin
        FCritSect.Enter;
        try
          FBuffer.Read(buf, buf_size);
        finally
          FCritSect.Leave;
        end;
      end;

      //...and play it
      ao_play(FDevice, buf, buf_size);
    end;
  finally
    FreeMem(buf, buf_size);
  end;
end;

procedure TAuLibaoStreamingThread.Play;
begin
  FPaused := false;
end;

procedure TAuLibaoStreamingThread.Pause;
begin
  FPaused := true;
end;

initialization
  if init_libao() then
    AcRegSrv.RegisterClass(TAuLibaoDriver, @CreateLibaoDriver);

finalization
  finalize_libao();

end.
