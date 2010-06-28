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

File: AuLibao.pas
Author: Andreas Stöckel
}

{Contains the LibAO driver for Audorra.}
unit AuLibao;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AcSyncObjs, AcPersistent,
  AuTypes, AuDriverClasses,
  libao;

type

  { TAuLibaoDriver }

  TAuLibaoDriver = class(TAuDriver)
    public
      constructor Create;
      destructor Destroy;override;

      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;

      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;override;
  end;

  { TAuLibaoStreamingThread }

  TAuLibaoStreamingThread = class(TThread)
    private
      FPaused: boolean;
      FCritSect: TAcCriticalSection;
      FFormat: Tao_sample_format;
      FCallback: TAuStreamDriverProc;
      FDeviceID: Integer;
      FPosition: Int64;
      FInitialized: Integer;
    protected
      procedure Execute;override;
    public
      constructor Create(ADeviceID: integer; ACallback: TAuStreamDriverProc;
        AFormat: Tao_sample_format);
      destructor Destroy;override;

      procedure SetActive(AActive: Boolean);
      procedure Reset;
      function Initialized: Integer;
  end;

  { TAuLibaoStreamDriver }

  TAuLibaoStreamDriver = class(TAuStreamDriver)
    private
      FDeviceID: integer;
      FThread: TAuLibaoStreamingThread;
    public
      constructor Create(ADeviceID: integer);
      destructor Destroy;override;

      procedure SetActive(AActive: Boolean);override;
      procedure FlushBuffer;override;

      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): boolean;override;
      procedure Close;override;
  end;

const
  LIBAO_BUFSIZE = 100;

implementation

function GetBufByteCount(format: Tao_sample_format): integer;
begin
  result := format.channels * (format.bits div 8) * (LIBAO_BUFSIZE * format.rate div 1000);
  Writeln('Bytecount: ', result);
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

    Writeln(i, '=', dev.Name);

    //Fetch the next list element
    inc(lst);
  end;
end;

function TAuLibaoDriver.CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;
begin
  //Create a stream driver
  result := TAuLibaoStreamDriver.Create(ADeviceID);
end;

{ TAuLibaoStreamDriver }

constructor TAuLibaoStreamDriver.Create(ADeviceID: integer);
begin
  inherited Create;

  FDeviceID := ADeviceID;
end;

destructor TAuLibaoStreamDriver.Destroy;
begin
  inherited Destroy;
end;

procedure TAuLibaoStreamDriver.SetActive(AActive: Boolean);
begin
  if FThread <> nil then
    FThread.SetActive(AActive);
end;

procedure TAuLibaoStreamDriver.FlushBuffer;
begin
  if FThread <> nil then
    FThread.Reset;
end;

function TAuLibaoStreamDriver.Open(ADriverParams: TAuDriverParameters;
  ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): boolean;
var
  fmt: Tao_sample_format;
begin
  result := false;
  if FThread = nil then
  begin
    //Convert the sample format
    fmt := ao_sample_format(ADriverParams.BitDepth, ADriverParams.Frequency,
      ADriverParams.Channels);
    AWriteFormat := AuBitdepth(ADriverParams.BitDepth);

    //Create the streaming thread, which will open the device driver
    FThread := TAuLibaoStreamingThread.Create(FDeviceID, ACallback, fmt);
    while FThread.Initialized = -1 do
      Sleep(1);

    result := FThread.Initialized > 0;
    if (not result) then
      FreeAndNil(FThread);
  end;
end;

procedure TAuLibaoStreamDriver.Close;
begin
  if FThread <> nil then
  begin
    //Stop the output and clear the buffer
    FlushBuffer;

    //Terminate the streaming thread
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

{ TAuLibaoStreamingThread }

constructor TAuLibaoStreamingThread.Create(ADeviceID: Integer;
  ACallback: TAuStreamDriverProc; AFormat: Tao_sample_format);
begin
  inherited Create(false);

  FDeviceID := ADeviceID;
  FFormat := AFormat;
  FCallback := ACallback;
  FPaused := true;
  FPosition := 0;
  FDeviceID := ADeviceID;
  FInitialized := -1;

  //Create the critical section object used to protect access on the thread object
  FCritSect := TAcCriticalSection.Create;
end;

destructor TAuLibaoStreamingThread.Destroy;
begin
  FreeAndNil(FCritSect);
  inherited Destroy;
end;

procedure TAuLibaoStreamingThread.Execute;
var
  buf: PByte;
  buf_size, read_size: Integer;
  device: Pao_device;
  read_sample: Int64;
begin
  //Create and open the libao device
  device := ao_open_live(FDeviceID, @FFormat, nil);
  read_sample := 0;
  if (device <> nil) then
  begin
    FCritSect.Enter;
    try
      FInitialized := 1;
    finally
      FCritSect.Leave;
    end;
    //Reserve enough memory for the buffer
    buf_size := GetBufByteCount(FFormat);
    GetMem(buf, buf_size);
    try
      while not Terminated do
      begin
        //Fetch the audio data if not paused
        if not FPaused then
        begin
          //Increment the position by the number of samples which has been played
          read_size := FCallback(buf, buf_size, FPosition);

          //...and play it
          ao_play(device, buf, read_size);
          FPosition := FPosition + (read_size * 8) div (FFormat.bits * FFormat.channels);
        end else
          Sleep(1);
      end;
    finally
      ao_close(device);
      FreeMem(buf, buf_size);
    end;
  end else
  begin
    FCritSect.Enter;
    try
      FInitialized := 0;
    finally
      FCritSect.Leave;
    end;
  end;
end;

procedure TAuLibaoStreamingThread.SetActive(AActive: Boolean);
begin
  FCritSect.Enter;
  try
    FPaused := not AActive;
  finally
    FCritSect.Leave;
  end;
end;

procedure TAuLibaoStreamingThread.Reset;
begin
  FCritSect.Enter;
  try
    FPosition := 0;
    FPaused := true;
  finally
    FCritSect.Leave;
  end;
end;

function TAuLibaoStreamingThread.Initialized: Integer;
begin
  FCritSect.Enter;
  try
    result := FInitialized;
  finally
    FCritSect.Leave;
  end;
end;

initialization
  if init_libao() then
    AcRegSrv.RegisterClass(TAuLibaoDriver, @CreateLibaoDriver);

finalization
  finalize_libao();

end.
