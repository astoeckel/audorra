{*******************************************************}
{                                                       }
{       Audorra Digital Audio Library                   }
{       Copyright (c) Andreas Stöckel, 2010             }
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

File: AuALSA.pas
Author: Andreas Stöckel
}

{Contains the ALSA driver for Audorra.}
unit AuALSA;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AcSyncObjs, AcPersistent,
  AuTypes, AuDriverClasses,
  alsa;

type
  TAuALSADevice = record
    name: string;
    uid: string;
  end;
  PAuALSADevice = ^TAuALSADevice;

  { TAuALSADriver }
  TAuALSADriver = class(TAuDriver)
    private
      FDeviceList: TList;
      procedure AddDevice(AName: string; AUid: string);
      procedure ALSAEnumDevices(ACard: Integer);
      procedure ALSAEnumCards;
    public
      constructor Create;
      destructor Destroy;override;

      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;

      function CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;override;
  end;

  { TAuALSAStreamDriverThread }

  TAuALSAStreamDriverThread = class(TThread)
    private
      FCriticalSection: TAcCriticalSection;
      FCallback: TAuStreamDriverProc;
      FInitialized: Integer;
      FDeviceName: PAnsiChar;
      FFormat: TAuDriverParameters;
      FAction: Integer;

      function GetFormat: snd_pcm_format_t ;
      function Init(var hndl: Psnd_pcm_t): Boolean;
    protected
      procedure Execute;override;
    public
      constructor Create(ADeviceName: PAnsiChar; ACallback: TAuStreamDriverProc;
        AFormat: TAuDriverParameters);
      destructor Destroy;override;

      procedure SetActive(AActive: Boolean);
      procedure FlushBuffer;
      function Initialized: Integer;
  end;

  { TAuALSAStreamDriver }

  TAuALSAStreamDriver = class(TAuStreamDriver)
    private
      FThread: TAuALSAStreamDriverThread;
      FDeviceName: PAnsiChar;
    public
      constructor Create(ADeviceName: PAnsiChar);
      destructor Destroy;override;

      function Open(ADriverParams: TAuDriverParameters;
        ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;override;
      procedure Close;override;
      procedure SetActive(AActive: Boolean);override;
      procedure FlushBuffer;override;
    end;

implementation

{ TAuALSADriver }

constructor TAuALSADriver.Create;
begin
  inherited Create;

  FDeviceList := TList.Create;

  //Enumerate over all soundcards and write their devices to the device list
  AddDevice('Defaul Device', 'default');
  ALSAEnumCards;
end;

destructor TAuALSADriver.Destroy;
var
  i: integer;
begin
  //Free all allocated device specifiers
  for i := FDeviceList.Count - 1 downto 0 do
    Dispose(PAuALSADevice(FDeviceList[i]));

  FreeAndNil(FDeviceList);
  inherited Destroy;
end;

procedure TAuALSADriver.AddDevice(AName: string; AUid: string);
var
  dev: PAuALSADevice;
begin
  //Instanciate a new copy of the device record
  New(dev);

  //Copy "human" devie name and uid into the record and add the record to the list
  dev^.name := AName;
  dev^.uid := AUid;
  FDeviceList.Add(dev);
end;

procedure TAuALSADriver.ALSAEnumDevices(ACard: Integer);
var
  ctl: Psnd_ctl_t;
  pcm_info: Psnd_pcm_info_t;
  pcm_device: integer;
  dev: string;
  err: integer;
begin
  dev := 'hw:' + IntToStr(ACard);

  err := snd_ctl_open(@ctl, PChar(dev), 0);
  if (err < 0) then
    exit;

  snd_pcm_info_malloc(@pcm_info);

  while true do
  begin
    err := snd_ctl_pcm_next_device(ctl, @pcm_device);
    if (err < 0) or (pcm_device < 0) then
      break;

    snd_pcm_info_set_device(pcm_info, pcm_device);
    snd_pcm_info_set_subdevice(pcm_info, 0);
    snd_pcm_info_set_stream(pcm_info, SND_PCM_STREAM_PLAYBACK);

    err := snd_ctl_pcm_info(ctl, pcm_info);
    if err >= 0 then
    begin
      dev := dev + ':' + IntToStr(pcm_device);
      AddDevice(snd_pcm_info_get_name(pcm_info), dev);
    end;
  end;

  snd_ctl_close(ctl);
end;

procedure TAuALSADriver.ALSAEnumCards;
var
  card_index: integer = -1;
  card_name: PChar;
  err: integer;
begin
  //Enumerate through each device
  err := snd_card_next(@card_index);
  while (err = 0) and (card_index > -1) do
  begin
    //Get the name of the sound device
    snd_card_get_name(card_index, @card_name);
    ALSAEnumDevices(card_index);
    err := snd_card_next(@card_index);
  end;
end;

procedure TAuALSADriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  dev: TAuDevice;
  i: integer;
begin
  //Go through the devices list and report all listed devices via callback
  for i := 0 to FDeviceList.Count - 1 do
  begin
    dev.Name := PAuALSADevice(FDeviceList[i])^.name;
    dev.ID := i;

    if (i = 0) then
      dev.Priority := 100
    else
      dev.Priority := 50;

    dev.UserData := nil;

    ACallback(dev);
  end;
end;

function TAuALSADriver.CreateStreamDriver(ADeviceID: integer): TAuStreamDriver;
begin
  if (ADeviceID >= 0) and (ADeviceID < FDeviceList.Count) then
    result := TAuALSAStreamDriver.Create(PChar(PAuALSADevice(FDeviceList[ADeviceID])^.uid));
end;

{ TAuALSAStreamDriverThread }

const
  ACTION_NONE = 0;
  ACTION_PLAY = 1;
  ACTION_PAUSE = 2;
  ACTION_FLUSH = 3;

  INIT_PENDING = -1;
  INIT_SUCCESSFUL = 1;
  INIT_FAILED = 0;

  STATE_PAUSED = 0;
  STATE_STOPPED = 1;
  STATE_PLAYING = 2;
  STATE_START = 3;

constructor TAuALSAStreamDriverThread.Create(ADeviceName: PAnsiChar;
  ACallback: TAuStreamDriverProc; AFormat: TAuDriverParameters);
begin
  inherited Create(false);

  //Copy the parameters
  FDeviceName := ADeviceName;
  FCallback := ACallback;
  FFormat := AFormat;

  //Create the critical section
  FCriticalSection := TAcCriticalSection.Create;

  //Initialize some values
  FAction := ACTION_NONE;
  FInitialized := INIT_PENDING;
end;

destructor TAuALSAStreamDriverThread.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
end;

function TAuALSAStreamDriverThread.GetFormat: snd_pcm_format_t;
begin
  case FFormat.Bitdepth of
    8: result := SND_PCM_FORMAT_U8;
    16: result := SND_PCM_FORMAT_S16_LE;
    32: result := SND_PCM_FORMAT_S32_LE;
  end;
end;

function TAuALSAStreamDriverThread.Init(var hndl: Psnd_pcm_t): Boolean;
begin
  try
    result :=
      (snd_pcm_open(@hndl, FDeviceName, SND_PCM_STREAM_PLAYBACK, 0) >= 0) and
      (snd_pcm_set_params(hndl, GetFormat, SND_PCM_ACCESS_RW_INTERLEAVED,
        FFormat.Channels, FFormat.Frequency, 1, 10000) >= 0);
  except
    result := false;
  end;
end;

procedure TAuALSAStreamDriverThread.Execute;
var
  buf: PByte;
  frms, lst_size, buf_size, read_count: integer;
  hndl: Psnd_pcm_t;
  pos: Int64;
  state: Integer;
begin
  hndl := nil;
  buf := nil;
  buf_size := 0;
  lst_size := 0;
  pos := 0;
  state := STATE_START;

  try
    if Init(hndl) then
    begin
      //Set the initialization to successful
      FCriticalSection.Enter;
      try
        FInitialized := INIT_SUCCESSFUL;
      finally
        FCriticalSection.Leave;
      end;

      while not Terminated do
      begin
        //Check the action variable
        FCriticalSection.Enter;
        try
          if FAction <> ACTION_NONE then
          begin
            case FAction of
              ACTION_PLAY:
              begin
                case state of
                  STATE_START, STATE_STOPPED:
                  begin
                    snd_pcm_prepare(hndl);
                    snd_pcm_start(hndl);
                  end;
                  STATE_PAUSED:
                  begin
                    snd_pcm_pause(hndl, 0);
                  end;
                end;
                state := STATE_PLAYING;
              end;
              ACTION_PAUSE:
              begin
                snd_pcm_pause(hndl, 1);
                state := STATE_PAUSED;
              end;
              ACTION_FLUSH:
              begin
                snd_pcm_drop(hndl);
                state := STATE_STOPPED;
                pos := 0;
              end;
            end;
          end;

          FAction := ACTION_NONE;
        finally
          FCriticalSection.Leave;
        end;

        if state = STATE_PLAYING then
        begin
          //Wait until enough handles had been played
          snd_pcm_wait(hndl, 1000);

          //Get the count of samples which has to be read
          frms := snd_pcm_avail_update(hndl);
          if frms > 0 then
          begin
            //Reserve memory for the write process
            buf_size := frms * Integer(FFormat.BitDepth * FFormat.Channels div 8);
            if (buf_size > lst_size) then
            begin
              ReallocMem(buf, buf_size);
              lst_size := buf_size;
            end;

            //Call the read callback and increment the buffer position
            read_count := FCallback(buf, buf_size, pos);
            pos := pos + frms;

            //Write the data to the sound device
            snd_pcm_writei(hndl, buf, (read_count * 8) div Integer(FFormat.BitDepth)
              div Integer(FFormat.Channels));
          end else
            snd_pcm_prepare(hndl);
        end else
          Sleep(1);
      end;
    end else
    begin
      //Set the initialization to failed
      FCriticalSection.Enter;
      try
        FInitialized := INIT_FAILED;
      finally
        FCriticalSection.Leave;
      end;
    end;
  finally
    //Free all opened resources
    if hndl <> nil then
      snd_pcm_close(hndl);
    if buf <> nil then
      FreeMem(buf);
  end;
end;


procedure TAuALSAStreamDriverThread.SetActive(AActive: Boolean);
begin
  FCriticalSection.Enter;
  try
    if AActive then
      FAction := ACTION_PLAY //Play
    else
      FAction := ACTION_PAUSE //Pause
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TAuALSAStreamDriverThread.FlushBuffer;
begin
  FCriticalSection.Enter;
  try
    FAction := ACTION_FLUSH; //Flush Buffer/Pause
  finally
    FCriticalSection.Leave;
  end;
end;

function TAuALSAStreamDriverThread.Initialized: Integer;
begin
  FCriticalSection.Enter;
  try
    result := FInitialized;
  finally
    FCriticalSection.Leave;
  end;
end;

{ TAuALSAStreamDriver }

constructor TAuALSAStreamDriver.Create(ADeviceName: PAnsiChar);
begin
  inherited Create;

  FDeviceName := ADeviceName;
end;

destructor TAuALSAStreamDriver.Destroy;
begin
  inherited Destroy;
end;

function TAuALSAStreamDriver.Open(ADriverParams: TAuDriverParameters;
  ACallback: TAuStreamDriverProc; out AWriteFormat: TAuBitdepth): Boolean;
begin
  result := false;
  if FThread = nil then
  begin
    AWriteFormat := AuBitDepth(ADriverParams.BitDepth);

    FThread := TAuALSAStreamDriverThread.Create(FDeviceName, ACallback, ADriverParams);
    while FThread.Initialized = INIT_PENDING do
      Sleep(1);

    result := FThread.Initialized = INIT_SUCCESSFUL;
  end;
end;

procedure TAuALSAStreamDriver.Close;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

procedure TAuALSAStreamDriver.SetActive(AActive: Boolean);
begin
  if FThread <> nil then
    FThread.SetActive(AActive);
end;

procedure TAuALSAStreamDriver.FlushBuffer;
begin
  if FThread <> nil then
    FThread.FlushBuffer;
end;

{ Functions }

function CreateALSADriver(): TAuALSADriver;
begin
  result := TAuALSADriver.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuALSADriver, @CreateALSADriver);

end.

