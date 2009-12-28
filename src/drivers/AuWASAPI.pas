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

File: AuWASAPI.pas
Author: Andreas Stöckel
}

{Contains functions, which are useful for audio programming.}
unit AuWASAPI;

interface

uses
  Windows, SysUtils, Classes, SyncObjs,
  wasapi_interface,
  AcPersistent, AcSysUtils, AcBuffer,
  AuUtils, AuTypes, AuDriverClasses;

type
  TAuWASAPIStreamDriver = class(TAuStreamDriver)
    private
      FPlaying: boolean;
      FInst: PWasapiInstance;
      FCallback: TAuReadCallback;
    public
      constructor Create(AParameters: TAuAudioParametersEx);
      destructor Destroy;override;
      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;
      function Open: boolean;override;
      procedure Close;override;
      function Idle(AReadCallback: TAuReadCallback): boolean;override;
  end;
  
  TAuWASAPIDriver = class(TAuDriver)
    public
      constructor Create;
      destructor Destroy;override;
      
      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;
      function CreateStreamDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx): TAuStreamDriver;override;
  end;

implementation

{ TAuWASAPIDriver }

constructor TAuWASAPIDriver.Create;
begin
  inherited Create;
end;

function TAuWASAPIDriver.CreateStreamDriver(ADeviceID: integer;
  AParameters: TAuAudioParametersEx): TAuStreamDriver;
begin
  result := TAuWASAPIStreamDriver.Create(AParameters);
end;

destructor TAuWASAPIDriver.Destroy;
begin
  inherited;
end;

procedure TAuWASAPIDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  default: TAuDevice;
begin
  default.Name := 'WASAPI Default Audio Device';
  default.ID := -1;
  default.Priority := 10;
  default.UserData := nil;
  ACallback(default);
  
  inherited;
end;

{ TAuWASAPIStreamDriver }

procedure TAuWASAPIStreamDriver_wasapi_callback(pSender: Pointer;
  Buf: Pointer; BufSize: integer);cdecl;
var
  c: integer;
begin
  with TAuWASAPIStreamDriver(pSender) do
  begin
    c := BufSize * Integer(AuBytesPerSample(FParameters));
    
    //Clear the memory
    FillChar(PByte(Buf)^, c, 0);

    if FPlaying then
      FCallback(PByte(Buf), c, FSyncData);
  end;
end;

constructor TAuWASAPIStreamDriver.Create(AParameters: TAuAudioParametersEx);
begin
  inherited Create;

  FParameters := AParameters;
  FDelay := 20;
end;

destructor TAuWASAPIStreamDriver.Destroy;
begin
  Close;
  inherited;
end;

function TAuWASAPIStreamDriver.Idle(AReadCallback: TAuReadCallback): boolean;
begin
  if FInst <> nil then
  begin
    FCallback := AReadCallback;
    result := wasapi_idle(FInst, TAuWASAPIStreamDriver_wasapi_callback) = 1;
  end else
    result := false;
end;

function TAuWASAPIStreamDriver.Open: boolean;
begin
  result := false;
  if FInst = nil then
  begin
    FPlaying := false;

    //Try to open the WASPI Interface
    FInst := wasapi_open(self, FParameters.Channels, FParameters.BitDepth,
      FParameters.Frequency);
    result := FInst <> nil;
  end;
end;

procedure TAuWASAPIStreamDriver.Close;
begin
  if FInst <> nil then
  begin
    wasapi_stop(FInst);
    wasapi_close(FInst);
    FInst := nil;
  end;
end;

procedure TAuWASAPIStreamDriver.Pause;
begin
  if FInst <> nil then
  begin
    FPlaying := false;
    wasapi_stop(FInst);
  end;
end;

procedure TAuWASAPIStreamDriver.Play;
begin
  if FInst <> nil then
  begin
    FPlaying := true;
    wasapi_start(FInst);
  end;
end;

procedure TAuWASAPIStreamDriver.Stop;
begin
  if FInst <> nil then
  begin
    FPlaying := false;
    wasapi_reset(FInst);
  end;
end;

function CreateWASAPIDriver: TAuWASAPIDriver;
begin
  result := TAuWASAPIDriver.Create;
end;

initialization
  //Load the WASPI interface dll - only if this worked, load the driver
  if InitWASAPI then
    AcRegSrv.RegisterClass(TAuWASAPIDriver, @CreateWASAPIDriver);

finalization
  FinalizeWASAPI;

end.
