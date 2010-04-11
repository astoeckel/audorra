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

File: AuNULL.pas
Author: Andreas Stöckel
}

{Contains a NULL driver for Audorra.}
unit AuNULL;

interface

uses
  AcPersistent, AcSysUtils,
  AuTypes, AuUtils, AuDriverClasses;

type
  TAuNULLStreamDriver = class(TAuStreamDriver)
    protected
      FMem: PByte;
      FSmpls: Cardinal;
      FSize: Cardinal;
      FTC: Double;
      FFill: Boolean;
      FWait: Boolean;
    public
      constructor Create(AFmt: TAuAudioParametersEx; AWait: Boolean = true);
      destructor Destroy;override;
      
      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;
      function Open: boolean;override;
      procedure Close;override;

      function Idle(AReadCallback: TAuReadCallback): boolean;override;
  end;

  TAuNULLDriver = class(TAuDriver)
    public
      procedure EnumDevices(ACallback: TAuEnumDeviceProc);override;
      function CreateStreamDriver(ADeviceID: integer;
        AParameters: TAuAudioParametersEx): TAuStreamDriver;override;
  end;

const
  AuNULLBufferTime = 20;

implementation

{ TAuNULLStreamDriver }

constructor TAuNULLStreamDriver.Create(AFmt: TAuAudioParametersEx; AWait: Boolean);
begin
  inherited Create;
  
  FParameters := AFmt;
  FDelay := AuNULLBufferTime;

  FSmpls := AuNullBufferTime * AFmt.Frequency div 1000;
  FSize := AuBytesPerSample(AFmt) * FSmpls;
  FMem := GetMemory(FSize);
  FFill := true;
  FWait := AWait;
end;

destructor TAuNULLStreamDriver.Destroy;
begin
  if FMem <> nil then
    FreeMem(FMem);
  FMem := nil;
  FSize := 0;
  
  inherited;
end;

function TAuNULLStreamDriver.Idle(AReadCallback: TAuReadCallback): boolean;
var
  tc2: Double;
begin
  result := false;
  if FState = audsPlaying then
  begin
    tc2 := AcGetTickCount;
    if FWait or FFill or (tc2 - FTC > AuNULLBufferTime) then
    begin
      AReadCallback(FMem, FSize, FSyncData);

      FFill := false;
      FTC := tc2;
      result := not FWait;
    end;
  end;
end;

function TAuNULLStreamDriver.Open: boolean;
begin
  result := FMem <> nil;
  FState := audsOpened;
end;

procedure TAuNULLStreamDriver.Close;
begin
  FState := audsClosed;
end;

procedure TAuNULLStreamDriver.Pause;
begin
  if FState = audsPlaying then
    FState := audsPaused;
end;

procedure TAuNULLStreamDriver.Play;
begin
  if FState >= audsOpened then
    FState := audsPlaying;
end;

procedure TAuNULLStreamDriver.Stop;
begin
  if FState >= audsOpened then
  begin
    FState := audsOpened;
    FFill := true;
  end;
end;

{ TAuNULLDriver }

function TAuNULLDriver.CreateStreamDriver(ADeviceID: integer;
  AParameters: TAuAudioParametersEx): TAuStreamDriver;
begin
  if (ADeviceID >= 0) and (ADeviceID <= 1) then
    result := TAuNULLStreamDriver.Create(AParameters, ADeviceID = 1)
  else
    result := nil;
end;

procedure TAuNULLDriver.EnumDevices(ACallback: TAuEnumDeviceProc);
var
  dev: TAuDevice;
begin
  //Return the null device
  dev.Name := 'NULL Normal';
  dev.ID := 0;
  dev.Priority := 1;
  dev.UserData := nil;
  ACallback(dev);

  //Return the null device
  dev.Name := 'NULL Stream';
  dev.ID := 1;
  dev.Priority := 0;
  dev.UserData := nil;  
  ACallback(dev);
end;

function CreateNULLDriver: TAuNULLDriver;
begin
  result := TAuNULLDriver.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuNULLDriver, @CreateNULLDriver);

end.
