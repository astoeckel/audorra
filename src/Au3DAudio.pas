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

File: Au3DAudio.pas
Author: Andreas Stöckel
}

unit Au3DAudio;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils,
  AuTypes, AuDriverClasses,
  AuAudio, AuFilterGraph,
  Au3DAudioRenderer, Au3DAudioFilters;

type
  TAu3DAudio = class
    private
      FAudio: TAuAudio;
      FRenderer: TAu3DSoundRenderer;
      FOutputAdapter: TAu3DOutputFilterAdapter;
      FDriver: TAuStreamDriver;
      FDriverFilter: TAuDriverOutput;
      FFrequency: integer;
      FBitdepth: integer;
      FSpeakerPreset: TAu3DSpeakerPreset;
      FDeviceID: integer;  
      function GetParameters: TAuAudioParametersEx;
    public
      constructor Create(AAudio: TAuAudio; ASpeakerPreset: TAu3DSpeakerPreset;
        AFrequency, ABitdepth: integer; ADeviceID: integer = -1);
      destructor Destroy;override;

      procedure Lock;
      procedure Unlock;

      function Initialize: boolean;
      procedure Finalize;
      
      property Audio: TAuAudio read FAudio;
      property Renderer: TAu3DSoundRenderer read FRenderer;
      property OutputAdapter: TAu3DOutputFilterAdapter read FOutputAdapter;
      property SpeakerPreset: TAu3DSpeakerPreset read FSpeakerPreset;
      property Parameters: TAuAudioParametersEx read GetParameters;
      property DeviceID: integer read FDeviceID;
  end;

implementation

{ TAu3DAudio }

constructor TAu3DAudio.Create(AAudio: TAuAudio;
  ASpeakerPreset: TAu3DSpeakerPreset; AFrequency, ABitdepth: integer;
  ADeviceID: integer = -1);
begin
  inherited Create;

  //Copy some parameters
  FAudio := AAudio;
  FFrequency := AFrequency;
  FBitdepth := ABitdepth;
  FSpeakerPreset := ASpeakerPreset;
  FDeviceID := ADeviceID;

  //Fetch the standard devicd ID
  if FDeviceID = -1 then
    FDeviceID := FAudio.StandardDeviceID;
end;

destructor TAu3DAudio.Destroy;
begin
  Finalize;
  inherited;
end;

function TAu3DAudio.Initialize: boolean;
begin
  Finalize;

  result := false;

  //Create the 3D audio renderer
  FRenderer := TAu3DSoundRenderer.Create(FSpeakerPreset, FFrequency);

  //Create the output driver
  FDriver := FAudio.Driver.CreateStreamDriver(FDeviceID, GetParameters);
  if (FDriver <> nil) and (FDriver.Open) then
  begin
    //                                      FDriver
    //                                        / \
    //                                         |
    //  FRenderer --> FOutputAdapter --> FDriverFilter
    //                       |
    //                       |
    //                      \ /
    //                   FListener


    //Create the driver filter block
    FDriverFilter := TAuDriverOutput.Create(FDriver);
    FDriverFilter.Init(AuAudioParameters(FFrequency, FRenderer.Setup.OutputChannelCount));

    //Create the output adpter block and interconnect it to the renderer
    FOutputAdapter := TAu3DOutputFilterAdapter.Create(FRenderer);
    FOutputAdapter.Target := FDriverFilter;

    //Start the output
    FDriverFilter.Play;
    
    result := true;
  end;

  if not result then
    Finalize;
end;

procedure TAu3DAudio.Finalize;
begin
  FreeAndNil(FDriverFilter);
  FreeAndNil(FDriver);
  FreeAndNil(FOutputAdapter);
  FreeAndNil(FRenderer);
end;

function TAu3DAudio.GetParameters: TAuAudioParametersEx;
begin
  result.Frequency := FFrequency;
  result.Channels := FRenderer.Setup.OutputChannelCount;
  result.BitDepth := FBitdepth;
end;

procedure TAu3DAudio.Lock;
begin
  //Call the TAu3DSoundRenderer.Lock function
  FRenderer.Lock;
end;

procedure TAu3DAudio.Unlock;
begin
  //Call the TAu3DSoundRenderer.Unlock function
  FRenderer.Unlock;
end;

end.
