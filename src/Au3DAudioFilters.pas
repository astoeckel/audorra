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

File: Au3DAudioInterface.pas
Author: Andreas Stöckel
}

{Contains interface classes to integrate 3d audio into a filter graph environment.} 
unit Au3DAudioFilters;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils, Classes,
  AcSyncObjs,
  AuTypes, AuUtils, Au3DAudioRenderer, AuFilterGraph;

type
  {TAu3DSoundFilterAdapter is a class which simulates a driver output. This allows
   you to stream an audio stream created by a filtergraph into the 3D audio renderer.

   Instead of outputing the audio data to an audio driver, TAu3DSoundFilterAdapter
   connects the audio source to a 3D Audio Renderer. TAu3DSoundFilterAdapter adds
   an own TAu3DSound object to the audio renderer. TAu3DSoundFilterAdapter can e.g.
   be used to attach a TAuPlayer to TAu3DSoundRenderer. To playback the sound delivered
   by the sound renderer, use the TAu3DOutputFilterAdapter.
   
   TAu3DSoundFilterAdapter provides an "Sound" property to which you have to attach
   at least one TAu3DStreamedEmitter emitter object.
   
   The following example shows how TAu3DSoundFilterAdapter might be used in connection
   with TAuPlayer. Note that this behaviour is already encapsulated by TAuStreamedSound.
@longCode(#var
  adapter: TAu3DSoundFilterAdapter;
  player: TAuPlayer;
  renderer: TAu3DSoundRenderer;

[...]

  adapter := TAu3DSoundFilterAdapter.Create(renderer);
  player := TAuPlayer.Create(auaudio, adapter);
#)
  @seealso(TAu3DStreamedSound)
  @seealso(TAu3DStreamedEmitter)
  @seealso(TAuStreamedSound)
  @seealso(TAu3DAudio)}
  TAu3DSoundFilterAdapter = class(TAuOutputFilter)
    private
      FRenderer: TAu3DSoundRenderer;
      FSound: TAu3DStreamedSound;
      FTimeCode: Int64;
      FTCCritSect: TAcCriticalSection;
      procedure FreeSound;
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        APlaybackSample: Int64):Cardinal;
    protected
      {}
      function GetTimecode: Int64;override;
      function DoInit(const AParameters: TAuAudioParameters): Boolean;override;
      procedure DoFinalize;override;
      procedure DoFlush;override;
    public
      {Creates a new instance of TAu3DSoundFilterAdapter. The sound object won't be
       specified after creating the filter adapter! Before it can be used, "Init"
       has to be called.       
       @param(ARenderer specifies the 3D Audio renderer the filter should
        interconnect to.)}
      constructor Create(ARenderer: TAu3DSoundRenderer);
      {Destroys the instance of TAu3DSoundFilterAdapter including the sound object.}
      destructor Destroy;override;

      {Activates the managed TAu3DStreamedSound object.
      @seealso(TAuOutputFilter.Play)}
      procedure Play;override;
      {Deactivates the managed TAu3DStreamedSound object.
      @seealso(TAuOutputFilter.Pause)}
      procedure Pause;override;
      {Deactivates the managed TAu3DStreamedSound object and flushes its buffer.
      @seealso(TAuOutputFilter.Stop)}
      procedure Stop;override;

      {Pointer on the 3d audio renderer.}
      property Renderer: TAu3DSoundRenderer read FRenderer;
      {Pointer on the sound object.}
      property Sound: TAu3DStreamedSound read FSound;
  end;

  {TAu3DOutputFilterAdapter allows you to connect the output of an 3D audio renderer
   to a filter graph environment. Use TAu3DOutputFilterAdapter to connect the
   3D audio renderer to a driver output filter. TAu3DOutputFilterAdapter is the
   counterpart to TAu3DSoundFilterAdapter.
   
   TAu3DOutputFilterAdapter contains an 3D audio renderer listener object which
   controls the 3D renderer listener parameters. You can attach as many of these
   filter adapters to the 3d renderer as you like.
   
   TAu3DOutputFilterAdapter is used by TAu3DAudio which provides an 3D audio renderer
   with an attached filtergraph.}
  TAu3DOutputFilterAdapter = class(TAuSourceFilter)
    private
      FRenderer: TAu3DSoundRenderer;
      FListener: TAu3DListener;
    protected
      function DoAddSource(AFilter: TAuFilter): Boolean;override;
      function DoInit(const AParameters: TAuAudioParameters): Boolean;override;
      function DoCheckFilter: Boolean;override;
      procedure DoFinalize;override;
      procedure DoFlush;override;
      function DoReadCallback(ABuf: PSingle; ASize: Cardinal):Cardinal;override;
    public
      {Creates an instance of TAu3DOutputFilterAdapter.
       @params(ARenderer specifies the 3d audio renderer this filter should be
         interconnected with. Filter graph block parameters are automatically read
         from the renderer. A listener object will be created when creating this block.
         You're able to access it by using the "Listener" property.)}
      constructor Create(ARenderer: TAu3DSoundRenderer);
      {Destroys this instance of TAu3DOutputFilterAdapter. The lister object will
       be destroyed.}
      destructor Destroy;override;

      {Pointer to the listener object used in connection with this output filter.}
      property Listener: TAu3DListener read FListener;
  end;

implementation

{ TAu3DSoundFilterAdapter }

constructor TAu3DSoundFilterAdapter.Create(ARenderer: TAu3DSoundRenderer);
begin
  inherited Create;

  FRenderer := ARenderer;
  FSound := nil;
  FTCCritSect := TAcCriticalSection.Create;
end;

destructor TAu3DSoundFilterAdapter.Destroy;
begin
  FRenderer.Lock;
  try
    FreeSound;
  finally
    FRenderer.Unlock;
  end;

  FTCCritSect.Free;

  inherited;
end;

procedure TAu3DSoundFilterAdapter.FreeSound;
begin
  if FSound <> nil then
  begin
    //The sound is automatically freed when removing it from the list
    FSound.AutoFree := true;
    FRenderer.Sounds.Remove(FSound);
    FSound := nil;
  end;
end;

procedure TAu3DSoundFilterAdapter.Pause;
begin
  FRenderer.Lock;
  try
    FSound.Active := false;
  finally
    FRenderer.Unlock;
  end;
end;

procedure TAu3DSoundFilterAdapter.Play;
begin
  FRenderer.Lock;
  try
    FSound.Active := true;
  finally
    FRenderer.Unlock;
  end;
end;

procedure TAu3DSoundFilterAdapter.Stop;
begin
  FRenderer.Lock;
  try
    //Clear the sync data list
    FTimeCode := 0;

    //Pause the TAu3DSound and clear its buffers
    FSound.Active := false;
    FSound.ClearBuffers;
  finally
    FRenderer.Unlock;
  end;
end;

procedure TAu3DSoundFilterAdapter.DoFinalize;
begin
  //Free the sound object
  FreeSound;
end;

procedure TAu3DSoundFilterAdapter.DoFlush;
begin
  //Flush the internal buffers inside the sound object
  FRenderer.Lock;
  try
    FSound.ClearBuffers;
  finally
    FRenderer.Unlock;
  end;
end;

function TAu3DSoundFilterAdapter.DoInit(const AParameters: TAuAudioParameters): Boolean;
begin
  result := true;
  
  //Create a new sound object
  FSound := TAu3DStreamedSound.Create(ReadCallback, AParameters);
  //This sound object isn't playing
  FSound.Active := false;

  //Calculate the delay between reading and outputting the data. This value
  //is important for synchornizing the visualizations properly.
//  FDelay := FSound.BufferSamples * 500 div Integer(Parameters.Frequency);

  FRenderer.Lock;
  try
    //Add the sound to the renderer
    FRenderer.Sounds.Add(FSound);
  finally
    FRenderer.Unlock;
  end;
end;

function TAu3DSoundFilterAdapter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  APlaybackSample: Int64): Cardinal;
begin
  Mutex.Acquire;
  try
    if Sources.Count = 1 then
    begin
      //Read the audio data from the data source into the parameters passed by
      //the caller of this function
      result := TAuSourceFilter(Sources[0]).ReadCallback(PSingle(ABuf), ASize);

      FTCCritSect.Acquire;
      try
        FTimeCode := APlaybackSample;
      finally
        FTCCritSect.Release;
      end;
    end else
      result := 0;
  finally
    Mutex.Release;
  end;
end;

function TAu3DSoundFilterAdapter.GetTimecode: Int64;
begin
  FTCCritSect.Acquire;
  try
    result := FTimeCode;
  finally
    FTCCritSect.Release;
  end;
end;

{ TAu3DOutputFilterAdapter }

constructor TAu3DOutputFilterAdapter.Create(ARenderer: TAu3DSoundRenderer);
begin
  inherited Create;
  
  FRenderer := ARenderer;
end;

destructor TAu3DOutputFilterAdapter.Destroy;
begin
  inherited;
end;

function TAu3DOutputFilterAdapter.DoAddSource(AFilter: TAuFilter): Boolean;
begin
  //Connections to this block are not possible
  result := false;
end;

function TAu3DOutputFilterAdapter.DoCheckFilter: Boolean;
begin
  result := (Sources.Count = 0);
end;

procedure TAu3DOutputFilterAdapter.DoFlush;
begin
  //Do nothing, there is not buffer which needs to be flushed.
end;

function TAu3DOutputFilterAdapter.DoInit(
  const AParameters: TAuAudioParameters): Boolean;
begin
  result := (AParameters.Frequency = FRenderer.Frequency) and
    (AParameters.Channels = Cardinal(FRenderer.Setup.OutputChannelCount));

  //Create the listener object
  FListener := TAu3DListener.Create;
end;

procedure TAu3DOutputFilterAdapter.DoFinalize;
begin
  //Destroy the listener object
  FreeAndNil(FListener);
end;

function TAu3DOutputFilterAdapter.DoReadCallback(ABuf: PSingle;
  ASize: Cardinal): Cardinal;
begin
  try
    FRenderer.Render(FListener, ASize div AuBytesPerSample(Parameters), PByte(ABuf));
  finally
    result := ASize;
  end;
end;

end.
