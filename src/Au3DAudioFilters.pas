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
  SysUtils, Classes, Contnrs,
  AuTypes, AuUtils, Au3DAudioRenderer, AuFilterGraph;

type
  {TAuSyncDataPair is used internally by TAuSyncData list to attach an audio output
   timecode to a sync data record.}
  TAuSyncDataPair = record
    {The timecode of the audio output.}
    Timecode: Cardinal;
    {The original sync data time code at this position.}
    SyncData: TAuSyncData;
  end;
  {Pointer on TAuSyncDataPair}
  PAuSyncDataPair = ^TAuSyncDataPair;

  {TAuSyncData list is a class which organizes the attaching of audio output timecodes
   to decoder sync data records. This class is internally used by TAu3DSoundFilterAdapter.}
  TAuSyncDataList = class
    private
      FList: TQueue;
    public
      {Creates an instance of TAuSyncDataList.}
      constructor Create;
      {Destroys the instance of TAuSyncDataList.}
      destructor Destroy;override;

      {Clears the list.}      
      procedure Clear;
      {Adds a new sync data record to the list and attaches it to the given timecode.
       ATimecode is required to be steadily increasing! When jumping/seeking in the
       audio stream, clear has to be called in order to keep the pairs in order.}
      procedure AddSyncData(ATimecode: Cardinal; const ASyncData: TAuSyncData);
      {Returns the sync data record which matches the given timecode. The sync data
       gets written to the memory address specified by ASyncData.
       @returns(true if such a sync data record was found which is attached to the given
        timecode, false if not.)}
      function GetSyncData(ATimecode: Cardinal; ASyncData: PAuSyncData): Boolean;
  end;

  {TAu3DSoundFilterAdapter is a class which simulates a driver output. Instead of
   outputing the audio data to an audio driver, TAu3DSoundFilterAdapter connects
   the audio source to a 3D Audio Renderer. TAu3DSoundFilterAdapter adds an own
   TAu3DSound object to the audio renderer. TAu3DSoundFilterAdapter can e.g. be
   used to attach a TAuPlayer to TAu3DSoundRenderer. To playback the sound delivered
   by the sound renderer, use the TAu3DOutputFilterAdapter.}
  TAu3DSoundFilterAdapter = class(TAuOutputFilter)
    private
      FRenderer: TAu3DSoundRenderer;
      FSound: TAu3DStreamedSound;
      FCallback: TAuReadCallback;
      FSyncDataList: TAuSyncDataList;
      FTime: Double;
      FLastSyncData: TAuSyncData;
      procedure FreeSound;
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;
    public
      {Creates a new instance of TAu3DSoundFilterAdapter. ARenderer specifies the
       3D Audio renderer the filter should connect with. The sound object won't be
       specified after creating the filter adapter! Before it can be used, "Init"
       has to be called.}
      constructor Create(ARenderer: TAu3DSoundRenderer);
      {Destroys the instance of TAu3DSoundFilterAdapter including the sound object.}
      destructor Destroy;override;

      {Initializes the audio output and creates the sound object.}
      procedure Init(const AParameters: TAuAudioParameters);override;

      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;

      function GetOutputFilter: TAuOutputFilter;override;

      function AddSource(ACallback: TAuReadCallback): boolean;override;
      function RemoveSource(ACallback: TAuReadCallback): boolean;override;

      {Pointer on the 3d audio renderer.}
      property Renderer: TAu3DSoundRenderer read FRenderer;
      {Pointer on the sound object.}
      property Sound: TAu3DStreamedSound read FSound;
  end;

  {TAu3DOutputFilterAdapter allows you to connect the output of an 3D audio renderer
   to a filter graph environment. Use TAu3DOutputFilterAdapter to connect the
   3D audio renderer to a driver output filter. TAu3DOutputFilterAdapter is the
   counterpart to TAu3DSoundFilterAdapter.}
  TAu3DOutputFilterAdapter = class(TAuTargetFilter)
    private
      FRenderer: TAu3DSoundRenderer;
      FTimecode: Single;
      FState: TAuFrameType;
      FListener: TAu3DListener;
    protected      
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;override;
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

      function AddSource(ACallback: TAuReadCallback): boolean;override;
      function RemoveSource(ACallback: TAuReadCallback): boolean;override;

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
  FSyncDataList := TAuSyncDataList.Create;
end;

destructor TAu3DSoundFilterAdapter.Destroy;
begin
  FRenderer.Lock;
  try
    FSyncDataList.Free;
    FreeSound;
    inherited;
  finally
    FRenderer.Unlock;
  end;
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
    FSyncDataList.Clear;
    FTime := 0;
    FLastSyncData.Timecode := 0;
    FLastSyncData.FrameType := auftBeginning;

    //Pause the TAu3DSound and clear its buffers
    FSound.Active := false;
    FSound.ClearBuffers;
  finally
    FRenderer.Unlock;
  end;
end;

function TAu3DSoundFilterAdapter.GetOutputFilter: TAuOutputFilter;
begin
  result := self;
end;

procedure TAu3DSoundFilterAdapter.Init(const AParameters: TAuAudioParameters);
begin
  inherited;

  //Free the sound if it had been already created
  FreeSound;

  //Create a new sound object
  FSound := TAu3DStreamedSound.Create(ReadCallback, AParameters);
  //This sound object isn't playing
  FSound.Active := false;
  //Add the sound to the renderer
  FRenderer.Sounds.Add(FSound);

  //Calculate the delay between reading and outputting the data. This value
  //is important for synchornizing the visualizations properly.
  FDelay := FSound.BufferSamples * 500 div Integer(Parameters.Frequency);
end;

function TAu3DSoundFilterAdapter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
var
  sd: TAuSyncData;
begin
  if Assigned(FCallback) then
  begin 
    //Read the audio data from the data source
    result := FCallback(ABuf, ASize, ASyncData);

    //Connect the sync data given by the audio source to the current time position
    if (result > 0) and not AuCompSyncData(FLastSyncData, ASyncData) then
    begin
      FSyncDataList.AddSyncData(round(FTime * 1000), ASyncData);
      FLastSyncData := ASyncData;
    end;

    //Try to search for a sync data piece which fits to the current time position
    //of the sound object. 
    if FSyncDataList.GetSyncData(round((FSound.TimePosition shr 16 * 1000) /
      (Parameters.Frequency)), @sd) then
      FSyncData := sd;

    //If we're at the end of the stream, call the stop event
    if (FSyncData.FrameType = auftEnding) and (Assigned(FStopEvent)) then
      FStopEvent(self);

    FTime := FTime + result / AuBytesPerSecond(Parameters);
  end else
    result := 0;
end;

function TAu3DSoundFilterAdapter.AddSource(ACallback: TAuReadCallback): boolean;
begin
  if not Assigned(FCallback) then
  begin
    FCallback := ACallback;
    result := true;
  end else
    result := false;
end;

function TAu3DSoundFilterAdapter.RemoveSource(
  ACallback: TAuReadCallback): boolean;
begin
  if CompareMem(@ACallback, @FCallback, SizeOf(TMethod)) then
  begin
    FCallback := nil;
    result := true;
  end else
    result := false;
end;

{ TAu3DOutputFilterAdapter }

constructor TAu3DOutputFilterAdapter.Create(ARenderer: TAu3DSoundRenderer);
begin
  inherited Create(AuAudioParameters(ARenderer.Frequency,
    ARenderer.Setup.OutputChannelCount));
  FRenderer := ARenderer;
  FListener := TAu3DListener.Create;

  //Initialize the sync data type
  FState := auftBeginning;
  FTimecode := 0;
end;

destructor TAu3DOutputFilterAdapter.Destroy;
begin
  FListener.Free;
  inherited;
end;

function TAu3DOutputFilterAdapter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
begin
  //Set the sync data
  ASyncData.Timecode := round(FTimeCode);
  ASyncData.FrameType := FState;

  //Calculate the next timecode and the next sync data state
  FTimecode := FTimecode +
    (ASize div AuBytesPerSample(Parameters) * 1000) / Parameters.Frequency;
  FState := auftNormal;

  try
    FRenderer.Render(FListener, ASize div AuBytesPerSample(Parameters), ABuf);
  finally
    result := ASize;
  end;
end;

function TAu3DOutputFilterAdapter.AddSource(
  ACallback: TAuReadCallback): boolean;
begin
  //Connections to this block are not possible
  result := false;
end;

function TAu3DOutputFilterAdapter.RemoveSource(
  ACallback: TAuReadCallback): boolean;
begin
  //Connections to this block are not possible
  result := false;
end;

{ TAuSyncDataList }

constructor TAuSyncDataList.Create;
begin
  inherited Create;

  FList := TQueue.Create;
end;

destructor TAuSyncDataList.Destroy;
begin
  Clear;
  FList.Destroy; 
  inherited;
end;

procedure TAuSyncDataList.AddSyncData(ATimecode: Cardinal;
  const ASyncData: TAuSyncData);
var
  pp: PAuSyncDataPair;
begin
  //Create a new syncdata/timecode pair and add push it onto the list.
  New(pp);
  pp^.Timecode := ATimecode;
  pp^.SyncData := ASyncData;                                          
  FList.Push(pp);
end;

procedure TAuSyncDataList.Clear;
var
  i: integer;
begin
  //Destroy every item of the queue
  for i := FList.Count - 1 downto 0 do
    Dispose(FList.Pop);
end;

function TAuSyncDataList.GetSyncData(ATimecode: Cardinal;
  ASyncData: PAuSyncData): Boolean;
var
  pp: PAuSyncDataPair;
begin
  result := false;

  if FList.Count > 0 then
  begin
    //Have a glance at the next item in the queue...
    pp := FList.Peek;
    repeat
      //If it's timecode is smaller then the timecode requested, return it.
      if ATimeCode >= pp^.Timecode then
      begin
        result := true;
        pp := FList.Pop;
        ASyncData^ := pp^.SyncData;
        Dispose(pp);
      end;

      //This procedure should be repeated until no item is left which has a
      //timecode greater than the timecode specified by ATimecode.
      if FList.Count > 0 then
        pp := FList.Peek;
    until (FList.Count = 0) or (ATimecode < pp^.Timecode);
  end;
end;

end.
