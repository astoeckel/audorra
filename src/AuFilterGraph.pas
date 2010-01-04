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

File: AuFilterGraph.pas
Author: Andreas Stöckel
}

{Contains the audorra filter graph structure.} 
unit AuFilterGraph;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
  AcBuffer, AcSysUtils,
  AuTypes, AuDriverClasses, AuDecoderClasses, AuUtils,
  AuSyncUtils,
  AuAnalyzerClasses;

type
  TAuOutputFilter = class;
  
  TAuFilter = class(TObject)
    private
      FParameters: TAuAudioParameters;
    public
      constructor Create(AParameters: TAuAudioParameters);
      destructor Destroy;override;

      function GetOutputFilter: TAuOutputFilter;virtual;

      function AddSource(ACallback: TAuReadCallback): boolean;virtual;abstract;
      function RemoveSource(ACallback: TAuReadCallback): boolean;virtual;abstract;

      property Parameters: TAuAudioParameters read FParameters;
  end;

  TAuSyncDataChangeProc = procedure(ASyncData: TAuSyncData) of object;

  TAuIdleThread = class(TThread)
    private
      FCallback: TAuReadCallback;
      FDriver: TAuStreamDriver;
      FSyncDataChange: TAuSyncDataChangeProc;
      FOldSyncData: TAuSyncData;
      FPlaying: boolean;
      procedure SyncMethod;
      procedure SetSyncDataChange(AVal: TAuSyncDataChangeProc);
    protected
      procedure Execute;override;
    public
      constructor Create(ADriver: TAuStreamDriver; ACallback: TAuReadCallback);

      procedure BeforeDestruction;override;

      procedure Play;
      procedure Pause;
      
      property OnSyncDataChange: TAuSyncDataChangeProc read FSyncDataChange write
        SetSyncDataChange;
  end;

  TAuOutputFilter = class(TAuFilter)
    protected
      FSyncData: TAuSyncData;
      FStopEvent: TAuNotifyEvent;
      FDelay: Cardinal;
    public
      constructor Create;reintroduce;

      procedure Init(const AParameters: TAuAudioParameters);virtual;

      property SyncData: TAuSyncData read FSyncData;
      property OnStop: TAuNotifyEvent read FStopEvent write FStopEvent;
      property Delay: Cardinal read FDelay;

      procedure Play;virtual;abstract;
      procedure Pause;virtual;abstract;
      procedure Stop;virtual;abstract;      
  end;

  TAuDriverOutput = class(TAuOutputFilter)
    private
      FCallback: TAuReadCallback;
      FIdleThread: TAuIdleThread;
      FDriver: TAuStreamDriver;
      FFloatBuf: PByte;
      FFloatBufSize: integer;
      procedure SyncDataChange(ASyncData: TAuSyncData);
      procedure FreeThread;
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;
    protected
    public
      constructor Create(ADriver: TAuStreamDriver);
      destructor Destroy;override;

      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;

      function GetOutputFilter: TAuOutputFilter;override;

      function AddSource(ACallback: TAuReadCallback): boolean;override;
      function RemoveSource(ACallback: TAuReadCallback): boolean;override;
  end;

  TAuTargetFilter = class(TAuFilter)
    private
      FTarget: TAuFilter;
      FCritSect: TCriticalSection;
      function GetConnected: boolean;   
      procedure SetTarget(AValue: TAuFilter);
      procedure RemoveConnection;
    protected
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;virtual;abstract;
      property CritSect: TCriticalSection read FCritSect;
    public
      constructor Create(AParameters: TAuAudioParameters);
      destructor Destroy;override;
      
      procedure BeforeDestruction;override;

      function GetOutputFilter: TAuOutputFilter;override;
      
      property Target: TAuFilter read FTarget write SetTarget;
      property Connected: boolean read GetConnected;
  end;

  TAuPassthroughFilter = class(TAuTargetFilter)
    private
      FCallback: TAuReadCallback;
    protected
      property Callback: TAuReadCallback read FCallback;
    public
      function AddSource(ACallback: TAuReadCallback): boolean;override;
      function RemoveSource(ACallback: TAuReadCallback): boolean;override;
  end;

  TAuAnalyzeCallback = procedure(ABuf: PByte; ASize: Cardinal) of object;

  TAuAnalyzeThread = class(TThread)
    private
      FSampleCount: integer;
      FParameters: TAuAudioParameters;
      FCallback: TAuAnalyzeCallback;
      FCritSect: TCriticalSection;
      FBuffer: TAcBuffer;
      FOutput: TAuOutputFilter;
    protected
      procedure Execute;override;
    public
      constructor Create(ASampleCount: integer; AParameters: TAuAudioParameters;
        ACallback: TAuAnalyzeCallback);
      destructor Destroy;override;

      procedure WriteNextData(ABuf: PByte; ASize: Integer; ATimecode: Cardinal);

      property Output: TAuOutputFilter read FOutput write FOutput;
  end;

  TAuAnalyzeFilter = class(TAuPassthroughFilter)
    private
      FThread: TAuAnalyzeThread;
      FAnalyzers: TAuAnalyzerList;
      FCritSect: TCriticalSection;
      FBuf: PByte;
    protected
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;override;
      procedure AnalyzeData(ABuf: PByte; ASize: Cardinal);
    public
      constructor Create(AParameters: TAuAudioParameters);
      destructor Destroy;override;

      property Analyzers: TAuAnalyzerList read FAnalyzers;
      property CriticalSection: TCriticalSection read FCritSect;
  end;

  TAuVolumeFilter = class(TAuPassthroughFilter)
    private
      FMaster: Single;
      FChannels: array of Single;
      FUseChannels: boolean;
      function GetVolume(AChannel: Cardinal): Single;
      procedure SetVolume(AChannel: Cardinal; AValue: Single);
      procedure CheckArrBounds;
    protected
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;override;
    public
      procedure Reset;
      property Master: Single read FMaster write FMaster;
      property Volume[AChannel: Cardinal]: Single read GetVolume write SetVolume;
  end;

  TAuCompressorFilter = class(TAuPassthroughFilter)
    private
      FCompressionTarget: Single;
      FReleaseTime: Single;
      FCurrentCompressionFactor: Single;
      FFac: Single;
    protected
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;override;                  
    public
      constructor Create(AParameters: TAuAudioParameters);
      
      property CompressionTarget: Single read FCompressionTarget write FCompressionTarget;
      property ReleaseTime: Single read FReleaseTime write FReleaseTime;
  end;

  TAuDecoderThread = class(TThread)
    private
      FDecoder: TAuDecoder;
      FBuffer: TAcBuffer;
      FCritSect: TCriticalSection;
      FBufSize: integer;
      FLastFrame: boolean;
    protected
      procedure Execute;override;
    public
      constructor Create(ADecoder: TAuDecoder; ABuffer: TAcBuffer;
        ABufCritSect: TCriticalSection; ABufSize: integer);

      property LastFrame: boolean read FLastFrame write FLastFrame;
  end;

  TAuCustomDecoderFilter = class(TAuTargetFilter)
    private
      FBuffer: TAcBuffer;
      FCritSect: TCriticalSection;
      FFirstFrame: boolean;
      FLocked: boolean;
    protected
      property Buffer: TAcBuffer read FBuffer;
      property CritSect: TCriticalSection read FCritSect;

      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData):Cardinal;override;

      procedure SyncDataCallback(var ASyncData: TAuSyncData; AReadBytes: Integer);virtual;
    public
      constructor Create(AParameters: TAuAudioParameters);
      destructor Destroy;override;

      procedure FlushBuffer;virtual;
      procedure Lock;
      procedure Unlock;

      function AddSource(ACallback: TAuReadCallback): boolean;override;
      function RemoveSource(ACallback: TAuReadCallback): boolean;override;  
  end;

  TAuDecoderFilter = class(TAuCustomDecoderFilter)
    private
      FDecoderThread: TAuDecoderThread;
      FDecoder: TAuDecoder;
      FBufSize: integer;
      FIntpTimecode: Integer;
      FLastTimecode: Integer;
    protected
      procedure SyncDataCallback(var ASyncData: TAuSyncData; AReadBytes: Integer);override;
    public
      constructor Create(ADecoder: TAuDecoder; ABufSize: integer);
      destructor Destroy;override;

      procedure FlushBuffer;override;
  end;

implementation

var
  fs: TFileStream;

{ TAuFilter }

constructor TAuFilter.Create(AParameters: TAuAudioParameters);
begin
  inherited Create;

  FParameters := AParameters;
end;

destructor TAuFilter.Destroy;
begin
  inherited;
end;

function TAuFilter.GetOutputFilter: TAuOutputFilter;
begin
  result := nil;
end;

{ TAuIdleThread }

procedure TAuIdleThread.BeforeDestruction;
begin
  //Remove the pointer of this class from the SyncQueue
  AuQueueRemove(self);
end;

constructor TAuIdleThread.Create(ADriver: TAuStreamDriver;
  ACallback: TAuReadCallback);
begin
  FDriver := ADriver;
  FCallback := ACallback;
  FPlaying := false;

  inherited Create(false);

  //This thread has a very high priority in the whole application - it should
  //always be able to gain new data - if it isn't able to do this, sound output
  //will start to stutter or stop
  Priority := tpHighest;
end;

procedure TAuIdleThread.Execute;
begin
  try
    while (not Terminated) do
    begin
      if FPlaying then
      begin
        //Ask the driver whether it needs more data - if it needed new data call
        //the idle function again to try to fill the audio without further delay
        while FDriver.Idle(FCallback) do;

        //Call the on sync data change event if it has changed
        if (not AuCompSyncData(FOldSyncData, FDriver.SyncData)) and Assigned(FSyncDataChange) then
          AuQueueCall(SyncMethod);

        FOldSyncData := FDriver.SyncData;
      end;

      //Sleep a short moment to prevent the CPU workload from getting too high
      Sleep(1);
    end;
  except
    //
  end;
end;

procedure TAuIdleThread.Pause;
begin
  FPlaying := false;
end;

procedure TAuIdleThread.Play;
begin
  FPlaying := true;
end;

procedure TAuIdleThread.SetSyncDataChange(AVal: TAuSyncDataChangeProc);
begin
  FSyncDataChange := AVal;
end;

procedure TAuIdleThread.SyncMethod;
begin
  if Assigned(FSyncDataChange) then
    FSyncDataChange(FOldSyncData)
end;

{ TAuDriverOutput }

constructor TAuDriverOutput.Create(ADriver: TAuStreamDriver);
begin
  inherited Create;

  FDriver := ADriver;

  //Delay refers to the time between the data has been read by the driver
  //and when it is actually played
  FDelay := FDriver.Delay;
end;

destructor TAuDriverOutput.Destroy;
begin
  FreeThread;

  if FFloatBuf <> nil then
    FreeMem(FFloatBuf, FFloatBufSize);
  FFloatBufSize := 0;

  inherited;
end;

procedure TAuDriverOutput.FreeThread;
begin
  if FIdleThread <> nil then
  begin
    FIdleThread.Terminate;
    FIdleThread.WaitFor;
    FreeAndNil(FIdleThread);
  end;
end;

function TAuDriverOutput.GetOutputFilter: TAuOutputFilter;
begin
  result := self;
end;

function TAuDriverOutput.AddSource(ACallback: TAuReadCallback): boolean;
begin
  if not Assigned(FCallback) then
  begin
    FCallback := ACallback;

    //Free the idle thread (FIdleThread should already be nil at this position - but who cares?)
    FreeThread;

    //Create the idle thread
    FIdleThread := TAuIdleThread.Create(FDriver, ReadCallback);
    FIdleThread.OnSyncDataChange := SyncDataChange;

    result := true;
  end else
    result := false;
end;

function TAuDriverOutput.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
begin
  result := 0;

  //Exit if the "Init" function hasn't been called yet
  if (FParameters.Frequency = 0) or (FParameters.Channels = 0) then
    exit;  
  
  //Translate variable bit depth driver callback calls to 32-Bit floating value
  //callback calls for the filter graph
  if Assigned(FCallback) then
  begin
    FFloatBufSize := AuConvertByteCount(ASize, FDriver.Parameters,
      AuAudioParametersEx(FParameters, 32));
    ReallocMem(FFloatBuf, FFloatBufSize);

    result := FCallback(FFloatBuf, FFloatBufSize, ASyncData);
    result := AuConvertByteCount(result, AuAudioParametersEx(FParameters, 32), FDriver.Parameters);

    AuPCMFloatToInt(FDriver.Parameters, FFloatBuf, ABuf, AuBytesToSamples(result,
      FDriver.Parameters));

    fs.Write(ABuf^, result);
  end;
end;

function TAuDriverOutput.RemoveSource(ACallback: TAuReadCallback): boolean;
begin
  if CompareMem(@ACallback, @FCallback, SizeOf(TMethod)) then
  begin
    FCallback := nil;

    //Free the idle thread
    FreeThread;

    result := true;
  end else
    result := false;
end;

procedure TAuDriverOutput.SyncDataChange(ASyncData: TAuSyncData);
begin
  FSyncData := ASyncData;
  if (ASyncData.FrameType = auftEnding) and (Assigned(FStopEvent)) then
    FStopEvent(self);
end;

procedure TAuDriverOutput.Pause;
begin
  FIdleThread.Pause;
  FDriver.Pause;
end;

procedure TAuDriverOutput.Play;
begin
  FDriver.Play;
  FIdleThread.Play;
end;

procedure TAuDriverOutput.Stop;
begin
  FSyncData.Timecode := 0;
  FSyncData.FrameType := auftBeginning;
  FDriver.Stop;
end;

{ TAuTargetFilter }

procedure TAuTargetFilter.BeforeDestruction;
begin
  //Be sure to remove all instances of this class from the SyncQueue
  AuQueueRemove(self);

  //Be sure to remove the connection before freeing the filter
  RemoveConnection;
end;

constructor TAuTargetFilter.Create(AParameters: TAuAudioParameters);
begin
  inherited;

  FCritSect := TCriticalSection.Create;
end;

destructor TAuTargetFilter.Destroy;
begin
  FCritSect.Free;
  
  inherited;
end;

function TAuTargetFilter.GetConnected: boolean;
begin
  CritSect.Enter;
  try
    result := FTarget <> nil;
  finally
    CritSect.Leave;
  end;
end;

function TAuTargetFilter.GetOutputFilter: TAuOutputFilter;
begin
  CritSect.Enter;
  try
    if FTarget <> nil then
      result := FTarget.GetOutputFilter
    else
      result := inherited GetOutputFilter;
  finally
    CritSect.Leave;
  end;
end;

procedure TAuTargetFilter.RemoveConnection;
begin
  CritSect.Enter;
  try
    //Remove the connection the target filter
    if FTarget <> nil then
      FTarget.RemoveSource(ReadCallback);

    //We're not connected anymore
    FTarget := nil;
  finally
    CritSect.Leave;
  end;
end;

procedure TAuTargetFilter.SetTarget(AValue: TAuFilter);
begin
  //Make sure that we're not connected anymore
  RemoveConnection;

  CritSect.Enter;
  try
    //Connect to the given filter
    if (AValue <> nil) and AValue.AddSource(ReadCallback) then
      FTarget := AValue;
  finally
    CritSect.Leave;
  end;
end;

{ TAuDecoderThread }

constructor TAuDecoderThread.Create(ADecoder: TAuDecoder; ABuffer: TAcBuffer;
  ABufCritSect: TCriticalSection; ABufSize: integer);
begin
  FDecoder := ADecoder;
  FBuffer := ABuffer;
  FCritSect := ABufCritSect;
  FBufSize := ABufSize;

  inherited Create(false);

  Priority := tpHighest;
end;

procedure TAuDecoderThread.Execute;
var
  pckt: TAuPacket;
  FMem: PByte;
  smpls, bfsize: Integer;
begin
  FMem := nil;
  FLastFrame := false;
  
  try
    while not Terminated do
    begin
      //Check whether the target buffer is filled
      if (FBuffer.Filled < FBufSize) then
      begin
        //Tell the decoder to go on decoding
        case FDecoder.Decode of
          audsHasFrame:
          begin
            //Get the packet from the decoder
            FDecoder.GetPacket(pckt);

            //Recode the buffer to float values
            smpls := pckt.BufferSize div Integer(AuBytesPerSample(FDecoder.Info));
            bfsize := Integer(AuBytesPerSample(FDecoder.Info.Parameters)) * smpls;
            ReallocMem(FMem, bfsize);

            AuPCMIntToFloat(FDecoder.Info, pckt.Buffer, FMem, smpls);

            //Write the packet content into the buffer
            FCritSect.Enter;
            try
              FBuffer.Write(FMem, bfsize, pckt.Timecode);
            finally
              FCritSect.Leave;
            end;
          end;
          //Terminate the thread if the decoder is at the end
          audsEnd:
            FLastFrame := true;
        end;
        if FLastFrame then
          Sleep(1);
      end else
        Sleep(1);
    end;
  finally
    if FMem <> nil then
      FreeMem(FMem);
  end;
end;

{ TAuDecoderFilter }

constructor TAuDecoderFilter.Create(ADecoder: TAuDecoder; ABufSize: integer);
var
  i: integer;
begin
  inherited Create(ADecoder.Info.Parameters);

  FDecoder := ADecoder;
  FBufSize := ABufSize;
  FIntpTimecode := -1;

  //Create a new decoder thread
  FDecoderThread := TAuDecoderThread.Create(FDecoder, Buffer, CritSect, FBufSize);

  //Wait for the buffer to be filled the first time or 500ms
  i := 0;
  while (Buffer.Filled < FBufSize) and (i < 500) and
    (not FDecoderThread.LastFrame) do
  begin
    Sleep(1); i := i + 1;
  end;
end;

destructor TAuDecoderFilter.Destroy;
begin
  //Destroy the decoder thread
  FDecoderThread.Terminate;
  FDecoderThread.WaitFor;
  FDecoderThread.Free;

  inherited;
end;

procedure TAuDecoderFilter.FlushBuffer;
var
  i: integer;
begin
  inherited;

  FIntpTimecode := -1;
  
  //Wait for the buffer to be filled again or 500ms
  i := 0;
  while (Buffer.Filled < FBufSize) and (i < 50) and
    (not FDecoderThread.LastFrame) do
  begin
    Sleep(1); i := i + 1;
  end;

  FDecoderThread.FLastFrame := false;
end;

procedure TAuDecoderFilter.SyncDataCallback(var ASyncData: TAuSyncData; AReadBytes: Integer);
begin
  //Interpolate the timecodes
  if (Cardinal(FLastTimecode) = ASyncData.Timecode) and (FIntpTimecode > -1) then
  begin
    FIntpTimecode := FIntpTimecode +
      round(1000 * AReadBytes / (AuBytesPerSample(FParameters) * FParameters.Frequency));
    ASyncData.Timecode := FIntpTimecode;
  end else
  begin
    FLastTimecode := ASyncData.Timecode;
    FIntpTimecode := ASyncData.Timecode;
  end;

  if (FDecoderThread.LastFrame) and ((FBuffer.Filled = 0) or (AReadBytes = 0)) then
    ASyncData.FrameType := auftEnding
  else
    ASyncData.FrameType := auftNormal;
end;

{ TAuPassthroughFilter }

function TAuPassthroughFilter.AddSource(ACallback: TAuReadCallback): boolean;
begin
  if not Assigned(FCallback) then
  begin
    FCallback := ACallback;
    result := true;
  end else
    result := false;
end;

function TAuPassthroughFilter.RemoveSource(ACallback: TAuReadCallback): boolean;
begin
  if CompareMem(@ACallback, @FCallback, SizeOf(TMethod)) then
  begin
    FCallback := nil;
    result := true;
  end else
    result := false;
end;

{ TAuVolumeFilter }

function TAuVolumeFilter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
var
  mem: PSingle;
  i: Integer;
begin
  result := 0;
  if Assigned(Callback) then
  begin
    result := Callback(ABuf, ASize, ASyncData);

    mem := PSingle(ABuf);

    if FUseChannels then
    begin
      for i := 0 to Integer(result) div 4 - 1 do
      begin
        mem^ := mem^ * FMaster * FChannels[Cardinal(i) mod Parameters.Channels];
        inc(mem);
      end;
    end else
    begin
      //If the master channel value is one, stop here
      if Abs(FMaster - 1) > 0.00001 then
      begin
        for i := 0 to Integer(result) div 4 - 1 do
        begin
          mem^ := mem^ * FMaster;
          inc(mem);
        end;
      end;
    end;
  end;
end;

procedure TAuVolumeFilter.Reset;
begin
  FMaster := 1;
  FUseChannels := false;
  SetLength(FChannels, 0);
end;

procedure TAuVolumeFilter.CheckArrBounds;
var
  i: integer;
begin
  if FUseChannels then
  begin
    if Cardinal(Length(FChannels)) <> FParameters.Channels then
    begin
      SetLength(FChannels, FParameters.Channels);

      for i := 0 to High(FChannels) do
        FChannels[i] := 1;
    end;
  end;  
end;

function TAuVolumeFilter.GetVolume(AChannel: Cardinal): Single;
begin
  result := 1;

  if FUseChannels then
  begin
    CheckArrBounds;
    if (AChannel < Parameters.Channels) then
      result := FChannels[AChannel];
  end;
end;

procedure TAuVolumeFilter.SetVolume(AChannel: Cardinal; AValue: Single);
begin
  if (AChannel < FParameters.Channels) then
  begin
    FUseChannels := true;

    CheckArrBounds;
    FChannels[AChannel] := AValue;
  end;
end;

{ TAuAnalyzeFilter }

constructor TAuAnalyzeFilter.Create(AParameters: TAuAudioParameters);
begin
  inherited;

  FThread := TAuAnalyzeThread.Create(128, FParameters, AnalyzeData);
  FAnalyzers := TAuAnalyzerList.Create;
  FCritSect := TCriticalSection.Create;
end;

destructor TAuAnalyzeFilter.Destroy;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  FAnalyzers.Free;
  FCritSect.Free;

  if FBuf <> nil then
    FreeMem(FBuf);
  FBuf := nil;

  inherited;
end;

function TAuAnalyzeFilter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
var
  output: TAuOutputFilter;
begin
  result := 0;
  if Assigned(Callback) then
  begin
    result := Callback(ABuf, ASize, ASyncData);

    output := Target.GetOutputFilter;
    if output <> nil then
      FThread.Output := output;

    FThread.WriteNextData(ABuf, result, ASyncData.Timecode);  
  end;
end;

procedure TAuAnalyzeFilter.AnalyzeData(ABuf: PByte; ASize: Cardinal);
begin
  FCritSect.Enter;
  try
    FAnalyzers.AnalyzeData(PSingle(ABuf), ASize div AuBytesPerSample(FParameters));
  finally
    FCritSect.Leave;
  end;
end;

{ TAuAnalyzeThread }

constructor TAuAnalyzeThread.Create(ASampleCount: integer;
  AParameters: TAuAudioParameters; ACallback: TAuAnalyzeCallback);
begin
  //Set thread parameters
  FSampleCount := ASampleCount;
  FParameters := AParameters;
  FCallback := ACallback;
  FOutput := nil;

  //Create needed objects
  FCritSect := TCriticalSection.Create;
  FBuffer := TAcBuffer.Create;

  inherited Create(false);
end;

destructor TAuAnalyzeThread.Destroy;
begin
  FBuffer.Free;
  FCritSect.Free;

  inherited;
end;

procedure TAuAnalyzeThread.Execute;
var
  bufsize: Integer;
  readsize, s: integer;
  mem: PByte;
  wait: boolean;
  stamp_data, stamp_output: integer;
  
begin
  try
    //Wait until an output object is available
    repeat
      Sleep(1);
    until (FOutput <> nil) or (Terminated);

    //Quit the function if the thread has been terminated
    if Terminated then
      exit;

    //Calculate how many bytes have to be in the buffer to be in sync with the
    //output
    bufsize := round(AuBytesPerSecond(FParameters) / 1000 * FOutput.Delay);

    //Calculate how many bytes have to be read every visualisation step
    readsize := FSampleCount * Integer(AuBytesPerSample(FParameters));
    GetMem(mem, readsize);

    while not Terminated do
    begin
      wait := true;
      stamp_data := FBuffer.CurrentTag;
      stamp_output := FOutput.SyncData.Timecode;

      if (FBuffer.Filled > bufsize) or (stamp_data < stamp_output) then
      begin
        if stamp_data < stamp_output then
          wait := false;

        FCritSect.Enter;
        try
          s := FBuffer.Read(mem, readsize);
        finally
          FCritSect.Leave;
        end;

        FCallback(mem, s);
      end;

      if wait then
        Sleep(1);

      //! TODO: This has to be significantly improved:
      //  -- Pices have to be read after a certain delay - problem: Time functions
      //  are OS dependend. Problems when the stream is paused.
    end;

    FreeMem(mem, readsize);
  except
    //
  end;
end;

procedure TAuAnalyzeThread.WriteNextData(ABuf: PByte; ASize: Integer; ATimecode: Cardinal);
begin
  FCritSect.Enter;
  try
    FBuffer.Write(ABuf, ASize, ATimeCode);
  finally
    FCritSect.Leave;
  end;
end;

{ TAuCustomDecoderFilter }

constructor TAuCustomDecoderFilter.Create(AParameters: TAuAudioParameters);
begin
  inherited;

  FBuffer := TAcBuffer.Create;
  FCritSect := TCriticalSection.Create;
  FFirstFrame := true;
end;

destructor TAuCustomDecoderFilter.Destroy;
begin
  FBuffer.Free;
  FCritSect.Free;

  inherited;
end;

procedure TAuCustomDecoderFilter.FlushBuffer;
begin
  FFirstFrame := true;
  
  FCritSect.Enter;
  try
    FBuffer.Clear;
  finally
    FCritSect.Leave;
  end;
end;

procedure TAuCustomDecoderFilter.Unlock;
begin
  FLocked := false;
end;

procedure TAuCustomDecoderFilter.Lock;
begin
  FLocked := true;
end;

function TAuCustomDecoderFilter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
begin
  if not FLocked then
  begin
    FCritSect.Enter;
    try
      result := FBuffer.Read(ABuf, ASize);
    finally
      FCritSect.Leave;
    end;

    ASyncData.Timecode := FBuffer.CurrentTag;

    //Determine the current frame type
    if FFirstFrame then
    begin
      ASyncData.FrameType := auftBeginning;
      FFirstFrame := false;
    end;

    SyncDataCallback(ASyncData, result);
  end else
    result := 0;
end;

function TAuCustomDecoderFilter.AddSource(ACallback: TAuReadCallback): boolean;
begin
  //Connections to this block are not possible
  result := false;
end;

function TAuCustomDecoderFilter.RemoveSource(
  ACallback: TAuReadCallback): boolean;
begin
  //Connections to this block are not possible
  result := false;
end;

procedure TAuCustomDecoderFilter.SyncDataCallback(var ASyncData: TAuSyncData; AReadBytes: Integer);
begin
  //
end;

{ TAuCompressorFilter }

constructor TAuCompressorFilter.Create(AParameters: TAuAudioParameters);
begin
  inherited;

  FCompressionTarget := 1;
  FCurrentCompressionFactor := 1;
  FReleaseTime := 0.1;
end;

function TAuCompressorFilter.ReadCallback(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
var
  i: integer;
  smpls: Integer;
  ps: PSingle;
  v: Single;
begin
  result := 0;
  if Assigned(Callback) then
  begin
    //Read the output data
    result := Callback(ABuf, ASize, ASyncData);

    ps := PSingle(ABuf);

    smpls := result div AuBytesPerSample(Parameters);

    for i := 0 to (smpls * Integer(Parameters.Channels)) - 1 do
    begin
      if i mod Integer(Parameters.Channels) = 0 then
      begin
        //Try to reach the estimated compression target
        if FCurrentCompressionFactor < 1 then
          FCurrentCompressionFactor := FCurrentCompressionFactor + FFac;

        if FCurrentCompressionFactor > 1 then        
          FCurrentCompressionFactor := 1;
      end;
      
      v := ps^;
      ps^ := ps^ * FCurrentCompressionFactor;
      if abs(ps^) > FCompressionTarget then
      begin
        FCurrentCompressionFactor := FCompressionTarget / abs(v);
        ps^ := v * FCurrentCompressionFactor;
        FFac := (1 - FCurrentCompressionFactor) / (Parameters.Frequency * FReleaseTime);
      end;

      inc(ps);
    end;
  end;
end;

{ TAuOutputFilter }

constructor TAuOutputFilter.Create;
begin
  //The output filter does not perform an initialization of the audio parameters
  //at creation 
  inherited Create(AuAudioParameters(0, 0));
end;

procedure TAuOutputFilter.Init(const AParameters: TAuAudioParameters);
begin
  FParameters := AParameters;
end;

initialization
  fs := TFileStream.Create('C:\test.raw', fmCreate);

finalization
  FreeAndNil(fs);

end.
