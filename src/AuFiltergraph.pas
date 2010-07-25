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

File: AuFiltergraph.pas
Author: Andreas Stöckel
}

{Contains the audorra filter graph structure.}
unit AuFiltergraph;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs,
  AcSyncObjs, AcBuffer, AcSysUtils,
  AuDriverClasses, AuDecoderClasses, AuAnalyzerClasses,
  AuTypes, AuUtils, AuMessages;

type
  //! Move to own exception unit?
  EFilter = class(Exception);
  EFilterLocked = class(EFilter);
  EFilterRecursion = class(EFilter);
  ESyncData = class(Exception);

  TAuFrameInfoPlaybackState = (
    auisUndefined,
    auisBuffering,
    auisPlaying,
    auisFinished
  );

  TAuFrameInfo = record
    PlaybackState: TAuFrameInfoPlaybackState;
    MediaTimestamp: Cardinal;
    SampleTimestamp: Int64;
    Interpolated: Boolean;
  end;
  PAuFrameInfo = ^TAuFrameInfo;

  TAuSyncDataList = class
    private
      FList: TQueue;
    public
      constructor Create;
      destructor Destroy;override;

      procedure Clear;
      procedure AddFrame(const AFrameInfo: TAuFrameInfo);
      function GetFrame(ASample: Int64; out AFrameInfo: TAuFrameInfo): Boolean;
  end;

  TAuFilter = class;

  TAuFilterList = class(TList)
    private
      function GetFilter(AIndex: integer): TAuFilter;
    public
      property Items[AIndex: integer]: TAuFilter read GetFilter;default;
  end;

  TAuFilterState = (
    aufsFinalized,
    aufsInitialized,
    aufsSuspended
  );

  TAuFilter = class
    private
      FTarget: TAuFilter;
      FSources: TAuFilterList;
      FParameters: TAuAudioParameters;
      FState: TAuFilterState;
      FMutex: TAcMutex;
      procedure SetTarget(const AValue: TAuFilter);
    protected
      function DoAddSource(AFilter: TAuFilter): Boolean;virtual;abstract;
      function DoInit(const AParameters: TAuAudioParameters): Boolean;virtual;abstract;
      function DoCheckFilter: Boolean;virtual;abstract;
      procedure DoFinalize;virtual;abstract;
      procedure DoFlush;virtual;abstract;
       
      property Sources: TAuFilterList read FSources;
      property Mutex: TAcMutex read FMutex;
    public
      constructor Create;
      destructor Destroy;override;

      procedure BeforeDestruction;override;

      function Init(AParameters: TAuAudioParameters): Boolean;
      procedure Finalize;

      procedure FlushFiltergraph;
      procedure SuspendFiltergraph;
      function ResumeFiltergraph(AParameters: PAuAudioParameters = nil): Boolean;

      function GlobalTargetFilter: TAuFilter;
      procedure GlobalSourceFilters(ALst: TAuFilterList);

      function AddSource(AFilter: TAuFilter): Boolean;
      procedure RemoveSource(AFilter: TAuFilter);

      property Target: TAuFilter read FTarget write SetTarget;
      property State: TAuFilterState read FState;
      property Parameters: TAuAudioParameters read FParameters;
  end;              

  TAuSourceFilter = class(TAuFilter)
    protected
      function DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;virtual;abstract;
    public
      function ReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
  end;

  TAuOutputFilter = class(TAuFilter)
    protected
//      FDelay: Cardinal;
      
      function DoAddSource(AFilter: TAuFilter): Boolean;override;
      function DoCheckFilter: Boolean;override;

      function GetTimecode: Int64;virtual;
    public
      procedure Play;virtual;abstract;
      procedure Pause;virtual;abstract;
      procedure Stop;virtual;abstract;

      property Timecode: Int64 read GetTimecode;
//      property Delay: Cardinal read FDelay;
  end;

  TAuPassthroughFilter = class(TAuSourceFilter)
    protected
      function DoInit(const AParameters: TAuAudioParameters): Boolean;override;
      function DoCheckFilter: Boolean;override;
      procedure DoFinalize;override;
      procedure DoFlush;override;

      function DoAddSource(AFilter: TAuFilter): Boolean;override;
  end;

  TAuDriverOutput = class(TAuOutputFilter)
    private
      FDriver: TAuStreamDriver;
      FDriverParams: TAuAudioParametersEx;
      FBitDepth: Cardinal;      
      FBuf: PSingle;
      FBufSize: Cardinal;
      FPlaybackSample: Int64;
      FSampleOffset: Int64;
      FTimecodeCT: TAcCriticalSection;
      FLastTick: Double;
      procedure FreeBuf;
      function ReadCallback(ABuf: PByte; ASize: Cardinal;
        APlaybackSample: Int64): Cardinal;
    protected
      function GetTimecode: Int64;override;
      function DoInit(const AParameters: TAuAudioParameters): Boolean;override;
      procedure DoFinalize;override;
      procedure DoFlush;override;
    public
      constructor Create(ADriver: TAuStreamDriver; ABitDepth: Cardinal;
        ASampleOffset: Int64 = 0);
      destructor Destroy;override;

      procedure Play;override;
      procedure Pause;override;
      procedure Stop;override;

      property SampleOffset: Int64 read FSampleOffset;
  end;

  TAuDecoderThread = class(TThread)
    private
      FDecoder: TAuDecoder;
      FBuffer: TAcBuffer;
      FMutex: TAcMutex;
      FBufSize: Integer;
      FFinished: Boolean;
    protected
      procedure Execute;override;
    public
      constructor Create(ADecoder: TAuDecoder; ABuffer: TAcBuffer;
        AMutex: TAcMutex; ABufSize: integer);

      property Finished: Boolean read FFinished;
    end;

  TAuCustomDecoderFilter = class(TAuSourceFilter)
    protected
      function DoAddSource(AFilter: TAuFilter): Boolean;override;
      function DoCheckFilter: Boolean;override;
  end;

  TAuDecoderFilter = class(TAuCustomDecoderFilter)
    private
      FBuffer: TAcBuffer;
      FMutex: TAcMutex;
      FDecoder: TAuDecoder;
      FThread: TAuDecoderThread;
      FSample: Int64;
      FFrameInfoQueue: TAuSyncDataList;
      FLastTimecode: Int64;
      FIntpTimecode: Int64;
    protected
      function DoInit(const AParameters: TAuAudioParameters): Boolean;override;
      procedure DoFinalize;override;
      function DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;override;
      procedure DoFlush;override;
    public
      constructor Create;
      destructor Destroy;override;

      procedure SetDecoder(ADecoder: TAuDecoder);

      function GetFrameInfo(APlaybackSample: Int64;
        out AFrameInfo: TAuFrameInfo): Boolean;
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
      function DoReadCallback(ABuf: PSingle; ASize: Cardinal):Cardinal;override;
    public
      constructor Create;

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
      function DoReadCallback(ABuf: PSingle; ASize: Cardinal):Cardinal;override;
    public
      constructor Create;

      property CompressionTarget: Single read FCompressionTarget write FCompressionTarget;
      property ReleaseTime: Single read FReleaseTime write FReleaseTime;
  end;

  TAuAnalyzeCallback = procedure(ABuf: PByte; ASize: Cardinal) of object;

  TAuAnalyzeThread = class(TThread)
    private
      FSampleCount: integer;
      FParameters: TAuAudioParameters;
      FCallback: TAuAnalyzeCallback;
      FCritSect: TAcCriticalSection;
      FBuffer: TAcBuffer;
      FOutput: TAuOutputFilter;
      FReadSample: Int64;
      FOffs: Int64;
    protected
      procedure Execute;override;
    public
      constructor Create(ASampleCount: integer; AParameters: TAuAudioParameters;
        ACallback: TAuAnalyzeCallback; AOutputFilter: TAuOutputFilter);
      destructor Destroy;override;

      procedure WriteNextData(ABuf: PSingle; ASize: Integer);
      procedure Flush;

      property Output: TAuOutputFilter read FOutput write FOutput;
  end;

  TAuAnalyzeFilter = class(TAuPassthroughFilter)
    private
      FThread: TAuAnalyzeThread;
      FAnalyzers: TAuAnalyzerList;
      FCritSect: TAcCriticalSection;
      FBuf: PByte;
    protected
      function DoInit(const AParameters: TAuAudioParameters): Boolean;override;
      procedure DoFinalize;override;
      procedure DoFlush;override;
      function DoReadCallback(ABuf: PSingle; ASize: Cardinal):Cardinal;override;
      procedure AnalyzeData(ABuf: PByte; ASize: Cardinal);
    public
      constructor Create;
      destructor Destroy;override;

      property Analyzers: TAuAnalyzerList read FAnalyzers;
      property CriticalSection: TAcCriticalSection read FCritSect;
  end;

implementation

{ TAuFilterList }

function TAuFilterList.GetFilter(AIndex: integer): TAuFilter;
begin
  result := inherited Items[AIndex];
end;

{ TAuFilter }

constructor TAuFilter.Create;
begin
  inherited Create;

  FSources := TAuFilterList.Create;
  FState := aufsFinalized;
  FMutex := TAcMutex.Create;
end;

destructor TAuFilter.Destroy;
var
  i: integer;
begin
  //Finalize the filter before destroying it
  Finalize;
  
  //Remove all targets from the list
  for i := FSources.Count - 1 downto 0 do
    FSources[i].Target := nil;

  //Set own target to nil
  SetTarget(nil);
  
  FreeAndNil(FSources);

  FMutex.Free;
  
  inherited;
end;

procedure TAuFilter.BeforeDestruction;
begin
  //Force finalization before the filter is finally destroyed
  Finalize;

  inherited;
end;

function TAuFilter.Init(AParameters: TAuAudioParameters): Boolean;
var
  i: integer;
begin
  result := false;
  if FState = aufsFinalized then
  begin
    result := true;

    //Initialize the source filters before initializing this filter
    for i := 0 to FSources.Count - 1 do
      result := result and FSources[i].Init(AParameters);

    //If initializing the source filters was successful and
    if result and DoCheckFilter and DoInit(AParameters) then
    begin
      FParameters := AParameters;
      FState := aufsInitialized;

      result := true;
    end else
    begin
      //Initialization of this filter failed. Revert initialization
      Finalize;

      result := false;
    end;
  end;
end;

procedure TAuFilter.Finalize;
var
  i: integer;
begin
  if FState >= aufsInitialized then
  begin
    //If the filtergraph has been suspended, unlock the filtergraph by releasing the
    //mutex. DoFinalize has to care, that there will be no thread collisions.
    if FState = aufsSuspended then
      FMutex.Release;

    //Do the actual finalization
    DoFinalize;

    //Zero the parameters record
    FillChar(FParameters, SizeOf(FParameters), 0);

    //Set the filterstate to finalized
    FState := aufsFinalized;
  end;

  //Finalize the connected sources
  for i := 0 to FSources.Count - 1 do
    FSources[i].Finalize;
end;

procedure TAuFilter.FlushFiltergraph;
var
  i: Integer;
begin
  if FState >= aufsInitialized then
  begin
    Mutex.Acquire;
    try
      //Flush this filter
      DoFlush;

      //Flush all source filters
      for i := 0 to FSources.Count - 1 do
        FSources[i].FlushFiltergraph;
    finally
      Mutex.Release;
    end;
  end;  
end;

procedure TAuFilter.SuspendFiltergraph;
var
  i: integer;
begin
  if FState = aufsInitialized then
  begin
    FMutex.Acquire;

    FState := aufsSuspended;

    //Suspend the sources
    for i := 0 to FSources.Count - 1 do
      FSources[i].SuspendFiltergraph;
  end else
    raise EFilter.Create(MsgFilterSuspendImpossible);
end;

function TAuFilter.ResumeFiltergraph(AParameters: PAuAudioParameters = nil): Boolean;
var
  i: integer;
  setparams: Boolean;
  wfin: Boolean;
begin
  if FState in [aufsSuspended, aufsFinalized] then
  begin
    result := true;
    wfin := FState = aufsFinalized;
    try
      setparams := false;

      //If the filter is currently finalized, it needs the audio parameters for
      //initialization
      if wfin then
      begin
        //If AParameters is set, copy the parameters
        if (AParameters <> nil) then
          if AParameters^.Frequency <> 0 then
            FParameters := AParameters^
          else
            setparams := true;
      end else
      begin
        //Copy our own parameters back to the pointer if the given parameters
        //variable is uninitialized
        if (AParameters <> nil) and (AParameters^.Frequency = 0) then
          AParameters^ := FParameters;
      end;

      //Resume the sources
      for i := 0 to FSources.Count - 1 do
        result := result and FSources[i].ResumeFiltergraph(@FParameters);

      //Check whether the FParameters record is set properly
      if FParameters.Frequency = 0 then
        raise EFilter.Create(MsgFiltergraphGuessingParametersFailed);

      if setparams then
        AParameters^ := FParameters;

      if result and DoCheckFilter and
         ((FState = aufsSuspended) or DoInit(FParameters)) then
        FState := aufsInitialized
      else begin
        Finalize;
        result := false;
      end;
    finally
      if not wfin then
        FMutex.Release;
    end;             
  end else
    raise EFilter.Create(MsgFilterResumeImpossible);
end;  

function TAuFilter.AddSource(AFilter: TAuFilter): Boolean;
begin
  result := false;
  if FState <> aufsInitialized then
  begin
    FMutex.Acquire;
    try
      //Check whether another source can be added to the filter.
      if DoAddSource(AFilter) then
      begin
        //Add the filter to the sources and set the filter target properly
        FSources.Add(AFilter);
        AFilter.FTarget := self; //IMPORTANT: write on FTarget, not Target to prevent from infinite recursion

        result := true;
      end;
    finally
      FMutex.Release;
    end;
  end else
    raise EFilterLocked.Create(MsgFiltergraphLocked);
end;

procedure TAuFilter.RemoveSource(AFilter: TAuFilter);
begin
  if FState <> aufsInitialized then
  begin
    FMutex.Acquire;
    try
      //Try to remove the filter from the sources
      if FSources.Remove(AFilter) >= 0 then
        AFilter.FTarget := nil; //IMPORTANT: write on FTarget, not Target to prevent from infinite recursion
    finally
      FMutex.Release;
    end;
  end else
    raise EFilterLocked.Create(MsgFiltergraphLocked);
end;

procedure TAuFilter.GlobalSourceFilters(ALst: TAuFilterList);
var
  i: integer;
begin
  //Obtain the childrens of the filtergraph
  FMutex.Acquire;
  try
    if FSources.Count = 0 then
      ALst.Add(self)
    else
      for i := 0 to FSources.Count - 1 do
        FSources[i].GlobalSourceFilters(ALst);
  finally
    FMutex.Release;
  end;
end;

function TAuFilter.GlobalTargetFilter: TAuFilter;
begin
  //Obtain the end of the filtergraph
  FMutex.Acquire;
  try
    if FTarget = nil then
      result := self
    else
      result := FTarget.GlobalTargetFilter;
  finally
    FMutex.Release;
  end;
end;

procedure TAuFilter.SetTarget(const AValue: TAuFilter);
begin
  //Check the current filtergraph state
  if FState <> aufsInitialized then
  begin
    FMutex.Acquire;
    try
      //Recursion prevention
      if (AValue <> nil) and (AValue.GlobalTargetFilter = self) then
        raise EFilterRecursion.Create(MsgFiltergraphRecursion);

      //Disconnect from the current filter
      if FTarget <> nil then
        FTarget.RemoveSource(self);

      //Try to connect to the new filter
      if AValue <> nil then
        //FTarget will automatically be set
        if not AValue.AddSource(self) then
          raise EFilter.Create(MsgFiltergraphConnectionFailed);
    finally
      FMutex.Release;
    end;
  end else
    raise EFilterLocked.Create(MsgFiltergraphLocked);
end;

{ TAuOutputFilter }

function TAuOutputFilter.DoAddSource(AFilter: TAuFilter): Boolean;
begin
  //An output filter can only have one source and this source must be a
  //descendant of TAuSourceFilter
  result := (FSources.Count = 0) and (AFilter is TAuSourceFilter);
end;

function TAuOutputFilter.DoCheckFilter: Boolean;
begin
  result := (FSources.Count = 1); 
end;

function TAuOutputFilter.GetTimecode: Int64;
begin
  result := 0;
end;

{ TAuPassthroughFilter }

function TAuPassthroughFilter.DoAddSource(AFilter: TAuFilter): Boolean;
begin
  //An passthrough filter can only have one source and this source must be a
  //descendant of TAuSourceFilter
  result := (FSources.Count = 0) and (AFilter is TAuSourceFilter);
end;

function TAuPassthroughFilter.DoCheckFilter: Boolean;
begin
  //Initialization of a pass through filter can only be successfull if it has
  //at least one source
  result := FSources.Count = 1;
end;

procedure TAuPassthroughFilter.DoFinalize;
begin
  //
end;

procedure TAuPassthroughFilter.DoFlush;
begin
  //Do nothing here - normally passthrough filters don't have to flush anything.
end;

function TAuPassthroughFilter.DoInit(const AParameters: TAuAudioParameters): Boolean;
begin
  //Descendant classes should override this behaviour if a passthrough filter
  //might fail to initialize
  result := true;
end;

{ TAuDriverOutputFilter }

constructor TAuDriverOutput.Create(ADriver: TAuStreamDriver;
  ABitDepth: Cardinal; ASampleOffset: Int64 = 0);
begin
  inherited Create;

  FDriver := ADriver;
  FBitDepth := ABitDepth;
  FSampleOffset := ASampleOffset;
  FPlaybackSample := 0;
  FTimecodeCT := TAcCriticalSection.Create;
end;

destructor TAuDriverOutput.Destroy;
begin
  FTimecodeCT.Free;
  FreeBuf;
  inherited;
end;

procedure TAuDriverOutput.FreeBuf;
begin
  if FBuf <> nil then
    FreeMem(FBuf);

  FBuf := nil;
  FBufSize := 0;
end;

function TAuDriverOutput.GetTimecode: Int64;
var
  delta: Double;
begin
  FTimecodeCT.Enter;
  try
    result := FPlaybackSample + FSampleOffset;
    if FPlaybackSample > 0 then
    begin
      delta := AcGetTickCount - FLastTick;
      if delta > 0 then
        result := result + round(delta * (Parameters.Frequency / 1000));
    end;
  finally
    FTimecodeCT.Leave;
  end;
end;

procedure TAuDriverOutput.DoFinalize;
begin
  FDriver.Close;
end;

procedure TAuDriverOutput.DoFlush;
begin
  if FDriver <> nil then  
    FDriver.FlushBuffer;
end;

function TAuDriverOutput.DoInit(const AParameters: TAuAudioParameters): Boolean;
begin
  //Open the driver and retrieve the output bitdepth format
  result := FDriver.Open(AuDriverParameters(AParameters, FBitDepth), ReadCallback,
    FDriverParams.BitDepth);
  FDriverParams.Parameters := AParameters;
end;

procedure TAuDriverOutput.Pause;
begin
  FDriver.SetActive(false);
end;

procedure TAuDriverOutput.Play;
begin
  FDriver.SetActive(true);
end;

procedure TAuDriverOutput.Stop;
begin
  FTimecodeCT.Enter;
  try
    FPlaybackSample := 0;
    FSampleOffset := 0;
  finally
    FTimecodeCT.Leave;
  end;
  FDriver.FlushBuffer;
end;

function TAuDriverOutput.ReadCallback(ABuf: PByte; ASize: Cardinal;
  APlaybackSample: Int64): Cardinal;
begin
  result := 0;

  FMutex.Acquire;
  try
    if FSources.Count = 1 then
    begin
      FTimecodeCT.Enter;
      try
        FPlaybackSample := APlaybackSample;
        FLastTick := AcGetTickCount;
      finally
        FTimecodeCT.Leave;
      end;

      //Translate variable bit depth driver callback calls to 32-Bit floating value
      //callback calls for the filter graph
      FBufSize := AuConvertByteCount(ASize, FDriverParams,
        AuAudioParametersEx(FParameters, auFloat32Bit));
      ReallocMem(FBuf, FBufSize);

      //Do the actual read callback and store the frame data in the frame info queue
      result := TAuSourceFilter(FSources[0]).ReadCallback(FBuf, FBufSize);
      result := AuConvertByteCount(result, AuAudioParametersEx(FParameters, auFloat32Bit),
        FDriverParams);
      AuWriteSamples(FDriverParams, PByte(FBuf), ABuf, AuBytesToSamples(result, FDriverParams));
    end;
  finally
    FMutex.Release;
  end;
end;

{ TAuCustomDecoderFilter }

function TAuCustomDecoderFilter.DoAddSource(AFilter: TAuFilter): Boolean;
begin
  //As a decoder filter is always the first filter in a filtergraph, it cannot
  //have a own source
  result := false;
end;

function TAuCustomDecoderFilter.DoCheckFilter: Boolean;
begin
  result := (FSources.Count = 0);
end;

{ TAuDecoderThread }

constructor TAuDecoderThread.Create(ADecoder: TAuDecoder; ABuffer: TAcBuffer;
  AMutex: TAcMutex; ABufSize: integer);
begin
  inherited Create(false);

  //Copy the parameters
  FDecoder := ADecoder;
  FBuffer := ABuffer;
  FBufSize := ABufSize;
  FMutex := AMutex;
  FFinished := false;
end;

procedure TAuDecoderThread.Execute;
var
  pckt: TAuPacket;
  mem: PByte;
  smpls, bfsize: Integer;
  filled: Integer;
begin
  mem := nil;

  try
    while not Terminated do
    begin
      //Check whether the target buffer is filled
      FMutex.Acquire;
      try
        filled := FBuffer.filled;
      finally
        FMutex.Release;;
      end;

      if (filled < FBufSize) then
      begin
        //Tell the decoder to go on decoding
        case FDecoder.Decode of
          audsHasFrame:
          begin
            //Get the packet from the decoder
            FillChar(pckt, SizeOf(pckt), 0);
            FDecoder.GetPacket(pckt);

            //Recode the buffer to float values
            smpls := pckt.BufferSize div Integer(AuBytesPerSample(FDecoder.Info));
            bfsize := Integer(AuBytesPerSample(FDecoder.Info.Parameters)) * smpls;
            ReallocMem(mem, bfsize);

            AuReadSamples(FDecoder.Info, pckt.Buffer, mem, smpls);

            //Write the packet content into the buffer
            FMutex.Acquire;
            try
              FBuffer.Write(mem, bfsize, pckt.Timecode);
            finally
              FMutex.Release;
            end;
          end;
          //Terminate the thread if the decoder is at the end
          audsEnd:
          begin
            FMutex.Acquire;
            try
              FFinished := true;
            finally
              FMutex.Release;
            end;
          end;
        end;
        if FFinished then
          Sleep(1);
      end else
        Sleep(1);
    end;
  finally
    if mem <> nil then
      FreeMem(mem);
  end;
end;

{ TAuDecoderFilter }

constructor TAuDecoderFilter.Create;
begin
  inherited Create;
  FBuffer := TAcBuffer.Create;
  FMutex := TAcMutex.Create;
  FFrameInfoQueue := TAuSyncDataList.Create;
  FIntpTimecode := -1;
  FLastTimecode := -1;
end;

destructor TAuDecoderFilter.Destroy;
begin
  FFrameInfoQueue.Free;
  FMutex.Free;
  FBuffer.Free;
  inherited;
end;

procedure TAuDecoderFilter.DoFinalize;
begin
  //Terminate and free the thread
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
  
  FDecoder := nil;
end;

function TAuDecoderFilter.DoInit(const AParameters: TAuAudioParameters): Boolean;
var
  bufsize: Cardinal;
begin
  result := false;

  if FDecoder <> nil then
  begin
    result := true;

    //Create the buffer thread
    bufsize := AuBytesPerSecond(AParameters);
    FThread := TAuDecoderThread.Create(FDecoder, FBuffer, FMutex, bufsize);
  end;
end;

procedure TAuDecoderFilter.DoFlush;
begin
  FSample := 0;
  FIntpTimecode := -1;
  FLastTimecode := -1; 

  FMutex.Acquire;
  try
    FFrameInfoQueue.Clear;
    FBuffer.Clear;
  finally
    FMutex.Release;
  end;
end;

function TAuDecoderFilter.GetFrameInfo(APlaybackSample: Int64;
  out AFrameInfo: TAuFrameInfo): Boolean;
begin
  FMutex.Acquire;
  try
    result := FFrameInfoQueue.GetFrame(APlaybackSample, AFrameInfo);
  finally
    FMutex.Release;
  end;
end;

function TAuDecoderFilter.DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
var
  read: Cardinal;
  pdata: PByte;
  fi: TAuFrameInfo;
begin
  result := 0;
  
  FMutex.Acquire;
  try
    //Read the data from the buffer
    read := FBuffer.Read(PByte(ABuf), ASize);

    fi.MediaTimestamp := FBuffer.CurrentTag;
  finally
    FMutex.Release;
  end;

  //Fill the destination buff with zeros if not enough data is available
  //in the buffer. This prevents the output stream from looping
  if read < ASize then
  begin
    pdata := PByte(ABuf);
    inc(pdata, result);
    FillChar(pdata^, ASize - read, 0);
  end;

  //Increment the sample counter
  FSample := FSample + ASize div AuBytesPerSample(Parameters);

  //Fill the frame info record
  fi.SampleTimestamp := FSample;
  if (FThread.Finished) and (read = 0) then
    fi.PlaybackState := auisFinished
  else if (read = 0) then
    fi.PlaybackState := auisBuffering
  else
    fi.PlaybackState := auisPlaying;

  //Interpolate the timecodes - some decoders only provide a timecode for each raw data frame, which might last a few seconds
  if (Cardinal(FLastTimecode) = fi.MediaTimestamp) and (FIntpTimecode > -1) then
  begin
    fi.Interpolated := true;
    FIntpTimecode := FIntpTimecode +
      round(1000 * read / (AuBytesPerSample(FParameters) * FParameters.Frequency));
    fi.MediaTimestamp := FIntpTimecode;
  end else
  begin
    fi.Interpolated := false;
    FLastTimecode := fi.MediaTimestamp;
    FIntpTimecode := fi.MediaTimestamp;
  end;

  FMutex.Acquire;
  try
    FFrameInfoQueue.AddFrame(fi);
  finally
    FMutex.Release;
  end;

  result := ASize;
end;

procedure TAuDecoderFilter.SetDecoder(ADecoder: TAuDecoder);
begin
  if State = aufsFinalized then
  begin
    FDecoder := ADecoder;
  end else
    raise EFilterLocked.Create(MsgFiltergraphLocked);  
end;

{ TAuVolumeFilter }

constructor TAuVolumeFilter.Create;
begin
  inherited;

  Reset;
end;

function TAuVolumeFilter.DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
var
  i: integer;
  mem: PSingle;
begin
  result := TAuSourceFilter(FSources[0]).ReadCallback(ABuf, ASize);

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

{ TAuCompressorFilter }

constructor TAuCompressorFilter.Create;
begin
  inherited;

  FCompressionTarget := 1;
  FCurrentCompressionFactor := 1;
  FReleaseTime := 0.1;
end;

function TAuCompressorFilter.DoReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
var
  i: integer;
  smpls: Integer;
  ps: PSingle;
  v: Single;
begin
  //Read the output data  
  result := TAuSourceFilter(FSources[0]).ReadCallback(ABuf, ASize);

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

{ TAuSourceFilter }

function TAuSourceFilter.ReadCallback(ABuf: PSingle; ASize: Cardinal): Cardinal;
begin
  result := 0;
  
  Mutex.Acquire;
  try
    if (State = aufsInitialized) and (DoCheckFilter) then
      //Call the actual read callback function provided by descendant classes
      result := DoReadCallback(ABuf, ASize);
  finally
    Mutex.Release;
  end;
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

  FreeAndNil(FList);

  inherited;
end;

procedure TAuSyncDataList.Clear;
var
  i: integer;
begin
  //Destroy every item of the queue
  for i := FList.Count - 1 downto 0 do
    Dispose(PAuFrameInfo(FList.Pop));
end;

procedure TAuSyncDataList.AddFrame(const AFrameInfo: TAuFrameInfo);
var
  pfi: PAuFrameInfo;
begin
  //Check whether the frame info which will be added has a smaller sample timestamp
  //than the preceding one.
  if FList.Count > 0 then  
    pfi := FList.Peek
  else
    pfi := nil;

  if (pfi = nil) or (pfi^.SampleTimestamp <= AFrameInfo.SampleTimestamp) then
  begin
    //Create a new syncdata/timecode pair and add push it onto the list.
    New(pfi);
    pfi^ := AFrameInfo;
    FList.Push(pfi);
  end else
    raise ESyncData.Create(MsgSyncData);
end;

function TAuSyncDataList.GetFrame(ASample: Int64;
  out AFrameInfo: TAuFrameInfo): Boolean;
var
  pfi: PAuFrameInfo;
begin
  result := false;

  if FList.Count > 0 then
  begin
    //Have a glance at the next item in the queue...
    pfi := FList.Peek;
    repeat
      //If it's timecode is smaller then the timecode requested, return it.
      if ASample >= pfi^.SampleTimestamp then
      begin
        result := true;
        pfi := FList.Pop;
        AFrameInfo := pfi^;
        Dispose(pfi);
      end;

      //This procedure should be repeated until no item is left which has a
      //timecode greater than the timecode specified by ATimecode.
      if FList.Count > 0 then
        pfi := FList.Peek;
    until (FList.Count = 0) or (ASample < pfi^.SampleTimestamp);
  end;
end;

{ TAuAnalyzeFilter }

constructor TAuAnalyzeFilter.Create;
begin
  inherited;

  FAnalyzers := TAuAnalyzerList.Create;
  FCritSect := TAcCriticalSection.Create;
end;

destructor TAuAnalyzeFilter.Destroy;
begin
  FAnalyzers.Free;
  FCritSect.Free;

  if FBuf <> nil then
    FreeMem(FBuf);
  FBuf := nil;

  inherited;
end;

function TAuAnalyzeFilter.DoInit(const AParameters: TAuAudioParameters): Boolean;
begin
  result := GlobalTargetFilter is TAuOutputFilter;
  if result then  
    FThread := TAuAnalyzeThread.Create(128, AParameters, AnalyzeData,
      TAuOutputFilter(GlobalTargetFilter));
end;

procedure TAuAnalyzeFilter.DoFinalize;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  inherited;
end;

procedure TAuAnalyzeFilter.DoFlush;
begin
  inherited;
  if FThread <> nil then
    FThread.Flush;
end;

function TAuAnalyzeFilter.DoReadCallback(ABuf: PSingle; ASize: Cardinal):Cardinal;
begin
  //Read the output data
  result := TAuSourceFilter(FSources[0]).ReadCallback(ABuf, ASize);

  FThread.WriteNextData(ABuf, result);
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
  AParameters: TAuAudioParameters; ACallback: TAuAnalyzeCallback;
  AOutputFilter: TAuOutputFilter);
begin
  inherited Create(false);

  //Set thread parameters
  FSampleCount := ASampleCount;
  FParameters := AParameters;
  FCallback := ACallback;
  FOutput := AOutputFilter;

  //Create needed objects
  FCritSect := TAcCriticalSection.Create;
  FBuffer := TAcBuffer.Create;

  Flush;
end;

destructor TAuAnalyzeThread.Destroy;
begin
  FBuffer.Free;
  FCritSect.Free;

  inherited;
end;

procedure TAuAnalyzeThread.Execute;
var
  jumpsize: integer;
  readsize, s: integer;
  mem: PByte;
  tc: Int64;
  wait: boolean;
begin
  //Calculate how many bytes have to be read for every visualisation step
  readsize := FSampleCount * Integer(AuBytesPerSample(FParameters));
  jumpsize := 2 * Integer(FParameters.Frequency) * Integer(AuBytesPerSample(FParameters));
  GetMem(mem, readsize);

  try
    while not Terminated do
    begin
      s := 0;
      wait := true;

      tc := FOutput.Timecode;

      FCritSect.Enter;

      try
        tc := tc - FOffs;;
        //If readsample minus the timecode is greater than sample count, we have enough data to read again
        if ((tc - FReadSample) >= FSampleCount) then
        begin
          while FBuffer.Filled > jumpsize do
            FReadSample := FReadSample + FBuffer.Read(mem, readsize) div Integer(AuBytesPerSample(FParameters));

          s := FBuffer.Read(mem, readsize);
          FReadSample := FReadSample + s div Integer(AuBytesPerSample(FParameters));
          wait := false;
        end;
      finally
        FCritSect.Leave;
      end;

      if s > 0 then
        FCallback(mem, s);

      if wait then
        Sleep(1);
    end;

  finally
    FreeMem(mem, readsize);
  end;
end;

procedure TAuAnalyzeThread.Flush;
begin
  FCritSect.Enter;
  try
    FBuffer.Clear;

    //Reset the read sample position
    if FOutput is TAuDriverOutput then
      FOffs := TAuDriverOutput(FOutput).SampleOffset
    else
      FOffs := 0;

    FReadSample := 0;
  finally
    FCritSect.Leave;
  end;
end;

procedure TAuAnalyzeThread.WriteNextData(ABuf: PSingle; ASize: Integer);
begin
  FCritSect.Enter;
  try
    FBuffer.Write(PByte(ABuf), ASize);
  finally
    FCritSect.Leave;
  end;
end;

end.
