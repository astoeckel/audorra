{
  Audorra Digital Audio Library - 3D Rendering Library
  Copyright (C) 2009 Andreas Stöckel

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

File: Au3DAudioRenderer.pas
Author: Andreas Stöckel
}
{This file contains an efficient 3d audio software renderer which is capable
 of calculating phase/dopplereffect, absorption and more.}
unit Au3DAudioRenderer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, Math,
  AcMath, AcTypes, AcSyncObjs,
  AuTypes, AuUtils, AuAudioSpline, AuSyncUtils, Au3DRingBuffer;

const
  AU3DPROP_PHASE = $01;
  AU3DPROP_GAIN = $02;
  AU3DPROP_ALL = $FF;

type
  //Speaker setup presets
  TAu3DSpeakerPreset = (
    au3dssMono = 1,
    au3dssStereo = 2,
    au3dssQuadraphonic = 4,
    au3dss51 = 6,
    au3dss71 = 8
  );

  TAu3DChannelMatrix = array of array of Single;

  TAu3DSpeaker = (
    au3dspCenter = 0,
    au3dspFrontLeft = -1,
    au3dspFrontRight = 1,
    au3dspSideLeft = -2,
    au3dspSideRight = 2,
    au3dspRearLeft = -3,
    au3dspRearRight = 3,
    au3dspSubwoofer = 100
  );

  TAu3DSpeakerSetting = record
    angle: Single;
    speaker: TAu3DSpeaker;    
  end;

  TAu3DSpeakerSettings = array of TAu3DSpeakerSetting;

  TAu3DChannelMapper = class
    private
      FChannelMatrix: TAu3DChannelMatrix;
      FSpeakerSettings: TAu3DSpeakerSettings;
      FChannelLayout: array of TAu3DSpeaker;
      FOutCount: integer;
      procedure IntBuildMatrix;
    public
      constructor Create;
      destructor Destroy;override;

      procedure BuildMatrix(ASettings: TAu3DSpeakerSettings; AOutCount: integer);overload;
      procedure BuildMatrix(AInCount, AOutCount: integer);overload;

      procedure Map(AIn: PSingle; var AOut: PSingle; AClear: boolean);

      property OutCount: integer read FOutCount;
  end;

  TAu3DSpeakerSetup = class
    private
      FChannelCount: integer;
      FOutputChannelCount: integer;
      FSpeakers: TAu3DSpeakerSettings;
      FTableSize: integer;
      FSpeakerPreset: TAu3DSpeakerPreset;
      FSpeakerFactors: array of array of Extended;
      FMapper: TAu3DChannelMapper;
      procedure LoadPreset(APreset: TAu3DSpeakerPreset);
      procedure CalcFacTable(ACount: integer);
    public
      constructor Create(ASpeakerPreset: TAu3DSpeakerPreset);
      destructor Destroy;override;

      function MultFac(AChannel: integer; AAngle: Single): Single;
      
      property ChannelCount: integer read FChannelCount;
      property OutputChannelCount: integer read FOutputChannelCount;
      property Mapper: TAu3DChannelMapper read FMapper;
      property Speakers: TAu3DSpeakerSettings read FSpeakers;
  end;

  TAu3DEmitter = class;

  TAu3DEmitterList = class(TList)
    private
      function GetItem(AIndex: integer): TAu3DEmitter;
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      property Items[AIndex: integer]: TAu3DEmitter read GetItem; default;
  end;

  TAu3DSound = class
    private
      FAutoFree: Boolean;
      FEmitters: TAu3DEmitterList;
      FTimePosition: TAuSamplestamp;
      FBuf: PByte;
      FBufSize: Cardinal;
      FRing: TAu3DAudioRingBuffer;
      FPitch: Single;
      FParameters: TAuAudioParameters;
      FCallback: TAuReadCallback;
      FBufferSamples: Integer;
      FStreamed: Boolean;
      FLoop: boolean;
      FMapper: TAu3DChannelMapper;
      FActive: boolean;

      procedure SetPitch(AValue: Single);
      procedure SetLoop(AValue: Boolean);
      procedure SetActive(AValue: Boolean);
      
      procedure ReadSamples(ACount: Cardinal);inline;
      procedure GenericInit(AParameters: TAuAudioParameters; ASamples: Cardinal);
    public
      constructor Create(ACallback: TAuReadCallback;
        const AParameters: TAuAudioParameters; ABufferTime: Single = 5.0);overload;
      constructor Create(ABuf: PByte; ASamples: Cardinal;
        const AParameters: TAuAudioParameters);overload;

      destructor Destroy; override;

      procedure Move(ATimeGap: Extended);

      procedure ClearBuffers;

      property Parameters: TAuAudioParameters read FParameters;
      property Pitch: Single read FPitch write SetPitch;
      property AutoFree: Boolean read FAutoFree write FAutoFree;
      property RingBuffer: TAu3DAudioRingBuffer read FRing;
      property BufferSamples: Integer read FBufferSamples;
      property Emitters: TAu3DEmitterList read FEmitters;
      property TimePosition: TAuSamplestamp read FTimePosition;
      property Streamed: Boolean read FStreamed;
      property Loop: Boolean read FLoop write SetLoop;
      property Mapper: TAu3DChannelMapper read FMapper;
      property Active: Boolean read FActive write SetActive;
  end;

  TAu3DEmitterProc = procedure(AEmitter: TAu3DEmitter;
    ATimeGap: Double) of object;

  TAu3DDistanceModel = (
    au3ddmInverseDistance,
    au3ddmInverseDistanceClamped,
    au3ddmLinearDistance,
    au3ddmLinearDistanceClamped,
    au3ddmNone
  );

  TAu3DEmitter = class
    private
      FSound: TAu3DSound;
      FPosition: TAuVector3;
      FGlobalEmitter: boolean;
      FGain: Single;
      FUserData: Pointer;
      FMoveProc: TAu3DEmitterProc;
      FStopProc: TAuNotifyEvent;
      FAutoFree: Boolean;
      FRolloffFactor: Single;
      FMaxDistance: Single;
      FReferenceDistance: Single;
      FTimePosition: TAuSampleStamp;
      FTimeOffset: TAuSampleStamp;
      FProperties: Byte;
      FActive: boolean;
      FManualPositionChange: boolean;

      function GetGlobalEmitter: Boolean;
      procedure SetGain(AValue: Single);
      procedure SetRolloff(AValue: Single);
      procedure SetMaxDistance(AValue: Single);
      procedure SetReferenceDistance(AValue: Single);

      procedure StopProc;
    public
      constructor Create(ASound: TAu3DSound);
      destructor Destroy;override;

      procedure Move(ATimeGap: Extended);

      property Sound: TAu3DSound read FSound;
      property Position: TAuVector3 read FPosition write FPosition;
      property AutoFree: Boolean read FAutoFree write FAutoFree;
      property Gain: Single read FGain write SetGain;
      property GlobalEmitter: boolean read GetGlobalEmitter write FGlobalEmitter;
      property UserData: Pointer read FUserData write FUserData;
      property RolloffFactor: Single read FRolloffFactor write SetRolloff;
      property MaxDistance: Single read FMaxDistance write SetMaxDistance;
      property ReferenceDistance: Single read FReferenceDistance write SetReferenceDistance;
      property TimeOffset: TAuSampleStamp read FTimeOffset;
      property Properties: Byte read FProperties write FProperties;
      property Active: boolean read FActive write FActive;
      property ManualPositionChange: Boolean read FManualPositionChange;

      function TimePosition: TAuSampleStamp;
      procedure SetTimePosition(AVal: TAuSampleStamp);

      property OnMove: TAu3DEmitterProc read FMoveProc write FMoveProc;
      property OnStop: TAuNotifyEvent read FStopProc write FStopProc;
  end;

  TAu3DSoundList = class(TList)
    private
      function GetItem(AIndex: integer): TAu3DSound;
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      property Items[AIndex: integer]: TAu3DSound read GetItem; default;
  end;

  TAu3DGainValues = array of Single;

  TAu3DEmitterProps = record
    Source: Pointer;
    GainValues: TAu3DGainValues;
    Position: TAuSamplestamp;
    Used: boolean;
    Tag: Pointer;
  end;
  PAu3DEmitterProps = ^TAu3DEmitterProps;

  TAu3DEmitterPropsList = class
    private
      FList: TList;
    public
      constructor Create;
      destructor Destroy;override;
      
      procedure BeginScene;
      procedure EndScene;
      procedure GetSourceObj(ASource: Pointer; var AProps: PAu3DEmitterProps);
  end;

  TAu3DEnvironment = class
    private
      FScale: Single;
      FSpeedOfSound: Single;
      FDistanceModel: TAu3DDistanceModel;
      procedure SetScale(AValue: Single);
      procedure SetSpeedOfSound(AValue: Single);
    public
      constructor Create;
      destructor Destroy;override;

      function DistanceGainFactor(ADist, AMax, ARolloff, AReference: Single): Single;

      property Scale: Single read FScale write SetScale;
      property SpeedOfSound: Single read FSpeedOfSound write SetSpeedOfSound;
      property DistanceModel: TAu3DDistanceModel read FDistanceModel write FDistanceModel;
  end;

  TAu3DListener = class;

  TAu3DListenerProc = procedure(AListener: TAu3DListener; ATimeGap: Double);

  TAu3DListener = class
    private
      FPosition: TAuVector3;
      FOrientation: TAuOrientation;
      FGain: Single;
      FSources: TAu3DEmitterPropsList;
      FProperties: Byte;
      FViewMatrix: TAcMatrix;
      FMoveProc: TAu3DListenerProc;
    public
      constructor Create;
      destructor Destroy;override;

      procedure Setup3DScene(const APos, ADir, AUp: TAcVector3);
      procedure SetupView(const AMat: TAcMatrix);

      procedure Move(ATimeGap: Double);

      property Position: TAuVector3 read FPosition write FPosition;
      property Orientation: TAuOrientation read FOrientation write FOrientation;
      property Gain: Single read FGain write FGain;
      property Sources: TAu3DEmitterPropsList read FSources;
      property Properites: Byte read FProperties write FProperties;
      property ViewMatrix: TAcMatrix read FViewMatrix;

      property OnMove: TAu3DListenerProc read FMoveProc write FMoveProc;
  end;

  TAu3DSoundRenderer = class
    private
      FSpeakerSetup: TAu3DSpeakerSetup;
      FFrequency: Cardinal;
      FSounds: TAu3DSoundList;
      FEnvironment: TAu3DEnvironment;
      FOutvalues: TAu3DGainValues;
      FGainvalues: TAu3DGainValues;
      FMutex: TAcMutex;
      FWroteData: boolean;
      function CalculateSoundAngle(APos: TAcVector4;
        var AAlpha: Single): Boolean;
      procedure CalculatePositionalSoundData(AListener: TAu3DListener;
        ASampleCount: integer; ABuf: PByte; AEmitter: TAu3DEmitter; AClear: boolean;
        AObj: PAu3DEmitterProps);
      procedure CalculateStaticSoundData(AListener: TAu3DListener;
        ASampleCount: integer; ABuf: PByte; AEmitter: TAu3DEmitter; AClear: boolean; AObj: PAu3DEmitterProps);
    public
      constructor Create(ASpeakerPreset: TAu3DSpeakerPreset;
        AFrequency: Cardinal);
      destructor Destroy;override;
      
      procedure Render(AListener: TAu3DListener; ASampleCount: integer;
        ABuf: PByte);

      procedure Lock;
      procedure Unlock;

      property Sounds: TAu3DSoundList read FSounds;
      property Setup: TAu3DSpeakerSetup read FSpeakerSetup;
      property Environment: TAu3DEnvironment read FEnvironment;
      property Frequency: Cardinal read FFrequency;
  end;                                             
  

implementation

//Helper functions

function LinInt(av1, av2, ap: Single): Single;inline;
begin
  result := av1 * (1 - ap) + av2 * ap;
end;

function LinIntExt(av1, av2: Extended; ap: Single): Extended;inline;
begin
  result := av1 * (1 - ap) + av2 * ap;
end;

function PositiveAngle(aa: single): single;
begin
  result := aa;
  while result < 0 do
    result := result + 2 * PI;
end;

function AngleBetween(a1, a2: single): single;
begin
  result := abs(PositiveAngle(a1) - PositiveAngle(a2));
  if result > PI then
    result := result - PI;
end;

function NormAngle(aa: single): single;
begin
  result := aa;
  if result > 0 then
    while result > PI do
      result := result - 2 * PI
  else
    while result < -PI do
      result := result + 2 * PI;
end;

type
  TExtVec2 = record
    x, y: Extended;
  end;

function SolveMat(v1, v2, x: TExtVec2): TExtVec2;
var
  deta: Extended;
begin
  //By default return zero
  result.x := 0; result.y := 0;

  deta := v1.x * v2.y - v1.y * v2.x;
  if not IsZero(deta) then
  begin
    result.x := (x.x * v2.y - x.y * v2.x) / deta;
    result.y := (v1.x * x.y - v1.y * x.x) / deta;
  end;
end;

function Norm(v: TExtVec2): TExtVec2;
var
  l: Extended;
begin
  l := Sqrt(Sqr(v.x) + Sqr(v.y));
  if l > 0 then
  begin
    v.x := v.x / l;
    v.y := v.y / l;
  end;

  result := v;
end;

{ TAu3DSoundRenderer }

constructor TAu3DSoundRenderer.Create(ASpeakerPreset: TAu3DSpeakerPreset;
  AFrequency: Cardinal);
begin
  inherited Create;

  //Create the mutex used in the lock and unlock function.
  FMutex := TAcMutex.Create;

  FEnvironment := TAu3DEnvironment.Create;
  FSpeakerSetup := TAu3DSpeakerSetup.Create(ASpeakerPreset);
  FSounds := TAu3DSoundList.Create;
  FFrequency := AFrequency;


  SetLength(FOutvalues, FSpeakerSetup.ChannelCount);
  SetLength(FGainvalues, FSpeakerSetup.ChannelCount);
end;

destructor TAu3DSoundRenderer.Destroy;
begin
  FSounds.Free;
  FSpeakerSetup.Free;
  Environment.Free;

  //Free the mutex
  FMutex.Free;
  inherited;
end;

procedure TAu3DSoundRenderer.Lock;
begin
  FMutex.Acquire;
end;

procedure TAu3DSoundRenderer.Unlock;
begin
  FMutex.Release;
end;

procedure TAu3DSoundRenderer.Render(AListener: TAu3DListener;
  ASampleCount: integer; ABuf: PByte);
var
  i, j: integer;
  pobj: PAu3DEmitterProps;
begin
  FWroteData := false;

  //Exit in invalid sample count values
  if (ASampleCount <= 0) or (ABuf = nil) or (AListener = nil) then
    exit;
    
  Lock;
  try
    AListener.Sources.BeginScene;

    AListener.Move(ASampleCount / Frequency);

    for i := 0 to FSounds.Count - 1 do
    begin
      FSounds[i].Move(ASampleCount / FFrequency);

      for j := 0 to FSounds[i].Emitters.Count - 1 do
      begin
        if (FSounds[i].Emitters[j].Active) and
           (FSounds[i].Streamed or FSounds[i].Loop or
            (FSounds[i].Emitters[j].TimePosition shr 16 <= FSounds[i].BufferSamples)
           ) then
        begin
          //Get the listener information attached to the sound
          AListener.Sources.GetSourceObj(FSounds[i].Emitters[j], pobj);

          FSounds[i].Emitters[j].Move(ASampleCount / FFrequency);
          
          //Do the actual rendering
          if not FSounds[i].Emitters[j].GlobalEmitter then
            CalculatePositionalSoundData(AListener, ASampleCount, ABuf,
              FSounds[i].Emitters[j], not FWroteData, pobj)
          else
            CalculateStaticSoundData(AListener, ASampleCount, ABuf,
              FSounds[i].Emitters[j], not FWroteData, pobj);
        end;
      end;
    end;

    //If no emitter has written any data, zero the buffer memory
    if not FWroteData then
      FillChar(ABuf^, FSpeakerSetup.OutputChannelCount * SizeOf(Single) *
        ASampleCount, 0);

    AListener.Sources.EndScene;
  finally
    Unlock;
  end;
end;             

function TAu3DSoundRenderer.CalculateSoundAngle(APos: TAcVector4;
  var AAlpha: Single): boolean;
var
  l: Double;
begin
  result := false;
  
  l := Sqrt(Sqr(APos.x) + Sqr(APos.y));
  if not IsZero(l) then
  begin
    AAlpha := ArcCos(APos.x / l);
    if APos.y < 0 then
      AAlpha := 2 * PI - AAlpha;
    result := true;
  end;
end;

procedure TAu3DSoundRenderer.CalculateStaticSoundData(AListener: TAu3DListener;
  ASampleCount: integer; ABuf: PByte; AEmitter: TAu3DEmitter;
  AClear: boolean; AObj: PAu3DEmitterProps);
var
  j, k: integer;
  ip: Single;
  gain: Single;
  pos: Int64;
  posadd: Int64;
  props: Byte;
  outvals: TAu3DGainValues;
  ps: PSingle;
begin
  ps := PSingle(ABuf);
  SetLength(outvals, AEmitter.Sound.Parameters.Channels);
  
  props := AEmitter.Properties and AListener.Properites;

  //Initialize the channel mapper
  if AEmitter.Sound.Mapper.OutCount = 0 then
    AEmitter.Sound.Mapper.BuildMatrix(
      AEmitter.Sound.Parameters.Channels,
      FSpeakerSetup.OutputChannelCount);

  if props and AU3DPROP_GAIN > 0 then
    gain := AEmitter.Gain * AListener.Gain
  else
    gain := 1;

  if not Assigned(AObj^.GainValues) then
  begin
    SetLength(AObj^.GainValues, 1);
    AObj^.GainValues[0] := gain;
  end;

  posadd := (AEmitter.TimePosition - AObj^.Position) div ASampleCount;
  pos := AObj^.Position - AEmitter.TimeOffset;

  FWroteData := true;

  for j := 0 to ASampleCount - 1 do
  begin
    ip := j / ASampleCount;

    //Read a sample from the sound buffer
    for k := 0 to AEmitter.Sound.Parameters.Channels - 1 do
      outvals[k] := AEmitter.Sound.RingBuffer.GetSample(pos, k) *
        LinInt(AObj^.GainValues[0], gain, ip);

    AEmitter.Sound.Mapper.Map(@outvals[0], PSingle(ps), AClear);

    pos := pos + posadd;
  end;

  AObj^.GainValues[0] := gain;
  AObj^.Position := AEmitter.TimePosition;
end;

procedure TAu3DSoundRenderer.CalculatePositionalSoundData(
  AListener: TAu3DListener; ASampleCount: integer; ABuf: PByte;
  AEmitter: TAu3DEmitter; AClear: boolean; AObj: PAu3DEmitterProps);
var
  dist: Single;
  smpl: Single;
  gain: Single;
  hasalpha: Boolean;
  alpha: Single;
  pos, posd: Int64;
  posadd: Int64;
  k, j: integer;
  ip: Single;
  ps: PByte;
  props: Byte;
  pos4: TAcVector4;
begin
  ps := ABuf;

  props := AEmitter.Properties and AListener.Properites;
  dist := 0;

  //Get the position vector
  pos4 := AcMatrix_Multiply_Vector(AListener.ViewMatrix,
    AcVector4(AEmitter.Position, 1));

  //Calculate the distance towards the listener
  if (props and (AU3DPROP_PHASE or AU3DPROP_GAIN) > 0) then
  begin
    //Calculate the distance
    dist := Sqrt(
      Sqr(pos4.x) +
      Sqr(pos4.y) +
      Sqr(pos4.z));
  end;

  //Calculate the gain values
  if props and AU3DPROP_GAIN > 0 then
  begin
    gain := AEmitter.Gain * AListener.Gain *
      FEnvironment.DistanceGainFactor(
        dist * FEnvironment.Scale, AEmitter.MaxDistance, AEmitter.RolloffFactor,
        AEmitter.ReferenceDistance);
    hasalpha := CalculateSoundAngle(pos4, alpha);
  end else
  begin
    gain := 1;
    hasalpha := false;
  end;

  //Read the channel factors
  for k := 0 to FSpeakerSetup.ChannelCount - 1 do
    if hasalpha then
      FGainvalues[k] := FSpeakerSetup.MultFac(k, alpha) * gain
    else
      FGainvalues[k] := gain;

  if not Assigned(AObj^.GainValues) then
    AObj^.GainValues := Copy(FGainvalues);

  //Phase/Dopplereffect/Time calculation
  if props and AU3DPROP_PHASE > 0 then
    posd := AEmitter.TimePosition -
      round((dist * FEnvironment.Scale) / FEnvironment.SpeedOfSound
        * AEmitter.Sound.Parameters.Frequency * (1 shl 16))
  else
    posd := AEmitter.TimePosition;

  FWroteData := true;

  posadd := (posd - AObj^.Position) div ASampleCount;
  pos := AObj^.Position - AEmitter.TimeOffset;
  for j := 0 to ASampleCount - 1 do
  begin
    ip := j / ASampleCount;

    //Read a sample from the sound buffer
    smpl := AEmitter.Sound.RingBuffer.GetSample(pos, 0);

    //Calculate the current sample value and write it to the output buffer
    for k := 0 to FSpeakerSetup.ChannelCount - 1 do
      //Mix the new sample value with the old buffer content
      FOutvalues[k] :=
        LinInt(AObj^.GainValues[k], FGainvalues[k], ip) * smpl;

    FSpeakerSetup.Mapper.Map(@FOutvalues[0], PSingle(ps), AClear);

    pos := pos + posadd;
  end;

  for k := 0 to FSpeakerSetup.ChannelCount - 1 do
    AObj^.GainValues[k] := FGainvalues[k];
  AObj^.Position := posd;
end;

{ TAu3DSpeakerSetup }

constructor TAu3DSpeakerSetup.Create(ASpeakerPreset: TAu3DSpeakerPreset);
begin
  inherited Create;

  FMapper := TAu3DChannelMapper.Create();
  FSpeakerPreset := ASpeakerPreset;
  LoadPreset(ASpeakerPreset);

  FTableSize := 4 * 360;
  SetLength(FSpeakerFactors, FChannelCount);
  CalcFacTable(FTableSize);
end;

destructor TAu3DSpeakerSetup.Destroy;
begin
  FMapper.Free;
  inherited;
end;

procedure TAu3DSpeakerSetup.LoadPreset(APreset: TAu3DSpeakerPreset);
begin
  case FSpeakerPreset of
    au3dssMono, au3dssStereo, au3dssQuadraphonic:
    begin
      FChannelCount := 4;
      SetLength(FSpeakers, FChannelCount);
      FSpeakers[0].angle := -135 * PI / 180;
      FSpeakers[0].speaker := au3dspSideLeft;
      FSpeakers[1].angle :=  -45 * PI / 180;
      FSpeakers[1].speaker := au3dspFrontLeft;
      FSpeakers[2].angle :=   45 * PI / 180;
      FSpeakers[2].speaker := au3dspFrontRight;
      FSpeakers[3].angle :=  135 * PI / 180;
      FSpeakers[3].speaker := au3dspSideRight;
    end;

    au3dss51:
    begin
      FChannelCount := 5;
      SetLength(FSpeakers, FChannelCount);
      FSpeakers[0].angle := -110 * PI / 180;
      FSpeakers[0].speaker := au3dspSideLeft;
      FSpeakers[1].angle :=  -30 * PI / 180;
      FSpeakers[1].speaker := au3dspFrontLeft;
      FSpeakers[2].angle :=    0 * PI / 180;
      FSpeakers[2].speaker := au3dspCenter;
      FSpeakers[3].angle :=   30 * PI / 180;
      FSpeakers[3].speaker := au3dspFrontRight;
      FSpeakers[4].angle :=  110 * PI / 180;
      FSpeakers[4].speaker := au3dspSideRight;
    end;

    au3dss71:
    begin
      FChannelCount := 7;
      SetLength(FSpeakers, FChannelCount);
      FSpeakers[0].angle := -150  * PI / 180;
      FSpeakers[0].speaker := au3dspRearLeft;
      FSpeakers[1].angle :=  -90  * PI / 180;
      FSpeakers[1].speaker := au3dspSideLeft;
      FSpeakers[2].angle :=  -30  * PI / 180;
      FSpeakers[2].speaker := au3dspFrontLeft;
      FSpeakers[3].angle :=    0  * PI / 180;
      FSpeakers[3].speaker := au3dspCenter;
      FSpeakers[4].angle :=   30  * PI / 180;
      FSpeakers[4].speaker := au3dspFrontRight;
      FSpeakers[5].angle :=   90  * PI / 180;
      FSpeakers[5].speaker := au3dspSideRight;
      FSpeakers[6].angle :=  150  * PI / 180;
      FSpeakers[6].speaker := au3dspRearRight;
    end;
  end;

  FOutputChannelCount := Ord(FSpeakerPreset);       
  FMapper.BuildMatrix(FSpeakers, FOutputChannelCount);
end;

procedure TAu3DSpeakerSetup.CalcFacTable(ACount: integer);
var
  v1, r1: TExtVec2;
  l1, l2: TExtVec2;
  step, sum: Double;
  i, j: Integer;
  s1, s2: Integer;
begin
  for i := 0 to FChannelCount - 1 do
    SetLength(FSpeakerFactors[i], ACount);

  //Zero all values
  for i := 0 to FChannelCount - 1 do
    for j := 0 to ACount - 1 do
      FSpeakerFactors[i][j] := 0;

  step := 2 * PI / ACount;
  for i := 0 to ACount - 1 do
  begin
    //Calculate the sound vector
    SinCos(i * step + PI/2, v1.y, v1.x);

    for j := 0 to FChannelCount - 1 do
    begin
      //Calculate the two speakers to select for base translation
      s1 := j;
      s2 := (j + 1) mod FChannelCount;

      //Obtain the speaker 1 vector
      SinCos(FSpeakers[s1].angle, l1.y, l1.x);

      //Obtain the speaker 2 vector
      SinCos(FSpeakers[s2].angle, l2.y, l2.x);

      //Treat l1 and l2 as a new vector room basis and translate v1 to this
      //vector room.
      //      / r \
      //v1' = |   |  = r1
      //      \ s /
      //
      //l1 * r + l2 * s = v1

      r1 := SolveMat(l1, l2, v1);

      //Normalize the output vector
      r1 := Norm(r1);

      if r1.x > 0 then
        FSpeakerFactors[s1][i] := FSpeakerFactors[s1][i] + r1.x;

      if r1.y > 0 then
        FSpeakerFactors[s2][i] := FSpeakerFactors[s2][i] + r1.y;
    end;
  end;

  //Normalize the output values
  for i := 0 to ACount - 1 do
  begin
    sum := 0;
    for j := 0 to FChannelCount - 1 do
      sum := sum + FSpeakerFactors[j][i];
    sum := 1 / sum;
    for j := 0 to FChannelCount - 1 do
      FSpeakerFactors[j][i] := FSpeakerFactors[j][i] * sum;
  end;
end;

function TAu3DSpeakerSetup.MultFac(AChannel: integer; AAngle: Single): Single;
begin
  result := FSpeakerFactors[AChannel][round(FTableSize * (AAngle / (2 * PI)))];
end;

{ TAu3DSoundList }

function TAu3DSoundList.GetItem(AIndex: integer): TAu3DSound;
begin
  result := inherited Items[AIndex];
end;

procedure TAu3DSoundList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and TAu3DSound(Ptr).AutoFree then
    TAu3DSound(Ptr).Free;
end;

{ TAu3DChannelMapper }

constructor TAu3DChannelMapper.Create;
begin
  inherited Create;

  FOutCount := 0;

  //Write the channel output configuration mask according to ANSI/CEA-863-A,
  //see http://en.wikipedia.org/wiki/Surround_sound#Channel_identification
  SetLength(FChannelLayout, 8);
  FChannelLayout[0] := au3dspFrontLeft;
  FChannelLayout[1] := au3dspFrontRight;
  FChannelLayout[2] := au3dspCenter;
  FChannelLayout[3] := au3dspSubwoofer;
  FChannelLayout[4] := au3dspSideLeft;
  FChannelLayout[5] := au3dspSideRight;
  FChannelLayout[6] := au3dspRearLeft;
  FChannelLayout[7] := au3dspRearRight;
end;

type
  TIntegerArray = array of Integer;

procedure TAu3DChannelMapper.IntBuildMatrix;

  //Function used to map a virtual input speaker (AIn) to the real output speakers (ARes).
  //The array ARes contains the actual real stream identification numbers
  //This function is used if there are less or equal virtual input speakers than
  //output speakers
  procedure GetOutputSpeaker1(AIn: TAu3DSpeaker; var ARes: TIntegerArray);
  var
    i: integer;
    best: Integer;
    delta: Integer;
  begin
    best := 0;

    //Determine the best distance value
    for i := 0 to High(FSpeakerSettings) do
    begin
      delta := Abs(Ord(AIn) - Ord(FSpeakerSettings[i].speaker));
      if (i = 0) or (delta < best) then
        best := delta;
    end;

    //Add all speakers with this distance value to the output array
    SetLength(ARes, 0);
    for i := 0 to High(FSpeakerSettings) do
    begin
      delta := Abs(Ord(AIn) - Ord(FSpeakerSettings[i].speaker));
      if delta = best then
      begin
        SetLength(ARes, Length(ARes) + 1);
        ARes[High(ARes)] := i;
      end;
    end;
  end;

  //Function used to map a virtual input speaker (AIn) to the real output speakers (ARes).
  //The array ARes contains the actual real stream identification numbers
  //This function is used if there are more virtual input speakers than
  //output speakers
  procedure GetOutputSpeaker2(AIn: TAu3DSpeaker; var ARes: TIntegerArray);
  var
    i: integer;
    best: Integer;
    delta : Integer;
  begin
    best := trunc(Length(FSpeakerSettings) / FOutCount);

    //Determine the best speakers
    SetLength(ARes, 0);
    for i := 0 to High(FSpeakerSettings) do
    begin
      delta := Abs(Ord(AIn) - Ord(FSpeakerSettings[i].speaker));
      if (delta < best) then
      begin
        SetLength(ARes, Length(ARes) + 1);
        ARes[High(ARes)] := i;
      end;
    end;
  end;

var
  i, j: integer;
  res: TIntegerArray;
begin
  //Create and initialize the channel matrix
  SetLength(FChannelMatrix, FOutCount, Length(FSpeakerSettings));
  for i := 0 to FOutCount - 1 do
    for j := 0 to High(FSpeakerSettings) do
      FChannelMatrix[i][j] := 0;

  //Pick every output speaker
  for i := 0 to FOutCount - 1 do
  begin
    //If the output speaker isn't a subwoofer...
    if (FChannelLayout[i] <> au3dspSubwoofer) then
    begin
      //...pick all input channels which contribute to the selected output speaker
      if FOutCount >= Length(FSpeakerSettings) then
        GetOutputSpeaker1(FChannelLayout[i], res)
      else
        GetOutputSpeaker2(FChannelLayout[i], res);

      //and set this value in the channel matrix
      for j := 0 to High(res) do
        FChannelMatrix[i][res[j]] := 1 / Length(res);
        //FChannelMatrix[i][res[j]] := 1;
    end else
      //Every input channel contributes to the subwoofer output channel -
      //a low pass filter will be applied lateron
      for j := 0 to High(FSpeakerSettings) do
        //Setting this to one should be correct, as the sum of all channels is
        //normalized to one
        FChannelMatrix[i][j] := 1;
  end;  
end;

procedure TAu3DChannelMapper.BuildMatrix(ASettings: TAu3DSpeakerSettings;
  AOutCount: integer);  
begin
  FSpeakerSettings := Copy(ASettings);
  FOutCount := AOutCount;

  IntBuildMatrix;
end;

procedure TAu3DChannelMapper.BuildMatrix(AInCount, AOutCount: integer);
var
  i: integer;
begin
  FOutCount := AOutCount;

  //Initialize the speakers settings vector manually
  SetLength(FSpeakerSettings, AInCount);
  for i := 0 to AInCount - 1 do
  begin
    FSpeakerSettings[i].speaker := FChannelLayout[i];
    FSpeakerSettings[i].angle := 0; //actually "angle" is not used in this class
  end;

  IntBuildMatrix;
end;

procedure TAu3DChannelMapper.Map(AIn: PSingle; var AOut: PSingle; AClear: boolean);
var
  i, j: integer;
  psin: PSingle;
begin
  for i := 0 to FOutCount - 1 do
  begin
    if AClear then
      AOut^ := 0;

    psin := AIn;
    for j := 0 to Length(FSpeakerSettings) - 1 do
    begin
      AOut^ := AOut^ + psin^ * FChannelMatrix[i][j];
      inc(psin);
    end;
    inc(AOut);
  end;
end;

destructor TAu3DChannelMapper.Destroy;
begin

  inherited;
end;

{ TAu3DSoundSourcePropsList }

constructor TAu3DEmitterPropsList.Create;
begin
  inherited Create;

  FList := TList.Create;
end;

destructor TAu3DEmitterPropsList.Destroy;
var
  pobj: PAu3DEmitterProps;
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    pobj := FList[i];
    Dispose(pobj);
    FList.Delete(i);
  end;
  
  FList.Free;
  inherited;
end;

procedure TAu3DEmitterPropsList.BeginScene;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    PAu3DEmitterProps(FList[i])^.Used := false;
end;

procedure TAu3DEmitterPropsList.EndScene;
var
  pobj: PAu3DEmitterProps;
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    pobj := FList[i];

    //Reset the manual position change value
    TAu3DEmitter(pobj^.Source).FManualPositionChange := false;

    if not pobj^.Used then
    begin
      Dispose(pobj);
      FList.Delete(i);
    end;
  end;
end;

procedure TAu3DEmitterPropsList.GetSourceObj(ASource: Pointer;
  var AProps: PAu3DEmitterProps);
var
  pobj: PAu3DEmitterProps;
  i: integer;
begin
  AProps := nil;

  for i := 0 to FList.Count - 1 do
  begin
    pobj := FList[i];
    if pobj^.Source = ASource then
    begin
      AProps := pobj;
      pobj^.Used := true;
      break;
    end;
  end;

  //Remove the emitter object from the list if the position has been changed
  //manually.
  if TAu3DEmitter(ASource).ManualPositionChange and (AProps <> nil) then
  begin
    FList.Remove(AProps);
    AProps := nil;
  end;

  if (AProps = nil) then
  begin
    New(pobj);
    pobj^.Source := ASource;
    pobj^.Used := true;
    pobj^.Position := TAu3DEmitter(ASource).TimePosition;
    FList.Add(pobj);
    AProps := pobj;
  end;
end;

{ TAu3DSoundListener }

constructor TAu3DListener.Create;
begin
  inherited Create;

  FSources := TAu3DEmitterPropsList.Create;
  FGain := 1;
  FProperties := AU3DPROP_ALL;
  FViewMatrix := AcMatrix_Identity;
end;

destructor TAu3DListener.Destroy;
begin
  FSources.Free;
  inherited;
end;

procedure TAu3DListener.Move(ATimeGap: Double);
begin
  if Assigned(FMoveProc) then
    FMoveProc(self, ATimeGap);
end;

procedure TAu3DListener.Setup3DScene(const APos, ADir, AUp: TAcVector3);
begin
  FViewMatrix := AcMatrix_View_LookAt(APos, ADir, AUp);
end;

procedure TAu3DListener.SetupView(const AMat: TAcMatrix);
begin
  FViewMatrix := AMat;
end;

{ TAu3DEnvironment }

constructor TAu3DEnvironment.Create;
begin
  inherited Create;

  FScale := 1;
  FSpeedOfSound := 343.3;
  FDistanceModel := au3ddmInverseDistanceClamped;
end;

destructor TAu3DEnvironment.Destroy;
begin

  inherited;
end;

//see http://wiki.delphigl.com/index.php/alDistanceModel for mor details
function TAu3DEnvironment.DistanceGainFactor(ADist, AMax, ARolloff,
  AReference: Single): Single;
var
  divisor: Single;
begin
  result := 1.0;
  
  case FDistanceModel of
    au3ddmInverseDistance:
    begin
      //The divisor must not get zero
      divisor := (AReference + ARolloff * (ADist - AReference));
      if divisor < 0.00001 then
        divisor := 0.00001;

      result := AReference / divisor;
    end;

    au3ddmInverseDistanceClamped:
    begin
      if ADist < AReference then
        ADist := AReference;
      if ADist > AMax then
        ADist := AMax;

      result := AReference / (AReference + ARolloff * (ADist - AReference));
    end;

    au3ddmLinearDistance:
    begin
      if ADist > AMax then
        ADist := AMax;

      result := (1 - ARolloff * (ADist - AReference) / (AMax - AReference));
    end;

    au3ddmLinearDistanceClamped:
    begin
      if ADist < AReference then
        ADist := AReference;
      if ADist > AMax then
        ADist := AMax;

      result := (1 - ARolloff * (ADist - AReference) / (AMax - AReference));
    end;
  end;
end;

procedure TAu3DEnvironment.SetScale(AValue: Single);
begin
  if AValue > 0 then
    FScale := AValue;
end;

procedure TAu3DEnvironment.SetSpeedOfSound(AValue: Single);
begin
  if AValue > 0 then
    FSpeedOfSound := AValue;
end;

{ TAu3DSound }

procedure TAu3DSound.GenericInit(AParameters: TAuAudioParameters;
  ASamples: Cardinal);
begin
  FParameters := AParameters;

  //Create the ring buffer
  FRing := TAu3DAudioRingBuffer.Create(ASamples, FParameters.Channels);

  //Create the emitter list
  FEmitters := TAu3DEmitterList.Create;

  //Create a channel mapper
  FMapper := TAu3DChannelMapper.Create;

  //Preset some parameters
  FPitch := 1.0;
  FTimePosition := 0;
  FBufferSamples := ASamples;
  FBuf := nil;
  FBufSize := 0;
  FAutoFree := true;
  FActive := true;
end;

constructor TAu3DSound.Create(ACallback: TAuReadCallback;
  const AParameters: TAuAudioParameters; ABufferTime: Single = 5.0);
begin
  inherited Create;

  FLoop := false;

  //Initialize the class
  GenericInit(AParameters, Trunc(AParameters.Frequency * ABufferTime));
  FCallback := ACallback;

  FStreamed := true;
end;

constructor TAu3DSound.Create(ABuf: PByte; ASamples: Cardinal;
  const AParameters: TAuAudioParameters);
begin
  inherited Create;

  FLoop := false;

  //Initialize the class
  GenericInit(AParameters, ASamples);
  FCallback := nil;

  //Write the sound data into the
  FRing.WriteSamples(ASamples, PSingle(ABuf));

  FStreamed := false;
end;

destructor TAu3DSound.Destroy;
begin
  //Free the temporary audio data buffer
  if FBuf <> nil then
    FreeMem(FBuf, FBufSize);
  FBuf := nil;
  FBufSize := 0;

  //Free the emitters...
  FEmitters.Free;

  //...and the ring buffer
  FRing.Free;

  FMapper.Free;

  inherited;
end;

procedure TAu3DSound.ReadSamples(ACount: Cardinal);
var
  size: Cardinal;
  sd: TAuSyncData;
  smpls: Integer;
begin
  //Reserve memory for the reservation
  size := AuBytesPerSample(FParameters) * ACount;
  if (size <> FBufSize) or (FBuf = nil) then
    ReallocMem(FBuf, size);
  FBufSize := size;

  smpls := FCallback(FBuf, size, sd) div AuBytesPerSample(FParameters);
  FRing.WriteSamples(smpls, PSingle(FBuf));
end;

procedure TAu3DSound.Move(ATimeGap: Extended);
var
  c: integer;
begin
  if not FActive then
    exit;
    
  if FStreamed then
  begin
    //Check whether new samples have to be read into the sound buffer
    if (RingBuffer.Filled < RingBuffer.Size) or
      ((RingBuffer.SmplPos - FTimePosition shr 16) <
        RingBuffer.SmplSize / 2) then
    begin
      //Calculate the count of samples which have to be read into the ring buffer
      c := round(ATimeGap * Parameters.Frequency * FPitch * 2);
      ReadSamples(c);
    end;

    //Advance the sound position
    FTimePosition := FTimePosition +
      round((ATimeGap * FPitch * Parameters.Frequency * (1 shl 16)));
  end;
end;

procedure TAu3DSound.ClearBuffers;
var
  i: integer;
begin
  //Clear the buffer ring
  FRing.Clear;

  //Clear the own time position index
  FTimePosition := 0;

  //Clear the emitter time position index
  for i := 0 to FEmitters.Count - 1 do
    FEmitters[i].SetTimePosition(0);    
end;

procedure TAu3DSound.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  FRing.Locked := not AValue;
end;

procedure TAu3DSound.SetLoop(AValue: Boolean);
begin
  FLoop := AValue;
  FRing.Loop := FLoop;
end;

procedure TAu3DSound.SetPitch(AValue: Single);
begin
  //Only apply the pitch settings if AValue is greater zero. Other values are
  //not allowed.
  if AValue > 0 then
    FPitch := AValue;
end;

{ TAu3DEmitterList }

function TAu3DEmitterList.GetItem(AIndex: integer): TAu3DEmitter;
begin
  result := inherited Items[AIndex];
end;

procedure TAu3DEmitterList.Notify(ptr: Pointer; action: TListNotification);
begin
  if (action = lnDeleted) and (TAu3DEmitter(ptr).AutoFree)then
   TAu3DEmitter(ptr).Free;
end;

{ TAu3DEmitter }

constructor TAu3DEmitter.Create(ASound: TAu3DSound);
begin
  inherited Create;

  FSound := ASound;
  
  if ASound = nil then; //! RAISE EXCEPTION
  

  //Preset some parameters
  FGain := 1.0;
  FGlobalEmitter := false;
  FAutoFree := true;
  FUserData := nil;
  FPosition := AcVector3(0, 0, 0);
  FReferenceDistance := 1;
  FRolloffFactor := 1;
  FMaxDistance := 10000;
  FTimePosition := 0;
  FTimeOffset := 0;
  FProperties := AU3DPROP_ALL;
  FActive := true;

  FSound.Emitters.Add(self)
end;

destructor TAu3DEmitter.Destroy;
begin
  FAutoFree := false;
  FSound.Emitters.Remove(self);

  AuQueueRemove(self);

  inherited Destroy;
end;

function TAu3DEmitter.GetGlobalEmitter: Boolean;
begin
  result := FGlobalEmitter or (FSound.Parameters.Channels <> 1);
end;

procedure TAu3DEmitter.Move(ATimeGap: Extended);
begin
  if not FActive then
    exit;
    
  if Assigned(FMoveProc) then
    FMoveProc(self, ATimeGap);

  if not FSound.Streamed then
  begin
    //If the sound is not stream, each emitter is reponsible for it's own
    //position time stamp.  
    FTimePosition := FTimePosition +
      round((ATimeGap * FSound.Pitch * FSound.Parameters.Frequency * (1 shl 16)));

    if ((FTimePosition - FTimeOffset) shr 16) > FSound.BufferSamples then
    begin
      if Assigned(FStopProc) then
      begin
        AuQueueCall(StopProc);
      end;

      if FSound.Loop then
        while ((FTimePosition - FTimeOffset) shr 16 >= FSound.BufferSamples) do
          FTimeOffset := FTimeOffset + Int64(FSound.BufferSamples) shl 16;
    end;
  end;
end;

procedure TAu3DEmitter.SetGain(AValue: Single);
begin
  if AValue >= 0 then
    FGain := AValue;
end;

procedure TAu3DEmitter.SetMaxDistance(AValue: Single);
begin
  if AValue > 0 then
    FMaxDistance := AValue;
end;

procedure TAu3DEmitter.SetReferenceDistance(AValue: Single);
begin
  if AValue > 0 then
    FReferenceDistance := AValue;
end;

procedure TAu3DEmitter.SetRolloff(AValue: Single);
begin
  if AValue > 0 then
    FRolloffFactor := AValue;
end;

procedure TAu3DEmitter.SetTimePosition(AVal: TAuSampleStamp);
begin
  if not FSound.Streamed then
  begin
    FTimePosition := AVal;
    FTimeOffset := 0;
    FManualPositionChange := true;
  end;
end;

procedure TAu3DEmitter.StopProc;
begin
  if Assigned(FStopProc) then
    FStopProc(self);
end;

function TAu3DEmitter.TimePosition: TAuSamplestamp;
begin
  if FSound.Streamed then
    //If the sound is streamed, return the time position of the parent sound
    result := FSound.TimePosition
  else
    //If the sound is not streamed, return the very own time position value
    result := FTimePosition;
end;

end.
