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

File: AuAudio.pas
Author: Andreas Stöckel
}

{Contains abstraction classes for simple usage of Audorra.}
unit AuAudio;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AcPersistent, AcStrUtils, AcRegUtils, AcSyncObjs,
  AuTypes, AuFilterGraph, AuProtocolClasses, AuDecoderClasses,
  AuDriverClasses, AuAnalyzerClasses, AuSyncUtils, AuMessages;


type
  EAudioDriver = class(Exception);

  {TAuDeviceList is a list internally used by TAuAudio. TAuDeviceList contains
   an entry of the type "TAuDevice" for each device in the list.
   @seealso(TAuDevice)}
  TAuDeviceList = class(TList)
    private
      function GetItem(AIndex: integer): TAuDevice;
      procedure SetItem(AIndex: integer; ADev: TAuDevice);
      function GetItemById(AIndex: integer): TAuDevice;
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      {Provides access to the list elements based on their position in the list.
       "Index" may not be equal to the ID of a device.
       @seealso(ItemByID)}
      property Items[Index: integer]: TAuDevice read GetItem write SetItem; default;
      {Provides acces to the list elements based on their internal device id.
       The internal device id has nothing to do with the position in the list.
       @seelaso(Items)}
      property ItemById[AId: integer]: TAuDevice read GetItemById;
      {Adds a new device record to the list.
      @returns(The position of the element in the list)}
      function Add(ADev: TAuDevice): integer;
  end;
  
    
  TAuAudio = class
    private
      FDriverName: string;
      FStandardDeviceID: integer;
      FDriver: TAuDriver;
      FInitialized: boolean;
      FEnumDevicePriority: integer;
      FEnumPos: integer;
      FDevices: TAuDeviceList;
      FInitialize: TAuNotifyEvent;
      FFinalize: TAuNotifyEvent;
      FLastError: string;
      procedure DeviceEnum(ADevice: TAuDevice);
      procedure DeviceEnum2(ADevice: TAuDevice);
      procedure SetDriverName(AValue: string);
      procedure FillDeviceList;
    public
      constructor Create;
      destructor Destroy;override;

      function Initialize: boolean;
      procedure Finalize;

      procedure AutoChooseDriver;
      procedure AutoChooseDevice;

      function GetLastError: string;

      property StandardDeviceID: integer read FStandardDeviceID write FStandardDeviceID;
      property DriverName: string read FDriverName write SetDriverName;
      property Driver: TAuDriver read FDriver;
      property Initialized: boolean read FInitialized;
      property Devices: TAuDeviceList read FDevices;

      property OnInitialize: TAuNotifyEvent read FInitialize write FInitialize;
      property OnFinalize: TAuNotifyEvent read FFinalize write FFinalize;
  end;

  TAuPlayerState = (
    aupsClosed,
    aupsLoading,
    aupsLoaded,
    aupsOpened,
    aupsPlaying,
    aupsPaused
  );

  TAuCustomAudioObject = class
    private
      FState: TAuPlayerState;
      FStateChangeEvent: TAuNotifyEvent;
      FDestroyEvent: TAuNotifyEvent;
      FStream: TStream;
      FOwnStream: boolean;
      FProtocol: TAuProtocol;
      FOwnProtocol: boolean;
      FDeviceID: integer;
      FSetDeviceID: boolean;
      FLock: TAcLock;
      FURL: string;
      procedure SetDeviceID(AValue: integer);
      function GetDeviceID: integer;
      procedure CallStateChange;
    protected
      FParent: TAuAudio;
      procedure SetState(AValue: TAuPlayerState; ACallEvent: boolean = true);
      procedure FreeObjects;
      function GetLength: integer; virtual;

      property Protocol: TAuProtocol read FProtocol write FProtocol;
      property OwnProtocol: boolean read FOwnProtocol write FOwnProtocol;
      property Stream: TStream read FStream write FStream;
      property OwnStream: boolean read FOwnStream write FOwnStream;
      property Lock: TAcLock read FLock;
    public
      constructor Create(AParent: TAuAudio);
      destructor Destroy;override;

      procedure LoadFromStream(AStream: TStream; AOwnStream: boolean = false);
      procedure LoadFromFile(AFile: string);
      procedure LoadFromURL(AURL: string);
      procedure LoadFromProtocol(AProtocol: TAuProtocol; AOwnProtocol: boolean = false);

      function Open: boolean;virtual;abstract;
      procedure Close;virtual;abstract;

      property Parent: TAuAudio read FParent;
      property State: TAuPlayerState read FState;
      property DeviceID: integer read GetDeviceID write SetDeviceID;
      property Len: integer read GetLength;

      property OnStateChange: TAuNotifyEvent read FStateChangeEvent write FStateChangeEvent;
      property OnDestroy: TAuNotifyEvent read FDestroyEvent write FDestroyEvent;
  end;
  
  TAuPlayer = class(TAuCustomAudioObject)
    private
      FTarget: TAuFilter;
      FDriver: TAuStreamDriver;
      FOutput: TAuOutputFilter;
      FOwnTarget: boolean;
      FDecoder: TAuDecoder;
      FDeviceID: integer;
      FSource: TAuCustomDecoderFilter;
      FOwnSource: boolean;
      FBufSize: integer;
      FAnalyzeFilter: TAuAnalyzeFilter;
      FVolume: TAuVolumeFilter;
      FCompressor: TAuCompressorFilter;
      FMasterVolume: single;
      FSongFinishesEvent: TAuNotifyEvent;
      FAnalyzerList: TAuAnalyzerList;
      FHasDecoder: boolean;
      FReloadOnPlay: boolean;
      procedure FreeComponents(ADestroySources: boolean);
      function BuildFilterGraph: boolean;
      procedure SetMasterVolume(AValue: single);
      function GetPosition: integer;
      function GetSeekable: boolean;
      procedure SetPosition(AValue: integer);
      procedure StopHandler(Sender: TObject);
    protected
      function GetLength: integer;override;
    public
      constructor Create(AAudio: TAuAudio; ATarget: TAuFilter = nil;
        ASource: TAuCustomDecoderFilter = nil);
      destructor Destroy;override;

      procedure AddAnalzyer(AAnalyzer: TAuAnalyzer);
      procedure RemoveAnalyzer(AAnalyzer: TAuAnalyzer);

      function Open: boolean;override;
      procedure Close;override;
      procedure Play;
      procedure Pause;
      procedure Stop;

      property BufSize: integer read FBufSize write FBufSize;
      property MasterVolume: Single read FMasterVolume write SetMasterVolume;
      property Position: integer read GetPosition write SetPosition;
      property Seekable: boolean read GetSeekable;

      property Driver: TAuStreamDriver read FDriver;

      property VolumeFilter: TAuVolumeFilter read FVolume;

      property OnSongFinishes: TAuNotifyEvent read FSongFinishesEvent write FSongFinishesEvent;
  end;

implementation

{ TAuAudio }

constructor TAuAudio.Create;
begin
  inherited Create;

  FDriver := nil;
  FInitialized := false;
  FDevices := TAuDeviceList.Create;

  //Choose a default driver
  AutoChooseDriver;
end;

destructor TAuAudio.Destroy;
begin
  Finalize;

  FDevices.Free;
  
  inherited;
end;

procedure TAuAudio.DeviceEnum(ADevice: TAuDevice);
begin
  //Select the device if its priority is higher than the priority of the last
  //driver.
  if (ADevice.Priority > FEnumDevicePriority) or (FEnumPos = 0) then
  begin
    FStandardDeviceID := ADevice.ID;
    FEnumDevicePriority := ADevice.Priority;
    FEnumPos := FEnumPos + 1;
  end;
end;

procedure TAuAudio.DeviceEnum2(ADevice: TAuDevice);
begin
  FDevices.Add(ADevice)
end;

procedure TAuAudio.AutoChooseDevice;
begin
  if FInitialized then
  begin
    FEnumPos := 0;
    FEnumDevicePriority := -1;
    FDriver.EnumDevices(DeviceEnum);
  end;
end;

procedure TAuAudio_EnumProc(ASender: Pointer; AEntry: PAcRegisteredClassEntry);
begin
  if TAuAudio(ASender).FDriverName = '' then  
    TAuAudio(ASender).FDriverName := AEntry.Name;
end;

procedure TAuAudio.AutoChooseDriver;
begin
  AcRegSrv.EnumClasses(TAuDriver, TAuAudio_EnumProc, self);
end;

procedure TAuAudio.FillDeviceList;
begin
  if Initialized then
  begin
    FDevices.Clear;
    FDriver.EnumDevices(DeviceEnum2);
  end;
end;

procedure TAuAudio.Finalize;
begin
  if FInitialized then
  begin
    FDriver.Free;
    FInitialized := false;
    FDriver := nil;
  end;
end;

function TAuAudio.GetLastError: string;
begin
  result := FLastError;
end;

function TAuAudio.Initialize: boolean;
var
  proc: TAuCreateDriverProc;
begin
  //Finalize the current driver
  Finalize;

  result := false;
  try
    //Get the driver creation proc
    proc := TAuCreateDriverProc(AcRegSrv.GetConstructor(FDriverName));
    if @proc <> nil then
    begin
      FDriver := proc;
      if FDriver <> nil then
      begin
        FInitialized := true;

        //List all available devices
        FillDeviceList;

        if FDevices.Count > 0 then
        begin
          //Auto choose an output device
          AutoChooseDevice;

          result := true;
        end else
          FLastError := MsgNoDevice;
      end else
        FLastError := MsgUnableToCreateDriver;
    end else
      FLastError := MsgNoDriver;
  except      
    on e: Exception do
    begin
      FLastError := e.Message;
      result := false;
    end;
  end;
end;

procedure TAuAudio.SetDriverName(AValue: string);
begin
  if AcRegSrv.GetConstructor(AValue) <> nil then  
  begin
    FDriverName := AValue;
    Finalize;
  end else;
    raise EAudioDriver.CreateFmt(MsgDeviceDoesNotExist, [AValue]);
end;

{ TAuPlayer }

constructor TAuPlayer.Create(AAudio: TAuAudio; ATarget: TAuFilter = nil;
  ASource: TAuCustomDecoderFilter = nil);
begin
  FURL := '';
  
  inherited Create(AAudio);
  
  if ATarget <> nil then
  begin
    FTarget := ATarget;
    FOwnTarget := false;
  end;

  if ASource <> nil then
  begin
    FSource := ASource;
    FOwnSource := false;
    FState := aupsLoaded;
  end;

  //1024 KB standard buffer size
  //==> when using CD quality audio enough for 4 seconds
  FBufSize := 1024 * 1024 * 4;

  FMasterVolume := 1;

  //Create the analyzers list
  FAnalyzerList := TAuAnalyzerList.Create;

  //Fetch the standard device ID
  if FParent <> nil then
    FDeviceID := FParent.StandardDeviceID;
end;

destructor TAuPlayer.Destroy;
begin
  Lock.Enter;
  try
    FreeComponents(true);

    FAnalyzerList.Free;

    inherited;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.FreeComponents(ADestroySources: boolean);
begin
  Lock.Enter;
  try
    //Stop the output
    if (FOutput <> nil) then
    begin
      FOutput.Stop;
      FOutput := nil;
    end;

    //Free the source object          //  Filters have to be freed in order:
    if (FSource <> nil) and (FOwnSource) then
      FreeAndNil(FSource);            //  FSource -> FAnalyzerFilter -> FVolume -> FTarget
                                      //      |                                       |
    //Free the decoder object         //     \ /                                     \ /
    if (FDecoder <> nil) then         //  FDecoder                                 FDriver
      FreeAndNil(FDecoder);           //

    //Free the peakmeter
    if (FAnalyzeFilter <> nil) then
      FreeAndNil(FAnalyzeFilter);

    //Free the volume controler
    if (FVolume <> nil) then
      FreeAndNil(FVolume);

    if FCompressor <> nil then
      FreeAndNil(FCompressor);

    //Free the target object if it is owned by this instance
    if (FOwnTarget) and (FTarget <> nil) then
    begin
      FreeAndNil(FTarget);
      FreeAndNil(FDriver);
      FOwnTarget := false;
    end;

    //Free all objects held by the parent class
    if ADestroySources then
      FreeObjects;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.Close;
begin
  Lock.Enter;
  try
    //SetState(aupsClosed);
    FreeComponents(true);
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer_EnumDecoders(ASender: Pointer; AEntry: PAcRegisteredClassEntry);

  procedure DestroyDecoder;
  begin
    with TAuPlayer(ASender) do
    begin
      if Protocol.Seekable then
        Protocol.Seek(aupsFromBeginning, 0);

      FreeAndNil(FDecoder);
    end;
  end;

begin
  with TAuPlayer(ASender) do
  begin
    if not FHasDecoder then
    begin
      FDecoder := TAuCreateDecoderProc(AEntry.ClassConstructor)(Protocol);
      try
        if not FDecoder.OpenDecoder then
          DestroyDecoder
        else
          FHasDecoder := true;
      except
        DestroyDecoder;
      end;
    end;
  end;
end;

function TAuPlayer.Open: boolean;
begin
  Lock.Enter;
  try
    result := false;

    if State = aupsLoaded then
    begin
      //Reopen the URL-Protocol if this didn't happen.
      if (Protocol is TAuURLProtocol) and (not TAuURLProtocol(Protocol).Opened) then
        TAuURLProtocol(Protocol).Open(TAuURLProtocol(Protocol).URL);      

      if FSource = nil then
      begin
        FHasDecoder := false;
        AcRegSrv.EnumClasses(TAuDecoder, TAuPlayer_EnumDecoders, self);
      end else
        FHasDecoder := true;

      if not FHasDecoder then
        exit;
     
      if BuildFilterGraph then
      begin
        SetState(aupsOpened);
        result := true;
      end;
    end;
  finally
    Lock.Leave;
  end;
end;

function TAuPlayer.BuildFilterGraph: boolean;
var
  i: integer;
  params: TAuAudioParameters;
begin
  Lock.Enter;
  try
    result := false;

    //Get the audio parameters
    if FDecoder <> nil then
      params := FDecoder.Info.Parameters
    else if FSource <> nil then
      params := FSource.Parameters;


    //Create the target filter
    if (FTarget = nil) then
    begin
      if (FParent <> nil) then
      begin
        FDriver := FParent.Driver.CreateStreamDriver(DeviceID, AuAudioParametersEx(
          params.Frequency, params.Channels, 16));

        if (FDriver = nil) or (not FDriver.Open) then exit;
          //! RAISE EXCEPTION
          
        FTarget := TAuDriverOutput.Create(FDriver);
        FOutput := TAuOutputFilter(FTarget);
        FOwnTarget := true;
      end else exit;
        //! RAISE EXCEPTION
    end else
      FOutput := FTarget.GetOutputFilter;

    if FOutput <> nil then
    begin
      FOutput.OnStop := StopHandler;

      //Create the source filter
      if (FSource = nil) then
      begin
        FSource := TAuDecoderFilter.Create(FDecoder, FBufSize);
        FOwnSource := true;
      end;

      //Create an analyzer filter
      FAnalyzeFilter := TAuAnalyzeFilter.Create(params);

      //Connect all registered analyzers to the analyzer
      for i := 0 to FAnalyzerList.Count - 1 do
      begin
        //Update audio parameters
        FAnalyzerList[i].Parameters := params;

        //Connect the analyzer to the filter
        FAnalyzeFilter.Analyzers.Add(FAnalyzerList[i]);
      end;

      //Create the volume filter
      FVolume := TAuVolumeFilter.Create(params);
      FVolume.Master := FMasterVolume;

      //Create the compressor filter
      FCompressor := TAuCompressorFilter.Create(params);

      //Interconnect all filters
      FSource.Target := FAnalyzeFilter;
      FAnalyzeFilter.Target := FVolume;
      FVolume.Target := FCompressor;
      FCompressor.Target := FTarget;

      //Initialize the target filter
      TAuOutputFilter(FTarget).Init(params);  

      result := true;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.Pause;
begin
  Lock.Enter;
  try
    if FOutput <> nil then
    begin
      FOutput.Pause;
      SetState(aupsPaused);
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.Play;
begin
  Lock.Enter;
  try
    if FReloadOnPlay then
    begin
      FReloadOnPlay := false;
      Open;
    end;

    if FOutput <> nil then
    begin
      FSource.Unlock;
      FOutput.Play;
      SetState(aupsPlaying);
    end;
  finally
    Lock.Leave;
  end;
end;    

procedure TAuPlayer.Stop;
begin
  Lock.Enter;
  try
    if FOutput <> nil then
    begin
      FSource.Lock;
      FOutput.Stop;

      //Reset the state
      SetState(aupsOpened);

      if Seekable then
      begin
        //If the decoder perform seeks, simply seek back
        Position := 0;
      end else
      begin
        //If the decoder or the stream do not support streaming, perform a
        //"close" and a "open" operation
        if Protocol <> nil then
        begin
          //Free all components without destroying the file sources (the protocol)
          FreeComponents(false);

          if Protocol.Seekable then
            Protocol.Seek(aupsFromBeginning, 0)
          else if Protocol is TAuURLProtocol then
            TAuURLProtocol(Protocol).Close;

          //Open the file again
          SetState(aupsLoaded, false);

          FReloadOnPlay := true;
        end;
      end;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.StopHandler(Sender: TObject);
begin
  Stop;

  if Assigned(FSongFinishesEvent) then
    FSongFinishesEvent(self);
end;

procedure TAuPlayer.SetMasterVolume(AValue: single);
begin
  Lock.Enter;
  try
    FMasterVolume := AValue;
    if FVolume <> nil then
      FVolume.Master := FMasterVolume;
  finally
    Lock.Leave;
  end;
end;

function TAuPlayer.GetLength: integer;
begin
  Lock.Enter;
  try
    result := inherited GetLength;

    if FDecoder <> nil then
      result := FDecoder.StreamLength;
  finally
    Lock.Leave;
  end;
end;

function TAuPlayer.GetPosition: integer;
begin
  Lock.Enter;
  try
    result := -1;

    //Return the output position
    if FOutput <> nil then
    begin
      if (State = aupsPlaying) or (State = aupsPaused) then
        result := FOutput.SyncData.Timecode
      else
        result := 0;
    end;
  finally
    Lock.Leave;
  end;
end;
         
function TAuPlayer.GetSeekable: boolean;
begin
  Lock.Enter;
  try
    result := (State > aupsLoaded) and
              (GetLength > -1) and
              (Protocol <> nil) and
              (Protocol.Seekable);
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.SetPosition(AValue: integer);
begin
  Lock.Enter;
  try
    if (FDecoder <> nil) and (State >= aupsOpened) then
    begin
      //Reset playback
      TAuOutputFilter(FTarget).Stop; //Actually not needed because the decoder timeslices are very small

      //Seek to the given position
      if FDecoder.SeekTo(Position, AValue) then
        //Delete decoder data
        TAuDecoderFilter(FSource).FlushBuffer;

      if State = aupsPlaying then
        FDriver.Play;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.AddAnalzyer(AAnalyzer: TAuAnalyzer);
begin
  Lock.Enter;
  try
    //Add the analyzer to the local analyzer list
    FAnalyzerList.Add(AAnalyzer);

    //If the analyzer block exists, add it to its block too.
    if FAnalyzeFilter <> nil then
    begin
      FAnalyzeFilter.CriticalSection.Enter;
      try
        FAnalyzeFilter.Analyzers.Add(AAnalyzer);
      finally
        FAnalyzeFilter.CriticalSection.Leave;
      end;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.RemoveAnalyzer(AAnalyzer: TAuAnalyzer);
begin
  Lock.Enter;
  try
    FAnalyzerList.Remove(AAnalyzer);
  finally
    Lock.Leave;
  end;
end;

{ TAuDeviceList }

function TAuDeviceList.Add(ADev: TAuDevice): integer;
var
  ptr: PAuDevice;
begin
  new(ptr);
  ptr^ := ADev;
  result := inherited Add(ptr);
end;

function TAuDeviceList.GetItem(AIndex: integer): TAuDevice;
begin
  result := PAuDevice(inherited Items[AIndex])^;
end;

function TAuDeviceList.GetItemById(AIndex: integer): TAuDevice;
var
  i: integer;
begin
  FillChar(result, SizeOf(TAuDevice), 0);

  for i := 0 to Count - 1 do
    if Items[i].ID = AIndex then
    begin
      result := Items[i];
      break;
    end;
end;

procedure TAuDeviceList.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    FreeMem(ptr, SizeOf(TAuDevice));
end;

procedure TAuDeviceList.SetItem(AIndex: integer; ADev: TAuDevice);
begin
  PAuDevice(inherited Items[AIndex])^ := ADev;
end;

{ TAuCustomAudioObject }

constructor TAuCustomAudioObject.Create(AParent: TAuAudio);
begin
  inherited Create;

  FParent := AParent;  
  FState := aupsClosed;
  FLock := TAcLock.Create;
end;

destructor TAuCustomAudioObject.Destroy;
begin
  FLock.Enter;
  try
    AuQueueRemove(self);

    FreeObjects;
  finally
    FLock.Leave;
  end;

  if Assigned(FDestroyEvent) then
    FDestroyEvent(self);

  FreeAndNil(FLock);
  
  inherited;
end;

procedure TAuCustomAudioObject.CallStateChange;
begin
  if Assigned(FStateChangeEvent) then
    FStateChangeEvent(self);
end;

procedure TAuCustomAudioObject.FreeObjects;
begin
  FLock.Enter;
  try
    //Free the protocol
    if FOwnProtocol and (FProtocol <> nil) then
      FProtocol.Free;

    //Reset the protocol variables
    FProtocol := nil;
    FOwnProtocol := false;

    //Free the stream
    if FOwnStream and (FStream <> nil) then
      FStream.Free;

    //Reset the stream variables
    FStream := nil;
    FOwnStream := false;
  finally
    FLock.Leave;
  end;
end;

procedure TAuCustomAudioObject.LoadFromFile(AFile: string);
begin
  FLock.Enter;
  try
    //Close if not in the "loading" state
    if FState <> aupsLoading then
      Close;

    //Switch to the "loading" state
    SetState(aupsLoading);

    FUrl := AFile;

    try
      //Create a file stream and open the file.
      FStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
      FOwnStream := true;

      LoadFromStream(FStream);
    except
      Close;
      raise;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TAuCustomAudioObject.LoadFromProtocol(AProtocol: TAuProtocol; AOwnProtocol: boolean = false);
begin
  FLock.Enter;
  try
    FOwnProtocol := FOwnProtocol or AOwnProtocol;
    
    //Close if not in the "loading" state
    if FState <> aupsLoading then
    begin
      FURL := '';
      Close;
    end;

    //If we've reached this line, the loading progress has been finished
    SetState(aupsLoaded);
    FProtocol := AProtocol;
  finally
    FLock.Leave;
  end;
end;

procedure TAuCustomAudioObject.LoadFromStream(AStream: TStream; AOwnStream: boolean);
begin
  FLock.Enter;
  try
    FOwnStream := FOwnStream or AOwnStream;
    
    //Close if not in the "loading" state
    if FState <> aupsLoading then
    begin
      FURL := '';
      Close;
    end;

    SetState(aupsLoading);

    FProtocol := TAuStreamProtocol.Create(AStream);
    TAuStreamProtocol(FProtocol).URL := FURL;
    FOwnProtocol := true;

    LoadFromProtocol(FProtocol);
  finally
    FLock.Leave;
  end;
end;

procedure TAuCustomAudioObject.LoadFromURL(AURL: string);
var
  Prot, User, Pass, Host, Port, Path, Para: string;
  lst: TStringList;
  i: integer;
begin
  FLock.Enter;
  try
    ParseURL(AUrl, Prot, User, Pass, Host, Port, Path, Para);

    if Prot = '' then
      LoadFromFile(AUrl)
    else begin

      //Close if not in the "loading" state
      if FState <> aupsLoading then
        Close;

      //Switch to the "loading" state
      SetState(aupsLoading);

      lst := TStringList.Create;
      AcEnumRegClasses(TAuURLProtocol, lst);
      for i := 0 to lst.Count - 1 do
      begin
        FProtocol := TAuCreateURLProtocolProc(AcRegSrv.GetEntry(lst[i])^.ClassConstructor);
        if TAuURLProtocol(FProtocol).SupportsProtocol(LowerCase(Prot)) then
        begin
          FOwnProtocol := true;
          if TAuURLProtocol(FProtocol).Open(AUrl) then
            LoadFromProtocol(FProtocol)
          else
            FreeAndNil(FProtocol);          
        end else
          FreeAndNil(FProtocol);
      end;
      lst.Free;
    end;
    
  finally
    FLock.Leave;
  end;
end;

procedure TAuCustomAudioObject.SetState(AValue: TAuPlayerState; ACallEvent: boolean);
begin
  if AValue <> FState then
  begin
    FState := AValue;
          
    if ACallEvent then    
      AuQueueCall(CallStateChange);
  end;
end;

function TAuCustomAudioObject.GetDeviceID: integer;
begin
  if (FSetDeviceID) or (FParent = nil) then
    result := FDeviceID
  else
    result := FParent.StandardDeviceID;
end;

function TAuCustomAudioObject.GetLength: integer;
begin
  result := -1;
end;

procedure TAuCustomAudioObject.SetDeviceID(AValue: integer);
begin
  FSetDeviceID := true;
  FDeviceID := AValue;
end;

end.

