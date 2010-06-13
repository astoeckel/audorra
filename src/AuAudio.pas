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
  AcPersistent, AcStrUtils, AcRegUtils, AcNotify, AcSyncObjs,
  AuProtocolClasses, AuDecoderClasses, AuAnalyzerClasses, AuDriverClasses, 
  AuTypes, AuFiltergraph,
  AuMessages;


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

  {TAuAudio is the base class for audio output in Audorra. It manages the creation
   of the backend device drivers and autoselects an output device.}    
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
      FMaxPriority: integer;
      procedure DeviceEnum(ADevice: TAuDevice);
      procedure DeviceEnum2(ADevice: TAuDevice);
      procedure SetDriverName(AValue: string);
      procedure FillDeviceList;
    public
      {Creates a new instance of TAuAudio. TAuAudio will automatically try to
       choose the best output driver (OpenAL, DirectSound, etc.).}
      constructor Create;
      {Destroys this instance of TAuAudio. Important: Never free the TAuAuio
       when any audio module using this instance is still running! Always free
       all other audio modules which could be using this instance first.}
      destructor Destroy;override;

      {Tries to initialize the device driver set in the "DriverName" property.
       A device driver normally gets chosen when the class is created. Returns
       "true" if the initialization was successful, "false" if anything went wrong.
       Use the "GetLastError" function to get information about the error which
       let the initialization fail.
       @seealso(DriverName)
       @see}
      function Initialize: boolean;
      {Finalizes the device driver. Never do this when any audio module is still
       using the current device driver.}
      procedure Finalize;
      //!TODO: Notify Events

      {This procedure automatically chooses an device driver (the audio backend),
       prefering drivers with a high priority.}
      procedure AutoChooseDriver;
      {This procedure automatically sets the "StandardDeviceID" property by choosing
       the output device with the highest priority. Choosing an output device
       is only possible when the driver has successfully been initialized.}
      procedure AutoChooseDevice;

      {Returns the last error string.}
      function GetLastError: string;

      {This property contans the standard device ID - the device which should be
       used if no other device is specified. Remember that "DeviceIDs" are internally
       created by the audio backend and do not represent their position in the devices
       list.
       @seealso(Devices)}
      property StandardDeviceID: integer read FStandardDeviceID write FStandardDeviceID;
      {Contains the class name of the audio driver - set this class name to set
       a specific audio driver or use the "AutoChooseDriver" function. This property
       should be set before calling the "Initialize" function.
       @seealso(Initialize)}
      property DriverName: string read FDriverName write SetDriverName;
      {Pointer on the audio backend driver class.}
      property Driver: TAuDriver read FDriver;
      {Describes whether the TAuAudio is initialized and a driver object is created.
       TAuAudio can be initilized by calling the "Initialize" function.}
      property Initialized: boolean read FInitialized;
      {"Devices" contans a list of all devices available within the current audio
       backend.}
      property Devices: TAuDeviceList read FDevices;

      {Notify event which is called on initialization.}
      property OnInitialize: TAuNotifyEvent read FInitialize write FInitialize;
      {Notify event which is called on finalization.}
      property OnFinalize: TAuNotifyEvent read FFinalize write FFinalize;
  end;

  {Describes the state of a TAuPlayer object.}
  TAuPlayerState = (
    {The player is currently closed, no file has been opened.}
    aupsClosed,
    {The player is in a state of loading a file, stream or protocol. This state
     is used internally and should actually never occur outside TAuPlayer.}
    aupsLoading,
    {The player has been loading a file, stream or protocol and is no waiting
     for the "Open" command.}
    aupsLoaded,
    {The player has successfuly been opened. This state means, that the player
     is currently in a "stop" mode.}
    aupsOpened,
    {The player is currently playing.}
    aupsPlaying,
    {The player is currently paused.}
    aupsPaused
  );

  {TAuCustomAudioObject is the base class for objects like TAuPlayer and TAuStaticSound
   and provides basic file, stream, url, protocol and state management.}
  TAuCustomAudioObject = class
    private
      FState: TAuPlayerState;
      FStateChangeEvent: TAuNotifyEvent;
      FDestroyEvent: TAuNotifyEvent;
      FStream: TStream;
      FOwnStream: boolean;
      FProtocol: TAuProtocol;
      FOwnProtocol: boolean;
      FLock: TAcLock;
      FURL: string;
      procedure CallStateChange(ASender: TObject; AUserData: Pointer);
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
      {Creates a new instance of TAuCustomAudioObject.
       @param(AParent is a pointer on the parent TAuAudio object.)}
      constructor Create(AParent: TAuAudio);
      {Destroys the instance of TAuCustomAudioObject. All opened files, streams,
       protocols, etc. will be freed, if they were not user defined (by opening
       a stream via "LoadFromStream" or a protocol via "LoadFromProtocol" and
       "AOwnStream" is set to false).}
      destructor Destroy;override;

      {Loads a stream and maps it to an Audorra protocol. The stream is not opened,
       no data will be read. Like all LoadFrom* functions, this will only
       set the data source for further operations.
       @param(AStream is a pointer on the stream which should be opened.)
       @param(AOwnStream defines whether the stream object should be owned by
         the TAuCustomAudio instance. If this parameter is true, TAuCustomAudio
         will cope with freeing the stream instance.)
       @returns(true if loading the stream was successful.)}
      function LoadFromStream(AStream: TStream; AOwnStream: boolean = false): Boolean;
      {Loads a specified file. No data will be read from the specified file.
       Like all LoadFrom* functions, this will only set the data source for further operations.
       Note: To open URLs use the LoadFromURL function.
       @param(AFile describes the file which should be opened.)
       @returns(true if loading the file was successful.)}
      function LoadFromFile(AFile: string): Boolean;
      {Loads a specified URL. No data will be read from the desired URL. Like all
       LoadFrom* functions, this will only set the data source for further operations.
       Note: To open files, prefix "file://" to the URL or use the
       LoadFromFile function.
       @param(AURL describes the URL which should be opened.)
       @returns(true if loading the URL was successful.)}
      function LoadFromURL(AURL: string): Boolean;
      {Loads the specified Audorra protocol. No data will be read from the protocol
       when calling this function - like all LoadFrom* functions, this will only
       set the data source for further operations.
       Note: To open files, prefix "file://" to the URL or use the
       LoadFromFile function.
       @param(AFile describes the file which should be opened.)
       @param(AOwnProtocol defines whether the protocol object should be owned by
         the TAuCustomAudio instance. If this parameter is true, TAuCustomAudio
         will cope with freeing the protocol instance.)
       @returns(true if loading the protocol was successful, which should always
         be the case.)}
      function LoadFromProtocol(AProtocol: TAuProtocol; AOwnProtocol: boolean = false): Boolean;

      {All classes derived from TAuCustomAudioObject have to implement the open function -
       it should choose a decoder, fill all buffers and prepare everything for playback.
       A data source has to be selected using the LoadFrom* functions before "Open" can
       be called. If opening was successful, "Open" should return true.}
      function Open: boolean;virtual;abstract;
      {All classes derived from TAuCustomAudioObject have to implement the close function.
       They are ought to destroy all objects created in the "Open" function. The data source
       specified in the LoadFrom* functions will be reseted to @nil.}
      procedure Close;virtual;abstract;

      {Pointer on the parent TAuAudio object.}
      property Parent: TAuAudio read FParent;
      {Defines the current audio object state.}
      property State: TAuPlayerState read FState;
      {Returns the length of the audio object or -1 if the length can - for whatever reason -
       not be retrieved.}
      property Len: integer read GetLength;

      {Notify event which is called when the "State" property of the object changes.}
      property OnStateChange: TAuNotifyEvent read FStateChangeEvent write FStateChangeEvent;
      {Notify event which is called when the object is being destroyed. The event
       is called after all objects owned by the instance have been destroyed.}
      property OnDestroy: TAuNotifyEvent read FDestroyEvent write FDestroyEvent;
  end;

  {Used internally by TAuPlayer to receive information about notification data change
   from the TAuPlayerNotification thread.}
  TAuSyncDataChangedCallback = procedure(ASyncData: TAuFrameInfo) of object;

  {Used internally by TAuPlayer in order to notify the TAuPlayer instance about
   state changes in the filtergraph environment.}
  TAuPlayerNotificationThread = class(TThread)
    private
      FOutputFilter: TAuOutputFilter;
      FDecoderFilter: TAuCustomDecoderFilter;
      FCallback: TAuSyncDataChangedCallback; 
    protected
      procedure Execute;override;
    public
      constructor Create(AOutputFilter: TAuOutputFilter;
        ADecoderFilter: TAuCustomDecoderFilter;
        ACallback: TAuSyncDataChangedCallback);
  end;

  {TAuPlayer is a simple solution for playing back audio using the filter graph
   environment. TAuPlayer let's you Load, Open and Play audio files. Additionally
   you're able to add analyzers (visualisations) and change the playback volume.
   A limiter filter is included in TAuPlayer's filtergraph environment to prevent
   the audio from clipping.

   The "source" and "target" filtergraph blocks can be replaced by setting own
   instances in the constructor. When integrating the TAuPlayer class into the
   3D-Audio environment, use an instance of TAu3DSoundFilterAdapter from the unit
   TAu3DAudioFilters and set this filter as a target.

   Remember that changing the player's volume may take a certain time - this depends
   on the latency of the audio driver. When using the 3D-Audio environment as
   described above, the latency time may be very long as 3d audio renderer buffers
   around ten seconds of audio data, so you should use the corresponding
   properties of the 3D-Audio environment instead.

   TAuPlayer uses all registered, additional protocols and decoders.}
  TAuPlayer = class(TAuCustomAudioObject)
    private
      FTarget: TAuFilter;
      FDriver: TAuStreamDriver;
      FOutput: TAuOutputFilter;
      FOwnTarget: boolean;
      FDecoder: TAuDecoder;
      FSource: TAuCustomDecoderFilter;
      FOwnSource: boolean;
      FBufSize: integer;
      FAnalyzeFilter: TAuAnalyzeFilter;
      FVolume: TAuVolumeFilter;
      FCompressor: TAuCompressorFilter;
      FMasterVolume: single;
      FSongFinishesEvent: TAuNotifyEvent;
      FAnalyzerList: TAuAnalyzerList;
      FReloadOnPlay: boolean;
      FDeviceID: integer;
      FSetDeviceID: boolean;      
      FCurrentFrameInfo: TAuFrameInfo;
      FNotifyThreadMutex: TAcMutex;
      FNotifyThread: TAuPlayerNotificationThread;
      procedure SyncDataChanged(ASyncData: TAuFrameInfo);      
      procedure FreeComponents(ADestroySources: boolean);
      function BuildFilterGraph: boolean;
      procedure SetMasterVolume(AValue: single);
      function GetPosition: integer;
      function GetSeekable: boolean;
      procedure SetPosition(AValue: integer);
      procedure StopHandler(ASender: TObject; AUserData: Pointer);
      procedure SetDeviceID(AValue: integer);
      function GetDeviceID: integer;
    protected
      function GetLength: integer;override;
    public
      {Creates a new instance of TAuPlayer.
       @param(AAudio is a pointer on the parent TAuAudio instance)
       @param(ATarget is an optional parameter, which can be used to replace
         the output filter with the given filter instance.
       @param(ASource is an optional parameter, which can be used to replace
         the source decoder filter.}
      constructor Create(AAudio: TAuAudio; ATarget: TAuFilter = nil;
        ASource: TAuCustomDecoderFilter = nil);
      {Destroys the TAuPlayer instance.}
      destructor Destroy;override;

      {Adds a certain analyzer instance to the analyzer filter.}
      procedure AddAnalyzer(AAnalyzer: TAuAnalyzer);
      {Removes a previously registered analyter from the analyzer filter.}
      procedure RemoveAnalyzer(AAnalyzer: TAuAnalyzer);

      {Opens the previously loaded file. If the file couldn't be opend, e.g. because
       the format is unknown, "Open" returns false.}
      function Open: boolean;override;
      {Closes the previously opened file.}
      procedure Close;override;

      {Starts playback.}
      procedure Play;
      {Pauses playback.}
      procedure Pause;
      {Pauses playback and seeks back to the beginning of the file.}
      procedure Stop;

      {The buffer size in bytes. //!TODO: What the heck is this?? And why is it writable}
      property BufSize: integer read FBufSize write FBufSize;

      {Set this to a value greater or equal to zero to change the master volume.
       A value of zero will mute the audio output (-INF dB), a value of one will
       output the audio with it's original volume (0 dB). Remember that amplifing
       the audio with values greater than one might distort the audio output.

       Use the "VolumeFilter" property to set the volume for each audio channel
       independently.

       @seealso(VolumeFilter)}
      property MasterVolume: Single read FMasterVolume write SetMasterVolume;
      {Indicates the current playback position of the TAuPlayer component in milliseconds.
       Set this property to seek to a certain position.}
      property Position: integer read GetPosition write SetPosition;
      {Returns whether the current instance of TAuPlayer, with the current media stream
       opened, is seekable.}
      property Seekable: boolean read GetSeekable;

      {Pointer to the stream driver class. May be @nil if ATarget is set manually.}
      property Driver: TAuStreamDriver read FDriver;

      {Pointer to the internally used volume filter. May be @nil if no media file
       is opened.}
      property VolumeFilter: TAuVolumeFilter read FVolume;

      {Callback function, which indicates that the current media stream has finished.
       The callback is called within the context of the main thread/the audorra
       internal notify thread.}
      property OnSongFinishes: TAuNotifyEvent read FSongFinishesEvent write FSongFinishesEvent;

      {The current output device id.}
      property DeviceID: integer read GetDeviceID write SetDeviceID;
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
var
  inst: TAuDriver;
begin
  with TAuAudio(ASender) do
  begin
    inst := TAuCreateDriverProc(AEntry^.ClassConstructor);
    try
      if (FDriverName = '') or (inst.Priority > FMaxPriority) then
      begin
        FDriverName := AEntry^.Name;
        FMaxPriority := inst.Priority;
      end;
    finally
      inst.Free;
    end;
  end;
  if TAuAudio(ASender).FDriverName = '' then
    TAuAudio(ASender).FDriverName := AEntry.Name;
end;

procedure TAuAudio.AutoChooseDriver;
begin
  //Initialize the driver search
  FMaxPriority := 0;
  FDriverName := '';
  
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
    AcNotifyRemoveObject(self);

    FreeObjects;
  finally
    FLock.Leave;
  end;

  if Assigned(FDestroyEvent) then
    FDestroyEvent(self);

  FreeAndNil(FLock);

  inherited;
end;

procedure TAuCustomAudioObject.CallStateChange(ASender: TObject; AUserData: Pointer);
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

function TAuCustomAudioObject.LoadFromFile(AFile: string): boolean;
begin
  result := false;
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

      result := LoadFromStream(FStream);
    except
      Close;
      raise;
    end;
  finally
    FLock.Leave;
  end;
end;

function TAuCustomAudioObject.LoadFromProtocol(AProtocol: TAuProtocol;
  AOwnProtocol: boolean = false): boolean;
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

    result := true;
  finally
    FLock.Leave;
  end;
end;

function TAuCustomAudioObject.LoadFromStream(AStream: TStream; AOwnStream: boolean): Boolean;
begin
  FLock.Enter;
  try
    FOwnStream := FOwnStream or AOwnStream;
    
    //Close and reset FURL if not in the "loading" state
    if FState <> aupsLoading then
    begin
      FURL := '';
      Close;
    end;

    SetState(aupsLoading);

    //Create a stream protocol and connect it to the stream
    FProtocol := TAuStreamProtocol.Create(AStream);

    //If the stream has been loaded from a file, set the "URL" parameter.
    TAuStreamProtocol(FProtocol).URL := FURL;

    //This is our own protocol
    FOwnProtocol := true;

    //Try to open the protocol
    result := LoadFromProtocol(FProtocol);
  finally
    FLock.Leave;
  end;
end;

function TAuCustomAudioObject.LoadFromURL(AURL: string): boolean;
var
  Prot, User, Pass, Host, Port, Path, Para: string;
  lst: TStringList;
  i: integer;
begin
  result := false;
  
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

      //Enumerate all registered protocls
      lst := TStringList.Create;
      try
        AcEnumRegClasses(TAuURLProtocol, lst);
        for i := 0 to lst.Count - 1 do
        begin
          //Create an instance of the selected protocol
          FProtocol := TAuCreateURLProtocolProc(AcRegSrv.GetEntry(lst[i])^.ClassConstructor);

          //Check whether it supports the protocol identifier.
          if TAuURLProtocol(FProtocol).SupportsProtocol(LowerCase(Prot)) then
          begin
            //Keep this instance of the protocol and try to open it
            if TAuURLProtocol(FProtocol).Open(AUrl) then
            begin
              FOwnProtocol := true;
              result := LoadFromProtocol(FProtocol);
              break;
            end else
              FreeAndNil(FProtocol);
          end else
            FreeAndNil(FProtocol);
        end;
      finally
        lst.Free;
      end;
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
      AcNotifyQueue(self, CallStateChange);
  end;
end;

function TAuCustomAudioObject.GetLength: integer;
begin
  result := -1;
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

  FNotifyThreadMutex := TAcMutex.Create;
end;

destructor TAuPlayer.Destroy;
begin
  Lock.Enter;
  try
    FreeComponents(true);

    FAnalyzerList.Free;

    inherited;

    FNotifyThreadMutex.Free;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.FreeComponents(ADestroySources: boolean);
begin
  Lock.Enter;
  try
    //Stop the notification thread before freeing any other component
    if (FNotifyThread <> nil) then
    begin
      FNotifyThread.Terminate;
      FNotifyThread.WaitFor;
      FreeAndNil(FNotifyThread);
    end;

    //Stop the output
    if (FOutput <> nil) then
    begin
      FOutput.Stop;
      FOutput := nil;
    end;

    if (FTarget <> nil) and (FTarget.State = aufsInitialized) then
      FTarget.SuspendFiltergraph;    

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

    //Remove all notifications for this player instance from the queue, as they
    //are associated with the old object
    AcNotifyRemoveObject(self);

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
        FDecoder := AuFindDecoder(Protocol);

      if (FDecoder = nil) and (FSource = nil) then
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
        FDriver := FParent.Driver.CreateStreamDriver(DeviceID);
        if (FDriver = nil) then exit;
          //! RAISE EXCEPTION

        FTarget := TAuDriverOutput.Create(FDriver, FDecoder.Info.BitDepth.bits);
        FOutput := TAuOutputFilter(FTarget);
        FOwnTarget := true;
      end else exit;
        //! RAISE EXCEPTION
    end else
    begin
      if FTarget.GlobalTargetFilter is TAuOutputFilter then
        FOutput := TAuOutputFilter(FTarget.GlobalTargetFilter)
      else exit;
        //! RAISE EXCEPTION
    end;

    if FOutput <> nil then
    begin
      //Create the source filter
      if (FSource = nil) then
      begin
        FSource := TAuDecoderFilter.Create;
        TAuDecoderFilter(FSource).SetDecoder(FDecoder);
        FOwnSource := true;
      end;

      //Create an analyzer filter
      FAnalyzeFilter := TAuAnalyzeFilter.Create;

      //Connect all registered analyzers to the analyzer
      for i := 0 to FAnalyzerList.Count - 1 do
      begin
        //Update audio parameters
        FAnalyzerList[i].Parameters := params;

        //Connect the analyzer to the filter
        FAnalyzeFilter.Analyzers.Add(FAnalyzerList[i]);
      end;                                             

      //Create the volume filter
      FVolume := TAuVolumeFilter.Create;
      FVolume.Master := FMasterVolume;

      //Create the compressor filter
      FCompressor := TAuCompressorFilter.Create;

      //Interconnect all filters
      FSource.Target := FAnalyzeFilter;
      FAnalyzeFilter.Target := FVolume;
      FVolume.Target := FCompressor;
      FCompressor.Target := FTarget;

      //Initialize the target filter
      TAuOutputFilter(FTarget).Init(params);

      //Create the notify thread
      FNotifyThread := TAuPlayerNotificationThread.Create(TAuOutputFilter(FTarget),
        FSource, SyncDataChanged);  

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
//      FSource.Unlock;
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
      FillChar(FCurrentFrameInfo, SizeOf(FCurrentFrameInfo), 0);

//      FSource.Lock;
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

procedure TAuPlayer.SyncDataChanged(ASyncData: TAuFrameInfo);
begin
  FNotifyThreadMutex.Acquire;
  try
    FCurrentFrameInfo := ASyncData;
    if (FCurrentFrameInfo.PlaybackState = auisFinished) then
      AcNotifyQueue(self, StopHandler);

  finally
    FNotifyThreadMutex.Release;
  end;
end;

procedure TAuPlayer.StopHandler(ASender: TObject; AUserData: Pointer);
begin
  if FState > aupsOpened then
  begin
    //Remove the all notifications for the player from the notify queue as there
    //may come other calls from the notification queue
    AcNotifyRemoveObject(self);

    Stop;

    if Assigned(FSongFinishesEvent) then
      FSongFinishesEvent(self);
  end;
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
    FNotifyThreadMutex.Acquire;
    try
      if (FOutput <> nil) and (FCurrentFrameInfo.PlaybackState <> auisUndefined) then
      begin
        if (State = aupsPlaying) or (State = aupsPaused) then
          result := FCurrentFrameInfo.MediaTimestamp
        else
          result := 0;
      end;
    finally
      FNotifyThreadMutex.Release;
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

      FillChar(FCurrentFrameInfo, SizeOf(FCurrentFrameInfo), 0);

      //Seek to the given position, if that worked, flush the whole filtergraph
      if FDecoder.SeekTo(Position, AValue) then
        FTarget.FlushFiltergraph;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TAuPlayer.AddAnalyzer(AAnalyzer: TAuAnalyzer);
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

procedure TAuPlayer.SetDeviceID(AValue: integer);
begin
  FSetDeviceID := true;
  FDeviceID := AValue;
end;

function TAuPlayer.GetDeviceID: integer;
begin
  if (FSetDeviceID) or (FParent = nil) then
    result := FDeviceID
  else
    result := FParent.StandardDeviceID;
end;

{ TAuPlayerNotificationThread }

constructor TAuPlayerNotificationThread.Create(AOutputFilter: TAuOutputFilter;
  ADecoderFilter: TAuCustomDecoderFilter;
  ACallback: TAuSyncDataChangedCallback);
begin
  inherited Create(false);

  FOutputFilter := AOutputFilter;
  FDecoderFilter := ADecoderFilter;
  FCallback := ACallback;
end;

procedure TAuPlayerNotificationThread.Execute;
var
  oldtime, time: Int64;
  olddata, data: TAuFrameInfo;
begin
  //Preset the variables
  oldtime := -1;
  FillChar(olddata, SizeOf(olddata), 0);
  FillChar(data, SizeOf(data), 0);

  //Just loop until the thread gets terminated  
  while not Terminated do
  begin
    //Get the current timecode from the output filter
    time := FOutputFilter.Timecode;
    
    if time <> oldtime then
    begin
      if FDecoderFilter is TAuDecoderFilter then
      begin
        //Obtain the frame info from the decoder filter
        if TAuDecoderFilter(FDecoderFilter).GetFrameInfo(time, data) then
        begin
          //If the frame info has changed, call the callback
          if not CompareMem(@data, @olddata, SizeOf(data)) then
            FCallback(data);
          olddata := data;
        end;
      end else
      begin
        //Assemble an own frame info record if a custom decoder filter is used
        data.MediaTimestamp := time;
        data.SampleTimestamp := time;
        data.Interpolated := true;
        data.PlaybackState := auisPlaying;

        FCallback(data);
      end;
    end;
    
    Sleep(1);
  end;
end;

end.

