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

{Contains classes for the simple usage of 3d audio in Audorra.}
unit Au3DAudio;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AcDataStore, AcPersistent, AcNotify,
  AuDriverClasses, AuDecoderClasses,
  AuTypes, AuUtils, AuAudio, AuFilterGraph, AuMessages,
  Au3DAudioRenderer, Au3DAudioFilters;

type
  {TAu3DAudio is the high level class for providing 3D audio inside an audorra filtergraph
   environment. Such an 3D audio environment is also needed if you simply want to playback
   short sounds from memory with low latency. Then TAu3DAudio can be used together
   with TAuStaticSound and/or TAuSoundList. TAu3DAudio can be interconnected to an
   TAuPlayer instance by using a TAu3DSoundFilterAdapter as a target or by using
   TAuStreamed sound instead.
   TAu3DAudio is a wrapper class around TAu3DSoundRenderer and the filtergraph adpters
   and initializes and manages these objects in a convenient manner.
   You should always use TAu3DAudio instead of directly instanciating the managed
   object if you want to use the 3D audio capabilites.
   @seealso(TAu3DSoundRenderer)
   @seealso(TAuStreamedSound)
   @seealso(TAuStaticSound)
   @seealso(TAuSoundList)
   @seealso(TAu3DOutputFilterAdapter)
   @seealso(TAu3DSoundFilterAdapter)}
  TAu3DAudio = class
    private
      FAudio: TAuAudio;
      FOwnAudio: Boolean;
      FRenderer: TAu3DSoundRenderer;
      FOutputAdapter: TAu3DOutputFilterAdapter;
      FDriver: TAuStreamDriver;
      FDriverFilter: TAuDriverOutput;
      FFrequency: integer;
      FBitdepth: integer;
      FSpeakerPreset: TAu3DSpeakerPreset;
      FDeviceID: integer;
      FLastError: String;

      function GetParameters: TAuAudioParametersEx;
      function GetListener: TAu3DListener;
    public
      {Creates an instance of TAu3DAudio by using an external TAuAudio instance.
       @param(AAudio is the parent audio environment to be used. You have to care about
         initializing, chosing the right driver etc. Pass nil if you want TAu3DAudio
         to create its own TAuAudio instance. You can access this instance by using
         the "Audio" property.)
       @param(ASpeakerPreset specifies the count and type of loud speeker setup.)
       @param(AFrequency specifies the output frequency of the output. Use 44100
         for CD-Quality audio, 48000 for DAT-Quality audio, etc. You should be
         aware, that the frequency directly specifies the computational power
         needed for calculating the audio output. As higher this value is, as higher
         will be the CPU load. You should also remember to specify a value here,
         which matches the frequency of most audio samples used, as this will
         save cpu power needed for interpolating.)
       @param(ABitdepth specifies the bitdepth of the audio output. The highest
         value supported by the audio system should be used here. 32-Bit output
         might the fastest from the Audorra point of view, if the audio card
         has floating point support.)
       @param(ADeviceID specifies the id of the device that should be used to output
         the audio. A value of -1 (default) will take the default device. You can
         enumerate all devices available by using the TAuAudio class.)}
      constructor Create(AAudio: TAuAudio; ASpeakerPreset: TAu3DSpeakerPreset;
        AFrequency, ABitdepth: integer; ADeviceID: integer = -1);
      {Destroys this instance of TAu3DAudio. Please remember to free all classes
       using TAu3DAudio before freeing TAu3DAudio itself.}
      destructor Destroy;override;

      {All changes made to the 3D audio environment (adding sources, changing values,
       adding walls, etc.) have to be synchronized with the audio thread in order
       to prevent both thread from going into race conditions or read/write accessing
       resources at the same time. Always call the "Lock" function before performing
       such tasks. You must always protect the actions performed after calling the
       lock function with a "try/finally" block, calling "Unlock" in the finally
       part. Do not perform computationally expensive tasks while the audio renderer
       is locked, as this may lead to audio stuttering. Trying to free the renderer
       inside the lock or trying to perform other complex operations inside the lock
       may lead to a dead lock!
       @seealso(TAu3DSoundRenderer.Lock)
       @seealso(TAu3DSoundRenderer.Unlock)
       @seealso(Unlock)}
      procedure Lock;
      {Unlocks the 3D audio renderer.
       @seealso(Lock)}
      procedure Unlock;

      {Initializes the 3D audio environment: Creates the renderer, filter adapter
       and driver output. If no own TAuAudio object has been
       passed, the internal TAuAudio object will now been initialized. To set
       any options in the internal audio object, use the Audio property.
       @return(true if initialization was successfull, false if something went wrong.
         use the "GetLastError" function to get an detailed description about what
         has happened.)
       @seealso(Finalize)}
      function Initialize: boolean;
      {Finalizes the 3d audio environment by freeing the renderer, filter graph adapter
       and output object. If no own TAuAudio object has been passed, the internal
       audio object will be freed here.}
      procedure Finalize;

      {Returns the last error that occured during the initialization process.}
      function GetLastError: string;

      {Reference to the TAuAudio object passed during initialization or the internal
       TAuAudio object if nil had been passed in the constructor.}
      property Audio: TAuAudio read FAudio;
      {Reference to the 3d audio software renderer.}
      property Renderer: TAu3DSoundRenderer read FRenderer;
      {Reference to the filter graph adapter, which is used to interconnect the
       device driver object with the software renderer.}
      property OutputAdapter: TAu3DOutputFilterAdapter read FOutputAdapter;
      {The speaker preset which had been chosen in the constructor.}
      property SpeakerPreset: TAu3DSpeakerPreset read FSpeakerPreset;
      {The audio parameters the 3D audio environment is using.}
      property Parameters: TAuAudioParametersEx read GetParameters;
      {The device id which is used to output the data.}
      property DeviceID: integer read FDeviceID;
      {The 3d audio listener object that is used in connection with the audio renderer.
       Use this object to set the position, orientation, pitch and gain of the listener.
       Don't forget to synchronize access to the listener by using TAuRenderer.Lock and
       Unlock.}
      property Listener: TAu3DListener read GetListener;
  end;

  TAuSoundList = class;
  
  {TAuStaticSound is a class which represents a short sound which completely resists
  in memory. Therefore it can be instantly played without any delay and loop without
  any gaps in playback. It is well suited for effects like gun-shots, looping background noises etc.
  It should not be used for things like background music etc. as the memory footprint for that
  would be fairly large. For these applications you should use TAuStreamedSound.
  TAuStaticSound can only be used in connection with a 3D environment provided by TAu3DAudio.
  If you want to manage multiple sounds and load/save them as a collection from a file,
  you can use the TAuSoundList class.
  
  To load/open the sound you can use the "LoadFrom*" and "Open" functions provided
  by TAuCustomAudioObject.
  
  To playback the sound you first have to create an TAu3DStaticEmitter emitter object
  and append that to the TAu3DStaticSound object provided by the sound property.
  
  Example usage:
  @longCode(#var
  sound: TAuStaticSound;
  emitter: TAu3DStaticEmitter;
begin
  //Create the sound object, load and open it
  sound := TAuStaticSound.Create(_3daudio);
  sound.LoadFromFile('sound.wav');
  if sound.Open() then
  begin
    //Now attach a new emitter to the sound object. There can be as many
    //emitters attached to a sound object as you like.
    _3daudio.Lock();
    try    
      emitter := TAu3DStaticEmitter.Create(sound.Sound);
      
      //To start playback, simply set the active property of the emitter
      //to true
      emitter.Active := true;      
    finally
      _3daudio.Unlock();
    end;    
  end;
end;
#)
  @seealso(TAuSoundList)
  @seealso(TAuStreamedSound)
  @seealso(TAu3DAudio)}
  TAuStaticSound = class(TAuCustomAudioObject)
    private
      FSound: TAu3DStaticSound;
      FMs: TMemoryStream;
      FFormat: TAuAudioParametersEx;
      FLoop: boolean;
      FName: AnsiString;
      FOwner: Pointer;
      F3DAudio: TAu3DAudio;
      procedure FreeComponents;
      function DecodeStream: boolean;
      procedure SetLoop(AValue: boolean);
      function CreateSoundObj: boolean;
    protected
      {Function from TAuCustomAudioObject overwritten to provide the length of the
       sound.}
      function GetLength: integer; override;
    public
      {Creates a new instance of the TAuStaticSound class.
       @param(A3DAudio is a reference to the 3D audio environment which should be
       used as a parent.)}
      constructor Create(A3DAudio: TAu3DAudio);
      {Destroys this instance of TAuStaticSound.}
      destructor Destroy;override;

      {Implements the Open function from TAuCustomAudioObject. Call this function
       after you have been loading a sound from a file/store. The open function will
       create a 3d sound object and add this one to the sound source list of the 3d audio
       software renderer. As the software renderer is a performance critical part,
       you should only open sound effects if you really need them. Once a TAuStaticSound
       is opened, you're able to access the Sound property containing the actual
       3D software renderer sound object.
       @seealso(Close)
       @seealso(Sound)}
      function Open: boolean;override;
      {Implements the TAuCustomAudioObject close function and removes this object
       from the software renderer list.
       @seealso(Open)}
      procedure Close;override;

      {Stores the sound in a Andorra Commons data store. The sound will be stored
       as uncompressed PCM data.
       @param(AStore is the TAcStoreNode object a new "sound" node should be appended to)
       @returns(The "sound" store node object which has been created and contains the actual
       data.)
       @seealso(LoadItemFromStore)}
      function SaveItemToStore(AStore: TAcStoreNode): TAcStoreNode;
      {Loads the sound from a "sound" store node item. Then performs a "open" operation:
       The sound will be added to the 3D audio software renderer source list.
       @param(AStore is the "sound" TAcStoreNode object the data should be loaded from.)}
      procedure LoadItemFromStore(AStore: TAcStoreNode);
      
      {Defines whether the sound should be looping or not.}
      property Loop: boolean read FLoop write SetLoop;
      {An optional name which can be used to identify a sound in a sound list.
       @seealso(TAuSoundList)
       @seealso(TAuSoundList.Find)
       @seealso(TAuSoundList.IndexOf)}
      property Name: AnsiString read FName write FName;
      {Property set by the sound list, if you're creating a new TAuStaticSound using
       the "AddNew" function.}
      property Owner: Pointer read FOwner write FOwner;
      {The audio format the currently loaded sound is stored in. If no sound is opened,
       all components of the record will be resetted to zero.
       @seealso(DecodedData)}
      property Format: TAuAudioParametersEx read FFormat;
      {A memorystream containing the decoded PCM sound data. The format the data is
       stored in is described in the format property.
       @seealso(Format)}
      property DecodedData: TMemoryStream read FMs;
      {The low level sound object which is connected to the software renderer.
       @nil if the sound isn't opened yet.}
      property Sound: TAu3DStaticSound read FSound;
  end;
  
  {TAuStreamed class represents a streamed sound which can be used inside a 3D audio
   environment. As it is derrived from TAuPlayer it basically provides the same
   functionality. TAuStreamedSound internally creates an TAu3DSoundFilterAdapter which
   is set as the output filter for the underlying TAuPlayer. This component is well
   suited for playing e.g. background music in a 3D audio environment as the audio
   data is streamed from the resource containing it. Therefore there is a certain
   delay until playback starts and looping (which has to be done using the OnSongFinishes event)
   will create a short gap in audio playback in most cases.
   
   As with TAuStaticSound you'll also have to connect a emitter (of the class TAu3DStreamedEmitter) to
   the TAu3DStreamedSound object provided by the "Sound" property of this class.
   
   Example usage:
@longCode(#var
  stream: TAuStreamedSound;
  emitter: TAu3DStaticEmitter;
begin
  //Create the sound object, load and open it
  stream := TAuStreamedSound.Create(_3daudio);
  stream.LoadFromFile('music.oga');
  if stream.Open() then
  begin
    //Now attach a new emitter to the sound object. There can be as many
    //emitters attached to a sound object as you like.
    _3daudio.Lock();
    try    
      emitter := TAu3DStreamedEmitter.Create(stream.Sound);
    finally
      _3daudio.Unlock();
    end;

    //To start playback, simply use the "Play", "Pause", "Stop" functions
    //provided by TAuStreamedSound. All emitters will perform the same action
    //synchronously.
    stream.Play();    
  end;
end;
#)
   @seealso(TAuPlayer)
   @seealso(TAuPlayer.OnSongFinishes)
   @seealso(TAu3DSoundFilterAdapter)
   @seealso(Adapter)
   @seealso(TAuStaticSound)}
  TAuStreamedSound = class(TAuPlayer)
    private
      FFilterAdapter: TAu3DSoundFilterAdapter;
      FParent: TAu3DAudio;
      FName: AnsiString;
      function GetSound: TAu3DStreamedSound;
    public
      {Creates a new TAuStreamedSound instance.
       @param(A3DAudio is a reference to the 3d audio environment the sound should
        be created in.)}
      constructor Create(A3DAudio: TAu3DAudio);
      {Destroys this instance of TAuStreamedSound.}
      destructor Destroy;override;
      
      {A name property which might be used when you want to manage multiple instances
       of TAuStreamedSound inside a list.}
      property Name: AnsiString read FName write FName;
      {Reference to the internally used TAu3DSoundFilterAdapter which is used as an
       output filter for the underlying TAuPlayer.}
      property Adapter: TAu3DSoundFilterAdapter read FFilterAdapter;
      {Reference to the low level sound object which is connected to the 3d audio renderer.
       @nil if the local filtergraph environment isn't initialized yet, which is the case
       when no audio file is opened.}
      property Sound: TAu3DStreamedSound read GetSound;
  end;
  
  {TAuSoundList is a class which can be used to manage multiple TAuStaticSound objects
   in one list. The sound objects can be loaded and stored inside files or an TAcStoreNode
   of the Andorra Commons Data Store system.}
  TAuSoundList = class(TList)
    private
      F3DAudio: TAu3DAudio;
      function GetItem(AIndex: integer): TAuStaticSound;
      procedure SetItem(AIndex: integer; AItem: TAuStaticSound);
    protected
      {Function overwritten from TList in order to free sound objects which belong
       to this list once they are freed.}
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      {Creates this instance of TAuSoundList.
       @param(A3DAudio is the 3d audio environment which should be used for elements
        being loaded from a store or created using the AddNew function.
        @seealso(AddNew))}
      constructor Create(A3DAudio: TAu3DAudio);
      {Destroys this instance of TAuSoundList and frees all sounds inside it with their
       Owner property set to this list.
       @seealso(TAuStaticSound.Owner)}
      destructor Destroy;override;

      {Returns the index of the sound object with the given name. If such an object
       cannot be found, -1 is returned.}
      function IndexOf(AName: AnsiString): integer;overload;
      {Returns the index of the given sound object. If this object is not found,
       -1 is returned.}
      function IndexOf(AObj: TAuStaticSound): integer;overload;
      {Tries to find a sound object with the given name inside the list. If it cannot
       be found, @nil is returned.}
      function Find(AName: AnsiString): TAuStaticSound;
      {Adds a new TAuStaticSound object to the list and gives it the name specified
       by AName. The owner propety of the sound object will be set to this list
       instance so that it gets automatically freed when this list is destroyed.
       @seealso(TAuStaticSound.Owner)}
      function AddNew(AName: AnsiString): TAuStaticSound;
      
      {Saves the complete sound list to the given stream.}
      procedure SaveToStream(AStream: TStream);
      {Loads the complete sound list from the given stream.}
      procedure LoadFromStream(AStream: TStream);
      {Saves the sound list to the file specified by the AFile parameter.}
      procedure SaveToFile(AFile: string);
      {Loads the complete sound list from the file specified by the AFile parameter.
       The sound list will not be cleared before the new sound objects are added.}
      procedure LoadFromFile(AFile: string);
      {Saves the complete sound list to the given Andorra Commons Data Store store
       node.}
      function SaveToStore(AStore: TAcStoreNode): TAcStoreNode;
      {Loads the complete sound list from the given Andorra Commons Data Store store
       node. The content of this list will not be cleared before the new elements
       are added to the list.}
      procedure LoadFromStore(AStore: TAcStoreNode);
      
      {The parent TAu3DAudio object which was specified in the constructor.}
      property Parent: TAu3DAudio read F3DAudio;
      {Property which can be used to access each TAuStaticSound item by index.}
      property Items[AIndex: integer]: TAuStaticSound read GetItem write SetItem; default;
  end;

implementation

{ TAu3DAudio }

constructor TAu3DAudio.Create(AAudio: TAuAudio;
  ASpeakerPreset: TAu3DSpeakerPreset; AFrequency, ABitdepth: integer;
  ADeviceID: integer = -1);
begin
  inherited Create;

  if AAudio = nil then
  begin
    FAudio := TAuAudio.Create;
    FOwnAudio := true;
  end else
  begin
    FAudio := AAudio;
    FOwnAudio := false;
  end;

  //Copy some parameters
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

  if FOwnAudio then
    FreeAndNil(FAudio);

  inherited;
end;

function TAu3DAudio.Initialize: boolean;
begin
  result := false;
  if FOwnAudio and (not FAudio.Initialized) and (not FAudio.Initialize) then
  begin
    FLastError := FAudio.GetLastError;
    exit;
  end;

  //Create the 3D audio renderer
  FRenderer := TAu3DSoundRenderer.Create(FSpeakerPreset, FFrequency);

  //Create the output driver
  FDriver := FAudio.Driver.CreateStreamDriver(FDeviceID);
  if (FDriver <> nil) then
  begin
    //                                      FDriver
    //                                        / \
    //                                         |
    //                                         |
    //  FRenderer --> FOutputAdapter --> FDriverFilter
    //                       |
    //                       |
    //                      \ /
    //                   FListener


    //Create the driver filter block
    FDriverFilter := TAuDriverOutput.Create(FDriver, FBitdepth);

    //Create the output adpter block and interconnect it to the renderer
    FOutputAdapter := TAu3DOutputFilterAdapter.Create(FRenderer);
    FOutputAdapter.Target := FDriverFilter;

    //Start the output
    if FDriverFilter.Init(AuAudioParameters(FFrequency,
      FRenderer.Setup.OutputChannelCount)) then
    begin
      FDriverFilter.Play;

      result := true;
    end;
  end else
    FLastError := Msg3DAudioDriverCreationFailed;

  if not result then
    Finalize;
end;

procedure TAu3DAudio.Finalize;
begin
  //Finalize the filtergraph before freeing the other components
  if FDriverFilter <> nil then
  begin
    FDriverFilter.Finalize;
    FreeAndNil(FDriverFilter);
  end;
  
  FreeAndNil(FOutputAdapter);
  FreeAndNil(FDriver);
  FreeAndNil(FRenderer);
end;

function TAu3DAudio.GetLastError: string;
begin
  result := FLastError;
end;

function TAu3DAudio.GetListener: TAu3DListener;
begin
  result := nil;
  if FOutputAdapter <> nil then
    result := FOutputAdapter.Listener;
end;

function TAu3DAudio.GetParameters: TAuAudioParametersEx;
begin
  result.Frequency := FFrequency;
  result.Channels := FRenderer.Setup.OutputChannelCount;
  result.BitDepth := AuBitdepth(FBitdepth);
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

{ TAuStaticSound }

constructor TAuStaticSound.Create(A3DAudio: TAu3DAudio);
begin
  inherited Create(A3DAudio.Audio);
  F3DAudio := A3DAudio;
end;

destructor TAuStaticSound.Destroy;
begin
  Lock.Enter;
  try
    SetState(aupsClosed, false);
    Close;
    inherited;
  finally
    Lock.Leave;
  end;
end;

procedure TAuStaticSound.FreeComponents;
begin
  Lock.Enter;
  try
    //Free the 3d sound object
    if FSound <> nil then
    begin
      F3DAudio.Lock;
      try
        //FSound is automatically freed
        FSound.AutoFree := true;
        F3DAudio.Renderer.Sounds.Remove(FSound);
        FSound := nil;
      finally
        F3DAudio.Unlock;
      end;
    end;

    //Free the memory stream if it was our own
    FreeAndNil(FMs);

    //Free all objects which were delivered by the parent class
    FreeObjects;
    
    //Clear the format object
    FillChar(FFormat, SizeOf(FFormat), 0);
  finally
    Lock.Leave;
  end;
end;

function TAuStaticSound.GetLength: integer;
begin
  Lock.Enter;
  try
    result := inherited GetLength;

    if FMs <> nil then
      result := round((FMs.Size * 1000) /  AuBytesPerSecond(FFormat.Parameters));
  finally
    Lock.Leave;
  end;
end;

function TAuStaticSound.DecodeStream: boolean;
var
  decoder: TAuDecoder;
  res: TAuDecoderState;
  pckg:  TAuPacket;
  mem: PByte;
  buf_size: integer;
begin
  result := false;
  mem := nil;
  
  try
    //Search a decoder for the given protocol
    decoder := AuFindDecoder(Protocol);
    if decoder <> nil then
    begin
      //Create a memory stream for the decoded data, store the decoder format
      FMs := TMemoryStream.Create;
      FFormat := decoder.Info;

      repeat
        //Decode each package of the input data
        res := decoder.Decode;

        //Convert each frame to 32-Bit floating point data, and write it to the memory stream
        if res = audsHasFrame then
        begin
          //Get the packet data
          decoder.GetPacket(pckg);

          //Calculate how many bytes the converted package will take and reserve that memory
          buf_size := AuConvertByteCount(pckg.BufferSize, FFormat,
            AuAudioParametersEx(FFormat.Frequency, FFormat.Channels, AuBitDepth(32)));
          ReallocMem(mem, buf_size);

          //Convert the samples and write them to the memory stream
          AuReadSamples(FFormat, pckg.Buffer, mem, Cardinal(pckg.BufferSize)
            div AuBytesPerSample(FFormat));
          FMs.Write(mem^, buf_size);
        end;
      until res = audsEnd;

      decoder.Free;

      result := true;
    end;
  finally
    if mem <> nil then
      FreeMem(mem);
  end;
end;

function TAuStaticSound.Open: boolean;
begin
  Lock.Enter;
  try
    result := false;
    if (State = aupsLoaded) and (DecodeStream) then
    begin
      result := CreateSoundObj;
      SetState(aupsOpened);
    end;
  finally
    Lock.Leave;
  end;
end;

function TAuStaticSound.CreateSoundObj: boolean;
begin
  result := true;
  FSound := TAu3DStaticSound.Create(PByte(FMs.Memory),
    FMs.Size div AuBytesPerSample(FFormat.Parameters), FFormat.Parameters);
  FSound.Loop := FLoop;
    
  F3DAudio.Lock;
  try
    F3DAudio.Renderer.Sounds.Add(FSound);
  finally
    F3DAudio.Unlock;
  end;
end;

procedure TAuStaticSound.Close;
begin
  Lock.Enter;
  try
    SetState(aupsClosed);
    FreeComponents;
  finally
    Lock.Leave;
  end;
end;

procedure TAuStaticSound.SetLoop(AValue: boolean);
begin
  Lock.Enter;
  try
    FLoop := AValue;
    if FSound <> nil then
      FSound.Loop := FLoop;
  finally
    Lock.Leave;
  end;
end;

procedure TAuStaticSound.LoadItemFromStore(AStore: TAcStoreNode);
var
  fmt_node: TAcStoreNode;
  strm_node: TAcStreamNode;
begin
  Lock.Enter;
  try
    Close;

    FName := AStore.StringValue('name');

    fmt_node := AStore.Nodes.ItemNamed['fmt'];
    FillChar(FFormat, SizeOf(FFormat), 0);
    if fmt_node <> nil then
    begin
      FFormat.Frequency := fmt_node.IntValue('freq', 44100);
      FFormat.BitDepth := AuBitDepth(fmt_node.IntValue('bits', 16)); //Load whole bit format
      FFormat.Channels := fmt_node.IntValue('chan', 2);
    end;

    strm_node := TAcStreamNode(AStore.Nodes.ItemNamed['data']);
    if (strm_node <> nil) and (strm_node is TAcStreamNode) then
    begin
      strm_node.Open(acsoRead);
      try
        FMs := TMemoryStream.Create;
        FMs.CopyFrom(strm_node.Stream, 0);
        FMs.Position := 0;
      finally
        strm_node.Close;
      end;
    end;

    if CreateSoundObj then
      SetState(aupsOpened);
  finally
    Lock.Leave;
  end;
end;

function TAuStaticSound.SaveItemToStore(AStore: TAcStoreNode): TAcStoreNode;
var
  node, fmt_node: TAcStoreNode;
  strm_node: TAcStreamNode;
begin
  Lock.Enter;
  try
    node := AStore.Add('sound');
    node.Add('name', FName);

    fmt_node := node.Add('fmt');
    fmt_node.Add('freq', FFormat.Frequency);
    fmt_node.Add('bits', FFormat.BitDepth.bits); //! Save whole bit format
    fmt_node.Add('chan', FFormat.Channels);

    if FMs <> nil then
    begin
      strm_node := TAcStreamNode(node.Add('data', TAcStreamNode));
      strm_node.Open(acsoWrite);
      try
        strm_node.Stream.CopyFrom(FMs, 0);
      finally
        strm_node.Close;
      end;
    end;

    result := node;
  finally
    Lock.Leave;
  end;
end;

{ TAu3DSoundList }

constructor TAuSoundList.Create(A3DAudio: TAu3DAudio);
begin
  inherited Create;

  F3DAudio := A3DAudio;
end;

destructor TAuSoundList.Destroy;
begin
  inherited;
end;

function TAuSoundList.AddNew(AName: AnsiString): TAuStaticSound;
begin
  result := TAuStaticSound.Create(F3DAudio);
  result.Name := AName;
  result.Owner := self;
  Add(result);
end;

function TAuSoundList.GetItem(AIndex: integer): TAuStaticSound;
begin
  result := inherited Items[AIndex];
end;

procedure TAuSoundList.SetItem(AIndex: integer; AItem: TAuStaticSound);
begin
  inherited Items[AIndex] := AItem;
end;

function TAuSoundList.IndexOf(AName: AnsiString): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = AName then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TAuSoundList.IndexOf(AObj: TAuStaticSound): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i] = AObj then
    begin
      result := i;
      exit;
    end;
  end;
end;

procedure TAuSoundList.Notify(ptr: Pointer; action: TListNotification);
begin
  F3DAudio.Lock;
  try
    if (action = lnDeleted) and (TAuStaticSound(ptr).Owner = self) then
      TAuStaticSound(ptr).Free;
  finally
    F3DAudio.Unlock;
  end;
end;

function TAuSoundList.Find(AName: AnsiString): TAuStaticSound;
var
  ind: integer;
begin
  result := nil;
  ind := IndexOf(AName);
  if ind > -1 then
    result := Items[ind];
end;

procedure TAuSoundList.LoadFromStore(AStore: TAcStoreNode);
var
  i: integer;
  tmp: TAuStaticSound;
begin
  for i := 0 to AStore.Nodes.Count - 1 do
  begin
    if AStore.Nodes[i].Name = 'sound' then
    begin
      tmp := TAuStaticSound.Create(F3DAudio);
      tmp.LoadItemFromStore(AStore.Nodes[i]);
      tmp.Owner := self;
      Add(tmp);
    end;
  end;
end;

procedure TAuSoundList.LoadFromStream(AStream: TStream);
var
  store: TAcStoreNode;
begin
  store := TAcStoreNode.Create(nil);
  store.LoadFromStream(AStream);
  LoadFromStore(store);
  store.FinishLoading;
  store.Free;
end;

function TAuSoundList.SaveToStore(AStore: TAcStoreNode): TAcStoreNode;
var
  i: integer;
begin
  if AStore <> nil then  
    result := AStore.Add
  else
    result := TAcStoreNode.Create(nil);

  result.Name := 'soundlist';

  for i := 0 to Count - 1 do
    Items[i].SaveItemToStore(result);
end;

procedure TAuSoundList.SaveToStream(AStream: TStream);
begin
  with SaveToStore(nil) do
  begin
    try
      SaveToStream(AStream);
    finally
      Free;
    end;
  end;
end;

procedure TAuSoundList.LoadFromFile(AFile: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAuSoundList.SaveToFile(AFile: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;                

{ TAuStreamedSound }

constructor TAuStreamedSound.Create(A3DAudio: TAu3DAudio);
begin
  FParent := A3DAudio;
  FFilterAdapter := TAu3DSoundFilterAdapter.Create(FParent.Renderer);
  inherited Create(FParent.Audio, FFilterAdapter);
end;

destructor TAuStreamedSound.Destroy;
begin
  inherited;
  FreeAndNil(FFilterAdapter);
end;

function TAuStreamedSound.GetSound: TAu3DStreamedSound;
begin
  result := nil;
  if FFilterAdapter <> nil then
    result := FFilterAdapter.Sound;
end;

end.
