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
  SysUtils, Classes,
  AcDataStore, AcPersistent, AcRegUtils,
  AuTypes, AuUtils, AuDriverClasses, AuDecoderClasses,
  AuAudio, AuFilterGraph, AuSyncUtils,
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
      function GetListener: TAu3DListener;
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
      property Listener: TAu3DListener read GetListener;
  end;

  TAuSoundList = class;

  TAuStaticSound = class(TAuCustomAudioObject)
    private
      FSound: TAu3DSound;
      FMs: TMemoryStream;
      FFormat: TAuAudioParametersEx;
      FLoop: boolean;
      FName: AnsiString;
      FOwner: Pointer;
      F3DAudio: TAu3DAudio;
      FFinished: boolean;
      procedure FreeComponents;
      function DecodeStream: boolean;
      procedure SetLoop(AValue: boolean);
      function CreateSoundObj: boolean;
    protected
      function GetLength: integer; override;
    public
      constructor Create(A3DAudio: TAu3DAudio);
      destructor Destroy;override;

      function Open: boolean;override;
      procedure Close;override;

      function SaveItemToStore(AStore: TAcStoreNode): TAcStoreNode;
      procedure LoadItemFromStore(AStore: TAcStoreNode);

      property Loop: boolean read FLoop write SetLoop;
      property Name: AnsiString read FName write FName;
      property Owner: Pointer read FOwner write FOwner;
      property Format: TAuAudioParametersEx read FFormat;
      property DecodedData: TMemoryStream read FMs;
      property Sound: TAu3DSound read FSound write FSound;
  end;

  TAuSoundList = class(TList)
    private
      F3DAudio: TAu3DAudio;
      function GetItem(AIndex: integer): TAuStaticSound;
      procedure SetItem(AIndex: integer; AItem: TAuStaticSound);
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      constructor Create(A3DAudio: TAu3DAudio);
      destructor Destroy;override;

      function IndexOf(AName: AnsiString): integer;overload;
      function IndexOf(AObj: TAuStaticSound): integer;overload;
      function Find(AName: AnsiString): TAuStaticSound;
      function AddNew(AName: AnsiString): TAuStaticSound;

      procedure SaveToStream(AStream: TStream);
      procedure LoadFromStream(AStream: TStream);
      procedure SaveToFile(AFile: string);
      procedure LoadFromFile(AFile: string);
      function SaveToStore(AStore: TAcStoreNode): TAcStoreNode;
      procedure LoadFromStore(AStore: TAcStoreNode);

      property Parent: TAu3DAudio read F3DAudio;
      property Items[AIndex: integer]: TAuStaticSound read GetItem write SetItem; default;
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

{ TAuStaticSound }

constructor TAuStaticSound.Create(A3DAudio: TAu3DAudio);
begin
  inherited Create(A3DAudio.Audio);
  F3DAudio := A3DAudio;
end;

destructor TAuStaticSound.Destroy;
begin
  SetState(aupsClosed, false);
  Close;
  inherited;
end;

procedure TAuStaticSound.FreeComponents;
begin
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
end;

function TAuStaticSound.GetLength: integer;
begin
  result := inherited GetLength;

  if FMs <> nil then
    result := round((FMs.Size * 1000)/  AuBytesPerSecond(FFormat));
end;

procedure TAuStaticSound_EnumDecoders(ASender: Pointer; AEntry: PAcRegisteredClassEntry);
var
  res: TAuDecoderState;
  decoder: TAuDecoder;
  pckg: TAuPacket;
  i: integer;
  s: Single;
begin
  with TAuStaticSound(ASender) do
  begin
    if not FFinished then
    begin
      decoder := TAuCreateDecoderProc(AEntry.ClassConstructor)(Protocol);
      try
        if decoder.OpenDecoder then
        begin
          FMs := TMemoryStream.Create;

          FFormat := decoder.Info;
          repeat
            res := decoder.Decode;
            if res = audsHasFrame then
            begin
              decoder.GetPacket(pckg);
              for i := 0 to (pckg.BufferSize div (Integer(FFormat.BitDepth) div 8)) - 1 do
              begin
                s := AuReadSample(pckg.Buffer, FFormat.BitDepth);
                FMs.Write(s, SizeOf(s));
              end;
            end;
          until res = audsEnd;
          FFinished := true;
        end;
      finally
        decoder.Free;
      end;
    end;
  end;
end;

function TAuStaticSound.DecodeStream: boolean;
begin
  FFinished := false;
  try
    AcRegSrv.EnumClasses(TAuDecoder, TAuStaticSound_EnumDecoders, self);
  finally
    result := FFinished;
  end;
end;

function TAuStaticSound.Open: boolean;
begin
  result := false;
  if (State = aupsLoaded) and (DecodeStream) then
  begin
    result := CreateSoundObj;
    SetState(aupsOpened);
  end;
end;

function TAuStaticSound.CreateSoundObj: boolean;
begin
  result := true;
  FSound := TAu3DSound.Create(PByte(FMs.Memory),
    FMs.Size div AuBytesPerSample(FFormat.Parameters), FFormat.Parameters);
  F3DAudio.Renderer.Sounds.Add(FSound);
end;

procedure TAuStaticSound.Close;
begin
  SetState(aupsClosed);
  FreeComponents;
end;

procedure TAuStaticSound.SetLoop(AValue: boolean);
begin
  FLoop := AValue;
  if FSound <> nil then
    FSound.Loop := FLoop;
end;

procedure TAuStaticSound.LoadItemFromStore(AStore: TAcStoreNode);
var
  fmt_node: TAcStoreNode;
  strm_node: TAcStreamNode;
begin
  Close;

  FName := AStore.StringValue('name');

  fmt_node := AStore.Nodes.ItemNamed['fmt'];
  FillChar(FFormat, SizeOf(FFormat), 0);
  if fmt_node <> nil then
  begin
    FFormat.Frequency := fmt_node.IntValue('freq', 44100);
    FFormat.BitDepth := fmt_node.IntValue('bits', 16);
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
end;

function TAuStaticSound.SaveItemToStore(AStore: TAcStoreNode): TAcStoreNode;
var
  node, fmt_node: TAcStoreNode;
  strm_node: TAcStreamNode;
begin
  node := AStore.Add('sound');
  node.Add('name', FName);

  fmt_node := node.Add('fmt');
  fmt_node.Add('freq', FFormat.Frequency);
  fmt_node.Add('bits', FFormat.BitDepth);
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


end.
