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

File: win_cdrom.pas
Author: Andreas Stöckel
}

{Contains low-level cd audio access classes for windows.}
unit win_cdrom;

//Reference used: http://www.codeproject.com/KB/audio-video/SimpleAudioCD.aspx?msg=1686942

interface

uses
  Windows,
  SysUtils, Classes;

type
  //Records end declarations can be found in the Windows DDK ntddcdrm.h
  TTrackDataAddressArray = array[0..3] of Byte;

  TTrackData = packed record
    Reserved: Byte;
    ControlAdr: Byte;
    TrackNumber: Byte;
    Reserved1: Byte;
    Address: TTrackDataAddressArray;
  end;

  TCDROMTOC = packed record
    Length: Word;
    FirstTrack: Byte;
    LastTrack: Byte;
    TrackData: array[0..99] of TTrackData;
  end;


  TRAWReadInfo = packed record
    DiskOffset: Int64;
    SectorCount: Cardinal;
    TrackMode: Cardinal;
  end;

const
  IOCTL_CDROM_READ_TOC = $00024000;
  IOCTL_CDROM_RAW_READ = $0002403E;

  YellowMode2 = 0;
  XAForm2 = 1;
  CDDA = 2;

const
  sector_size = 2352;
  sector_count = 20;

type
  Tcdrom_track = record
    number: integer;
    length: Cardinal; //Length in bytes
  end;
  Pcdrom_track = ^Tcdrom_track;

  Tcdrom_toc = class(TList)
    private
      function GetItem(AIndex: integer): Tcdrom_track;
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      procedure Add(const ATrack: Tcdrom_track);
      property Items[AIndex: integer]: Tcdrom_track read GetItem; default;

      function TrackNumberd(ANB: integer): Pcdrom_track;

      function TotalLength: Cardinal;
  end;

  TBlockRead = record
    block_size: integer;
    sector_size: integer;
    track_addr: int64;
    track_length: int64;
    offset: int64;
    sectors_left: integer;
  end;
  PBlockRead = ^TBlockRead;

  Tcdrom = class
    private
      FTOC: Tcdrom_toc;
      FCDDATOC: TCDROMTOC;
      FDevice: HFILE;
    public
      constructor Create;
      destructor Destroy;override;

      function OpenDrive(ADriveLetter: Char): boolean;
      procedure CloseDrive;

      function InitBlockRead(const ATrack: Tcdrom_track): PBlockRead;
      procedure FinalizeBlockRead(ABlockRead: PBlockRead);
      procedure SeekToTrackSector(ABlockRead: PBlockRead; ASector: integer); 
      function ReadTrackBlock(ABlockRead: PBlockRead; ABuf: PByte): Cardinal;
      
      property TOC: Tcdrom_toc read FTOC;
  end;


implementation

function AddressToSectors(addr: TTrackDataAddressArray): int64;
begin
  result := addr[1] * 75 * 60 + addr[2] * 75 + addr[3] - 150;
end;

{ Tcdrom_toc }

procedure Tcdrom_toc.Add(const ATrack: Tcdrom_track);
var
  ptrack: Pcdrom_track;
begin
  New(ptrack);
  ptrack^ := ATrack;
  inherited Add(ptrack);
end;

function Tcdrom_toc.GetItem(AIndex: integer): Tcdrom_track;
begin
  result := Pcdrom_track(inherited Items[AIndex])^;
end;

function Tcdrom_toc.TrackNumberd(ANB: integer): Pcdrom_track;
var
  i: integer;
begin
  result := nil;

  for i := 0 to Count - 1 do
  begin
    if Items[i].number = ANB then
      result := inherited Items[i];
  end;                        
end;

procedure Tcdrom_toc.Notify(ptr: Pointer; action: TListNotification);
begin
  if action = lnDeleted then
    Dispose(Pcdrom_track(ptr));
end;

function Tcdrom_toc.TotalLength: Cardinal;
var
  i: integer;
begin
  result := 0;

  for i := 0 to Count - 1 do
    result := result + Items[i].length;
end;

{ Tcdrom }

constructor Tcdrom.Create;
begin
  inherited Create;
  FTOC := Tcdrom_toc.Create;
end;

destructor Tcdrom.Destroy;
begin
  CloseDrive;
  FTOC.Free;
  inherited Destroy;
end;

function Tcdrom.OpenDrive(ADriveLetter: Char): boolean;
var
  dummy: Cardinal;
  track: Tcdrom_track;
  i: integer;
begin
  result := false;
  CloseDrive;

  //Open the CD-Audio
  FDevice := CreateFile(PChar('\\.\'+ADriveLetter+':'), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if FDevice <> INVALID_HANDLE_VALUE then
  begin

    //Read the table of contents
    if DeviceIoControl(FDevice, IOCTL_CDROM_READ_TOC, nil, 0, @FCDDATOC, SizeOf(FCDDATOC), dummy,
      nil) then
    begin
      for i := FCDDATOC.FirstTrack to FCDDATOC.LastTrack do
      begin
        //Read the track information
        track.number := i;
        track.length :=
          (AddressToSectors(FCDDATOC.TrackData[i - FCDDATOC.FirstTrack + 1].Address) -
           AddressToSectors(FCDDATOC.TrackData[i - FCDDATOC.FirstTrack].Address)) * sector_size;

        //Add the track to the table of content
        FTOC.Add(track);
      end;
        
      result := true
    end else
      CloseHandle(FDevice);
  end
  else
    FDevice := 0;   
end;

procedure Tcdrom.CloseDrive;
begin
  if FDevice <> 0 then
    CloseHandle(FDevice);

  FDevice := 0;
  FTOC.Clear;
end;

function Tcdrom.InitBlockRead(const ATrack: Tcdrom_track): PBlockRead;
begin
  result := nil;
  if FDevice <> 0 then
  begin
    New(result);
    result^.block_size := sector_size * sector_count;
    result^.sector_size := sector_size;
    result^.track_addr :=
      AddressToSectors(FCDDATOC.TrackData[ATrack.number - FCDDATOC.FirstTrack].Address);
    result^.track_length :=
      AddressToSectors(FCDDATOC.TrackData[ATrack.number - FCDDATOC.FirstTrack + 1].Address) -
      AddressToSectors(FCDDATOC.TrackData[ATrack.number - FCDDATOC.FirstTrack].Address);

    SeekToTrackSector(result, 0);
  end;
end;

procedure Tcdrom.FinalizeBlockRead(ABlockRead: PBlockRead);
begin
  Dispose(ABlockRead);
end;

function Tcdrom.ReadTrackBlock(ABlockRead: PBlockRead; ABuf: PByte): Cardinal;
var
  info: TRAWReadInfo;
begin
  result := 0;
  if FDevice <> 0 then
  begin
    if ABlockRead^.sectors_left > 0 then
    begin
      //Fill the TRAWReadInfo record
      if ABlockRead^.sectors_left > sector_count then
        info.SectorCount := sector_count
      else
        info.SectorCount := ABlockRead^.sectors_left;
      info.TrackMode := CDDA;
      info.DiskOffset := ABlockRead^.offset;

      //Read the data
      if not DeviceIoControl(FDevice, IOCTL_CDROM_RAW_READ, @info, SizeOf(info), ABuf,
        info.SectorCount * 2352, result, nil) then
        RaiseLastOSError;

      //Calculate the next read address
      ABlockRead^.sectors_left := ABlockRead^.sectors_left - Integer(info.SectorCount);
      ABlockRead^.offset := ABlockRead^.offset + info.SectorCount * 2048;
    end;
  end;
end;

procedure Tcdrom.SeekToTrackSector(ABlockRead: PBlockRead; ASector: integer);
begin
  if ASector < ABlockRead^.track_length then
  begin
    ABlockRead^.offset := (ABlockRead^.track_addr + ASector) * 2048;
    ABlockRead^.sectors_left := ABlockRead^.track_length - ASector;
  end;
end;

end.
