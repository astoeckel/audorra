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

File: AuCDAudio.pas
Author: Andreas Stöckel
}

{Adds support for audio CDs to Audorra (windows only).}
unit AuCDAudio;

interface

uses
  SysUtils, Classes,
  win_cdrom,
  AcSyncObjs, AcStrUtils, AcPersistent, AcBuffer,
  AuUtils, AuTypes, AuProtocolClasses, AuDecoderClasses, AuWAV, AuWAVFormat;

type
  TAuCDThread = class(TThread)
    private
      FBuf: TAcBuffer;
      FCritSect: TAcCriticalSection;
      FCDROM: Tcdrom;
      FReader: PBlockRead;
      FBufSize: integer;
    protected
      procedure Execute;override;
    public
      constructor Create(ABuf: TAcBuffer; ACritSect: TAcCriticalSection;
        ACDROM: Tcdrom; AReader: PBlockRead; ABufSize: integer);
      destructor Destroy;override;
  end;
  
  TAuCDDAProtocol = class(TAuURLProtocol)
    private
      FBuf: TAcBuffer;
      FCDThread: TAuCDThread;
      FCritSect: TAcCriticalSection;
      FCDROM: Tcdrom;
      FReader: PBlockRead;
    public
      constructor Create;
      destructor Destroy;override;
      
      function Read(ABuf: PByte; ACount: Integer): Integer;override;
      function Seekable: boolean;override;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;override;
      function SupportsProtocol(const AName: string):boolean;override;
      procedure Open(AUrl: string);override;
  end;

  TAuCDADecoder = class(TAuWAVDecoder)
    private
      FCDDAProtocol: TAuCDDAProtocol;
      FTrackNumber: integer;
      FURL: string;
      function ParseCDDA(AProtocol: TAuProtocol): boolean;
    public
      constructor Create(AProtocol: TAuProtocol);override;
      destructor Destroy;override;

      function OpenDecoder: boolean;override;
  end;

const
  RIFF_CDDA = $41444443;  

implementation

{ TAuCDDAProtocol }

constructor TAuCDDAProtocol.Create;
begin
  inherited Create;

  FBuf := TAcBuffer.Create;
  FCritSect := TAcCriticalSection.Create;
  FCDROM := Tcdrom.Create;
end;

destructor TAuCDDAProtocol.Destroy;
begin
  if FCDThread <> nil then
  begin
    FCDThread.Terminate;
    FCDThread.WaitFor;
    FreeAndNil(FCDThread);
  end;

  if FReader <> nil then
    FCDROM.FinalizeBlockRead(FReader);
  FReader := nil;  

  FCDROM.Free;
  FCritSect.Free;
  FBuf.Free;
  inherited Destroy;
end;

procedure TAuCDDAProtocol.Open(AUrl: string);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  track_number: integer;
  track: Pcdrom_track;
  c: Cardinal;
  hdr: TAuWAVFmt;
  wait: Cardinal;

begin
  ParseURL(AUrl, Prot, User, Pass, Host, Port, Path, Para);

  track_number := StrToInt(Copy(Path, 2));

  if FCDROM.OpenDrive(Host[1]) then
  begin
    track := FCDROM.TOC.TrackNumberd(track_number);
    if track <> nil then
    begin
      FReader := FCDROM.InitBlockRead(track^);
      if FReader <> nil then
      begin
        FBuf.Clear;

        //Write the WAVE Header to the bufffer
        c := RIFF_HDR; FBuf.Write(PByte(@c), 4);
        c := track^.length +  40; FBuf.Write(PByte(@c), 4);
        c := RIFF_WAVE; FBuf.Write(PByte(@c), 4);
        c := RIFF_FMT; FBuf.Write(PByte(@c), 4);

        //Write the FMT Header
        hdr.HeadSize := SizeOf(hdr);
        hdr.FormatTag := 1;
        hdr.Channels := 2;
        hdr.SampleRate := 44100;
        hdr.BytesPSec := 4 * 44100;
        hdr.BlockAlign := 4;
        hdr.BitsPSmp := 16;
        FBuf.Write(PByte(@hdr), SizeOf(hdr));

        //Write the DATA header
        c := RIFF_DATA; FBuf.Write(PByte(@c), 4);
        c := track^.length;
        FBuf.Write(PByte(@c), 4);

        //Start the read thread
        FCDThread := TAuCDThread.Create(FBuf, FCritSect, FCDROM, FReader,
          FReader^.block_size * 10);

        //Wait until the buffer is filled
        wait := track^.length + 40;
        if wait > Integer(FReader^.block_size) * 10 div 2 then
          wait := Integer(FReader^.block_size) * 10 div 2;

        while FBuf.Filled < Integer(wait) do
          Sleep(1);
      end;
    end;
  end;
end;

function TAuCDDAProtocol.Read(ABuf: PByte; ACount: Integer): Integer;
begin
  FCritSect.Enter;
  try
    result := FBuf.Read(ABuf, ACount);
  finally
    FCritSect.Leave;
  end;
end;

function TAuCDDAProtocol.Seek(ASeekMode: TAuProtocolSeekMode;
  ACount: Int64): Int64;
begin
  result := 0;
end;

function TAuCDDAProtocol.Seekable: boolean;
begin
  result := false;
end;

function TAuCDDAProtocol.SupportsProtocol(const AName: string): boolean;
begin
  result := AName = 'cdda';
end;

{ TAuCDThread }

constructor TAuCDThread.Create(ABuf: TAcBuffer; ACritSect: TAcCriticalSection;
  ACDROM: Tcdrom; AReader: PBlockRead; ABufSize: integer);
begin
  inherited Create(false);

  //Copy the addresses of the needed objects
  FBuf := ABuf;
  FCritSect := ACritSect;
  FCDROM := ACDROM;
  FReader := AReader;
  FBufSize := ABufSize;
end;

destructor TAuCDThread.Destroy;
begin
  inherited;
end;

procedure TAuCDThread.Execute;
var
  read: Cardinal;
  buf: PByte;
begin
  //Reserve enough memory for the reader
  GetMem(buf, FReader^.block_size);
  try
    while not Terminated do
    begin
      if FBuf.Filled < FBufSize div 2 then
      begin
        //Read the next block
        read := FCDROM.ReadTrackBlock(FReader, buf);

        if read > 0 then
        begin
          FCritSect.Enter;
          try
            FBuf.Write(buf, read);
          finally
            FCritSect.Leave;
          end;
        end;
      end else
        Sleep(1);
    end;
  finally
    FreeMem(buf, FReader^.block_size);
  end;
end;

{ TAuCDADecoder }

constructor TAuCDADecoder.Create(AProtocol: TAuProtocol);
var
  letter: Char;
begin
  FTrackNumber := -1;

  //Try to obtain the file url from the protocol
  FURL := '';
  if AProtocol is TAuStreamProtocol then
    FURL := TAuStreamProtocol(AProtocol).URL;
  if AProtocol is TAuURLProtocol then
    FURL := TAuURLProtocol(AProtocol).URL;

  if FURL <> '' then
  begin
    ParseCDDA(AProtocol);

    //Get the drive letter
    letter := FURL[1]; //!

    if FTrackNumber > 0 then
    begin
      //Create a new CDDA protocol and open the track described in the CDA File
      FCDDAProtocol := TAuCDDAProtocol.Create;
      FCDDAProtocol.Open('cdda://'+letter+'/'+IntToStr(FTrackNumber));

      //Open the WAVE-Decoder with the TAuCDDAProtocol
      inherited Create(FCDDAProtocol);

      exit;
    end;
  end;

  inherited Create(AProtocol);
end;

destructor TAuCDADecoder.Destroy;
begin
  //Free the TAuCDDAProtocol instance
  FreeAndNil(FCDDAProtocol);
  
  inherited;
end;

function TAuCDADecoder.OpenDecoder: boolean;
begin
  //Return false if the CDA-File couldn't be opened
  result := false;

  if FTrackNumber > -1 then
    result := inherited OpenDecoder;
end;

function TAuCDADecoder.ParseCDDA(AProtocol: TAuProtocol): boolean;
var
  buf: PByte;
  read: integer;
  pc: PCardinal;
  pw: PWord;
begin
  result := false;

  //Reserve some memory for the CDA file - they usually have a size of 44Byte
  GetMem(buf, 44);
  try
    //Read the CDA file to memory
    read := AProtocol.Read(buf, 44);

    //Check whether 44 Bytes had been read.
    if read = 44 then
    begin
      //Check the "RIFF" header
      pc := PCardinal(buf);
      if pc^ <> RIFF_HDR then exit;

      //Check the file size
      inc(pc);
      if pc^ <> 36 then exit;

      //Check the CDA chunk
      inc(pc);
      if pc^ <> RIFF_CDDA then exit;

      //Check the FMT chunk
      inc(pc);
      if pc^ <> RIFF_FMT then exit;

      //Ok... Now we can be quite sure that this really is a CDA file.
      //Let's seek to the position of the track number and read it 
      pw := PWord(buf);
      inc(pw, 11);
      FTrackNumber := pw^;

      //Return true
      result := true;
    end;
  finally
    FreeMem(buf, 44);
  end;
end;

function CreateCDDAProtocol: TAuCDDAProtocol;
begin
  result := TAuCDDAProtocol.Create;
end;

function CreateCDADecoder(AProtocol: TAuProtocol): TAuCDADecoder;
begin
  result := TAuCDADecoder.Create(AProtocol);
end;


initialization
  AcRegSrv.RegisterClass(TAuCDDAProtocol, @CreateCDDAProtocol);
  AcRegSrv.RegisterClass(TAuCDADecoder, @CreateCDADecoder);

end.
