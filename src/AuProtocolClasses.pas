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

File: AuProtocolClasses.pas
Author: Andreas Stöckel
}

{Contains the audorra protocol class definitions.} 
unit AuProtocolClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  AuTypes, AcPersistent;

type
  {TAuProtocolSeekMode is used to define the origin of a seek operation.}
  TAuProtocolSeekMode = (
    {aupsFromBeginning defines the beginning of the stream as the origin for
     the seek operation. Only positive seek offsets are allowed.}
    aupsFromBeginning = 0,
    {aupsFromCurrent defines the current position of the stream as origin of the
     seek operation. Positive and negative seek offsets are allowed.}
    aupsFromCurrent = 1,
    {aupsFromEnd definces the end of the stream as origin of the seek operation.
     Only negative offsets (or zero) are allowed.}
    aupsFromEnd = 2
  );

  TAuProtocol = class(TAcPersistent)
    private
      FOnDestroy: TAuNotifyEvent;
      FUrl: String;
    protected
      property OnDestroy: TAuNotifyEvent read FOnDestroy write FOnDestroy;
    public
      procedure BeforeDestruction;override;

      function Read(ABuf: PByte; ACount: Integer): Integer;virtual;abstract;
      function Seekable: boolean;virtual;abstract;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;virtual;abstract;

      property URL: string read FUrl write FUrl;
  end;

  TAuMemoryProtocol = class(TAuProtocol)
    private
      FMemory: PByte;
      FSize: integer;
      FReadPosition: integer;
      FOwnMemory: Boolean;
    public
      constructor Create(ABuf: PByte; ACount: Integer; ACopy: Boolean = true);
      destructor Destroy;override;
      
      function Read(ABuf: PByte; ACount: Integer): Integer;override;
      function Seekable: boolean;override;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;override;
  end;

  TAuStreamProtocol = class(TAuProtocol)
    private
      FStream: TStream;
      FCanSeek: boolean;
      function StreamIsSeekable: boolean;
    public
      constructor Create(AStream: TStream);

      function Read(ABuf: PByte; ACount: Integer): Integer;override;
      function Seekable: boolean;override;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;override;

      property Stream: TStream read FStream;
  end;

  TAuURLProtocol = class(TAuProtocol)
    public
      function SupportsProtocol(const AName: string):boolean;virtual;abstract;
      function Open(AUrl: string): boolean;virtual;
      procedure Close;virtual;abstract;
      function Opened: boolean;virtual;abstract;
  end;

  TAuProtocolAdapter = class(TAuProtocol)
    private
      FParent: TAuProtocol;
      procedure ParentDestroy(Sender: TObject);
    public
      constructor Create(AParent: TAuProtocol);
      destructor Destroy;override;
  end;

  //Non seekable
  TAuInitialMemAdapter = class(TAuProtocolAdapter)
    private
      FMem: PByte;
      FSize: Integer;
      FReadPos: Integer;
    public
      constructor Create(AParent: TAuProtocol; AMem: PByte; ASize: Integer);
      destructor Destroy;override;

      function Read(ABuf: PByte; ACount: Integer): Integer;override;
      function Seekable: boolean;override;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;override;
  end;

  TAuCreateURLProtocolProc = function: TAuURLProtocol;

implementation

{ TAuStreamProtocol }

constructor TAuStreamProtocol.Create(AStream: TStream);
begin
  inherited Create;

  FStream := AStream;

  //Check whether the stream is seekable
  FCanSeek := StreamIsSeekable;
end;

function TAuStreamProtocol.Read(ABuf: PByte; ACount: Integer): Integer;
begin
  result := FStream.Read(ABuf^, ACount);
end;

function TAuStreamProtocol.Seek(ASeekMode: TAuProtocolSeekMode;
  ACount: Int64): Int64;
begin
  result := FStream.Seek(ACount, TSeekOrigin(Ord(ASeekMode)));
end;

function TAuStreamProtocol.Seekable: boolean;
begin
  result := FCanSeek;
end;

type
  TSeek64_Proc = function(const Offset: Int64; Origin: TSeekOrigin): Int64 of object;
  TSeek_Proc = function(Offset: Longint; Origin: Word): Longint of object;

function TAuStreamProtocol.StreamIsSeekable: boolean;
var
  current_seek_proc_64, base_seek_proc_64: TSeek64_Proc;
  current_seek_proc, base_seek_proc: TSeek_Proc;
  base_class: TClass;
begin
  //Get the current seek implementation
  current_seek_proc_64 := FStream.Seek;
  current_seek_proc := FStream.Seek;

  //Get the old implementation of the seek method
  base_class := TStream;
  base_seek_proc_64 := TStream(@base_class).Seek;
  base_seek_proc := TStream(@base_class).Seek;

  //The seek method is implemented if the current address of the seek method
  //is unequal to the address of the TStream seek method
  result :=
    (TMethod(current_seek_proc).Code <> TMethod(base_seek_proc).Code) or
    (TMethod(current_seek_proc_64).Code <> TMethod(base_seek_proc_64).Code);
end;     

{ TAuURLProtocol }

function TAuURLProtocol.Open(AUrl: string): boolean;
begin
  result := false;
end;

{ TAuMemoryProtocol }

constructor TAuMemoryProtocol.Create(ABuf: PByte; ACount: Integer;
  ACopy: Boolean);
begin
  inherited Create;

  //If ACopy is true, copy the given memory else only store a reference to the
  //given buffer
  if ACopy then
  begin
    GetMem(FMemory, ACount);
    Move(ABuf^, FMemory^, ACount);
  end else
    FMemory := ABuf;
    
  FSize := ACount;
  FOwnMemory := ACopy;
  FReadPosition := 0; 
end;

destructor TAuMemoryProtocol.Destroy;
begin
  //Free the possibly reserved memory
  if FOwnMemory then
    FreeMem(FMemory, FSize);

  FSize := 0;
  FMemory := nil;

  inherited;
end;

function TAuMemoryProtocol.Read(ABuf: PByte; ACount: Integer): Integer;
var
  cnt: Integer;
  src: PByte;
begin
  //Calculate the maximum amount of bytes which can be read
  cnt := FSize - FReadPosition;

  //If this value is greater than ACount, limit it
  if cnt > ACount then
    cnt := ACount;
  if cnt < 0 then //should never occur
    cnt := 0;

  //Calculate a temporary read pointer and copy the data
  src := FMemory;
  inc(src, FReadPosition);
  Move(src^, ABuf^, cnt);

  //Increment the read position and return the actual read count  
  FReadPosition := FReadPosition + cnt;
  result := cnt;
end;

function TAuMemoryProtocol.Seek(ASeekMode: TAuProtocolSeekMode;
  ACount: Int64): Int64;
begin
  //Calculate the new read position
  case ASeekMode of
    aupsFromBeginning:
      FReadPosition := ACount;
    aupsFromCurrent:
      FReadPosition := FReadPosition + ACount;
    aupsFromEnd:
      FReadPosition := FSize + ACount;
  end;

  //Limit the read position
  if FReadPosition < 0 then
    FReadPosition := 0;
  if FReadPosition > FSize then
    FReadPosition := FSize;

  //Return the new position
  result := FReadPosition;
end;

function TAuMemoryProtocol.Seekable: boolean;
begin
  result := true;
end;

{ TAuProtocol }

procedure TAuProtocol.BeforeDestruction;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(self);

  inherited;
end;

{ TAuProtocolAdapter }

constructor TAuProtocolAdapter.Create(AParent: TAuProtocol);
begin
  inherited Create;

  //Set the parent on destroy handler
  FParent := AParent;
  FParent.OnDestroy := ParentDestroy;  
  FUrl := AParent.URL;
end;

destructor TAuProtocolAdapter.Destroy;
begin
  //Clean all references to parent
  FParent.OnDestroy := nil;
  FParent := nil;

  inherited;
end;

procedure TAuProtocolAdapter.ParentDestroy(Sender: TObject);
begin
  Free;
end;

{ TAuInitialMemAdapter }

constructor TAuInitialMemAdapter.Create(AParent: TAuProtocol; AMem: PByte;
  ASize: Integer);
begin
  inherited Create(AParent);

  //Reserve ASize bytes of memory and copy the content of AMem to it
  FSize := ASize;
  GetMem(FMem, FSize);
  Move(AMem^, FMem^, FSize);

  FReadPos := 0;
end;

destructor TAuInitialMemAdapter.Destroy;
begin
  //Destroy all reserved memory
  if FMem <> nil then
    FreeMem(FMem);
  FMem := nil;
  FSize := 0;
  
  inherited;
end;

function TAuInitialMemAdapter.Read(ABuf: PByte; ACount: Integer): Integer;
var
  ptar, psrc: PByte;
  cnt, maxcnt: Integer;
  read: integer;
begin
  ptar := ABuf;
  cnt := ACount;
  read := 0;

  if FReadPos < FSize then
  begin
    //Get the maximum count of bytes which can be read from memory
    maxcnt := FSize - FReadPos;
    if cnt < maxcnt then
      maxcnt := cnt;

    //Setup the source pointer
    psrc := FMem;
    inc(psrc, FReadPos);

    //Copy maxcnt bytes from psrc to ptar
    Move(psrc^, ptar^, maxcnt);

    //Increment the target pointer and increment the read count/decrement the
    //count of bytes which have to be read
    inc(ptar, maxcnt);
    read := read + maxcnt;
    cnt := cnt - maxcnt;
  end;

  //If all buffer bytes have been read, ask the adapted protocol for data
  if cnt > 0 then
    read := read + FParent.Read(ptar, cnt);

  result := read;
  FReadPos := FReadPos + read;  
end;

function TAuInitialMemAdapter.Seek(ASeekMode: TAuProtocolSeekMode;
  ACount: Int64): Int64;
begin
  result := 0;
end;

function TAuInitialMemAdapter.Seekable: boolean;
begin
  result := false;
end;

end.

