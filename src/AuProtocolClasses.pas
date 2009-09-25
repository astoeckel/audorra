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
  AcPersistent;

type
  TAuProtocolSeekMode = (
    aupsFromBeginning = 0,
    aupsFromCurrent = 1,
    aupsFromEnd = 2
  );

  TAuProtocol = class(TAcPersistent)
    public
      function Read(ABuf: PByte; ACount: Integer): Integer;virtual;abstract;
      function Seekable: boolean;virtual;abstract;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;virtual;abstract;
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
  end;

  TAuURLProtocol = class(TAuProtocol)
    public
      function SupportsProtocol(const AName: string):boolean;virtual;abstract;
      procedure Open(AUrl: string);virtual;abstract;
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

end.

