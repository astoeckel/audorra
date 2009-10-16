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

File: AuHTTP.pas
Author: Andreas Stöckel
}

{Contains an HTTP protocol handler - uses an modified version of Synapse (use httpsend.pas from the "lib" folder)}
unit AuHTTP;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
  httpsend,
  AcBuffer, AcPersistent,
  AuProtocolClasses;

type
  TAuHTTPThread = class(TThread)
    private
      FCritSect: TCriticalSection;
      FBuffer: TAcBuffer;
      FHttpSend: THTTPSend;
      FAddr: string;
      procedure ReceiveData(Sender: TObject; const AStr: AnsiString);
    protected
      procedure Execute;override;
    public
      constructor Create(ABuf: TAcBuffer; ACritSect: TCriticalSection; AAddr: string);
      destructor Destroy;override;
  end;
  
  TAuHTTPPrebufferEvent = procedure(Sender: TObject; APos: single) of object;

  TAuHTTPProtocol = class(TAuURLProtocol)
    private
      FBuffer: TAcBuffer;
      FCritSect: TCriticalSection;
      FThread: TAuHTTPThread;
      procedure WaitForBytecount(ACount: integer);
    public
      constructor Create;
      destructor Destroy;override;
      function SupportsProtocol(const AName: string): boolean;override;
      procedure Open(AUrl: string);override;
      function Read(ABuf: PByte; ACount: Integer): Integer;override;
      function Seekable: boolean;override;
      function Seek(ASeekMode: TAuProtocolSeekMode; ACount: Int64): Int64;override;
  end;

implementation

const
  buf = 1024 * 64;

{ TAuHTTPProtocol }

constructor TAuHTTPProtocol.Create;
begin
  inherited Create;

  FBuffer := TAcBuffer.Create;
  FCritSect := TCriticalSection.Create;
end;

destructor TAuHTTPProtocol.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;

  FBuffer.Free;
  FCritSect.Free;

  inherited;
end;

procedure TAuHTTPProtocol.Open(AUrl: string);
begin
  if FThread = nil then
  begin
    FThread := TAuHTTPThread.Create(FBuffer, FCritSect, AUrl);
    WaitForByteCount(buf);
  end;
end;

function TAuHTTPProtocol.Read(ABuf: PByte; ACount: Integer): Integer;
begin
  WaitForByteCount(ACount * 2);
  
  FCritSect.Enter;
  try
    result := FBuffer.Read(ABuf, ACount);
  finally
    FCritSect.Leave;
  end;
end;

function TAuHTTPProtocol.Seek(ASeekMode: TAuProtocolSeekMode;
  ACount: Int64): Int64;
begin
  result := -1;
end;

function TAuHTTPProtocol.Seekable: boolean;
begin
  result := false;
end;

function TAuHTTPProtocol.SupportsProtocol(const AName: string): boolean;
begin
  result := (AName = 'http') or (AName = 'https');
end;

procedure TAuHTTPProtocol.WaitForBytecount(ACount: integer);
var
  size: int64;
begin
  repeat
    FCritSect.Enter;
    try
      size := FBuffer.Filled;
    finally
      FCritSect.Leave;
    end;
    Sleep(1);
  until (size > ACount);
end;

{ TAuHTTPThread }

constructor TAuHTTPThread.Create(ABuf: TAcBuffer; ACritSect: TCriticalSection;
  AAddr: string);
begin
  FBuffer := ABuf;
  FCritSect := ACritSect;
  FAddr := AAddr;
  FHttpSend := THTTPSend.Create;
  FHttpSend.OnReceiveData := ReceiveData;
  FHttpSend.Headers.Add('Accept: icy-metadata:1');
  FHttpSend.UserAgent := 'Synapse/Acinerella HTTP Component';
  FHttpSend.KeepAlive := false;

  inherited Create(false);
end;

destructor TAuHTTPThread.Destroy;
begin
  FHttpSend.Free;

  inherited;
end;

procedure TAuHTTPThread.Execute;
begin
  FHttpSend.HTTPMethod('GET', FAddr);
end;

procedure TAuHTTPThread.ReceiveData(Sender: TObject; const AStr: AnsiString);
begin
  FCritSect.Enter;
  try
    FBuffer.Write(PByte(@AStr[1]), Length(AStr));
  finally
    FCritSect.Leave;
  end;

  if Terminated then  
    FHttpSend.Abort;
end;

function CreateHTTPProtocol: TAuHTTPProtocol;
begin
  result := TAuHTTPProtocol.Create;
end;

initialization
  AcRegSrv.RegisterClass(TAuHTTPProtocol, @CreateHTTPProtocol);

end.
