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
  buf = 1024 * 256;

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
