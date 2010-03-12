program custom_audio;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,

  {$IFDEF WIN32}AuDirectSound,{$ELSE}AuOpenAL,{$ENDIF}

  AcTypes,
  AuAudio, AuDriverClasses, AuTypes, AuEffects;

type
  TAuApp = class
    private
      FOutput: TAuStreamDriver;
      FAudio: TAuAudio;
      FTime: Double;
      FFreq: Double;
      FAmplitude: Double;
      function ReadProc(ABuf: PByte; ASize: Cardinal;
        var ASyncData: TAuSyncData): Cardinal;
    public
      constructor Create;
      procedure Run;
      destructor Destroy;override;
  end;


{ TAuApp }

constructor TAuApp.Create;
begin
  inherited;

  FAudio := TAuAudio.Create;
  FFreq := 440;
  FAmplitude := 0.5; 
end;

destructor TAuApp.Destroy;
begin
  FAudio.Free;
  inherited;
end;

function TAuApp.ReadProc(ABuf: PByte; ASize: Cardinal;
  var ASyncData: TAuSyncData): Cardinal;
var
  p: PAcInt16;
  i: integer;
begin
  p := PAcInt16(ABuf);
  for i := 0 to ASize div 2 - 1 do //2 Byte per sample (mono)
  begin
    p^ := Round(AuWaveformSine(FTime * FFreq) * High(AcInt16) * FAmplitude);
    inc(p);
    FTime := FTime + 1 / 44100;
  end;

  result := ASize;
end;

procedure TAuApp.Run;
begin
  Writeln('Initializing Audorra...');
  if FAudio.Initialize then
  begin
    Writeln('Opening output driver.');
    FOutput := FAudio.Driver.CreateStreamDriver(FAudio.StandardDeviceID,
      AuAudioParametersEx(44100, 1, 16));

    if Assigned(FOutput) then
    begin
      try
        if FOutput.Open then
        begin
          FOutput.Play;
          Writeln('Playing a sine waveform at 440Hz.');
          while true do
            if not FOutput.Idle(ReadProc) then
              Sleep(1);
        end;
      finally
        FOutput.Free;
      end;
    end else
      Writeln('Output driver could not be created.');
  end else
    Writeln(FAudio.GetLastError);
end;

var
  app: TAuApp;
begin
  app := TAuApp.Create;
  try
    app.Run;
  finally
    app.Free;
  end;
end.
