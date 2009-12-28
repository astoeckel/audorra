unit wasapi_interface;

interface

uses
  AcSysUtils;

//WASAPI Interface DLL header
type
  PWasapiInstance = Pointer;
  TWasapiReadCallback = procedure(
    pSender: Pointer; Buf: Pointer; BufSize: integer);cdecl;

var
  wasapi_open: function(psender: Pointer; channels, bitdepth,
    frequency: integer): PWasapiInstance; cdecl;
  wasapi_idle: function(pinst: PWasapiInstance;
    pcallback: TWasapiReadCallback): Integer; cdecl;
  wasapi_close: procedure(pinst: PWasapiInstance); cdecl;
  wasapi_start: procedure(pinst: PWasapiInstance); cdecl;
  wasapi_stop: procedure(pinst: PWasapiInstance); cdecl;
  wasapi_reset: procedure(pinst: PWasapiInstance); cdecl;
  wasapi_available: function: Integer; cdecl;
  

const
  wasapi_interface_dll = 'wasapi_interface.dll';

function InitWASAPI(ADLL: string = ''): boolean;
procedure FinalizeWASAPI;

implementation

var
  hndl: THandle;

function InitWASAPI(ADLL: string = ''): boolean;
begin
  if hndl <> 0 then
  begin
    result := true;
    exit;
  end;
    
  result := false;
  if ADLL = '' then
    ADLL := wasapi_interface_dll;

  hndl := AcLoadLibrary(ADLL);
  if hndl <> 0 then
  begin
    @wasapi_open := AcGetProcAddress(hndl, 'wasapi_open');
    @wasapi_idle := AcGetProcAddress(hndl, 'wasapi_idle');
    @wasapi_close := AcGetProcAddress(hndl, 'wasapi_close');
    @wasapi_start := AcGetProcAddress(hndl, 'wasapi_start');
    @wasapi_stop := AcGetProcAddress(hndl, 'wasapi_stop');
    @wasapi_reset := AcGetProcAddress(hndl, 'wasapi_reset');
    @wasapi_available := AcGetProcAddress(hndl, 'wasapi_available');
    
    result := (@wasapi_open <> nil) and (@wasapi_idle <> nil) and
      (@wasapi_close <> nil) and (@wasapi_start <> nil) and
      (@wasapi_stop <> nil) and (@wasapi_reset <> nil) and
      (@wasapi_available <> nil) and (wasapi_available() = 1);
    if not result then
      FinalizeWASAPI;
  end;  
end;

procedure FinalizeWASAPI;
begin
  AcFreeLibrary(hndl);
end;

end.
