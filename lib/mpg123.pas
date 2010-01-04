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

File: mpg123.pas
Author: Andreas Stöckel
}

{This library is an partial translation of the mpg123.h header for libmpg123.
 libmg123 is loaded dynamically. To load it, call the "InitMPG123" function,
 when exiting your program, call the "FinalizeMPG123" function.}
unit mpg123;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AcSysUtils;
  
const
{$IFDEF WIN32}
  MPG123_LibName = 'libmpg123.dll';
{$ELSE}
  MPG123_LibName = 'libmpg123.so';
{$ENDIF}

type
  PMPG123Handle = Pointer;

  Tmpg123_readproc = function(fd: integer; buf: PByte; count: integer): integer;cdecl;
  Tmpg123_seekproc = function(fd: integer; count: integer; offset: integer): integer;cdecl; 

const
  MPG123_VERBOSE = 0;
  MPG123_FLAGS = 1;
  MPG123_ADD_FLAGS = 2;
  MPG123_FORCE_RATE = 3;
  MPG123_DOWN_SAMPLE = 4;
  MPG123_RVA = 5;
  MPG123_DOWNSPEED = 6;
  MPG123_UPSPEED = 7;
  MPG123_START_FRAME = 8;
  MPG123_DECODE_FRAMES = 9;
  MPG123_ICY_INTERVAL = 10;
  MPG123_OUTSCALE = 11;
  MPG123_TIMEOUT = 12;
  MPG123_REMOVE_FLAGS = 13;
  MPG123_RESYNC_LIMIT = 14;
  MPG123_INDEX_SIZE = 15;
  MPG123_PREFRAMES = 16;

  MPG123_FORCE_MONO = $7;
  MPG123_MONO_LEFT = $1;
  MPG123_MONO_RIGHT = $2;
  MPG123_MONO_MIX = $4;
  MPG123_FORCE_STEREO = $8;
  MPG123_FORCE_8BIT = $10;
  MPG123_QUIET = $20;
  MPG123_GAPLESS = $40;
  MPG123_NO_RESYNC = $80;
  MPG123_SEEKBUFFER = $100;
  MPG123_FUZZY = $200;
  MPG123_FORCE_FLOAT = $400;
  MPG123_PLAIN_ID3TEXT = $800;

  MPG123_RVA_OFF = 0;
  MPG123_RVA_MIX = 1;
  MPG123_RVA_ALBUM = 2;
  MPG123_RVA_MAX = 2;

  MPG123_DONE = -12;
  MPG123_NEW_FORMAT = -11;
  MPG123_NEED_MORE = -10;
  MPG123_ERR = -1;
  MPG123_OK = 0;
  MPG123_BAD_OUTFORMAT = 1;
  MPG123_BAD_CHANNEL = 2;
  MPG123_BAD_RATE = 3;
  MPG123_ERR_16TO8TABLE = 4;
  MPG123_BAD_PARAM = 5;
  MPG123_BAD_BUFFER = 6;
  MPG123_OUT_OF_MEM = 7;
  MPG123_NOT_INITIALIZED = 8;
  MPG123_BAD_DECODER = 9;
  MPG123_BAD_HANDLE = 10;
  MPG123_NO_BUFFERS = 11;
  MPG123_BAD_RVA = 12;
  MPG123_NO_GAPLESS = 13;
  MPG123_NO_SPACE = 14;
  MPG123_BAD_TYPES = 15;
  MPG123_BAD_BAND = 16;
  MPG123_ERR_NULL = 17;
  MPG123_ERR_READER = 18;
  MPG123_NO_SEEK_FROM_END = 19;
  MPG123_BAD_WHENCE = 20;
  MPG123_NO_TIMEOUT = 21;
  MPG123_BAD_FILE = 22;
  MPG123_NO_SEEK = 23;
  MPG123_NO_READER = 24;
  MPG123_BAD_PARS = 25;
  MPG123_BAD_INDEX_PAR = 26;
  MPG123_OUT_OF_SYNC = 27;
  MPG123_RESYNC_FAIL = 28;
  MPG123_NO_8BIT = 29;
  MPG123_BAD_ALIGN = 30;
  MPG123_NULL_BUFFER = 31;
  MPG123_NO_RELSEEK = 32;
  MPG123_NULL_POINTER = 33;
  MPG123_BAD_KEY = 34;
  MPG123_NO_INDEX = 35;
  MPG123_INDEX_FAIL = 36;
  MPG123_BAD_DECODER_SETUP = 37;
  MPG123_MISSING_FEATURE = 38;
  MPG123_BAD_VALUE = 39;
  MPG123_LSEEK_FAILED = 40;

  MPG123_ENC_8 = $00f;
  MPG123_ENC_16 = $040;
  MPG123_ENC_32 = $100;
  MPG123_ENC_SIGNED = $080;
  MPG123_ENC_FLOAT = $e00;
  MPG123_ENC_SIGNED_16 = MPG123_ENC_16 or MPG123_ENC_SIGNED or $10;
  MPG123_ENC_UNSIGNED_16 = MPG123_ENC_16 or $20;
  MPG123_ENC_UNSIGNED_8 = $01;
  MPG123_ENC_SIGNED_8 = MPG123_ENC_SIGNED or $02;
  MPG123_ENC_ULAW_8 = $04;
  MPG123_ENC_ALAW_8 = $08;
  MPG123_ENC_SIGNED_32 = MPG123_ENC_32 or MPG123_ENC_SIGNED or $1000;
  MPG123_ENC_UNSIGNED_32 = MPG123_ENC_32 or $2000;
  MPG123_ENC_FLOAT_32 = $200;
  MPG123_ENC_FLOAT_64 = $400;
  MPG123_ENC_ANY =
    MPG123_ENC_SIGNED_16  or MPG123_ENC_UNSIGNED_16 or MPG123_ENC_UNSIGNED_8 or
    MPG123_ENC_SIGNED_8   or MPG123_ENC_ULAW_8      or MPG123_ENC_ALAW_8 or
    MPG123_ENC_SIGNED_32  or MPG123_ENC_UNSIGNED_32 or MPG123_ENC_FLOAT_32 or
    MPG123_ENC_FLOAT_64;
    
  MPG123_MONO = 1;
  MPG123_STEREO = 2;

  MPG123_LEFT = $1;
  MPG123_RIGHT = $2;
  MPG123_LR = $3;

  MPG123_CBR = 0;
  MPG123_VBR = 1;
  MPG123_ABR = 2;

  MPG123_1_0 = 0;
  MPG123_2_0 = 1;
  MPG123_2_5 = 2;

  MPG123_M_STEREO = 0;
  MPG123_M_JOINT = 1;
  MPG123_M_DUAL = 2;
  MPG123_M_MONO = 3;

  MPG123_CRC = $1;
  MPG123_COPYRIGHT = $2;
  MPG123_PRIVATE = $4;
  MPG123_ORIGINAL = $8;

  MPG123_ACCURATE = 1;

  mpg123_text_unknown = 0;
  mpg123_text_utf8 = 1;
  mpg123_text_latin1 = 2;
  mpg123_text_icy = 3;
  mpg123_text_cp1252 = 4;
  mpg123_text_utf16 = 5;
  mpg123_text_utf16bom = 6;
  mpg123_text_utf16be = 7;
  mpg123_text_max = 7;

  mpg123_id3_latin1 = 0;
  mpg123_id3_utf16bom = 1;
  mpg123_id3_utf16be = 2;
  mpg123_id3_utf8 = 3;
  mpg123_id3_enc_max = 3;

var
  mpg123_close: function(mh: PMPG123Handle): integer; cdecl;
  mpg123_delete: function(mh: PMPG123Handle): integer; cdecl;
  mpg123_exit: procedure; cdecl;
  mpg123_init: procedure; cdecl;
  mpg123_new: function(decoders: PAnsiChar; error: PInteger): PMPG123Handle; cdecl;
  mpg123_param: function(mh: PMPG123Handle; param: Integer; value: Integer;
    fvalue: double): integer; cdecl;
  mpg123_getparam: function(mh: PMPG123Handle; param: Integer; value: Integer;
    fvalue: double): integer; cdecl;
  mpg123_open_feed: function(mh: PMPG123Handle): integer; cdecl;
  mpg123_open_fd: function(mh: PMPG123Handle; fd: integer): integer; cdecl;
  mpg123_decode: function(mh: PMPG123Handle; const inmemory: PByte;
    inmemsize: Cardinal; outmemory: PByte; outmemsize: Cardinal; done: PCardinal): Integer; cdecl;
  mpg123_read: function(mh: PMPG123Handle; outmemory: PByte;
    outmemsize: Cardinal; done: PCardinal): Integer; cdecl; 
  mpg123_getformat: function(mh: PMPG123Handle;
  	rate: PInteger; channels: PInteger; encoding: PInteger): integer;cdecl;
  mpg123_tell: function(mh: PMPG123Handle): Cardinal;cdecl;
  mpg123_replace_reader: function(mh: PMPG123Handle; readproc: Tmpg123_readproc;
    seekproc: Tmpg123_seekproc): integer;cdecl;
  mpg123_scan: function(mh: PMPG123Handle): Cardinal;cdecl;
  mpg123_length: function(mh: PMPG123Handle): Cardinal;cdecl;
  mpg123_seek: function(mh: PMPG123Handle; sampleoff: integer; whence: integer): integer;cdecl; 

function InitMPG123(ALib: string = ''): boolean;
procedure FinalizeMPG123;

implementation

var
  lib_handle: THandle = 0;

procedure FinalizeMPG123;
begin
  if lib_handle <> 0 then
  begin
    mpg123_exit;    
    AcFreeLibrary(lib_handle);    
  end;
end; 

function InitMPG123(ALib: string = ''): boolean;
begin
  result := true;

  if ALib = '' then
    ALib := MPG123_LibName;

  lib_handle := AcLoadLibrary(MPG123_LibName);
  if lib_handle <> 0 then
  begin
    mpg123_close := AcGetProcAddress(lib_handle, 'mpg123_close');
    mpg123_delete := AcGetProcAddress(lib_handle, 'mpg123_delete');
    mpg123_exit := AcGetProcAddress(lib_handle, 'mpg123_exit');
    mpg123_init := AcGetProcAddress(lib_handle, 'mpg123_init');
    mpg123_new := AcGetProcAddress(lib_handle, 'mpg123_new');
    mpg123_param := AcGetProcAddress(lib_handle, 'mpg123_param');
    mpg123_getparam := AcGetProcAddress(lib_handle, 'mpg123_getparam');
    mpg123_open_feed := AcGetProcAddress(lib_handle, 'mpg123_open_feed');
    mpg123_open_fd := AcGetProcAddress(lib_handle, 'mpg123_open_fd');
    mpg123_decode := AcGetProcAddress(lib_handle, 'mpg123_decode');
    mpg123_read := AcGetProcAddress(lib_handle, 'mpg123_read');
    mpg123_getformat := AcGetProcAddress(lib_handle, 'mpg123_getformat');
    mpg123_tell := AcGetProcAddress(lib_handle, 'mpg123_tell');
    mpg123_replace_reader := AcGetProcAddress(lib_handle, 'mpg123_replace_reader');
    mpg123_scan := AcGetProcAddress(lib_handle, 'mpg123_scan');
    mpg123_length := AcGetProcAddress(lib_handle, 'mpg123_length');
    mpg123_seek := AcGetProcAddress(lib_handle, 'mpg123_seek');  

    if @mpg123_init <> nil then
    begin
      mpg123_init;
      result := true;
    end else
    begin
      AcFreeLibrary(lib_handle);
      lib_handle := 0;
    end;
  end;
end;

end.
