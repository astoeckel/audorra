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
GNU General Public License license (the GPL License), in which case the provisions of
GPL License are applicable instead of those above. If you wish to allow use
of your version of this file only under the terms of the GPL License and not
to allow others to use your version of this file under the MPL, indicate your
decision by deleting the provisions above and replace them with the notice and
other provisions required by the GPL License. If you do not delete the
provisions above, a recipient may use your version of this file under either the
MPL or the GPL License.

File: libao.pas
Author: Andreas Stöckel
}

unit libao;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AcSysUtils;

const
  AO_TYPE_LIVE = 1;
  AO_TYPE_FILE = 2;

  AO_ENODRIVER = 1;
  AO_ENOTFILE = 2;
  AO_ENOTLIVE = 3;
  AO_EBADOPTION = 4;
  AO_EOPENDEVICE = 5;
  AO_EOPENFILE = 6;
  AO_EFILEEXISTS = 7;

  AO_EFAIL = 100;

  AO_FMT_LITTLE = 1;
  AO_FMT_BIG = 2;
  AO_FMT_NATIVE = 4;

type
  PPAnsiChar = ^PAnsiChar;
  Tao_info = record
    output_type: integer;
    name: PAnsiChar;
    short_name: PAnsiChar;
    author: PAnsiChar;
    comment: PAnsiChar;
    preferred_byte_format: integer;
    priority: integer;
    options: ^PPAnsiChar;
    option_count: integer;
  end;
  Pao_info = ^Tao_info;
  PPao_info = ^Pao_info;

  Pao_functions = ^Tao_functions;

  Tao_device = record
    device_type: integer;
    driver_id: integer;
    functions: Pao_functions;
    output_file: Pointer;
    client_byte_format: integer;
    machine_byte_format: integer;
    driver_byte_format: integer;
    swap_buffer: PAnsiChar;
    swap_buffer_size: integer;
    internal: Pointer;
  end;
  Pao_device = ^Tao_device;

  Tao_sample_format = record
    bits, rate, channels, byte_format: integer;
  end;
  Pao_sample_format = ^Tao_sample_format;

  Tao_functions = record
    test: function(): integer;cdecl;
    driver_info: function(): Pao_info;cdecl;
    device_init: function(device: Pao_device): integer;cdecl;
    set_option: function(device: Pao_device; const key: PAnsiChar;
      const value: PAnsiChar): integer;cdecl;
    open: function(device: Pao_device;
      format: Pao_sample_format): integer;cdecl;
    play: function(device: Pao_device; output_samples: PAnsiChar;
      num_bytes: Cardinal): integer;cdecl;
    close: function(device: Pao_device): integer;cdecl;
    device_clear: function(device: Pao_device): integer;cdecl;
    file_extension: function():PAnsiChar;cdecl;
  end;

  Pao_option = ^Tao_option;
  Tao_option = record
    key: PAnsiChar;
    value: PAnsiChar;
    next: Pao_option;
  end;
  PPao_option = ^Pao_option;

var
  ao_append_option: function(options: PPao_option; const key: PAnsiChar;
    const value: PAnsiChar): integer;cdecl;
  ao_free_options: procedure(options: Pao_option);cdecl;
  ao_open_live: function(driver_id: integer; format: Pao_sample_format;
    option: Pao_option): Pao_device;cdecl;
  ao_open_file: function(driver_id: integer; const filename: PAnsiChar;
    overwrite: integer; format: Pao_sample_format;
    option: Pao_option): Pao_device;cdecl;
  ao_play: function(device: Pao_device; output_samples: PByte;
    num_bytes: Cardinal): integer;cdecl;
  ao_close: function(device: Pao_device): integer;cdecl;
  ao_driver_id: function(const short_name: PAnsiChar): integer;cdecl;
  ao_default_driver_id: function: integer;cdecl;
  ao_driver_info: function(driver_id: integer): Pao_info;cdecl;
  ao_driver_info_list: function(driver_count: PInteger): PPao_info;cdecl;
  ao_file_extension: function(driver_id: Integer): PAnsiChar;cdecl;
  ao_is_big_endian: function(): integer;cdecl;

const
  default_libname = 'libao.so.2';

function init_libao(libname: string = ''): boolean;
procedure finalize_libao();

function ao_sample_format(bits, rate, channels: integer): Tao_sample_format;

implementation

function ao_sample_format(bits, rate, channels: integer): Tao_sample_format;
begin
  result.bits := bits;
  result.rate := rate;
  result.channels := channels;
  result.byte_format := AO_FMT_NATIVE;
end;

var
  ao_initialize: procedure();cdecl;
  ao_shutdown: procedure();cdecl;

  handle: TAcHandle;

function init_libao(libname: string = ''): boolean;
begin
  result := true;
  if handle <> 0 then
    exit;

  if libname = '' then
    libname := default_libname;

  handle := AcLoadLibrary(libname);
  if handle <> 0 then
  begin
    ao_initialize := AcGetProcAddress(handle, 'ao_initialize');
    ao_shutdown := AcGetProcAddress(handle, 'ao_shutdown');
    ao_append_option := AcGetProcAddress(handle, 'ao_append_option');
    ao_free_options := AcGetProcAddress(handle, 'ao_free_options');
    ao_open_live := AcGetProcAddress(handle, 'ao_open_live');
    ao_open_file := AcGetProcAddress(handle, 'ao_open_file');
    ao_play := AcGetProcAddress(handle, 'ao_play');
    ao_close := AcGetProcAddress(handle, 'ao_close');
    ao_driver_id := AcGetProcAddress(handle, 'ao_driver_id');
    ao_default_driver_id := AcGetProcAddress(handle, 'ao_default_driver_id');
    ao_driver_info := AcGetProcAddress(handle, 'ao_driver_info');
    ao_driver_info_list := AcGetProcAddress(handle, 'ao_driver_info_list');
    ao_file_extension := AcGetProcAddress(handle, 'ao_file_extension');
    ao_is_big_endian := AcGetProcAddress(handle, 'ao_is_big_endian');

    //Initialize libao
    ao_initialize();
  end else
    result := false;
end;


procedure finalize_libao();
begin
  if handle <> 0 then
  begin
    //Finalize libao
    ao_shutdown();

    AcFreeLibrary(handle);
    handle := 0;
  end;
end;

end.

