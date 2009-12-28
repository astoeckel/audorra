/*
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

File: wasapi_interface.h
Author: Andreas Stöckel
*/

{Contains functions, which are useful for audio programming.}
#ifndef WASAPI_INTERFACE
#define WASAPI_INTERFACE

typedef void (*wasapi_read_callback)(void* sender, void* buf, int bufsize);
typedef void* lp_wasapi_instance;

extern "C" __declspec(dllexport) lp_wasapi_instance wasapi_open(void* psender, int channels, int bitdepth, int frequency);
extern "C" __declspec(dllexport) int wasapi_idle(lp_wasapi_instance pinst, wasapi_read_callback pcallback);
extern "C" __declspec(dllexport) void wasapi_close(lp_wasapi_instance pinst);
extern "C" __declspec(dllexport) void wasapi_start(lp_wasapi_instance pinst);
extern "C" __declspec(dllexport) void wasapi_stop(lp_wasapi_instance pinst);
extern "C" __declspec(dllexport) void wasapi_reset(lp_wasapi_instance pinst);

#endif /*WASAPI_INTERFACE*/