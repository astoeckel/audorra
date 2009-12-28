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

File: wasapi_interface.cpp
Author: Andreas Stöckel
*/

#include <windows.h>
#include <mmdeviceapi.h>
#include <audioclient.h>
#include <KsMedia.h>
#include <stdio.h>

#include "wasapi_interface.h"

#define EXIT_ON_ERROR(hres) \
  if (FAILED(hres)) goto Exit;
  
#define SAFE_RELEASE(punk)  \
  if ((punk) != NULL)  \
  { (punk)->Release(); (punk) = NULL; }
  
#define REFTIMES_PER_SEC  10000000
#define REFTIMES_PER_MILLISEC  10000

struct _wasapi_data {
  void *pSender;
  IMMDeviceEnumerator *pEnumerator;
  IMMDevice *pDevice;
  IAudioClient *pAudioClient;
  IAudioRenderClient *pAudioRenderClient;
  UINT32 bufferSampleCount;  
};

typedef struct _wasapi_data wasapi_data;
typedef wasapi_data* lp_wasapi_data;

void get_wfx(int channels, int bitdepth, int frequency, WAVEFORMATEXTENSIBLE* pwfx) {
  pwfx->Format.cbSize = 0;
  pwfx->Format.nAvgBytesPerSec = channels * bitdepth * frequency / 8;
  pwfx->Format.nBlockAlign = bitdepth * channels / 8;
  pwfx->Format.nChannels = channels;
  pwfx->Format.nSamplesPerSec = frequency;
  pwfx->Format.wBitsPerSample = bitdepth;
  pwfx->Format.wFormatTag = WAVE_FORMAT_PCM;  
  
  if (channels > 2) {
    pwfx->Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE;
    pwfx->Format.cbSize = 22;
    pwfx->SubFormat = KSDATAFORMAT_SUBTYPE_PCM;
    pwfx->dwChannelMask = 0;
    int i = 0;
    for (i;i++;i<channels) {
      pwfx->dwChannelMask |= (1 << i);
    }
    pwfx->Samples.wValidBitsPerSample = bitdepth;
  }
}

lp_wasapi_instance wasapi_open(void* psender, int channels, int bitdepth, int frequency) {
  HRESULT hr = 0;
  lp_wasapi_data pinst;
  WAVEFORMATEXTENSIBLE wfx; 
  BYTE *pData;
  REFERENCE_TIME hnsRequestedDuration = REFTIMES_PER_SEC / 50;  
  
  //Obtain and clear a WASAPI data struture
  pinst = (lp_wasapi_data)malloc(sizeof(wasapi_data));
  memset(pinst, 0, sizeof(wasapi_data));
  pinst->pSender = psender;
  
  //Initialize the COM client
  CoInitialize(NULL);  
 
  //Create the device enumerator (--> Use this lateron to enumerate WASAPI output devices)
  hr = CoCreateInstance(__uuidof (MMDeviceEnumerator), NULL,
    CLSCTX_ALL,__uuidof (IMMDeviceEnumerator), (void**)&(pinst->pEnumerator));
  EXIT_ON_ERROR(hr)  
  
  //Get the default audio device (--> The library should support exporting the a list of audio devices and
  //let the user choose one).
  hr = pinst->pEnumerator->GetDefaultAudioEndpoint(eRender, eConsole, &(pinst->pDevice));
  EXIT_ON_ERROR(hr)
  
  //Activate the audio device and get the audio client interface
  hr = pinst->pDevice->Activate(__uuidof(IAudioClient), CLSCTX_ALL, NULL, (void**)&(pinst->pAudioClient));
  EXIT_ON_ERROR(hr); 
  
  //Get the waveformat descriptor
  get_wfx(channels, bitdepth, frequency, &wfx); 
  
  hr = pinst->pAudioClient->Initialize(AUDCLNT_SHAREMODE_EXCLUSIVE, 0, hnsRequestedDuration, 0, (PWAVEFORMATEX)&wfx, NULL);
  EXIT_ON_ERROR(hr);   
 
  hr = pinst->pAudioClient->GetBufferSize(&(pinst->bufferSampleCount));
  EXIT_ON_ERROR(hr);
  
  hr = pinst->pAudioClient->GetService(__uuidof(IAudioRenderClient), (void**)&(pinst->pAudioRenderClient));
  EXIT_ON_ERROR(hr);
  
  hr = pinst->pAudioRenderClient->GetBuffer(pinst->bufferSampleCount, &pData);
  EXIT_ON_ERROR(hr);   
  
  //Clear the buffer memory
  memset(pData, 0, pinst->bufferSampleCount * wfx.Format.nBlockAlign);  
  
  hr = pinst->pAudioRenderClient->ReleaseBuffer(pinst->bufferSampleCount, 0);
  EXIT_ON_ERROR(hr);   
  
  return pinst;
Exit:
  wasapi_close(pinst);
  return NULL;
}

void wasapi_close(lp_wasapi_instance pinst) {
  //Cast the void pointer to lp_wasapi_data
  lp_wasapi_data data = (lp_wasapi_data)pinst;
  
  //Release all COM objects
  SAFE_RELEASE(data->pAudioRenderClient);
  SAFE_RELEASE(data->pAudioClient);
  SAFE_RELEASE(data->pDevice)
  SAFE_RELEASE(data->pEnumerator)
  
  //Free the memory reserved for this WASAPI instance
  free(pinst);  
}

int wasapi_idle(lp_wasapi_instance pinst, wasapi_read_callback pcallback) {
  UINT32 numFramesPadding;
  UINT32 numFramesAvailable;
  BYTE *pData; 
  
  //Cast instance to data
  lp_wasapi_data pinst_data = (lp_wasapi_data)pinst;
  
  // See how much buffer space is available.
  pinst_data->pAudioClient->GetCurrentPadding(&numFramesPadding);
  numFramesAvailable = pinst_data->bufferSampleCount - numFramesPadding;  
  if (numFramesAvailable > 64) {
    // Grab all the available space in the shared buffer.
    pinst_data->pAudioRenderClient->GetBuffer(numFramesAvailable, &pData);
 
    // Get next 1/2-second of data from the audio source.
    pcallback(pinst_data->pSender, pData, numFramesAvailable);      
    
    pinst_data->pAudioRenderClient->ReleaseBuffer(numFramesAvailable, 0);
    
    return 1;
  } else {
    return 0;
  }
}

void wasapi_stop(lp_wasapi_instance pinst) {
  ((lp_wasapi_data)pinst)->pAudioClient->Stop();
}

void wasapi_start(lp_wasapi_instance pinst) {
  ((lp_wasapi_data)pinst)->pAudioClient->Start();
}

void wasapi_reset(lp_wasapi_instance pinst) {
  ((lp_wasapi_data)pinst)->pAudioClient->Reset();
}
