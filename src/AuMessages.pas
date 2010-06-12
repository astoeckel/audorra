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

File: AuMessages.pas
Author: Andreas Stöckel
}

{Contains all messages used by the Audorra framework so that it may be
 translated by simply exchanging this unit}
unit AuMessages; //< @exclude

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

resourcestring
  MsgUnableToCreateDriver = 'An unspecified error occured during the ' +
    'creation of the Audorra output driver. Probably the loaded Audorra output ' +
    'driver is not supported by your audio hardware. Try to install the newest ' +
    'hardware drivers.';

  MsgNoDriver = 'No output driver has been specified. Probably no audio ' +
    'driver has been included. If this is not the case, the audio driver may have ' +
    'refused to work, because it is not supported on this machine or some libraries ' +
    'are missing.';
    
  MsgNoDevice = 'No output device had been found during the initialization of ' +
    'Audorra. Try to use another Audorra output driver.';

  MsgNoDeviceSpecified = 'No output device has been specified for audio output.';

  MsgDeviceDoesNotExist = 'The device "%s" you have specified does not exist!';

  MsgSyncdata = 'The audio frame information data has been added with decreasing ' +
    'sample timestamp. An increasing sample timestamp is mandatory.';

  MsgFiltergraphLocked = 'Connection to the target filter could not be established.';
  MsgFiltergraphConnectionFailed = 'Connection to the target filter could not be established.';
  MsgFiltergraphRecursion = 'A filter cannot target on its own instance!';
  MsgFiltergraphInitializationFailed = 'Inatialization of the filtergraph failed.';
  MsgFilterSuspendImpossible = 'A filter cannot be suspended, if it is not initialized.';
  MsgFilterResumeImpossible = 'A filter cannot be resumed, if it is initialized.';
  MsgFiltergraphGuessingParametersFailed = 'The filtergraph parameters could not be guessed from the context while resuming.';

  Msg3DAudioDriverCreationFailed = 'The 3D output driver could not be created. ' +
    'Try a smaller bit count, sample rate or channel count.';

implementation

end.
