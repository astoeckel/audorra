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

File: audorra_conf.inc
Author: Andreas Stöckel
}

//Comment: This file is used for configuring Audorra. Simply copy this file
//to your project directory and add it to your project directory.
//Deactivate or activate the compiler switches as you like.
//IMPORTANT! HOW TO USE THE CONFIGURATION FILE!
//If you do not want to use the settings in this file for all Audorra 2D projects,
//rename this file in the Audorra 2D source directory and copy this file to your Audorra 2D
//projects.

{$IFNDEF ANDORRA_CONF}
{$DEFINE ANDORRA_CONF}

//--------------------------------
//--- AUDORRA CORE SETTINGS ---

//Deactivate this compiler switch, if your project use the VCL. This has to be done
//because event handling between multiple threads normally uses the "synchronize" method,
//which is not available without the VCL.
{$DEFINE DO_NOT_USE_VCL}

//--------------------------------
//--- AUDORRA MISC ---

//Activate this switch if you want to know, whether this configuration file
//is actually used.
{'$DEFINE TEST}
{$IFDEF TEST}
  {$MESSAGE HINT 'This instance of the audorra_conf.inc is used'}
{$ENDIF}   

{$ENDIF}

