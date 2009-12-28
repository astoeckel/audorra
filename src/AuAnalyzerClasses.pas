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

File: AuAnalyzerClasses.pas
Author: Andreas Stöckel
}

{Contains the class definitions for audio analyzers.} 
unit AuAnalyzerClasses;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils, Classes, SyncObjs,
  
  AuTypes;

type
  TAuAnalyzer = class
    private
      FParameters: TAuAudioParameters;
      FCritSect: TCriticalSection;
      FActive: boolean;
    protected
      procedure DoAnalyze(ASamples: PSingle; ACount: Cardinal);virtual;
      procedure DoSetParameters;virtual;
      procedure SetParameters(AParams: TAuAudioParameters);

      property CritSect: TCriticalSection read FCritSect;
    public
      constructor Create;
      destructor Destroy;override;

      procedure AnalyzeData(ASamples: PSingle; ACount: Cardinal);
      property Parameters: TAuAudioParameters read FParameters write SetParameters;
      property Active: boolean read FActive write FActive;
  end;

  TAuAnalyzerList = class(TList)
    private
      FAutoFree: boolean;
      function GetItem(AIndex: integer): TAuAnalyzer;
      procedure SetItem(AIndex: integer; AItem: TAuAnalyzer);
    protected
      procedure Notify(ptr: Pointer; action: TListNotification);override;
    public
      constructor Create;

      procedure AnalyzeData(ASamples: PSingle; ACount: Cardinal);

      property Items[Index: integer]: TAuAnalyzer read GetItem write SetItem;default;
      property AutoFree: boolean read FAutoFree write FAutoFree;
  end;

implementation

{ TAuAnalyzer }

constructor TAuAnalyzer.Create;
begin
  inherited Create;

  FCritSect := TCriticalSection.Create;
  FParameters := AuAudioParameters(44100, 2);
  FActive := false;
end;

destructor TAuAnalyzer.Destroy;
begin
  FCritSect.Free;

  inherited;
end;

procedure TAuAnalyzer.DoAnalyze(ASamples: PSingle; ACount: Cardinal);
begin
  //
end;

procedure TAuAnalyzer.DoSetParameters;
begin
  //
end;

procedure TAuAnalyzer.AnalyzeData(ASamples: PSingle; ACount: Cardinal);
begin
  if not FActive then
    exit;
    
  FCritSect.Enter;
  try
    //Call the user handler
    DoAnalyze(ASamples, ACount);
  finally
    FCritSect.Leave;
  end;
end;

procedure TAuAnalyzer.SetParameters(AParams: TAuAudioParameters);
begin
  FCritSect.Enter;
  try
    FParameters := AParams;

    //Call the user handler
    DoSetParameters;
  finally
    FCritSect.Leave;
  end;
end;

{ TAuAnalyzerList }

constructor TAuAnalyzerList.Create;
begin
  inherited;

  FAutoFree := false;
end;

procedure TAuAnalyzerList.AnalyzeData(ASamples: PSingle; ACount: Cardinal);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AnalyzeData(ASamples, ACount);
end;

function TAuAnalyzerList.GetItem(AIndex: integer): TAuAnalyzer;
begin
  result := inherited Items[AIndex];
end;

procedure TAuAnalyzerList.SetItem(AIndex: integer; AItem: TAuAnalyzer);
begin
  inherited Items[AIndex] := AItem;
end;

procedure TAuAnalyzerList.Notify(ptr: Pointer; action: TListNotification);
begin
  if (FAutoFree) and (Action = lnDeleted) then
    TAuAnalyzer(ptr).Free;
end;


end.
