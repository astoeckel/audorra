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

File: AuSyncUtils.pas
Author: Andreas Stöckel
}

{AuSyncUtils allows threads to send messages to each other.}
unit AuSyncUtils;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I commons_conf.inc}

uses
  SysUtils, Classes,
  AcSyncObjs, AcPersistent, AcSysUtils;

{Adds a method to the queue. It is unknown wheter or when the method will be executed.
 Do not use this function for important messages, but only for notifying. If you're
 working in a console/non VCL-Application, you have to declare the NO_VCL compiler switch.}
procedure AuQueueCall(AProc: TThreadMethod);

{Removes an object from the queue - this prozedure should be called, if a method,
 which is able to have calls on the queue, is freed.}
procedure AuQueueRemove(AObj: Pointer);

implementation

type
  PThreadMethod = ^TThreadMethod;
  PMethod = ^TMethod;

  TAuSyncMgr = class(TThread)
    private
      FCallList: TList;
      FCritSect: TAcCriticalSection;
      FCurMem: PThreadMethod;
      FDeletedCurMem: boolean;
    protected
      procedure Execute;override;
    public
      procedure QueueCall(AProc: TThreadMethod);
      procedure DeleteObject(AObj: Pointer);
      constructor Create;
      destructor Destroy;override;
  end;

constructor TAuSyncMgr.Create;
begin
  FCallList := TList.Create;
  
  FCritSect := TAcCriticalSection.Create;

  inherited Create(False);
end;

procedure TAuSyncMgr.DeleteObject(AObj: Pointer);
var
  i: Integer;
  mem: PMethod;
begin
  FCritSect.Enter;
  try
    i := 0;
    while i < FCallList.Count do
    begin
      mem := PMethod(FCallList[i]);
      if mem^.data = AObj then
      begin
        FreeMem(FCallList[i], SizeOf(TThreadMethod));
        FCallList.Delete(i);

        FDeletedCurMem := FDeletedCurMem or (mem = PMethod(FCurMem));
      end else
        i := i + 1;
    end;
  finally
    FCritSect.Leave;
  end;
end;

destructor TAuSyncMgr.Destroy;
var
  i: integer;
begin
  for i := 0 to FCallList.Count - 1 do
    FreeMem(FCallList[i], SizeOf(TThreadMethod));

  FCallList.Free;
  FCritSect.Free;
  inherited;
end;

procedure TAuSyncMgr.Execute;
begin
  try
    while not Terminated do
    begin
      while true do
      begin
        FCurMem := nil;
        FDeletedCurMem := false;

        FCritSect.Enter;
        try
          if FCallList.Count > 0 then
            FCurMem := PThreadMethod(FCallList[0]);
        finally
          FCritSect.Leave;
        end;

        if FCurMem = nil then
          break;

        try
          try
            {$IFNDEF DO_NOT_USE_VCL}
            Synchronize(FCurMem^);
            {$ELSE}
            FCurMem^;
            {$ENDIF}
          finally
            if not FDeletedCurMem then
            begin
              //Remove the element from the list
              FCritSect.Enter;
              try
                FCallList.Delete(0);

                //Free the memory reserved for the method pointer
                FreeMem(FCurMem, SizeOf(TThreadMethod));
              finally
                FCritSect.Leave;
              end;
            end;
          end;
        except
          //
        end;
      end;
      Sleep(1);
    end;
  except
    //
  end;
end;

procedure TAuSyncMgr.QueueCall(AProc: TThreadMethod);
var
  mem: PThreadMethod;
begin
  FCritSect.Enter;
  try
    GetMem(mem, SizeOf(TThreadMethod));
    mem^ := AProc;
    FCallList.Add(mem);
  finally
    FCritSect.Leave;
  end;
end;

var
  mgr: TAuSyncMgr;

procedure AuQueueCall(AProc: TThreadMethod);
begin
  if mgr <> nil then
    mgr.QueueCall(AProc);
end;

procedure AuQueueRemove(AObj: Pointer);
begin
  if mgr <> nil then
    mgr.DeleteObject(AObj);
end;

initialization
  mgr := TAuSyncMgr.Create;

finalization
  mgr.Terminate;
  mgr.WaitFor;
  mgr.Free;
  mgr := nil;


end.
