{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UConsole;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$DEFINE SYNCHRONIZE_CONSOLE}

procedure ConsoleWriteLn(const msg: string);

implementation

uses
  // Do not include the unit 'Libc', it also defines TRTLCriticalSection,
  // use the TRTLCriticalSection
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  SyncObjs,
  Classes;

procedure _ConsoleWriteLn(const aString: string); {$IFDEF HasInline}inline;{$ENDIF} forward;

{$IFDEF SYNCHRONIZE_CONSOLE}
type
  {**
   * FPC uses threadvars (TLS) managed by FPC for console output locking.
   * Using WriteLn() from external threads (like in SDL callbacks)
   * will crash the program as those threadvars have never been initialized.
   * Access to the console by two non-managed threads in Delphi also causes a
   * crash.
   *
   * The solution is to create an FPC/Delphi-managed thread which has the TLS data
   * and use it to handle the console-output (hence it is called Console-Handler)
   * This must be a thread managed by FPC/Delphi, otherwise (e.g. SDL-thread)
   * it will crash when using Writeln.
   *}
  TConsoleHandler = class(TThread)
  private
    fMessageList: TStringList;
    fConsoleCriticalSection: TCriticalSection;
    fConsoleEvent: TEvent;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure WriteLn(const msg: string);

  protected
    procedure Execute(); override;
  end;

var
  ConsoleHandler: TConsoleHandler;

constructor TConsoleHandler.Create();
begin
  inherited Create(false);

  // init thread-safe output
  fMessageList := TStringList.Create();
  fConsoleCriticalSection := TCriticalSection.Create;
  fConsoleEvent := TEvent.Create(nil, false, false, 'ConsoleEvent');
end;

destructor TConsoleHandler.Destroy();
begin
  // terminate console-handler
  fConsoleCriticalSection.Enter;
  Terminate;
  fConsoleEvent.SetEvent();
  fConsoleCriticalSection.Leave();

  WaitFor();

  // free data
  fConsoleCriticalSection.Free;
  fConsoleEvent.Free;
  fMessageList.Free;
  
  inherited;
end;

{*
 * The console-handlers main-function.
 *}
procedure TConsoleHandler.Execute();
var
  i: integer;
begin
  while (true) do
  begin
    // wait for new output or quit-request
    fConsoleEvent.WaitFor(INFINITE);

    fConsoleCriticalSection.Enter;
    try
      // output pending messages
      for i := 0 to fMessageList.Count - 1 do
      begin
        _ConsoleWriteLn(fMessageList[i]);
      end;
      fMessageList.Clear();

      // use local quit-variable to avoid accessing
      // ConsoleQuit outside of the critical section
      if (Terminated) then
        Break;
    finally
      fConsoleEvent.ResetEvent();
      fConsoleCriticalSection.Leave;
    end;
  end;
end;

procedure TConsoleHandler.WriteLn(const msg: string);
begin
  // TODO: check for the main-thread and use a simple _ConsoleWriteLn() then?
  //GetCurrentThreadThreadId();
  fConsoleCriticalSection.Enter;
  try
    fMessageList.Add(msg);
    fConsoleEvent.SetEvent();
  finally
    fConsoleCriticalSection.Leave;
  end;
end;

{$ENDIF}

(*
 * Write to console if one is available.
 * It checks if a console is available before output so it will not
 * crash on windows if none is available.
 * Do not use this function directly because it is not thread-safe,
 * use ConsoleWriteLn() instead.
 *)
procedure _ConsoleWriteLn(const aString: string); {$IFDEF HasInline}inline;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // sanity check to avoid crashes with writeln()
  if (IsConsole) then
  begin
  {$ENDIF}
    Writeln(aString);
  {$IFDEF MSWINDOWS}
  end;
  {$ENDIF}
end;

procedure ConsoleWriteLn(const msg: string);
begin
{$IFDEF CONSOLE}
  {$IFDEF SYNCHRONIZE_CONSOLE}
  ConsoleHandler.WriteLn(msg);
  {$ELSE}
  _ConsoleWriteLn(msg);
  {$ENDIF}
{$ENDIF}
end;

procedure InitConsoleOutput();
begin
  {$IFDEF SYNCHRONIZE_CONSOLE}
  ConsoleHandler := TConsoleHandler.Create;
  {$ENDIF}
end;

procedure FinalizeConsoleOutput();
begin
  {$IFDEF SYNCHRONIZE_CONSOLE}
  ConsoleHandler.Free;
  {$ENDIF}
end;

initialization
  InitConsoleOutput();

finalization
  FinalizeConsoleOutput();

end.
