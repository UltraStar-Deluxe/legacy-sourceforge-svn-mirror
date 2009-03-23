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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UMain.pas $
 * $Id: UMain.pas 1629 2009-03-07 22:30:04Z k-m_schindler $
 *}

unit UPluginLoader_DLL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses UPluginDefines, UPlugin;

type
  TPluginLoader_DLL = class
    protected
      PluginDir: WideString;
    public
      constructor Create(PluginDir: WideString);
      procedure Browse(Dir: WideString);
  end;
  
  TPlugin_DLL = class (TPlugin)
    protected
      hLib: THandle; //< handle of loaded library
      Lib_Proc_Init: TUS_Plugin_Proc_Init;
      Lib_OnChangeStatus: TUS_Plugin_OnChangeStatus;
      
      procedure OnChangeStatus(Status: TUS_PluginStatus); override;
    public
      constructor Create(Handle: TUS_Handle; Filename: WideString); override;
      destructor Destroy; override;
  end;

const
  {$IF Defined(MSWINDOWS)}
    DLLExt  = '.dll';
  {$ELSEIF Defined(DARWIN)}
    DLLExt  = '.dylib';
  {$ELSEIF Defined(UNIX)}
    DLLExt  = '.so';
  {$IFEND}

implementation
uses
  {$IFDEF MSWINDOWS}
  windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  SysUtils,
  UPluginManager,
  UPlatform;

{ implementation of TPluginLoader_DLL }
constructor TPluginLoader_DLL.Create(PluginDir: WideString);
begin
  Self.PluginDir := PluginDir;
  Browse(PluginDir);
end;

procedure TPluginLoader_DLL.Browse(Dir: WideString);
  var
    Files: TDirectoryEntryArray;
    I: Integer;
begin
  Files := Platform.DirectoryFindFiles(Dir, DLLExt, True);

  for I := 0 to High(Files) do
  begin
    if (Files[I].IsDirectory) then
      Browse(Dir + Files[I].Name + PathDelim)
    else if (Files[I].IsFile) then
      PluginManager.AddPlugin(TPlugin_DLL.Create(PluginManager.GetHandle, Dir + Files[I].Name));
  end;
end;

{ implementation of TPlugin_DLL }
constructor TPlugin_DLL.Create(Handle: TUS_Handle; Filename: WideString);
begin
  inherited;
  
  hLib := 0;
  Lib_Proc_Init := nil;
  Lib_OnChangeStatus := nil;

  //try to load the library
  SetStatus(psWaitingInit);
end;

destructor TPlugin_DLL.Destroy;
begin

  inherited;
end;

procedure TPlugin_DLL.OnChangeStatus(Status: TUS_PluginStatus);
begin
  Case Status of
    psWaitingInit: begin
      //we have to try to load the plugin here
      hLib := LoadLibrary(PChar(AnsiString(Filename)));
      if (hlib <> 0) then
      begin
        @Lib_Proc_Init := GetProcAddress(hLib, PChar('Proc_Init'));

        If (not Assigned(Lib_Proc_Init)) then
        begin
          FreeLibrary(hLib);

          {$IFDEF MSWINDOWS}
          SetError('Can''t export Proc_Init procedure from library. Windows reports: ' + SysErrorMessage(GetLastError));
          {$ELSE}
          SetError('Can''t export Proc_Init procedure from library.');
          {$ENDIF}
          Exit;
        end;

        @Lib_OnChangeStatus := GetProcAddress(hLib, PChar('OnChangeStatus'));

        If (not Assigned(Lib_OnChangeStatus)) then
        begin
          FreeLibrary(hLib);

          {$IFDEF MSWINDOWS}
          SetError('Can''t export OnChangeStatus procedure from library. Windows reports: ' + SysErrorMessage(GetLastError));
          {$ELSE}
          SetError('Can''t export OnChangeStatus procedure from library.');
          {$ENDIF}
          Exit;
        end;  

      end
      else
      begin
        {$IFDEF MSWINDOWS}
        SetError('Error loading library. Windows reports: ' + SysErrorMessage(GetLastError));
        {$ELSE}
        SetError('Error loading library.');
        {$ENDIF}
        Exit;
      end;

    end;

  end;

  //call plugins OnChangeStatus procedure
  If assigned(Lib_OnChangeStatus) then
    Lib_OnChangeStatus(Self.Handle, Status);

  Self.Status := Status;
end;

end.