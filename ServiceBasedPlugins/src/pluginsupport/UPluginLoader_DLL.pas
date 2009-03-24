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
      PluginInterface: TUS_PluginInterface;

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
      function GetLoader: LongInt; override;
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

// procedures used in the PluginInterface record
// see UPluginDefines for descriptions
function DLL_Identify (Handle: TUS_Handle; Version: TUS_Version; Buffer: PUS_PluginInfo; BufferLength: LongInt): LongInt;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
function DLL_Error (Handle: TUS_Handle; Reason: PWideChar): LongInt;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
function DLL_GetFilename (Handle: TUS_Handle; Buffer: PWideChar; BufferLength: LongInt): LongInt;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

implementation
uses
  {$IFDEF MSWINDOWS}
  windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  SysUtils,
  UPluginManager,
  UPlatform,
  ULog;

{ implementation of TPluginLoader_DLL }
constructor TPluginLoader_DLL.Create(PluginDir: WideString);
begin
  //start to fill the plugininterface w/ default values
  PluginInterface.Handle        := US_HANDLE_UNDEFINED;
  PluginInterface.Version       := SDK_Version;
  PluginInterface.Identify      := DLL_Identify;
  PluginInterface.Error         := DLL_Error;
  PluginInterface.GetFilename   := DLL_GetFilename;
  PluginInterface.GetInterface  := nil;
  PluginInterface.SetInterface  := nil;
  PluginInterface.GetData       := nil;
  PluginInterface.SetData       := nil;

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

function TPlugin_DLL.GetLoader: LongInt;
begin
  Result := US_LOADER_LIBRARY;
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

        if (not Assigned(Lib_Proc_Init)) then
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

        if (not Assigned(Lib_OnChangeStatus)) then
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

      //library + procedure pointers are loaded
      //now try to do the handshake
      //call Proc_Init
      Lib_Proc_Init(Self.Handle, SDK_Version, nil);

      //check if plugin has identified itself
      if (Self.Info.Version = US_VERSION_UNDEFINED) then
      begin
        SetError('library did not identify');
        exit;
      end;    
    end;

  end;

  //call plugins OnChangeStatus procedure
  if assigned(Lib_OnChangeStatus) then
    try
      Lib_OnChangeStatus(Self.Handle, Status);
    except
      if (Status <> psError) then
        SetError('exception in librarys OnChangeStatus (from: ' + USPluginStatustoString(Self.Status) + ' to: ' + USPluginStatustoString(Status) + ')');
    end;

  Self.Status := Status;
end;

// procedures used in the PluginInterface record
// see UPluginDefines for descriptions
// --------
function DLL_Identify (Handle: TUS_Handle; Version: TUS_Version; Buffer: PUS_PluginInfo; BufferLength: LongInt): LongInt;
var
  Plugin: IUS_Plugin;
  Info: TUS_PluginInfo;
begin
  Result := US_ERROR_Unknown;
  Plugin := PluginManager.GetPluginbyHandle(Handle);

  if (Plugin <> nil) then
  begin
    if (Plugin.GetLoader = US_LOADER_LIBRARY) then
    begin
      If (Version = SDK_Version) then
      begin
        If (BufferLength = SizeOf(TUS_PluginInfo)) then
        begin
          try
            move(Buffer^, Info, BufferLength);
          except
            Plugin.SetError('error copying plugininfo from buffer in DLL_Identify');
          end;

          If Plugin.Identify(Info) then
          begin
            Result := US_ERROR_None;
          end;
        end
        else
        begin
          Result := US_ERROR_SizeMismatch;
          Plugin.SetError('reported buffersize is wrong in DLL_Identify');
        end;
      end
      else
      begin
        Result := US_ERROR_VersionMismatch;
        Plugin.SetError('SDK version is not supported in DLL_Identify');
      end;

    end
    else
    begin
      Result := US_ERROR_WrongHandle;
      Log.LogError('called w/ handle of plugin from another loader', 'DLL_Identify');
    end;
  end
  else
  begin
    Result := US_ERROR_WrongHandle;
    Log.LogError('called w/ invalid handle', 'DLL_Identify');
  end;
end;

function DLL_Error (Handle: TUS_Handle; Reason: PWideChar): LongInt;
var
  Plugin: IUS_Plugin;
  S: WideString;
begin
  Result := US_ERROR_Unknown;
  Plugin := PluginManager.GetPluginbyHandle(Handle);

  if (Plugin <> nil) then
  begin
    if (Plugin.GetLoader = US_LOADER_LIBRARY) then
    begin
      Result := US_ERROR_None;

      //try to copy the error reason
      try
        SetLength(S, Length(Reason^));
        S := copy(Reason^, 1, Length(S)); //or has this to be len*2 ?
      except
        Reason := 'couldn''t copy error reason in DLL_Error';
      end;

      Plugin.SetError(Reason);
    end
    else
    begin
      Result := US_ERROR_WrongHandle;
      Log.LogError('called w/ handle of plugin from another loader', 'DLL_Identify');
    end;
  end
  else
  begin
    Result := US_ERROR_WrongHandle;
    Log.LogError('called w/ invalid handle', 'DLL_Identify');
  end;
end;

function DLL_GetFilename (Handle: TUS_Handle; Buffer: PWideChar; BufferLength: LongInt): LongInt;
begin

end;

end.