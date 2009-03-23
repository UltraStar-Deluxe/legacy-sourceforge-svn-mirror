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
unit UPluginManager;

{ this class manages all loaded plugins }

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes, UPluginDefines;

type
  TPluginManager = class
    private
      NextHandle: TUS_Handle;

      Plugins: TInterfaceList;
      Events: TInterfaceList;
      Interfaces: TInterfaceList;

    public
      constructor Create;

      //returns a unique handle to use e.g. w/ a pluginloader
      function GetHandle: TUS_Handle;

      //returns the plugin w/ the specified handle
      function GetPluginbyHandle(Handle: TUS_Handle): IUS_Plugin;

      //just adds a plugin to the pluginlist
      //it will be inited on next call of init
      function AddPlugin(Plugin: IInterface): TUS_Handle;

      procedure Init;   //< inits all plugins w/ status psWaitingInit
      procedure DeInit; //< deinits all plugins w/ status psInited

      //called by plugins if they change to status psError
      //removes all saved data and callbacks by this handle
      procedure ReportError(Handle: TUS_Handle);

      destructor Destroy;
  end;

var
  PluginManager: TPluginManager;

implementation
uses SysUtils;

constructor TPluginManager.Create;
begin
  NextHandle := US_HANDLE_CORE + 1;

  Plugins := TInterfaceList.Create;
  Events := TInterfaceList.Create;
  Interfaces := TInterfaceList.Create;
end;

destructor TPluginManager.Destroy;
begin
  DeInit;

  Plugins.Destroy;
  Events.Destroy;
  Interfaces.Destroy;
end;

// returns a unique handle to use e.g. w/ a pluginloader
function TPluginManager.GetHandle: TUS_Handle;
begin
  Result := NextHandle;
  Inc(NextHandle);
end;

//returns the plugin w/ the specified handle
function TPluginManager.GetPluginbyHandle(Handle: TUS_Handle): IUS_Plugin; 
  var
    I: integer;
    Plugin: IUS_Plugin;
begin
  Result := nil;
  for I := 0 to Plugins.Count-1 do
  begin
    Plugin := IUS_Plugin(Plugins.Items[I]);
    If (Plugin.GetHandle = Handle) then
    begin
      Result := Plugin;
      Exit;
    end;
  end;
end;

// just adds a plugin to the pluginlist
// it will be inited on next call of init
function TPluginManager.AddPlugin(Plugin: IInterface): TUS_Handle;
begin
  If Supports(Plugin, IUS_Plugin) then
    Plugins.Add(Plugin);
end;

// inits all plugins w/ status psWaitingInit
procedure TPluginManager.Init;
  var
    I: integer;
    Plugin: IUS_Plugin;
begin
  for I := 0 to Plugins.Count-1 do
  begin
    Plugin := IUS_Plugin(Plugins.Items[I]);
    If (Plugin.GetStatus = psWaitingInit) then
    begin
      Plugin.Init;
    end;
  end;
end;

// deinits all plugins w/ status psInited
procedure TPluginManager.DeInit;
  var
    I: integer;
    Plugin: IUS_Plugin;
begin
  for I := 0 to Plugins.Count-1 do
  begin
    Plugin := IUS_Plugin(Plugins.Items[I]);
    If (Plugin.GetStatus = psInited) then
    begin
      Plugin.DeInit;
    end;
  end;
end;

//called by plugins if they change to errorstate
//removes all saved data and callbacks by this handle
procedure TPluginManager.ReportError(Handle: TUS_Handle);
  var
    I: integer;
    Event: IUS_HookableEvent;
begin
  for I := 0 to Events.Count-1 do
  begin
    Event := IUS_HookableEvent(Events.Items[I]);
    Event.UnHookbyParent(Handle);
  end;
end;

end.