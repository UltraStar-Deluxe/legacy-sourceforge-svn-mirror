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

unit ULuaUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua, ULuaCore;

{ converts a lua table with a structure like:
    * = 1 , * = 4 , * = 5
  to an integer with the value:
    0b11001
  does not pop anything }
function Lua_ToBinInt(L: PLua_State; idx: Integer): Integer;

{ returns plugin that is the owner of the given state
  may raise a lua error if the parent id is not found
  in states registry, if state owner does not exists
  or is not loaded. So a check for a nil value is not
  necessary }
function Lua_GetOwner(L: PLua_State): TLuaPlugin;

{ this is a helper in case an evenet owner don't has no use for the results
  returns number of popped elements }
function Lua_ClearStack(L: Plua_State): Integer;


implementation

{ converts a lua table with a structure like:
    * = 1 , * = 4 , * = 5
  to an integer with the value:
    0b11001
  does not pop anything }
function Lua_ToBinInt(L: PLua_State; idx: Integer): Integer;
  var
    I: Integer;
begin
  // default: no bits set
  Result := 0;

  lua_checkstack(L, 2);

  if (idx < 0) then
    dec(idx); // we will push one value before using this

  lua_PushNil(L);
  while (lua_next(L, idx) <> 0) do
  begin
    if (lua_isNumber(L, -1)) then
    begin //check if we got an integer value from 1 to 32
      I := lua_toInteger(L, -1);
      if (I >= 1) and (I <= 32) then
        Result := Result or 1 shl (I - 1);
    end;

    // pop value, so key is on top
    lua_pop(L, 1);
  end;
end;

{ returns plugin that is the owner of the given state
  may raise a lua error if the parent id is not found
  in states registry, if state owner does not exists
  or is not loaded. So a check for a nil value is not
  necessary }
function Lua_GetOwner(L: PLua_State): TLuaPlugin;
begin
  lua_checkstack(L, 1);

  lua_getfield (L, LUA_REGISTRYINDEX, '_USDX_STATE_ID');
  if (not lua_isNumber(L, -1)) then
    luaL_error(L, 'unable to get _USDX_STATE_ID');

  Result := LuaCore.GetPluginById(lua_toInteger(L, -1));

  lua_pop(L, 1); //< remove state id from stack

  if (Result = nil) then
    luaL_error(L, '_USDX_STATE_ID has invalid value')
  else if (Result.Status > psRunning) then
    luaL_error(L, 'owning plugin is not loaded or already unloaded in Lua_GetOwner');
end;

{ this is a helper in case an evenet owner don't has no use for the results
  returns number of popped elements }
function Lua_ClearStack(L: Plua_State): Integer;
begin
  Result := lua_gettop(L);
  lua_pop(L, Result);
end;

end.