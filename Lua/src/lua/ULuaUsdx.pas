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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/branches/experimental/Lua/src/lua/ULuaGl.pas $
 * $Id: ULuaGl.pas 1555 2009-01-11 18:19:42Z Hawkear $
 *}

unit ULuaUsdx;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua;

{ some basic lua c functions from usdx table }

{ usdx.time - returns sdl_time to have time numbers comparable with
              ultrastar delux ones. No Arguments }
function ULuaUsdx_Time(L: Plua_State): Integer; cdecl;
function ULuaUsdx_(L: Plua_State): Integer; cdecl;

const
  ULuaUsdx_Lib_f: array [0..1] of lual_reg = (
    (name:'Time'; func:ULuaUsdx_Time),
    (name:nil; func:nil)
  );

implementation
uses SDL;

function ULuaUsdx_Time(L: Plua_State): Integer; cdecl;
  var top: Integer;
begin
  //remove arguments (if any)
  top := lua_gettop(L);

  If (top > 0) then
    lua_pop(L, top);

  //push result
  lua_pushinteger(L, SDL_GetTicks);
  Result := 1; //one result
end;

function ULuaUsdx_(L: Plua_State): Integer; cdecl;
begin

end;

end.