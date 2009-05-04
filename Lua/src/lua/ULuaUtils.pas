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

unit ULuaUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua;

{ converts a lua table with a structure like:
    * = 1 , * = 4 , * = 5
  to an integer with the value:
    0b11001
  does not pop anything }
function Lua_ToBinInt(L: PLua_State; idx: Integer): Integer;

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

  lua_checkstack(L, 3);

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

{ this is a helper in case an evenet owner don't has no use for the results
  returns number of popped elements }
function Lua_ClearStack(L: Plua_State): Integer;
  var I: Integer;
begin
  Result := lua_gettop(L);
  lua_pop(L, Result);
end;

end.