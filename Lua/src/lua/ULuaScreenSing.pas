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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/branches/experimental/Lua/src/lua/ULuaTexture.pas $
 * $Id: ULuaTexture.pas 1551 2009-01-04 14:08:33Z Hawkear $
 *}

unit ULuaScreenSing;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  ULua;

{ returns a table with following structure:
    t[1..playercount] = score of player i }
function ULuaScreenSing_GetScores(L: Plua_State): Integer; cdecl;

{ finishes current song, if sing screen is not shown it will raise
  an error }
function ULuaScreenSing_Finish(L: Plua_State): Integer; cdecl;

const
  ULuaScreenSing_Lib_f: array [0..1] of lual_reg = (
    (name:'GetScores';func:ULuaScreenSing_GetScores),
    (name:'Finish';func:ULuaScreenSing_Finish)
  );

implementation
uses UScreenSing, UMain, UDisplay, UGraphic;

{ returns a table with following structure:
    t[1..playercount] = score of player i }
function ULuaScreenSing_GetScores(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Player[I].ScoreTotalInt);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ finishes current song, if sing screen is not shown it will raise
  an error }
function ULuaScreenSing_Finish(L: Plua_State): Integer; cdecl;
  var Top: Integer;
begin
  Result := 0;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  if (Display.CurrentScreen^ = ScreenSing) then
  begin
    ScreenSing.EndSong;
  end
  else
    LuaL_error(L, 'Usdx.ScreenSing.Finish is called, but sing screen is not shown.'); 
end;

end.