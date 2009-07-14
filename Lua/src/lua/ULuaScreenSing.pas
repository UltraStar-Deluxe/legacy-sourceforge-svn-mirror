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

{ returns a table with following structure:
    t[1..playercount] = rating of player i range: [0..1] }
function ULuaScreenSing_GetRating(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = rect of players score background: table(x, y, w, h) }
function ULuaScreenSing_GetScoreBGRect(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = rect of players rating bar: table(x, y, w, h) }
function ULuaScreenSing_GetRBRect(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetBeat() - returns current beat of lyricstate }
function ULuaScreenSing_GetBeat(L: Plua_State): Integer; cdecl;

{ finishes current song, if sing screen is not shown it will raise
  an error }
function ULuaScreenSing_Finish(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetSettings - no arguments
  returns a table filled with the data of TScreenSing }
function ULuaScreenSing_GetSettings(L: Plua_State): Integer; cdecl;

{ ScreenSing.SetSettings - arguments: Table
  sets all attributes of TScreenSing.Settings that are
  unequal to nil in Table }
function ULuaScreenSing_SetSettings(L: Plua_State): Integer; cdecl;

const
  ULuaScreenSing_Lib_f: array [0..7] of lual_reg = (
    (name:'GetScores';func:ULuaScreenSing_GetScores),
    (name:'GetRating';func:ULuaScreenSing_GetRating),
    (name:'GetBeat';func:ULuaScreenSing_GetBeat),
    (name:'GetScoreBGRect';func:ULuaScreenSing_GetScoreBGRect),
    (name:'GetRBRect';func:ULuaScreenSing_GetRBRect),
    (name:'Finish';func:ULuaScreenSing_Finish),
    (name:'GetSettings';func:ULuaScreenSing_GetSettings),
    (name:'SetSettings';func:ULuaScreenSing_SetSettings)
  );

implementation
uses UScreenSing, UMain, UDisplay, UGraphic, UMusic, ULuaUtils, SysUtils;

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

{ returns a table with following structure:
    t[1..playercount] = rating of player i range: [0..1] }
function ULuaScreenSing_GetRating(L: Plua_State): Integer; cdecl;
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
  for I := 0 to High(ScreenSing.Scores.Players) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushNumber(L, ScreenSing.Scores.Players[I].RBPos);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = rect of players ScoreBG: table(x, y, w, h) }
function ULuaScreenSing_GetScoreBGRect(L: Plua_State): Integer; cdecl;
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
  lua_createtable(L, Length(ScreenSing.Scores.Players), 0);

  // fill w/ values
  for I := 0 to High(ScreenSing.Scores.Players) do
  begin
    lua_pushInteger(L, I + 1);

    if (ScreenSing.Scores.Players[I].Position = High(Byte)) then
      // player has no position, prevent crash by pushing nil
      lua_pushNil(L)
    else
      with ScreenSing.Scores.Positions[ScreenSing.Scores.Players[I].Position] do
        lua_PushRect(L, BGX, BGY, BGW, BGH);


    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = rect of players rating bar: table(x, y, w, h) }
function ULuaScreenSing_GetRBRect(L: Plua_State): Integer; cdecl;
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
  lua_createtable(L, Length(ScreenSing.Scores.Players), 0);

  // fill w/ values
  for I := 0 to High(ScreenSing.Scores.Players) do
  begin
    lua_pushInteger(L, I + 1);

    if (ScreenSing.Scores.Players[I].Position = High(Byte)) then
      // player has no position, prevent crash by pushing nil
      lua_pushNil(L)
    else
      with ScreenSing.Scores.Positions[ScreenSing.Scores.Players[I].Position] do
        lua_PushRect(L, RBX, RBY, RBW, RBH);


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

{ ScreenSing.GetSettings - no arguments
  returns a table filled with the data of TScreenSing }
function ULuaScreenSing_GetSettings(L: Plua_State): Integer; cdecl;
  var Top: Integer;
begin
  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  lua_createtable(L, 0, 3);

  //fill table w/ info
  lua_pushBoolean(L, ScreenSing.Settings.LyricsVisible);
  lua_setField(L, -2, 'LyricsVisible');

  lua_pushBinInt(L, ScreenSing.Settings.NotesVisible);
  lua_setField(L, -2, 'NotesVisible');

  lua_pushBinInt(L, ScreenSing.Settings.PlayerEnabled);
  lua_setField(L, -2, 'PlayerEnabled');


  Result := 1;
end;

{ ScreenSing.SetSettings - arguments: Table
  sets all attributes of TScreenSing.Settings that are
  unequal to nil in Table }
function ULuaScreenSing_SetSettings(L: Plua_State): Integer; cdecl;
  var
    Key: String;
begin
  Result := 0;

  // check for table on stack
  luaL_checkType(L, 1, LUA_TTABLE);

  // go through table elements
  lua_pushNil(L);
  while (lua_Next(L, 1) <> 0) do
  begin
    Key := lowercase(lua_ToString(L, -2));

    if (Key = 'lyricsvisible') and (lua_isBoolean(L, -1)) then
      ScreenSing.settings.LyricsVisible := lua_toBoolean(L, -1)
    else if (Key = 'notesvisible') and (lua_isTable(L, -1)) then
      ScreenSing.settings.NotesVisible := lua_toBinInt(L, -1)
    else if (Key = 'playerenabled') and (lua_isTable(L, -1)) then
      ScreenSing.settings.PlayerEnabled := lua_toBinInt(L, -1);

    // pop value from stack so key is on top
    lua_pop(L, 1);
  end;

  // clear stack from table
  lua_pop(L, lua_gettop(L));

  ScreenSing.ApplySettings;
end;

{ ScreenSing.GetBeat() - returns current beat of lyricstate }
function ULuaScreenSing_GetBeat(L: Plua_State): Integer; cdecl;
var top: Integer;
begin
  //remove arguments (if any)
  top := lua_gettop(L);

  if (top > 0) then
    lua_pop(L, top);

  //push result
  lua_pushnumber(L, LyricsState.MidBeat);
  Result := 1; //one result
end;

end.