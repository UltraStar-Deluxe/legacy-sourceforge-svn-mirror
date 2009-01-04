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

unit ULuaTextGL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  TextGL,
  ULua;

function luaopen_TextGL (L: Plua_State): Integer; cdecl;

function ULuaTextGL_Dummy(L: Plua_State): Integer; cdecl;

implementation

function ULuaTextGL_Dummy(L: Plua_State): Integer; cdecl;
begin
  result:=0; // number of results
end;

const
  ULuaTextGL_Lib_f: array [0..1] of lual_reg = (
   (name:'Print';func:ULuaTextGL_Dummy),
   (name:nil;func:nil)
   );

function luaopen_TextGL (L: Plua_State): Integer; cdecl;
begin
    luaL_register(L,'TextGL',@ULuaTextGL_Lib_f[0]);
    result:=1;
end;
end.
