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

unit UUnicodeUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Windows;

(*
 * Character classes
 *)

function IsAlphaChar(ch: WideChar): boolean;
function IsNumericChar(ch: WideChar): boolean;
function IsAlphaNumericChar(ch: WideChar): boolean;
function IsPunctuationChar(ch: WideChar): boolean;
function IsControlChar(ch: WideChar): boolean;

function UTF8ToUCS4String(const str: UTF8String): UCS4String;
function UCS4ToUTF8String(const str: UCS4String): UTF8String;

implementation

function IsAlphaChar(ch: WideChar): boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := IsCharAlphaW(ch);
  {$ELSE}
    // TODO: add chars > 255
    case ch of
      'A'..'Z',  // A-Z
      'a'..'z',  // a-z
      #170,#181,#186,
      #192..#214,
      #216..#246,
      #248..#255:
        Result := true;
      else
        Result := false;
    end;
  {$ENDIF}
end;

function IsNumericChar(ch: WideChar): boolean;
begin
  // ignore non-arabic numerals as we do not want to handle them
  case ch of
    '0'..'9':
      Result := true;
    else
      Result := false;
  end;
end;

function IsAlphaNumericChar(ch: WideChar): boolean;
begin
  Result := (IsAlphaChar(ch) or IsNumericChar(ch));
end;

function IsPunctuationChar(ch: WideChar): boolean;
begin
  // TODO: add chars > 255?
  case ch of
    ' '..'/',':'..'@','['..'`','{'..'~',
    #160..#191,#215,#247:
      Result := true;
    else
      Result := false;
  end;
end;

function IsControlChar(ch: WideChar): boolean;
begin
  case ch of
    #0..#31,
    #127..#159:
      Result := true;
    else
      Result := false;
  end;
end;

function UTF8ToUCS4String(const str: UTF8String): UCS4String;
begin
  Result := WideStringToUCS4String(UTF8Decode(str));
end;

function UCS4ToUTF8String(const str: UCS4String): UTF8String;
begin
  Result := UTF8Encode(UCS4StringToWideString(str));
end;

end.
