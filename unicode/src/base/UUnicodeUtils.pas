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
  SysUtils
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF}
  ;
(*
 * Character classes
 *)

function IsAlphaChar(ch: WideChar): boolean;
function IsNumericChar(ch: WideChar): boolean;
function IsAlphaNumericChar(ch: WideChar): boolean;
function IsPunctuationChar(ch: WideChar): boolean;
function IsControlChar(ch: WideChar): boolean;

{*
 * String format conversion
 *}

function UTF8ToUCS4String(const str: UTF8String): UCS4String;
function UCS4ToUTF8String(const str: UCS4String): UTF8String; overload;
function UCS4ToUTF8String(ch: UCS4Char): UTF8String; overload;

{**
 * Returns the number of characters (not bytes) in string str.
 *}
function LengthUTF8(const str: UTF8String): integer;

{**
 * Converts a UCS-4 char ch to its upper-case representation.
 *}
function UCS4UpperCase(ch: UCS4Char): UCS4Char; overload;

{**
 * Converts a UCS-4 string str to its upper-case representation.
 *}
function UCS4UpperCase(const str: UCS4String): UCS4String; overload;

{**
 *
 *}
function UCS4CharToString(ch: UCS4Char): UCS4String;

(*

 * Converts a WideString to its upper-case representation.
 * Wrapper for WideUpperCase. Needed because some plattforms have problems with
 * unicode support.
 *
 * Note that characters in UTF-16 might consist of one or two WideChar valus
 * (see surrogates). So instead of using WideStringUpperCase(ch)[1] for single
 * character access, convert to UCS-4 where each character is represented by
 * one UCS4Char. 
 *)
function WideStringUpperCase(const str: WideString) : WideString;


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

function UCS4ToUTF8String(ch: UCS4Char): UTF8String;
begin
  Result := UCS4ToUTF8String(UCS4CharToString(ch));
end;

function LengthUTF8(const str: UTF8String): integer;
begin
  Result := Length(UTF8ToUCS4String(str));
end;

function UCS4UpperCase(ch: UCS4Char): UCS4Char;
begin
  Result := UCS4UpperCase(UCS4CharToString(ch))[0];
end;

function UCS4UpperCase(const str: UCS4String): UCS4String;
begin
  // convert to upper-case as WideString and convert result back to UCS-4
  Result := WideStringToUCS4String(
            WideStringUpperCase(
            UCS4StringToWideString(str)));
end;

function UCS4CharToString(ch: UCS4Char): UCS4String;
begin
  SetLength(Result, 2);
  Result[0] := ch;
  Result[1] := 0;
end;

function WideStringUpperCase(const str: WideString): WideString;
begin
  // On Linux and MacOSX the cwstring unit is necessary for Unicode function-calls.
  // Otherwise you will get an EIntOverflow exception (thrown by unimplementedwidestring()).
  // The Unicode manager cwstring does not work with MacOSX at the moment because
  // of missing references to iconv. So we have to use Ansi... for the moment.

  {.$IFNDEF DARWIN}
  {$IFDEF NOIGNORE}
    Result := WideUpperCase(str)
  {$ELSE}
    Result := UTF8Decode(AnsiUpperCase(UTF8Encode(str)));
  {$ENDIF}
end;

end.
