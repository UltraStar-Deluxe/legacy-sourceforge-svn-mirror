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

function IsAlphaChar(ch: WideChar): boolean; overload;
function IsAlphaChar(ch: UCS4Char): boolean; overload;

function IsNumericChar(ch: WideChar): boolean; overload;
function IsNumericChar(ch: UCS4Char): boolean; overload;

function IsAlphaNumericChar(ch: WideChar): boolean; overload;
function IsAlphaNumericChar(ch: UCS4Char): boolean; overload;

function IsPunctuationChar(ch: WideChar): boolean; overload;
function IsPunctuationChar(ch: UCS4Char): boolean; overload;

function IsControlChar(ch: WideChar): boolean; overload;
function IsControlChar(ch: UCS4Char): boolean; overload;

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

function UTF8CompareStr(const S1, S2: UTF8String): integer;
function UTF8CompareText(const S1, S2: UTF8String): integer;

function UTF8StartsText(const SubText, Text: UTF8String): boolean;

function UTF8ContainsStr(const Text, SubText: UTF8String): boolean;
function UTF8ContainsText(const Text, SubText: UTF8String): boolean;

function UTF8UpperCase(const str: UTF8String): UTF8String;
function UTF8LowerCase(const str: UTF8String): UTF8String;

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
function WideStringUpperCase(const str: WideString) : WideString; overload;
function WideStringUpperCase(ch: WideChar): WideString; overload;

function StringReplaceW(const text : WideString; search, rep: WideChar): WideString;

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

function IsAlphaChar(ch: UCS4Char): boolean;
begin
  Result := IsAlphaChar(WideChar(Ord(ch)));
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

function IsNumericChar(ch: UCS4Char): boolean;
begin
  Result := IsNumericChar(WideChar(Ord(ch)));
end;

function IsAlphaNumericChar(ch: WideChar): boolean;
begin
  Result := (IsAlphaChar(ch) or IsNumericChar(ch));
end;

function IsAlphaNumericChar(ch: UCS4Char): boolean;
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

function IsPunctuationChar(ch: UCS4Char): boolean;
begin
  Result := IsPunctuationChar(WideChar(Ord(ch)));
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

function IsControlChar(ch: UCS4Char): boolean;
begin
  Result := IsControlChar(WideChar(Ord(ch)));
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

function UTF8CompareStr(const S1, S2: UTF8String): integer;
begin
  // FIXME
  Result := WideCompareStr(UTF8Decode(S1), UTF8Decode(S2));
end;

function UTF8CompareText(const S1, S2: UTF8String): integer;
begin
  // FIXME
  Result := WideCompareText(UTF8Decode(S1), UTF8Decode(S2));
end;

function UTF8StartsStr(const SubText, Text: UTF8String): boolean;
begin
  // TODO: use WideSameStr ()?
  Result := (Pos(SubText, Text) = 1);
end;

function UTF8StartsText(const SubText, Text: UTF8String): boolean;
begin
  // TODO: use WideSameText?
  Result := (Pos(UTF8UpperCase(SubText), UTF8UpperCase(Text)) = 1);
end;

function UTF8ContainsStr(const Text, SubText: UTF8String): boolean;
begin
  Result := Pos(SubText, Text) > 0;
end;

function UTF8ContainsText(const Text, SubText: UTF8String): boolean;
begin
  Result := Pos(UTF8UpperCase(SubText), UTF8UpperCase(Text)) > 0;
end;

function UTF8UpperCase(const str: UTF8String): UTF8String;
begin
  Result := UTF8Encode(WideStringUpperCase(UTF8Decode(str)));
end;

function UTF8LowerCase(const str: UTF8String): UTF8String;
begin
  // FIXME
  Result := UTF8Encode(WideLowerCase(UTF8Decode(str)));
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

function WideStringUpperCase(ch: WideChar): WideString;
begin
  // If WideChar #0 is converted to a WideString in Delphi, a string with
  // length 1 and a single char #0 is returned. In FPC an empty (length=0)
  // string will be returned. This will crash, if a non printable key was
  // pressed, its char code (#0) is translated to upper-case and the the first
  // character is accessed with Result[1].
  // We cannot catch this error in the WideString parameter variant as the string
  // has length 0 already.
  
  // Force min. string length of 1
  if (ch = #0) then
    Result := #0
  else
    Result := WideStringUpperCase(WideString(ch));
end;

function WideStringUpperCase(const str: WideString): WideString;
begin
  // On Linux and MacOSX the cwstring unit is necessary for Unicode function-calls.
  // Otherwise you will get an EIntOverflow exception (thrown by unimplementedwidestring()).
  // The Unicode manager cwstring does not work with MacOSX at the moment because
  // of missing references to iconv.

  {.$IFNDEF DARWIN}
  {$IFDEF NOIGNORE}
    Result := WideUpperCase(str)
  {$ELSE}
    Result := UTF8Decode(UpperCase(UTF8Encode(str)));
  {$ENDIF}
end;

function StringReplaceW(const text : WideString; search, rep: WideChar) : WideString;
var
  iPos  : integer;
//  sTemp : WideString;
begin
(*
  result := text;
  iPos   := Pos(search, result);
  while (iPos > 0) do
  begin
    sTemp  := copy(result, iPos + length(search), length(result));
    result := copy(result, 1, iPos - 1) + rep + sTEmp;
    iPos   := Pos(search, result);
  end;
*)
  result := text;

  if search = rep then
    exit;

  for iPos := 1 to length(result) do
  begin
    if result[iPos] = search then
      result[iPos] := rep;
  end;
end;

end.
