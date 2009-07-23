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

unit UFiles;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  SysUtils,
  ULog,
  UMusic,
  USongs,
  USong;

procedure ResetSingTemp;

type
  TSaveSongResult = (ssrOK, ssrFileError, ssrEncodingError);

{**
 * Throws a TEncodingException if the song's fields cannot be encoded in the
 * requested encoding.
 *}
function SaveSong(const Song: TSong; const Lines: TLines; const Name: string; Relative: boolean): TSaveSongResult;

implementation

uses
  TextGL,
  UIni,
  UNote,
  UPlatform,
  UUnicodeUtils,
  UTextEncoding;

//--------------------
// Resets the temporary Sentence Arrays for each Player and some other Variables
//--------------------
procedure ResetSingTemp;
var
  Count:  integer;
begin
  SetLength(Lines, Length(Player));
  for Count := 0 to High(Player) do begin
    SetLength(Lines[Count].Line, 1);
    SetLength(Lines[Count].Line[0].Note, 0);
    Lines[Count].Line[0].Lyric := '';
    Player[Count].Score := 0;
    Player[Count].LengthNote := 0;
    Player[Count].HighNote := -1;
  end;
end;

//--------------------
// Saves a Song
//--------------------
function SaveSong(const Song: TSong; const Lines: TLines; const Name: string; Relative: boolean): TSaveSongResult;
var
  C:      integer;
  N:      integer;
  S:      AnsiString;
  B:      integer;
  RelativeSubTime: integer;
  NoteState: AnsiString;
  SongFile: TextFile;

  function EncodeToken(const Str: UTF8String): AnsiString;
  var
    Success: boolean;
  begin
    Success := EncodeStringUTF8(Str, Result, Song.Encoding);
    if (not Success) then
      SaveSong := ssrEncodingError;
  end;

  function EncodeFilename(const Filename: WideString): AnsiString;
  begin
    // TODO: Unicode
    Result := Filename;
  end;

begin
  //  Relative := true; // override (idea - use shift+S to save with relative)
  AssignFile(SongFile, Name);
  Rewrite(SongFile);

  Result := ssrOK;

  if (Song.Encoding = encUTF8) then
    Write(SongFile, UTF8_BOM);

  Writeln(SongFile, '#ENCODING:' + EncodingName(Song.Encoding));
  Writeln(SongFile, '#TITLE:'    + EncodeToken(Song.Title));
  Writeln(SongFile, '#ARTIST:'   + EncodeToken(Song.Artist));

  if Song.Creator     <> ''        then Writeln(SongFile, '#CREATOR:'   + EncodeToken(Song.Creator));
  if Song.Edition     <> 'Unknown' then Writeln(SongFile, '#EDITION:'   + EncodeToken(Song.Edition));
  if Song.Genre       <> 'Unknown' then Writeln(SongFile, '#GENRE:'     + EncodeToken(Song.Genre));
  if Song.Language    <> 'Unknown' then Writeln(SongFile, '#LANGUAGE:'  + EncodeToken(Song.Language));

  Writeln(SongFile, '#MP3:' + EncodeFilename(Song.Mp3));
  if Song.Cover       <> '' then    Writeln(SongFile, '#COVER:'       + EncodeFilename(Song.Cover));
  if Song.Background  <> '' then    Writeln(SongFile, '#BACKGROUND:'  + EncodeFilename(Song.Background));
  if Song.Video       <> '' then    Writeln(SongFile, '#VIDEO:'       + EncodeFilename(Song.Video));

  if Song.VideoGAP    <> 0  then    Writeln(SongFile, '#VIDEOGAP:'    + FloatToStr(Song.VideoGAP));
  if Song.Resolution  <> 4  then    Writeln(SongFile, '#RESOLUTION:'  + IntToStr(Song.Resolution));
  if Song.NotesGAP    <> 0  then    Writeln(SongFile, '#NOTESGAP:'    + IntToStr(Song.NotesGAP));
  if Song.Start       <> 0  then    Writeln(SongFile, '#START:'       + FloatToStr(Song.Start));
  if Song.Finish      <> 0  then    Writeln(SongFile, '#END:'         + IntToStr(Song.Finish));
  if Relative               then    Writeln(SongFile, '#RELATIVE:yes');

  Writeln(SongFile, '#BPM:' + FloatToStr(Song.BPM[0].BPM / 4));
  Writeln(SongFile, '#GAP:' + FloatToStr(Song.GAP));

  RelativeSubTime := 0;
  for B := 1 to High(Song.BPM) do
    Writeln(SongFile, 'B ' + FloatToStr(Song.BPM[B].StartBeat) + ' '
                           + FloatToStr(Song.BPM[B].BPM/4));

  for C := 0 to Lines.High do
  begin
    for N := 0 to Lines.Line[C].HighNote do
    begin
      with Lines.Line[C].Note[N] do
      begin
        //Golden + Freestyle Note Patch
        case Lines.Line[C].Note[N].NoteType of
          ntFreestyle: NoteState := 'F ';
          ntNormal: NoteState := ': ';
          ntGolden: NoteState := '* ';
        end; // case
        S := NoteState + IntToStr(Start-RelativeSubTime) + ' '
                       + IntToStr(Length) + ' '
                       + IntToStr(Tone) + ' '
                       + EncodeToken(Text);

        Writeln(SongFile, S);
      end; // with
    end; // N

    if C < Lines.High then // don't write end of last sentence
    begin
      if not Relative then
        S := '- ' + IntToStr(Lines.Line[C+1].Start)
      else
      begin
        S := '- ' + IntToStr(Lines.Line[C+1].Start - RelativeSubTime) +
          ' ' + IntToStr(Lines.Line[C+1].Start - RelativeSubTime);
        RelativeSubTime := Lines.Line[C+1].Start;
      end;
      Writeln(SongFile, S);
    end;
  end; // C

  Writeln(SongFile, 'E');

  CloseFile(SongFile);
end;

end.

