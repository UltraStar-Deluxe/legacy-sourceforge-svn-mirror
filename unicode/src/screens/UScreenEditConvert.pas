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

unit UScreenEditConvert;

{*
 * See
 * MIDI Recommended Practice (RP-017): SMF Lyric Meta Event Definition
 *   http://www.midi.org/techspecs/rp17.php
 * MIDI Recommended Practice (RP-026): SMF Language and Display Extensions
 *   http://www.midi.org/techspecs/rp26.php
 * MIDI File Format
 *   http://www.sonicspot.com/guide/midifiles.html
 *}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  math,
  UMenu,
  SDL,
  {$IFDEF UseMIDIPort}
  MidiFile,
  MidiOut,
  {$ENDIF}
  ULog,
  USongs,
  USong,
  UMusic,
  UThemes;

type
  TMidiNote = record
    Event:     integer;
    EventType: integer;
    Channel:   integer;
    Start:     real;
    Len:       real;
    Data1:     integer;
    Data2:     integer;
    Str:       AnsiString;
  end;

  TTrack = record
    Note:   array of TMidiNote;
    Name:   AnsiString;
    Status: set of (tsNotes, tsLyrics);
    LyricType: set of (ltKMIDI, ltSMFLyric);
    NoteType:  (ntNone, ntAvail);
  end;

  TNote = record
    Start:    integer;
    Len:      integer;
    Tone:     integer;
    Lyric:    AnsiString;
    NewSentence:  boolean;
  end;

  TArrayTrack = array of TTrack;

  TScreenEditConvert = class(TMenu)
    private
      Tracks:    TArrayTrack; // current track
      ColR:      array[0..100] of real;
      ColG:      array[0..100] of real;
      ColB:      array[0..100] of real;
      Len:       real;
      SelTrack:  integer;     // index of selected track
      //FileName:  string;

      {$IFDEF UseMIDIPort}
      MidiFile:  TMidiFile;
      MidiTrack: TMidiTrack;
      MidiEvent: PMidiEvent;
      MidiOut:   TMidiOutput;
      {$ENDIF}

      Song:      TSong;
      Lines:     TLines;
      BPM:       real;
      Ticks:     real;
      Note:      array of TNote;

      procedure AddLyric(Start: integer; Text: AnsiString);
      procedure Extract;

      {$IFDEF UseMIDIPort}
      procedure MidiFile1MidiEvent(event: PMidiEvent);
      {$ENDIF}

      function CountSelectedTracks: integer;

    public
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function Draw: boolean; override;
      procedure onHide; override;
  end;

var
  ConversionFileName: string;

implementation

uses
  SysUtils,
  TextGL,
  gl,
  UDrawTexture,
  UFiles,
  UGraphic,
  UIni,
  UMain,
  UPath,
  USkins,
  UTextEncoding,
  UUnicodeUtils;

const
  // MIDI/KAR lyrics are specified to be ASCII only.
  // Assume backward compatible CP1252 encoding.
  DEFAULT_ENCODING = encCP1252;

const
  MIDI_EVENTTYPE_NOTEOFF    = $8;
  MIDI_EVENTTYPE_NOTEON     = $9;
  MIDI_EVENTTYPE_META_SYSEX = $F;

  MIDI_EVENT_META = $FF;
  MIDI_META_TEXT   = $1;
  MIDI_META_LYRICS = $5;

function TScreenEditConvert.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SResult: TSaveSongResult;
  Playing: boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          {$IFDEF UseMIDIPort}
          MidiFile.StopPlaying;
          {$ENDIF}
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenEdit);
        end;

      SDLK_RETURN:
        begin
          if Interaction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            ScreenOpen.BackScreen := @ScreenEditConvert;
            FadeTo(@ScreenOpen);
          end;

          if Interaction = 1 then
          begin
            {$IFDEF UseMIDIPort}
            MidiFile.OnMidiEvent := MidiFile1MidiEvent;
            //MidiFile.GoToTime(MidiFile.GetTrackLength div 2);
            MidiFile.StartPlaying;
            {$ENDIF}
          end;

          if Interaction = 2 then
          begin
            {$IFDEF UseMIDIPort}
            MidiFile.OnMidiEvent := nil;
            MidiFile.StartPlaying;
            {$ENDIF}
          end;

          if Interaction = 3 then
          begin
            if CountSelectedTracks > 0 then
            begin
              Extract;
              SResult := SaveSong(Song, Lines, ChangeFileExt(ConversionFileName, '.txt'),
                       false);
              if (SResult <> ssrOK) then
              begin
                ScreenPopupError.ShowPopup('Could not save file');
              end;
            end;
          end;

        end;

      SDLK_SPACE:
        begin
          if (Tracks[SelTrack].NoteType = ntAvail) and
             (Tracks[SelTrack].LyricType <> []) then
          begin
            if (Tracks[SelTrack].Status = []) then
              Tracks[SelTrack].Status := [tsNotes]
            else if (Tracks[SelTrack].Status = [tsNotes]) then
              Tracks[SelTrack].Status := [tsLyrics]
            else if (Tracks[SelTrack].Status = [tsLyrics]) then
              Tracks[SelTrack].Status := [tsNotes, tsLyrics]
            else if (Tracks[SelTrack].Status = [tsNotes, tsLyrics]) then
              Tracks[SelTrack].Status := [];
          end
          else if (Tracks[SelTrack].NoteType = ntAvail) then
          begin
            if (Tracks[SelTrack].Status = []) then
              Tracks[SelTrack].Status := [tsNotes]
            else
              Tracks[SelTrack].Status := [];
          end
          else if (Tracks[SelTrack].LyricType <> []) then
          begin
            if (Tracks[SelTrack].Status = []) then
              Tracks[SelTrack].Status := [tsLyrics]
            else
              Tracks[SelTrack].Status := [];
          end;

          {$IFDEF UseMIDIPort}
          Playing := (MidiFile.GetCurrentTime > 0);
          MidiFile.StopPlaying();
          MidiTrack := MidiFile.GetTrack(SelTrack);
          if tsNotes in Tracks[SelTrack].Status then
            MidiTrack.OnMidiEvent := MidiFile1MidiEvent
          else
            MidiTrack.OnMidiEvent := nil;
          if (Playing) then
            MidiFile.ContinuePlaying();
          {$ENDIF}
        end;

      SDLK_RIGHT:
        begin
          InteractNext;
        end;

      SDLK_LEFT:
        begin
          InteractPrev;
        end;

      SDLK_DOWN:
        begin
          Inc(SelTrack);
          if SelTrack > High(Tracks) then
            SelTrack := 0;
        end;
      SDLK_UP:
        begin
          Dec(SelTrack);
          if SelTrack < 0 then
            SelTrack := High(Tracks);
        end;
    end;
  end;
end;

procedure TScreenEditConvert.AddLyric(Start: integer; Text: AnsiString);
var
  N:    integer;
begin
  for N := 0 to High(Note) do
  begin
    if Note[N].Start = Start then
    begin
      // Line Feed -> end of paragraph
      if Copy(Text, 1, 1) = #$0A then
        Delete(Text, 1, 1);
      // Carriage Return -> end of line
      if Copy(Text, 1, 1) = #$0D then
      begin
        Delete(Text, 1, 1);
        Note[N].NewSentence := true;
      end;

      // overwrite lyric or append
      if Note[N].Lyric = '-' then
        Note[N].Lyric := Text
      else
        Note[N].Lyric := Note[N].Lyric + Text;

      Exit;
    end;
  end;

  DebugWriteln('Missing: ' + Text);
end;

procedure TScreenEditConvert.Extract;
var
  T:    integer;
  C:    integer;
  N:    integer;
  Nu:   integer;
  NoteTemp: TNote;
  Move: integer;
  Max, Min: integer;
begin
  // song info
  Song := TSong.Create();
  Song.Clear();
  Song.Resolution := 4;
  SetLength(Song.BPM, 1);
  Song.BPM[0].BPM := BPM*4;

  SetLength(Note, 0);

  // extract notes
  for T := 0 to High(Tracks) do
  begin
    if tsNotes in Tracks[T].Status then
    begin
      for N := 0 to High(Tracks[T].Note) do
      begin
        if (Tracks[T].Note[N].EventType = MIDI_EVENTTYPE_NOTEON) and
           (Tracks[T].Note[N].Data2 > 0) then
        begin
          Nu := Length(Note);
          SetLength(Note, Nu + 1);
          Note[Nu].Start := Round(Tracks[T].Note[N].Start / Ticks);
          Note[Nu].Len := Round(Tracks[T].Note[N].Len / Ticks);
          Note[Nu].Tone := Tracks[T].Note[N].Data1 - 12*5;
          Note[Nu].Lyric := '-';
        end;
      end;
    end;
  end;

  // extract lyrics
  for T := 0 to High(Tracks) do
  begin
    if tsLyrics in Tracks[T].Status then
    begin
      for N := 0 to High(Tracks[T].Note) do
      begin
        if (Tracks[T].Note[N].Event = MIDI_EVENT_META) and
           (Tracks[T].Note[N].Data1 = MIDI_META_LYRICS) then
        begin
          AddLyric(Round(Tracks[T].Note[N].Start / Ticks), Tracks[T].Note[N].Str);
        end;
      end;
    end;
  end;

  // sort notes
  for N := 0 to High(Note) do
    for Nu := 0 to High(Note)-1 do
      if Note[Nu].Start > Note[Nu+1].Start then
      begin
        NoteTemp := Note[Nu];
        Note[Nu] := Note[Nu+1];
        Note[Nu+1] := NoteTemp;
      end;

  // move to 0 at beginning
  Move := Note[0].Start;
  for N := 0 to High(Note) do
    Note[N].Start := Note[N].Start - Move;

  // copy notes
  SetLength(Lines.Line, 1);
  Lines.Number := 1;
  Lines.High := 0;

  C := 0;
  N := 0;
  Lines.Line[C].HighNote := -1;

  for Nu := 0 to High(Note) do
  begin
    if Note[Nu].NewSentence then // new line
    begin
      SetLength(Lines.Line, Length(Lines.Line)+1);
      Lines.Number := Lines.Number + 1;
      Lines.High := Lines.High + 1;
      C := C + 1;
      N := 0;
      SetLength(Lines.Line[C].Note, 0);
      Lines.Line[C].HighNote := -1;

      //Calculate Start of the Last Sentence
      if (C > 0) and (Nu > 0) then
      begin
        Max := Note[Nu].Start;
        Min := Note[Nu-1].Start + Note[Nu-1].Len;
        
        case (Max - Min) of
          0:    Lines.Line[C].Start := Max;
          1:    Lines.Line[C].Start := Max;
          2:    Lines.Line[C].Start := Max - 1;
          3:    Lines.Line[C].Start := Max - 2;
          else
            if ((Max - Min) > 4) then
              Lines.Line[C].Start := Min + 2
            else
              Lines.Line[C].Start := Max;

        end; // case

      end;
    end;

    // create space for new note
    SetLength(Lines.Line[C].Note, Length(Lines.Line[C].Note)+1);
    Inc(Lines.Line[C].HighNote);

    // initialize note
    Lines.Line[C].Note[N].Start := Note[Nu].Start;
    Lines.Line[C].Note[N].Length := Note[Nu].Len;
    Lines.Line[C].Note[N].Tone := Note[Nu].Tone;
    Lines.Line[C].Note[N].Text := DecodeStringUTF8(Note[Nu].Lyric, DEFAULT_ENCODING);
    //All Notes are Freestyle when Converted Fix:
    Lines.Line[C].Note[N].NoteType := ntNormal;
    Inc(N);
  end;
end;

function TScreenEditConvert.CountSelectedTracks: integer;
var
  T:    integer; // track
begin
  Result := 0;
  for T := 0 to High(Tracks) do
    if tsNotes in Tracks[T].Status then
      Inc(Result);
end;

{$IFDEF UseMIDIPort}
procedure TScreenEditConvert.MidiFile1MidiEvent(event: PMidiEvent);
begin
  //Log.LogStatus(IntToStr(event.event), 'MIDI');
  try
    MidiOut.PutShort(event.event, event.data1, event.data2);
  except
    MidiFile.StopPlaying();
  end;
end;
{$ENDIF}

constructor TScreenEditConvert.Create;
var
  P:  integer;
begin
  inherited Create;
  AddButton(40, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(15, 5, 0, 0, 0, 'Open');
  //Button[High(Button)].Text[0].Size := 11;

  AddButton(160, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(25, 5, 0, 0, 0, 'Play');

  AddButton(280, 20, 200, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(25, 5, 0, 0, 0, 'Play Selected');

  AddButton(500, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(20, 5, 0, 0, 0, 'Save');

  ConversionFileName := 'D:/daten/africa.mid';//GamePath + 'file.mid';

  for P := 0 to 100 do
  begin
    ColR[P] := Random(10)/10;
    ColG[P] := Random(10)/10;
    ColB[P] := Random(10)/10;
  end;

end;

procedure TScreenEditConvert.OnShow;
var
  T:    integer; // track
  N:    integer; // note
begin
  inherited;

{$IFDEF UseMIDIPort}
  MidiOut := TMidiOutput.Create(nil);
  //if Ini.Debug = 1 then
  //  MidiOut.ProductName := 'Microsoft GS Wavetable SW Synth'; // for my kxproject without midi table
  Log.LogInfo(MidiOut.ProductName, 'MIDI');
  MidiOut.Open;
  MidiFile := nil;

  if FileExists(ConversionFileName) then
  begin
    MidiFile := TMidiFile.Create(nil);
    MidiFile.Filename := ConversionFileName;
    MidiFile.ReadFile;

    Len := 0;
    SelTrack := 0;
    BPM := MidiFile.Bpm;
    Ticks := MidiFile.TicksPerQuarter / 4;

    SetLength(Tracks, MidiFile.NumberOfTracks);
    for T := 0 to MidiFile.NumberOfTracks-1 do
    begin
      MidiTrack := MidiFile.GetTrack(T);
      MidiTrack.OnMidiEvent := nil;
      Tracks[T].Name := MidiTrack.getName;
      Tracks[T].NoteType := ntNone;
      Tracks[T].LyricType := [];
      Tracks[T].Status := [];

      SetLength(Tracks[T].Note, MidiTrack.getEventCount());
      for N := 0 to MidiTrack.getEventCount-1 do
      begin
        MidiEvent := MidiTrack.GetEvent(N);

        Tracks[T].Note[N].Start     := MidiEvent.time;
        Tracks[T].Note[N].Len       := MidiEvent.len;
        Tracks[T].Note[N].Event     := MidiEvent.event;
        Tracks[T].Note[N].EventType := MidiEvent.event shr 4;
        Tracks[T].Note[N].Channel   := MidiEvent.event and $0F;
        Tracks[T].Note[N].Data1     := MidiEvent.data1;
        Tracks[T].Note[N].Data2     := MidiEvent.data2;
        Tracks[T].Note[N].Str       := MidiEvent.str;

        if (Tracks[T].Note[N].Event = MIDI_EVENT_META) then
        begin
          case (Tracks[T].Note[N].Data1) of
            MIDI_META_TEXT: begin
              if (Copy(Tracks[T].Note[N].Str, 1, 6) = '@KMIDI') then
              begin
                Tracks[T].LyricType := Tracks[T].LyricType + [ltKMIDI];
                //Tracks[T].Status := [tsLyrics];
                //DebugWriteln('Text: ' + Tracks[T].Note[N].Str);
              end;
            end;
            MIDI_META_LYRICS: begin
              // lyrics in Standard Midi File format found
              Tracks[T].LyricType := Tracks[T].LyricType + [ltSMFLyric];
              //Tracks[T].Status := [tsLyrics];
            end;
          end;
        end
        else if (Tracks[T].Note[N].EventType = MIDI_EVENTTYPE_NOTEON) then
        begin
          // notes available
          Tracks[T].NoteType := ntAvail;
        end;

        if Tracks[T].Note[N].Start + Tracks[T].Note[N].Len > Len then
          Len := Tracks[T].Note[N].Start + Tracks[T].Note[N].Len;
      end;
    end;

  end;

  Interaction := 0;
{$ENDIF}
end;

function TScreenEditConvert.Draw: boolean;
var
  Count:  integer;
  Count2: integer;
  Bottom: real;
  X:      real;
  Y:      real;
  Height: real;
  YSkip:  real;
  TrackName: UTF8String;
begin
  // draw static menu
  inherited Draw;

  Y := 100;

  Height := min(480, 40 * Length(Tracks));
  Bottom := Y + Height;

  YSkip := Height / Length(Tracks);

  // highlight selected track
  DrawQuad(10, Y+SelTrack*YSkip, 780, YSkip, 0.8, 0.8, 0.8);

  // track-selection info
  for Count := 0 to High(Tracks) do
    if Tracks[Count].Status <> [] then
      DrawQuad(10, Y + Count*YSkip, 50, YSkip, 0.8, 0.3, 0.3);
  glColor3f(0, 0, 0);
  for Count := 0 to High(Tracks) do
  begin
    if Tracks[Count].NoteType = ntAvail then
    begin
      if tsNotes in Tracks[Count].Status then
        glColor3f(0, 0, 0)
      else
        glColor3f(0.7, 0.7, 0.7);
      SetFontPos(25, Y + Count*YSkip + 10);
      SetFontSize(15);
      glPrint('N');
    end;
    if Tracks[Count].LyricType <> [] then
    begin
      if tsLyrics in Tracks[Count].Status then
        glColor3f(0, 0, 0)
      else
        glColor3f(0.7, 0.7, 0.7);
      SetFontPos(40, Y + Count*YSkip + 10);
      SetFontSize(15);
      glPrint('L');
    end;
  end;

  DrawLine( 10, Y,  10, Bottom, 0, 0, 0);
  DrawLine( 60, Y,  60, Bottom, 0, 0, 0);
  DrawLine(790, Y, 790, Bottom, 0, 0, 0);

  for Count := 0 to Length(Tracks) do
    DrawLine(10, Y + Count*YSkip, 790, Y + Count*YSkip, 0, 0, 0);

  for Count := 0 to High(Tracks) do
  begin
    // track names should be ASCII only, but who knows
    TrackName := DecodeStringUTF8(Tracks[Count].Name, DEFAULT_ENCODING);

    SetFontPos(65, Y + Count*YSkip);
    SetFontSize(15);
    glPrint(TrackName);
  end;

  for Count := 0 to High(Tracks) do
  begin
    for Count2 := 0 to High(Tracks[Count].Note) do
    begin
      if Tracks[Count].Note[Count2].EventType = MIDI_EVENTTYPE_NOTEON then
        DrawQuad(60 + Tracks[Count].Note[Count2].Start/Len * 725,
                 Y + (Count+1)*YSkip - Tracks[Count].Note[Count2].Data1*35/127,
                 3, 3,
                 ColR[Count], ColG[Count], ColB[Count]);
      if Tracks[Count].Note[Count2].EventType = 15 then
        DrawLine(60 + Tracks[Count].Note[Count2].Start/Len * 725, Y + 0.75 * YSkip + Count*YSkip,
                 60 + Tracks[Count].Note[Count2].Start/Len * 725, Y + YSkip + Count*YSkip,
                 ColR[Count], ColG[Count], ColB[Count]);
    end;
  end;

  // playing line
  {$IFDEF UseMIDIPort}
  X := 60 + MidiFile.GetCurrentTime/MidiFile.GetTrackLength*730;
  {$ENDIF}
  DrawLine(X, Y, X, Bottom, 0.3, 0.3, 0.3);

  Result := true;
end;

procedure TScreenEditConvert.onHide;
begin
{$IFDEF UseMIDIPort}
  MidiFile.Free;
  MidiOut.Close;
  MidiOut.Free;
{$ENDIF}
end;

end.
