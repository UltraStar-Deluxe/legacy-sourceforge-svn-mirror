unit UFiles;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses SysUtils,
     ULog,
     UMusic,
     USongs,
     USong;

procedure ResetSingTemp;
function  SaveSong(Song: TSong; Lines: TLines; Name: string; Relative: boolean): boolean;

var
  SongFile: TextFile;   // all procedures in this unit operates on this file
  FileLineNo: integer;  //Line which is readed at Last, for error reporting

  // variables available for all procedures
  Base    : array[0..1] of integer;
  Rel     : array[0..1] of integer;
  Mult    : integer = 1;
  MultBPM : integer = 4;

implementation

uses TextGL,
     UIni,
		 UPlatform,
     UMain;
     
//--------------------
// Resets the temporary Sentence Arrays for each Player and some other Variables
//--------------------
procedure ResetSingTemp;
var
  Pet:  integer;
begin
  SetLength(Lines, Length(Player));
  for Pet := 0 to High(Player) do begin
    SetLength(Lines[Pet].Line, 1);
    SetLength(Lines[Pet].Line[0].Note, 0);
    Lines[Pet].Line[0].Lyric := '';
    Lines[Pet].Line[0].LyricWidth := 0;
    Player[pet].Score := 0;
    Player[pet].IlNut := 0;
    Player[pet].HighNote := -1;
  end;

  (* FIXME
  //Reset Path and Filename Values to Prevent Errors in Editor
  if assigned( CurrentSong ) then
  begin
    SetLength(CurrentSong.BPM, 0);
    CurrentSong.Path := '';
    CurrentSong.FileName := '';
  end;
  *)
  
//  CurrentSong := nil;
end;


//--------------------
// Saves a Song
//--------------------
function SaveSong(Song: TSong; Lines: TLines; Name: string; Relative: boolean): boolean;
var
  C:      integer;
  N:      integer;
  S:      string;
  B:      integer;
  RelativeSubTime:    integer;
  NoteState: String;

begin
//  Relative := true; // override (idea - use shift+S to save with relative)
  AssignFile(SongFile, Name);
  Rewrite(SongFile);

  WriteLn(SongFile, '#TITLE:' + Song.Title + '');
  WriteLn(SongFile, '#ARTIST:' + Song.Artist);

  if Song.Creator     <> '' then    WriteLn(SongFile, '#CREATOR:'     + Song.Creator);
  if Song.Edition     <> 'Unknown' then WriteLn(SongFile, '#EDITION:' + Song.Edition);
  if Song.Genre       <> 'Unknown' then   WriteLn(SongFile, '#GENRE:' + Song.Genre);
  if Song.Language    <> 'Unknown' then    WriteLn(SongFile, '#LANGUAGE:'    + Song.Language);

  WriteLn(SongFile, '#MP3:' + Song.Mp3);

  if Song.Cover       <> '' then    WriteLn(SongFile, '#COVER:'       + Song.Cover);
  if Song.Background  <> '' then    WriteLn(SongFile, '#BACKGROUND:'  + Song.Background);
  if Song.Video       <> '' then    WriteLn(SongFile, '#VIDEO:'       + Song.Video);
  if Song.VideoGAP    <> 0  then    WriteLn(SongFile, '#VIDEOGAP:'    + FloatToStr(Song.VideoGAP));
  if Song.Resolution  <> 4  then    WriteLn(SongFile, '#RESOLUTION:'  + IntToStr(Song.Resolution));
  if Song.NotesGAP    <> 0  then    WriteLn(SongFile, '#NOTESGAP:'    + IntToStr(Song.NotesGAP));
  if Song.Start       <> 0  then    WriteLn(SongFile, '#START:'       + FloatToStr(Song.Start));
  if Song.Finish      <> 0  then    WriteLn(SongFile, '#END:'         + IntToStr(Song.Finish));
  if Relative               then    WriteLn(SongFile, '#RELATIVE:yes');

  WriteLn(SongFile, '#BPM:' + FloatToStr(Song.BPM[0].BPM / 4));
  WriteLn(SongFile, '#GAP:' + FloatToStr(Song.GAP));

  RelativeSubTime := 0;
  for B := 1 to High(CurrentSong.BPM) do
    WriteLn(SongFile, 'B ' + FloatToStr(CurrentSong.BPM[B].StartBeat) + ' ' + FloatToStr(CurrentSong.BPM[B].BPM/4));

  for C := 0 to Lines.High do begin
    for N := 0 to Lines.Line[C].HighNote do begin
      with Lines.Line[C].Note[N] do begin


        //Golden + Freestyle Note Patch
        case Lines.Line[C].Note[N].NoteType of
          0: NoteState := 'F ';
          1: NoteState := ': ';
          2: NoteState := '* ';
        end; // case
        S := NoteState + IntToStr(Start-RelativeSubTime) + ' ' + IntToStr(Lenght) + ' ' + IntToStr(Tone) + ' ' + Text;


        WriteLn(SongFile, S);
      end; // with
    end; // N

    if C < Lines.High then begin      // don't write end of last sentence
      if not Relative then
        S := '- ' + IntToStr(Lines.Line[C+1].Start)
      else begin
        S := '- ' + IntToStr(Lines.Line[C+1].Start - RelativeSubTime) +
          ' ' + IntToStr(Lines.Line[C+1].Start - RelativeSubTime);
        RelativeSubTime := Lines.Line[C+1].Start;
      end;
      WriteLn(SongFile, S);
    end;

  end; // C


  WriteLn(SongFile, 'E');
  CloseFile(SongFile);
end;

end.
