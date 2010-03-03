unit UFiles;

interface

uses USongs,
     SysUtils,
     ULog,
     UMusic;

const
  DEFAULT_FADE_IN_TIME = 8;    //TODO in INI
  DEFAULT_FADE_OUT_TIME = 2;

procedure     InitializePaths; //Function sets All Absolute Paths eg. for Songs
function    ReadTXTHeader(var Song: TSong): boolean; //Reads Standard TXT Header
function    AnalyseFile(var Song: TSong): boolean; //Analyse Song File and Read Header
procedure   ClearSong(var Song: TSong); //Clears Song Header values

//Methodes Loading and Saving Songfiles
procedure ResetSingTemp;
procedure ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer; LoadFullFile: boolean);
function LoadSong(Name: string; LoadFullFile: boolean): boolean;
function CheckSong: boolean;
function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
procedure FindRefrainStart(var Song: TSong);
procedure SetMedleyMode;



var
  //Absolute Paths
  GamePath:         string;
  SoundPath:        string;
  SongPath:         string;
  LogPath:          string;
  ThemePath:        string;
  ScreenshotsPath:  string;
  CoversPath:       string;
  LanguagesPath:    string;
  PluginPath:       string;
  PlayListPath:     string;
  RecordingsPath:   string;

  SongFile: TextFile;   // all procedures in this unit operates on this file
  FileLineNo: integer;  //Line which is readed at Last, for error reporting

  // variables available for all procedures
  Base:       array[0..1] of integer;
  Rel:        array[0..1] of integer;
  Mult:     integer = 1;
  MultBPM:  integer = 4;

implementation
uses TextGL, UIni, UMain;


//--------------------
// Function sets all Absolute Paths e.g. Song Path and makes sure the Directorys exist
//--------------------
procedure InitializePaths;
var
  Writeable: Boolean;
begin
  GamePath :=   ExtractFilePath(ParamStr(0));

  SoundPath :=  GamePath + 'Sounds\';
  SongPath :=   GamePath + 'Songs\';
  LogPath := GamePath;
  ThemePath := GamePath + 'Themes\';
  ScreenshotsPath := GamePath + 'Screenshots\';
  CoversPath := GamePath + 'Covers\';
  LanguagesPath := GamePath + 'Languages\';
  PluginPath := GamePath + 'Plugins\';
  PlaylistPath := GamePath + 'Playlists\';
  RecordingsPath := GamePath + 'Recordings\';

  Writeable := true;

  //After Setting Paths, make sure that Paths exist
  If not DirectoryExists(SoundPath) then
    Writeable := ForceDirectories(SoundPath);

  If Writeable And (not DirectoryExists(SongPath)) then
    Writeable := ForceDirectories(SongPath);

  If Writeable And (not DirectoryExists(ThemePath)) then
    Writeable := ForceDirectories(ThemePath);

  If Writeable And (not DirectoryExists(ScreenshotsPath)) then
    Writeable := ForceDirectories(ScreenshotsPath);

  If Writeable And (not DirectoryExists(CoversPath)) then
    Writeable := ForceDirectories(CoversPath);

  If Writeable And (not DirectoryExists(LanguagesPath)) then
    Writeable := ForceDirectories(LanguagesPath);

  If Writeable And (not DirectoryExists(PluginPath)) then
    Writeable := ForceDirectories(PluginPath);

  If Writeable And (not DirectoryExists(PlaylistPath)) then
    Writeable := ForceDirectories(PlaylistPath);

  If Writeable And (not DirectoryExists(RecordingsPath)) then
    Writeable := ForceDirectories(RecordingsPath);

  if not Writeable then
    Log.LogError('Error: Dir is Readonly');

  DecimalSeparator := ',';
end;

//--------------------
// Clears Song Header values
//--------------------
procedure ClearSong(var Song: TSong);
begin
  //Main Information
  Song.Title := '';
  Song.Artist := '';

  //Sortings:
  Song.Genre := 'Unknown';
  Song.Edition := 'Unknown';
  Song.Language := 'Unknown'; //Language Patch

  //Required Information
  Song.Mp3 := '';
  Song.BPM := 0;
  Song.GAP := 0;
  Song.Start := 0;
  Song.Finish := 0;
  Song.Relative := false;

  //Additional Information
  Song.Background := '';
  Song.Cover := '';
  Song.Video := '';
  Song.VideoGAP := 0;
  Song.NotesGAP := 0;
  Song.Resolution := 4;
  Song.Creator := '';
  Song.Medley.Source:=msNone;
  Song.PreviewStart := 0;
  SetLength(Song.CustomTags, 0);
end;

//--------------------
// Reads Standard TXT Header
//--------------------
function ReadTXTHeader(var Song: TSong): boolean;
var
  Line, Identifier, Value: String;
  Temp: word;
  Done: byte;
  MedleyFlags: byte; //bit-vector for medley/preview tags
  lWarnIfTagsNotFound : Boolean;

  { adds a custom header tag to the song
    if there is no ':' in the read line, Tag should be empty
    and the whole line should be in Content } //from usdx 1.1
  procedure AddCustomTag(const Tag, Content: String);
    var Len: Integer;
  begin
    Len := Length(Song.CustomTags);
    SetLength(Song.CustomTags, Len + 1);
    Song.CustomTags[Len].Tag := Tag;
    Song.CustomTags[Len].Content := Content;
  end;

begin
  Result := true;
  Done := 0;
  MedleyFlags := 0;

  lWarnIfTagsNotFound := ( lowercase( Song.Filename ) <> 'license.txt' ) AND
                         ( lowercase( Song.Filename ) <> 'readme.txt'  ) ;


  //Read first Line
  ReadLn (SongFile, Line);

  if (Length(Line)<=0) then
  begin
    Log.LogError('File Starts with Empty Line: ' + Song.FileName);
    Result := False;
    Exit;
  end;

  //Read Lines while Line starts with #
  While (Length(Line) = 0) OR (Line[1] = '#') do
  begin
    //Increase Line Number
    Inc (FileLineNo);
    Temp := Pos(':', Line);

    //Line has a Seperator-> Headerline
    if (Temp <> 0) then
    begin
      //Read Identifier and Value
      Identifier  := Uppercase(Trim(Copy(Line, 2, Temp - 2))); //Uppercase is for Case Insensitive Checks
      Value       := Trim(Copy(Line, Temp + 1,Length(Line) - Temp));

      //Check the Identifier (If Value is given)
      if (Length(Value) <> 0) then
      begin

        //-----------
        //Required Attributes
        //-----------

        //Title
        if (Identifier = 'TITLE') then
        begin
          Song.Title := Value;

          //Add Title Flag to Done
          Done := Done or 1;
        end

        //Artist
        else if (Identifier = 'ARTIST') then
        begin
          Song.Artist := Value;

          //Add Artist Flag to Done
          Done := Done or 2;
        end

        //MP3 File //Test if Exists
        else if (Identifier = 'MP3') AND (FileExists(Song.Path + Value)) then
        begin
          Song.Mp3 := Value;

          //Add Mp3 Flag to Done
          Done := Done or 4;
        end

        //Beats per Minute
        else if (Identifier = 'BPM') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          SetLength(Song.BPM, 1);
          Song.BPM[0].StartBeat := 0;

          Song.BPM[0].BPM := StrtoFloatDef(Value, 0) * Mult * MultBPM;

          if Song.BPM[0].BPM <> 0 then
          begin
            //Add BPM Flag to Done
            Done := Done or 8;
          end;
        end

        //---------
        //Additional Header Information
        //---------

        // Video Gap
        else if (Identifier = 'GAP') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          Song.GAP := StrtoFloatDef (Value, 0);
        end

        //Cover Picture
        else if (Identifier = 'COVER') then
        begin
          Song.Cover := Value;
        end

        //Background Picture
        else if (Identifier = 'BACKGROUND') then
        begin
          Song.Background := Value;
        end

        // Video File
        else if (Identifier = 'VIDEO') then
        begin
          if (FileExists(Song.Path + Value)) then
            Song.Video := Value
          else
            Log.LogError('Can''t find Video File in Song: ' + Song.Path + Song.FileName);
        end

        // Video Gap
        else if (Identifier = 'VIDEOGAP') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          Song.VideoGAP := StrtoFloatDef (Value, 0);
        end

        //Genre Sorting
        else if (Identifier = 'GENRE') then
        begin
          Song.Genre := Value;
        end

        //Edition Sorting
        else if (Identifier = 'EDITION') then
        begin
          Song.Edition := Value;
        end

        //Creator Tag
        else if (Identifier = 'CREATOR') then
        begin
          Song.Creator := Value;
        end

        //Language Sorting
        else if (Identifier = 'LANGUAGE') then
        begin
          Song.Language := Value;
        end

        // Song Start
        else if (Identifier = 'START') then
        begin
          // Replace . with ,
          if (Pos('.', Value) <> 0) then
            Value[Pos('.', Value)] := ',';

          Song.Start := StrtoFloatDef(Value, 0);
        end

        // Song Ending
        else if (Identifier = 'END') then
        begin
          TryStrtoInt(Value, Song.Finish);
        end

        // Resolution
        else if (Identifier = 'RESOLUTION') then
        begin
          TryStrtoInt(Value, Song.Resolution);
        end

        // Notes Gap
        else if (Identifier = 'NOTESGAP') then
        begin
          TryStrtoInt(Value, Song.NotesGAP);
        end

        // Relative Notes
        else if (Identifier = 'RELATIVE') AND (uppercase(Value) = 'YES') then
        begin
          Song.Relative := True;
        end

        // PreviewStart
        else if (Identifier = 'PREVIEWSTART') then
        begin
          Song.PreviewStart := StrToFloatDef(Value, 0);
          if (Song.PreviewStart>0) then
            MedleyFlags := MedleyFlags or 1;
        end

        // MedleyStartBeat
        else if (Identifier = 'MEDLEYSTARTBEAT') and not Song.Relative then
        begin
          if TryStrtoInt(Value, Song.Medley.StartBeat) then
            MedleyFlags := MedleyFlags or 2;
        end

        // MedleyEndBeat
        else if (Identifier = 'MEDLEYENDBEAT') and not Song.Relative then
        begin
          if TryStrtoInt(Value, Song.Medley.EndBeat) then
            MedleyFlags := MedleyFlags or 4;
        end

        // unsupported tag
        else
        begin
          AddCustomTag(Identifier, Value);
        end;

      end;
    end;

    if not EOf(SongFile) then
      ReadLn (SongFile, Line)
    else
    begin
      Result := False;

      if lWarnIfTagsNotFound then
      begin
        Log.LogError('File Incomplete or not Ultrastar TxT: ' + Song.FileName);
      end;
      
      break;
    end;

    {//End on first empty Line
    if (Length(Line) = 0) then
      break;}
  end;

  //Check if all Required Values are given
  if (Done <> 15) then
  begin
    Result := False;
    If lWarnIfTagsNotFound then
    begin
      if (Done and 8) = 0 then      //No BPM Flag
       Log.LogError('BPM Tag Missing: ' + Song.FileName)
      else if (Done and 4) = 0 then //No MP3 Flag
        Log.LogError('MP3 Tag/File Missing: ' + Song.FileName)
      else if (Done and 2) = 0 then //No Artist Flag
        Log.LogError('Artist Tag Missing: ' + Song.FileName)
      else if (Done and 1) = 0 then //No Title Flag
        Log.LogError('Title Tag Missing: ' + Song.FileName)
      else //unknown Error
        Log.LogError('File Incomplete or not Ultrastar TxT: ' + Song.FileName);
    end;
  end else
  begin //check medley tags
    if (MedleyFlags and 6)=6 then //MedleyStartBeat and MedleyEndBeat are both set
    begin
      if Song.Medley.StartBeat >= Song.Medley.EndBeat then
        MedleyFlags := MedleyFlags - 6;
    end;

    if ((MedleyFlags and 1)=0) or (Song.PreviewStart<=0) then //PreviewStart is not set or <=0
    begin
      if (MedleyFlags and 2)=2 then
        Song.PreviewStart := GetTimeFromBeat(Song.Medley.StartBeat)  //fallback to MedleyStart
      else
        Song.PreviewStart := 0; //else set it to 0, it will be set in FindRefrainStart
    end;

    if (MedleyFlags and 6)=6 then
    begin
      Song.Medley.Source := msTag;

      //calculate fade time
      Song.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;

      Song.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
    end else
      Song.Medley.Source := msNone;
  end;

end;

//--------------------
// Analyse Song File and Read Header
//--------------------
function AnalyseFile(var Song: TSong): boolean;
begin
Result := False;
{try }
  //Reset LineNo
  FileLineNo := 0;

  //Open File and set File Pointer to the beginning
  AssignFile(SongFile, Song.Path + Song.FileName);
  Reset(SongFile);

  //Clear old Song Header
  ClearSong(Song);

  //Read Header
  Result := ReadTxTHeader(Song);

  //And Close File
  CloseFile(SongFile);
{except
  CloseFile(SongFile);

  Result := False;
  //Error Reporting
  Log.LogError('An Error occured reading Line ' + inttostr(FileLineNo) + ' from SongHeader: ' + Song.FileName);
end;}
end;

//--------------------
// Resets the temporary Sentence Arrays for each Player and some other Variables
//--------------------
procedure ResetSingTemp;
var
  Pet:  integer;
begin
  SetLength(Czesci, Length(Player));
  SetLength(AktSong.BPM, 0);
  for Pet := 0 to High(Player) do begin
    SetLength(Czesci[Pet].Czesc, 1);
    SetLength(Czesci[Pet].Czesc[0].Nuta, 0);
    Czesci[Pet].Czesc[0].Lyric := '';
    Czesci[Pet].Czesc[0].LyricWidth := 0;
    Player[pet].Score := 0;
    Player[pet].IlNut := 0;
    Player[pet].HighNut := -1;
  end;
  //Reset Path and Filename Values to Prevent Errors in Editor
  AktSong.Path := '';
  AktSong.FileName := '';
end;

//--------------------
// Parses Note Infos and save them to Array
//--------------------
procedure ParseNote(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
var
  Space:  boolean;
begin
  case Ini.Solmization of
    1:  // european
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' si ';
        end;
      end;
    2:  // japanese
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' so ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' shi ';
        end;
      end;
    3:  // american
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' ti ';
        end;
      end;
  end; // case

  with Czesci[NrCzesci].Czesc[Czesci[NrCzesci].High] do begin
    SetLength(Nuta, Length(Nuta) + 1);
    IlNut := IlNut + 1;
    HighNut := HighNut + 1;
    Muzyka.IlNut := Muzyka.IlNut + 1;

    Nuta[HighNut].Start := StartP;
    if IlNut = 1 then begin
      StartNote := Nuta[HighNut].Start;
      if Czesci[NrCzesci].Ilosc = 1 then
        Start := -100;
//        Start := Nuta[HighNut].Start;
    end;

    Nuta[HighNut].Dlugosc := DurationP;
    Muzyka.DlugoscNut := Muzyka.DlugoscNut + Nuta[HighNut].Dlugosc;

    // back to the normal system with normal, golden and now freestyle notes
    case TypeP of
      'F':  Nuta[HighNut].Wartosc := 0;
      ':':  Nuta[HighNut].Wartosc := 1;
      '*':  Nuta[HighNut].Wartosc := 2;
    end;

    Czesci[NrCzesci].Wartosc := Czesci[NrCzesci].Wartosc + Nuta[HighNut].Dlugosc * Nuta[HighNut].Wartosc;

    Nuta[HighNut].Ton := NoteP;
    if Nuta[HighNut].Ton < Base[NrCzesci] then Base[NrCzesci] := Nuta[HighNut].Ton;
    Nuta[HighNut].TonGamy := Nuta[HighNut].TonGamy mod 12;

    Nuta[HighNut].Tekst := Copy(LyricS, 2, 100);
    Lyric := Lyric + Nuta[HighNut].Tekst;

    if TypeP = 'F' then
      Nuta[HighNut].FreeStyle := true;

    Koniec := Nuta[HighNut].Start + Nuta[HighNut].Dlugosc;
  end; // with
end;

//--------------------
// Called when a new Sentence is found in the TXT File
//--------------------
procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer; LoadFullFile: boolean);
var
I: Integer;
begin

  // stara czesc //Alter Satz //Update Old Part
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].BaseNote := Base[NrCzesciP];
  if LoadFullFile then
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].LyricWidth := glTextWidth(PChar(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Lyric));

  //Total Notes Patch
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes := 0;
  for I := low(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta) to high(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta) do
  begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes := Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes + Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta[I].Dlugosc * Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta[I].Wartosc;
  end;
  //Total Notes Patch End


  // nowa czesc //Neuer Satz //Update New Part
  SetLength(Czesci[NrCzesciP].Czesc, Czesci[NrCzesciP].Ilosc + 1);
  Czesci[NrCzesciP].High := Czesci[NrCzesciP].High + 1;
  Czesci[NrCzesciP].Ilosc := Czesci[NrCzesciP].Ilosc + 1;
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].HighNut := -1;

  if not AktSong.Relative then
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;

  if AktSong.Relative then begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;
    Rel[NrCzesciP] := Rel[NrCzesciP] + Param2;
  end;

  Base[NrCzesciP] := 100; // high number
end;

function CheckSong: boolean;
var
  p, line, note:      integer;
  numLines, numNotes: integer;
  bt:                 integer;
  nextBeat:           integer;
  foundMedleyStart:   boolean;
  foundMedleyEnd:     boolean;
  medley:             boolean;

begin
  Result := true;
  bt := -32000;

  if(AktSong.Medley.Source = msTag) then
  begin
    medley := true;
    foundMedleyStart := false;
    foundMedleyEnd := false;
  end else
    medley := false;

  for p := 0 to {Length(Czesci)}1 - 1 do //TODO: why doesn't it work?
  begin
    numLines := Length(Czesci[p].Czesc);

    if(numLines=0) then
    begin
      Log.LogError('Song ' + AktSong.Path + AktSong.Filename + ' has no lines?');
      if (Ini.LoadFaultySongs=0) then
        Result := false;
    end;

    for line := 0 to numLines - 1 do
    begin
      numNotes := Length(Czesci[p].Czesc[line].Nuta);

      if(numNotes=0) then
      begin
        Log.LogError('Sentence ' + IntToStr(line+1) + ' in song ' + AktSong.Path + AktSong.Filename + ' has no notes?');
        if (Ini.LoadFaultySongs=0) then
          Result := false;
      end;

      if(bt>Czesci[p].Czesc[line].Start) then
      begin
        Log.LogError('Beat error in sentence ' + IntToStr(line+1) + ', on beat ' + IntToStr(Czesci[p].Czesc[line].Start) +
          ' in song ' + AktSong.Path + AktSong.Filename);
        if (Ini.LoadFaultySongs=0) then
          Result := false;
      end;
      bt := Czesci[p].Czesc[line].Start;

      for note := 0 to numNotes - 1 do
      begin
        if(bt>Czesci[p].Czesc[line].Nuta[note].Start) then
        begin
          Log.LogError('Beat error in sentence ' + IntToStr(line+1) + ', on beat ' + IntToStr(Czesci[p].Czesc[line].Nuta[note].Start) +
            ' in song ' + AktSong.Path + AktSong.Filename);
          if (Ini.LoadFaultySongs=0) then
            Result := false;
        end;
        bt := Czesci[p].Czesc[line].Nuta[note].Start;

        if (note<numNotes-1) then
          nextBeat := Czesci[p].Czesc[line].Nuta[note+1].Start
        else if (line<numLines-1) then
          nextBeat := Czesci[p].Czesc[line+1].Start
        else
          nextBeat := Czesci[p].Czesc[line].Koniec;

        if (bt+Czesci[p].Czesc[line].Nuta[note].Dlugosc>nextBeat) then
        begin
          Log.LogError('Note length error in sentence ' + IntToStr(line+1) + ', on beat ' + IntToStr(Czesci[p].Czesc[line].Nuta[note].Start) +
            ' in song ' + AktSong.Path + AktSong.Filename);
          //Exit;
        end;

        if(medley) then
        begin
          if(bt = AktSong.Medley.StartBeat) then
            foundMedleyStart := true;
          if(bt+Czesci[p].Czesc[line].Nuta[note].Dlugosc = AktSong.Medley.EndBeat) then
            foundMedleyEnd := true;
        end;
      end;
    end;
  end;

  if(medley and not foundMedleyStart) then
  begin
    Log.LogError('Error MedleyStartBeat: no corresponding note start (beat) in song ' + AktSong.Path + AktSong.Filename);
    if (Ini.LoadFaultySongs=0) then
      Result := false;
  end else if(medley and not foundMedleyEnd) then
  begin
    Log.LogError('Error MedleyEndBeat: no corresponding note start+length in song ' + AktSong.Path + AktSong.Filename);
    if (Ini.LoadFaultySongs=0) then
      Result := false;
  end;
end;


//--------------------
// Load a Song
//--------------------
function LoadSong(Name: string; LoadFullFile: boolean): boolean;
var
  TempC:    char;
  Tekst:    string;
  CP:       integer; // Current Player (0 or 1)
  Pet:      integer;
  Both:     boolean;
  Param1:   integer;
  Param2:   integer;
  Param3:   integer;
  ParamS:   string;
  I: Integer;
  isNewSentence: boolean;
begin
  Result := false;

  if not FileExists(Name) then begin
    Log.LogError('File not found: "' + Name + '"', 'LoadSong');
    exit;
  end;

  try
  MultBPM := 4; // 4 - mnoznik dla czasu nut
  Mult := 1; // 4 - dokladnosc pomiaru nut
  Base[0] := 100; // high number
//  Base[1] := 100; // high number
  Czesci[0].Wartosc := 0;
//  Czesci[1].Wartosc := 0; // here was the error in 0.3.2
  if LoadFullFile then
    AktSong.Relative := false;

  Rel[0] := 0;
//  Rel[1] := 0;
  CP := 0;
  Both := false;
  if Length(Player) = 2 then Both := true;

  FileMode := fmOpenRead;
  AssignFile(SongFile, Name);

  if LoadFullFile then
  begin
    Reset(SongFile);

    //Clear old Song Header
    ClearSong(AktSong);

    if (AktSong.Path = '') then
      AktSong.Path := ExtractFilePath(Name);

    if (AktSong.FileName = '') then
      AktSong.Filename := ExtractFileName(Name);
    //Read Header
    Result := ReadTxTHeader(AktSong);
    if not Result then
    begin
      CloseFile(SongFile);
      FileMode := fmOpenReadWrite;
      Log.LogError('Error Loading SongHeader, abort Song Loading. File: ' + Name);
      Exit;
    end;
  end;

  Result := False;

  Reset(SongFile);
  FileLineNo := 0;
  //Search for Note Begining
  repeat
    ReadLn(SongFile, Tekst);
    Inc(FileLineNo);
    
    if (EoF(SongFile)) then
    begin //Song File Corrupted - No Notes
      CloseFile(SongFile);
      FileMode := fmOpenReadWrite;
      Log.LogError('Could not load txt File, no Notes found: ' + Name);
      Result := False;
      Exit;
    end;
    Read(SongFile, TempC);
  until ((TempC = ':') or (TempC = 'F') or (TempC = '*'));

  SetLength(Czesci, 0);
  SetLength(Czesci, 2);
  for Pet := 0 to High(Czesci) do begin
    SetLength(Czesci[Pet].Czesc, 1);
    Czesci[Pet].High := 0;
    Czesci[Pet].Ilosc := 1;
    Czesci[Pet].Akt := 0;
    Czesci[Pet].Resolution := AktSong.Resolution;
    Czesci[Pet].NotesGAP := AktSong.NotesGAP;
    Czesci[Pet].Czesc[0].IlNut := 0;
    Czesci[Pet].Czesc[0].HighNut := -1;
  end;

//  TempC := ':';
//  TempC := Tekst[1]; // read from backup variable, don't use default ':' value
  isNewSentence := false;
  while (TempC <> 'E') AND (not EOF(SongFile)) do begin
    if (TempC = ':') or (TempC = '*') or (TempC = 'F') then begin
      // wczytuje nute
      Read(SongFile, Param1);
      Read(SongFile, Param2);
      Read(SongFile, Param3);
      Read(SongFile, ParamS);

      // dodaje nute
      if not Both then
        // P1
        ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
      else begin
        // P1 + P2
        ParseNote(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
        ParseNote(1, TempC, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
      end;
      isNewSentence := false;
    end; // if
    if TempC = '-' then begin
      if isNewSentence then
      begin
        Log.LogError('Double sentence break in file: "' + Name + '"; Line '+IntToStr(FileLineNo)+' (LoadSong)');
        Result := False;
        Exit;
      end;
      // reads sentence
      Read(SongFile, Param1);
      if AktSong.Relative then Read(SongFile, Param2); // read one more data for relative system

      // new sentence
      if not Both then
        // P1
        NewSentence(0, (Param1 + Rel[0]) * Mult, Param2, LoadFullFile)
      else begin
        // P1 + P2
        NewSentence(0, (Param1 + Rel[0]) * Mult, Param2, LoadFullFile);
        NewSentence(1, (Param1 + Rel[1]) * Mult, Param2, LoadFullFile);
      end;
      isNewSentence := true;
    end; // if

    if TempC = 'B' then begin
      SetLength(AktSong.BPM, Length(AktSong.BPM) + 1);
      Read(SongFile, AktSong.BPM[High(AktSong.BPM)].StartBeat);
      AktSong.BPM[High(AktSong.BPM)].StartBeat := AktSong.BPM[High(AktSong.BPM)].StartBeat + Rel[0];

      Read(SongFile, Tekst);
      AktSong.BPM[High(AktSong.BPM)].BPM := StrToFloat(Tekst);
      AktSong.BPM[High(AktSong.BPM)].BPM := AktSong.BPM[High(AktSong.BPM)].BPM * Mult * MultBPM;
    end;


    if not Both then begin
      Czesci[CP].Czesc[Czesci[CP].High].BaseNote := Base[CP];
      if LoadFullFile then
        Czesci[CP].Czesc[Czesci[CP].High].LyricWidth := glTextWidth(PChar(Czesci[CP].Czesc[Czesci[CP].High].Lyric));
      //Total Notes Patch
      Czesci[CP].Czesc[Czesci[CP].High].TotalNotes := 0;
      for I := low(Czesci[CP].Czesc[Czesci[CP].High].Nuta) to high(Czesci[CP].Czesc[Czesci[CP].High].Nuta) do
      begin
       Czesci[CP].Czesc[Czesci[CP].High].TotalNotes := Czesci[CP].Czesc[Czesci[CP].High].TotalNotes + Czesci[CP].Czesc[Czesci[CP].High].Nuta[I].Dlugosc * Czesci[CP].Czesc[Czesci[CP].High].Nuta[I].Wartosc;
      end;
      //Total Notes Patch End
    end else begin
      for Pet := 0 to High(Czesci) do begin
        Czesci[Pet].Czesc[Czesci[Pet].High].BaseNote := Base[Pet];
        if LoadFullFile then
          Czesci[Pet].Czesc[Czesci[Pet].High].LyricWidth := glTextWidth(PChar(Czesci[Pet].Czesc[Czesci[Pet].High].Lyric));
        //Total Notes Patch
        Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes := 0;
        for I := low(Czesci[Pet].Czesc[Czesci[Pet].High].Nuta) to high(Czesci[Pet].Czesc[Czesci[Pet].High].Nuta) do
        begin
          Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes := Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes + Czesci[Pet].Czesc[Czesci[Pet].High].Nuta[I].Dlugosc * Czesci[Pet].Czesc[Czesci[Pet].High].Nuta[I].Wartosc;
        end;
        //Total Notes Patch End
      end;
    end;

    Read(SongFile, TempC);
    Inc(FileLineNo);
  end; // while}

  CloseFile(SongFile);
  FileMode := fmOpenReadWrite;
  except
    try
      CloseFile(SongFile);
      FileMode := fmOpenReadWrite;
    except

    end;
    Result := false;
    Log.LogError('Error Loading File: "' + Name + '" in Line ' + inttostr(FileLineNo+1));
    exit;
  end;

  Result := CheckSong;
end;

//--------------------
// Saves a Song
//--------------------
function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
var
  C:      integer;
  N:      integer;
  S:      string;
  B:      integer;
  RelativeSubTime:    integer;
  NoteState: String;

  procedure WriteCustomTags; //from 1.1 (modified)
    var
      I: integer;
      Line: String;
  begin
    for I := 0 to High(Song.CustomTags) do
    begin
      Line := Song.CustomTags[I].Content;
      if (Length(Song.CustomTags[I].Tag) > 0) then
        Line := Song.CustomTags[I].Tag + ':' + Line;

      WriteLn(SongFile, '#' + Line);
    end;

  end;
begin
//  Relative := true; // override (idea - use shift+S to save with relative)
  Result := true;
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
  if Song.PreviewStart<> 0  then    WriteLn(SongFile, '#PREVIEWSTART:'+ FormatFloat('#0.000', Song.PREVIEWSTART));

  if (Song.Medley.Source=msTag) and not Relative then
  begin
    WriteLn(SongFile, '#MedleyStartBeat:' + IntToStr(Song.Medley.StartBeat));
    WriteLn(SongFile, '#MedleyEndBeat:' + IntToStr(Song.Medley.EndBeat));
  end;

  if Relative               then    WriteLn(SongFile, '#RELATIVE:yes');

  WriteLn(SongFile, '#BPM:' + FloatToStr(Song.BPM[0].BPM / 4));
  WriteLn(SongFile, '#GAP:' + FloatToStr(Song.GAP));

  RelativeSubTime := 0;
  for B := 1 to High(AktSong.BPM) do
    WriteLn(SongFile, 'B ' + FloatToStr(AktSong.BPM[B].StartBeat) + ' ' + FloatToStr(AktSong.BPM[B].BPM/4));

  // write custom header tags (from 1.1)
  WriteCustomTags;

  for C := 0 to Czesc.High do begin
    for N := 0 to Czesc.Czesc[C].HighNut do begin
      with Czesc.Czesc[C].Nuta[N] do begin


        //Golden + Freestyle Note Patch
        case Czesc.Czesc[C].Nuta[N].Wartosc of
          0: NoteState := 'F ';
          1: NoteState := ': ';
          2: NoteState := '* ';
        end; // case
        S := NoteState + IntToStr(Start-RelativeSubTime) + ' ' + IntToStr(Dlugosc) + ' ' + IntToStr(Ton) + ' ' + Tekst;


        WriteLn(SongFile, S);
      end; // with
    end; // N

    if C < Czesc.High then begin      // don't write end of last sentence
      if not Relative then
        S := '- ' + IntToStr(Czesc.Czesc[C+1].Start)
      else begin
        S := '- ' + IntToStr(Czesc.Czesc[C+1].Start - RelativeSubTime) +
          ' ' + IntToStr(Czesc.Czesc[C+1].Start - RelativeSubTime);
        RelativeSubTime := Czesc.Czesc[C+1].Start;
      end;
      WriteLn(SongFile, S);
    end;

  end; // C


  WriteLn(SongFile, 'E');
  CloseFile(SongFile);
end;

{* new procedure for preview
   tries find out the beginning of a refrain
   and the end... *}
procedure FindRefrainStart(var Song: TSong);
Const
  MEDLEY_MIN_DURATION = 40;   //minimum duration of a medley-song in seconds

Type
  TSeries = record
    start:    integer; //Start sentence of series
    end_:     integer; //End sentence of series
    len:      integer; //Length of sentence series
  end;

var
  I, J, K, num_lines:   integer;
  sentences:            array of String;
  series:               array of TSeries;
  temp_series:          TSeries;
  max:                  integer;
  len_lines, len_notes: integer;
  found_end:            boolean;
begin
  if AktSong.Medley.Source = msTag then
    Exit;

  //relative is not supported for medley by now!
  if AktSong.Relative then
  begin
    Log.LogError('Song '+Song.Artist+'-'+Song.Title+' contains #Relative, this is not supported by medley-function!');
    Song.Medley.Source := msNone;
    Exit;
  end;

  num_lines := Length(Czesci[0].Czesc);
  SetLength(sentences, num_lines);

  //build sentences array
  for I := 0 to num_lines - 1 do
  begin
    sentences[I] := '';
    for J := 0 to Length(Czesci[0].Czesc[I].Nuta) - 1 do
    begin
      if not Czesci[0].Czesc[I].Nuta[J].FreeStyle then
        sentences[I] := sentences[I] + Czesci[0].Czesc[I].Nuta[J].Tekst;
    end;
  end;

  //find equal sentences series
  SetLength(series, 0);

  for I := 0 to num_lines - 2 do
  begin
    for J := I+1 to num_lines - 1 do
    begin
      if sentences[I]=sentences[J] then
      begin
        temp_series.start := I;
        temp_series.end_  := I;

        if (J+J-I-1>num_lines-1) then
          max:=num_lines-1-J
        else
          max:=J-I-1;

        for K := 1 to max do
        begin
          if sentences[I+K]=sentences[J+K] then
            temp_series.end_ := I+K
          else
            break;
        end;
        temp_series.len := temp_series.end_ - temp_series.start + 1;
        SetLength(series, Length(series)+1);
        series[Length(series)-1] := temp_series;
      end;
    end;
  end;

  //search for longest sequence
  if Length(series)>0 then
  begin
    max := 0;
    for I := 0 to Length(series) - 1 do
    begin
      if series[I].len > series[max].len then
        max := I;
    end;
  end;

  len_lines := length(Czesci[0].Czesc);

  if (Length(series)>0) and (series[max].len > 3) then
  begin
    Song.Medley.StartBeat := Czesci[0].Czesc[series[max].start].Nuta[0].Start;
    len_notes := length(Czesci[0].Czesc[series[max].end_].Nuta);
    Song.Medley.EndBeat := Czesci[0].Czesc[series[max].end_].Nuta[len_notes-1].Start +
      Czesci[0].Czesc[series[max].end_].Nuta[len_notes-1].Dlugosc;

    found_end := false;

    //set end if duration > MEDLEY_MIN_DURATION
    if GetTimeFromBeat(Song.Medley.StartBeat)+ MEDLEY_MIN_DURATION >
      GetTimeFromBeat(Song.Medley.EndBeat) then
    begin
      found_end := true;
    end;

    //estimate the end: just go MEDLEY_MIN_DURATION
    //ahead an set to a line end (if possible)
    if not found_end then
    begin
      for I := series[max].start+1 to len_lines-1 do
      begin
        len_notes := length(Czesci[0].Czesc[I].Nuta);
        for J := 0 to len_notes - 1 do
        begin
          if GetTimeFromBeat(Song.Medley.StartBeat)+ MEDLEY_MIN_DURATION >
            GetTimeFromBeat(Czesci[0].Czesc[I].Nuta[J].Start+
            Czesci[0].Czesc[I].Nuta[J].Dlugosc) then
          begin
            found_end := true;
            Song.Medley.EndBeat := Czesci[0].Czesc[I].Nuta[len_notes-1].Start+
              Czesci[0].Czesc[I].Nuta[len_notes-1].Dlugosc;
            break;
          end;
        end;
      end;
    end;

    
    if found_end then
    begin
      Song.Medley.Source := msCalculated;

      //calculate fade time
      Song.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;    //TODO in INI
      Song.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;  //TODO in INI
    end;
  end;

  //set PreviewStart if not set
  if Song.PreviewStart=0 then
  begin
    len_notes := length(Czesci[0].Czesc[len_lines-1].Nuta);
    if Song.Medley.Source = msCalculated then
      Song.PreviewStart := GetTimeFromBeat(Song.Medley.StartBeat);{
    else
      Song.PreviewStart := (GetTimeFromBeat(Czesci[0].Czesc[len_lines-1].Nuta[len_notes-1].start+
        Czesci[0].Czesc[len_lines-1].Nuta[len_notes-1].Dlugosc))/4;      //TODO}
  end;
end;

//sets a song to medley-mod:
//converts all unneeded notes into freestyle
//updates score values
procedure SetMedleyMode;
var
  pl, line, note: integer;
  cut_line: array of integer;
  foundcut: array of boolean;
  start:          integer;
  end_:           integer;

begin
  start := AktSong.Medley.StartBeat;
  end_  := AktSong.Medley.EndBeat;
  SetLength(cut_line, Length(Czesci));
  SetLength(foundcut, Length(Czesci));

  for pl := 0 to Length(Czesci) - 1 do
  begin
    foundcut[pl] := false;
    cut_line[pl] := high(Integer);
    Czesci[pl].Wartosc := 0;
    for line := 0 to Length(Czesci[pl].Czesc) - 1 do
    begin
      Czesci[pl].Czesc[line].TotalNotes := 0;
      for note := 0 to Length(Czesci[pl].Czesc[line].Nuta) - 1 do
      begin
        if Czesci[pl].Czesc[line].Nuta[note].Start < start then      //check start
          Czesci[pl].Czesc[line].Nuta[note].FreeStyle := true
        else if Czesci[pl].Czesc[line].Nuta[note].Start>= end_ then  //check end
        begin
          Czesci[pl].Czesc[line].Nuta[note].FreeStyle := true;
          if not foundcut[pl] then
          begin
            if (note=0) then
              cut_line[pl] := line
            else
              cut_line[pl] := line+1;
          end;
          foundcut[pl] := true;
        end else
        begin
          //add this notes value ("notes length" * "notes scorefactor") to the current songs entire value
          Inc(Czesci[pl].Wartosc, Czesci[pl].Czesc[line].Nuta[note].Dlugosc * Czesci[pl].Czesc[line].Nuta[note].Wartosc);
          //and to the current lines entire value
          Inc(Czesci[pl].Czesc[line].TotalNotes, Czesci[pl].Czesc[line].Nuta[note].Dlugosc * Czesci[pl].Czesc[line].Nuta[note].Wartosc);
        end;
      end;
    end;
  end;

  for pl := 0 to Length(Czesci) - 1 do
  begin
    if (foundcut[pl]) and (Length(Czesci[pl].Czesc)>cut_line[pl]) then
    begin
      SetLength(Czesci[pl].Czesc, cut_line[pl]);
      Czesci[pl].high := cut_line[pl]-1;
    end;
  end;
end;

end.