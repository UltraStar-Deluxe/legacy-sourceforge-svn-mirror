unit USongs;

interface
uses SysUtils, ULog, SDL, UTexture, UCovers, UCatCovers, UMergeSort;

const
  SONG_LOAD_COMPLETE = true;
  SONG_LOAD_NOTES = false;

type
  TMedleySource = ( msNone, msCalculated, msTag );

  TMedley = record
    Source:       TMedleySource;  //source of the information
    StartBeat:    integer;  //start beat of medley
    EndBeat:      integer;  //end beat of medley
    FadeIn_time:  real;     //FadeIn-Time in seconds
    FadeOut_time: real;     //FadeOut-Time in seconds
  end;

  { used to hold header tags that are not supported by this version of
    usdx (e.g. some tags from ultrastar 0.7.0) when songs are loaded in
    songeditor. They will be written the end of the song header } //from usdx 1.1
  TCustomHeaderTag = record
    Tag: String;
    Content: String;
  end;

  TBPM = record
    BPM:        real;
    StartBeat:  real;
  end;

  TScore = record
    Name:       string;
    Score:      integer;
    Length:     string;
    Date:       string;
  end;

  TSong = record
    Path:       string;
    Folder:     string; // for sorting by folder
    FileName:   string;

    isDuet:     boolean;
    DuetNames:  array of string;
    Medley:     TMedley;
    CalcMedley: boolean;
    PreviewStart: real; //in seconds
    CustomTags: array of TCustomHeaderTag; // from 1.1

    // sorting methods
    Category:   array of string; // I think I won't need this
    Genre:      array of string;
    Edition:    array of string;
    Language:   string; // 0.5.0: new
    Year:       integer;

    Title:      string;
    Artist:     string;

    Text:       string;
    Creator:    string;

    Cover:      string;
    CoverTex:   TTexture;
    Mp3:        string;
    Background: string;
    Video:      string;
    VideoGAP:   real;
    VideoLoaded: boolean; // 0.5.0: true if the video has been loaded 
    NotesGAP:   integer;
    Start:      real; // in seconds
    Finish:     integer; // in miliseconds
    Relative:   boolean;
    Resolution: integer;
    BPM:        array of TBPM;
    GAP:        real; // in miliseconds

    Score:      array[0..2] of array of TScore;

    // these are used when sorting is enabled
    Visible:    boolean; // false if hidden, true if visible
    Main:       boolean; // false for songs, true for category buttons
    OrderNum:   integer; // has a number of category for category buttons and songs
    OrderTyp:   integer; // type of sorting for this button (0=name)
    CatNumber:  integer; // Count of Songs in Category for Cats and Number of Song in Category for Songs
  end;

  TSongs = class
  private
    BrowsePos:  Cardinal; //Actual Pos in Song Array
  public
    Song:       array of TSong; // array of songs
    SongSort:   array of TSong;
    Selected:   integer; // selected song index
    NumFaultySongs: integer;

    function FindSongFile(Dir, Mask: string): string;
    procedure LoadSongList; // load all songs
    procedure BrowseDir(Dir: string; Index: integer); // should return number of songs in the future
    procedure Sort(Order: integer);
  end;

  TCatSongs = class
    Song:       array of TSong; // array of categories with songs
    Selected:   integer; // selected song index
    Order:      integer; // order type (0=title)
    CatNumShow: integer; // Category Number being seen
    CatCount:   integer; //Number of Categorys

    procedure Refresh; // refreshes arrays by recreating them from Songs array
//    procedure Sort(Order: integer);
    procedure ShowCategory(Index: integer); // expands all songs in category
    procedure HideCategory(Index: integer); // hides all songs in category
    procedure ClickCategoryButton(Index: integer); // uses ShowCategory and HideCategory when needed
    procedure ShowCategoryList; //Hides all Songs And Show the List of all Categorys
    function FindNextVisible(SearchFrom:integer): integer; //Find Next visible Song
    function VisibleSongs: integer; // returns number of visible songs (for tabs)
    function VisibleIndex(Index: integer): integer; // returns visible song index (skips invisible)

    function SetFilter(FilterStr: String; const fType: Byte): Cardinal;
    function NumCatSongs(Cat: integer): integer;
    function NumSongs(): integer;
    function NumVisibleCats(): integer;
  end;

var
  Songs:      TSongs; // all songs
  CatSongs:   TCatSongs; // categorized songs
  AktSong:    TSong; // one song

implementation

uses UFiles, UIni, StrUtils, Umusic, UGraphic;

var
  TempSongArr:    array of TSong;
  SongSort:       array of TSong;
  SortOrder:  integer; // number used for ordernum

function SortCompare(TempIndex, SourceIndex: integer): boolean; cdecl;
begin
    Result := false;

    case SortOrder of
      sEdition: // by edition
        if CompareText(TempSongArr[TempIndex].Edition[0], Songs.SongSort[SourceIndex].Edition[0]) <= 0 then
          Result := true;

      sGenre: // by genre
        if CompareText(TempSongArr[TempIndex].Genre[0], Songs.SongSort[SourceIndex].Genre[0]) <= 0 then
          Result := true;

      sTitle: // by title
        if CompareText(TempSongArr[TempIndex].Title, Songs.SongSort[SourceIndex].Title) <= 0 then
          Result := true;

      sArtist: // by artist
        if CompareText(TempSongArr[TempIndex].Artist, Songs.SongSort[SourceIndex].Artist) <= 0 then
          Result := true;

      sFolder: // by folder
        if CompareText(TempSongArr[TempIndex].Folder, Songs.SongSort[SourceIndex].Folder) <= 0 then
          Result := true;

      sTitle2: // by title2
        if CompareText(TempSongArr[TempIndex].Title, Songs.SongSort[SourceIndex].Title) <= 0 then
          Result := true;

      sArtist2: // by artist2
        if CompareText(TempSongArr[TempIndex].Artist, Songs.SongSort[SourceIndex].Artist) <= 0 then
          Result := true;

      sLanguage: // by Language
        if CompareText(TempSongArr[TempIndex].Language, Songs.SongSort[SourceIndex].Language) <= 0 then
          Result := true;

      sRandom:
        if (Random(2) = 0) then
          Result := true;

      sYear: //by Year
        Result := (TempSongArr[TempIndex].Year <= Songs.SongSort[SourceIndex].Year);

    end; // case
end;

function SortCopy(iSourceIndex, iDestIndex: integer; bDirection: TDirection): boolean; cdecl;
var
  TempSong: TSong;

begin
  Result := true;
    case bDirection of
      dSourceSource:
        begin
          TempSong := Songs.SongSort[iSourceIndex];
          Songs.SongSort[iDestIndex] := TempSong;
        end;

      dSourceTemp:
        begin
          TempSong := Songs.SongSort[iSourceIndex];
          TempSongArr[iDestIndex] := TempSong;
        end;

      dTempTemp:
        begin
          TempSong := TempSongArr[iSourceIndex];
          TempSongArr[iDestIndex] := TempSong;
        end;

      dTempSource:
        begin
          TempSong := TempSongArr[iSourceIndex];
          Songs.SongSort[iDestIndex] := TempSong;
        end;
    end;
end;

procedure TSongs.LoadSongList;
var
  I:  integer;
begin
  Log.LogStatus('Initializing', 'LoadSongList');

  // clear
  Setlength(Song, 100);
  NumFaultySongs := 0;

  BrowsePos := 0;
  // browse directories
  for I := 0 to Length(SongPaths) - 1 do
    BrowseDir(SongPaths[I], I);

  //Set Correct SongArray Length
  SetLength(Song, BrowsePos);
end;

procedure TSongs.BrowseDir(Dir: string; Index: integer);
var
  SR:     TSearchRec;   // for parsing Songs Directory
  SLen:   integer;
  res:    boolean;
  Name:   string;

begin
  if FindFirst(Dir + '*', faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        BrowseDir(Dir + SR.Name + '\', Index);
    until FindNext(SR) <> 0;
  end; // if
  FindClose(SR);

  if FindFirst(Dir + '*.tx?', 0, SR) = 0 then
  begin
    repeat
      //New Mod for better Memory Management

      SLen := BrowsePos;

      Song[SLen].Path := Dir;
      Song[SLen].Folder := Copy(Dir, Length(SongPaths[Index])+1, 10000);
      Song[SLen].Folder := Copy(Song[SLen].Folder, 1, Pos('\', Song[SLen].Folder)-1);
      Song[SLen].FileName := SR.Name;

      res := AnalyseFile(Song[SLen]); //TODO Hash?

      if res then
      begin
        SetLength(Czesci, 1);
        AktSong := Song[SLen];
        res := LoadSong(Song[SLen].Path + Song[SLen].FileName, SONG_LOAD_NOTES);

        if not CheckOK then
          inc(NumFaultySongs);
          
        if res then
        begin
          Song[SLen]:=AktSong;

          //Medley and Duet - is it possible? Perhaps later...
          if not AktSong.isDuet then
            FindRefrainStart(Song[SLen])
          else
            Song[SLen].Medley.Source := msNone;

          // scanning complete, file is good
          // if there is no cover then try to find it
          if Song[SLen].Cover = '' then
          begin
            Song[SLen].Cover := FindSongFile(Dir, '*[CO].jpg');
            if not FileExists(Song[SLen].Path + Song[SLen].Cover) then
              Song[SLen].Cover := '';
          end;

          try
            Song[SLen].CoverTex.TexNum := -1;
            if (Song[SLen].Cover <> '') then
            begin
              Name := Song[SLen].Path + Song[SLen].Cover;
              Texture.Limit := 512;
              // cache texture if there is a need to this
              if not Covers.CoverExists(Name) then
              begin
                Texture.CreateCacheMipmap := true;
                Texture.GetTexture(Name, 'Plain', true); // preloads textures and creates cache mipmap
                Texture.CreateCacheMipmap := false;

                // puts this texture to the cache file
                Covers.AddCover(Name);

                // unload full size texture
                Texture.UnloadTexture(Name, false);
              end;

              Song[SLen].CoverTex := Texture.GetTexture(Name, 'Plain', true);
              Texture.Limit := 1024*1024;
            end;
          except
            Song[SLen].CoverTex.TexNum := -1;
            Texture.Limit := 1024*1024;
          end;

          Inc(BrowsePos);
        end;
      end;

      //Change Length Only every 100 Entrys
      if (BrowsePos mod 100 = 0) AND (BrowsePos <> 0) and res then
        SetLength(Song, Length(Song) + 100);

      if (BrowsePos mod 100 = 0) and res then
      begin
        UpdateScreenLoading('Songs: '+IntToStr(BrowsePos));
        SDL_Delay(1);
      end;

    until FindNext(SR) <> 0;
  end; // if FindFirst
  FindClose(SR);
end;

procedure TSongs.Sort(Order: integer);
var
  MergeSort:    TMergeSorter;


begin
  MergeSort := TMergeSorter.Create();

  SetLength(TempSongArr, 0);
  SetLength(TempSongArr, (Length(SongSort)+1) div 2);

  SortOrder := Order;
  MergeSort.Sort(Length(SongSort), @SortCompare, @SortCopy);

  FreeAndNil(MergeSort);
  SetLength(TempSongArr, 0);
end;

function TSongs.FindSongFile(Dir, Mask: string): string;
var
  SR:     TSearchRec;   // for parsing song directory
begin
  Result := '';
  if FindFirst(Dir + Mask, faDirectory, SR) = 0 then begin
    Result := SR.Name;
  end; // if
  FindClose(SR);
end;

procedure TCatSongs.Refresh;
var
  S, I:     integer; // temporary song index
  CatLen:   integer; // length of CatSongs.Song
  Letter:   char; // current letter for sorting using letter
  SS:       string; // current edition for sorting using edition, genre etc.
  Order:    integer; // number used for ordernum
  Letter2:  char; //
  CatNumber:integer; // Number of Song in Category
  tempstr:  string;

  procedure CheckEdition(SongNr: integer);
  var
    newNr: integer;
    I:     integer;

  begin
    if (Length(Songs.SongSort[SongNr].Edition)>1) then
    begin
      newNr := Length(Songs.SongSort);
      SetLength(Songs.SongSort, newNr+1);
      Songs.SongSort[newNr] := Songs.SongSort[SongNr];
      SetLength(Songs.SongSort[SongNr].Edition, 1);

      for I := 0 to Length(Songs.SongSort[newNr].Edition) - 2 do
        Songs.SongSort[newNr].Edition[I] := Songs.SongSort[newNr].Edition[I+1];

      SetLength(Songs.SongSort[newNr].Edition, Length(Songs.SongSort[newNr].Edition)-1);
      CheckEdition(newNr);
    end;
  end;

  procedure CheckGenre(SongNr: integer);
  var
    newNr: integer;
    I:     integer;

  begin
    if (Length(Songs.SongSort[SongNr].Genre)>1) then
    begin
      newNr := Length(Songs.SongSort);
      SetLength(Songs.SongSort, newNr+1);
      Songs.SongSort[newNr] := Songs.SongSort[SongNr];
      SetLength(Songs.SongSort[SongNr].Genre, 1);

      for I := 0 to Length(Songs.SongSort[newNr].Genre) - 2 do
        Songs.SongSort[newNr].Genre[I] := Songs.SongSort[newNr].Genre[I+1];

      SetLength(Songs.SongSort[newNr].Genre, Length(Songs.SongSort[newNr].Genre)-1);
      CheckGenre(newNr);
    end;
  end;
begin
  Log.BenchmarkStart(3);
  CatNumShow := -1;

  SetLength(Songs.SongSort, 0);
  SetLength(Songs.SongSort, Length(Songs.Song));

  for S := 0 to Length(Songs.Song) - 1 do
  begin
    Songs.SongSort[S] := Songs.Song[S];
    if (Ini.Tabs=1) then
    begin
      if (Ini.Sorting=sEdition) then
      begin
        //work-around:
        SetLength(Songs.SongSort[S].Edition, 0);
        SetLength(Songs.SongSort[S].Edition, Length(Songs.Song[S].Edition));
        for I := 0 to Length(Songs.Song[S].Edition) - 1 do
          Songs.SongSort[S].Edition[I] := Songs.Song[S].Edition[I];
        //end work-around

        CheckEdition(S);
      end else if (Ini.Sorting = sGenre) then
      begin
        //work-around:
        SetLength(Songs.SongSort[S].Genre, 0);
        SetLength(Songs.SongSort[S].Genre, Length(Songs.Song[S].Genre));
        for I := 0 to Length(Songs.Song[S].Genre) - 1 do
          Songs.SongSort[S].Genre[I] := Songs.Song[S].Genre[I];
        //end work-around

        CheckGenre(S);
      end;
    end;      
  end;
  Log.BenchmarkEnd(3);
  Log.LogBenchmark('====> Workaround for Editions/Genres', 3);

  Log.BenchmarkStart(3);
  case Ini.Sorting of
    sEdition:
        begin
          Songs.Sort(sArtist);
          Songs.Sort(sEdition);
        end;
    sGenre:
        begin
          Songs.Sort(sArtist);
          Songs.Sort(sGenre);
        end;
    sLanguage:
        begin
          Songs.Sort(sArtist);
          Songs.Sort(sLanguage);
        end;
    sFolder:
        begin
          Songs.Sort(sArtist);
          Songs.Sort(sFolder);
        end;
    sTitle:
        begin
          Songs.Sort(sArtist);
          Songs.Sort(sTitle);
        end;
    sArtist:
        begin
          Songs.Sort(sTitle);
          Songs.Sort(sArtist);
        end;
    sTitle2:
        begin
          Songs.Sort(sArtist);
          Songs.Sort(sTitle);
        end;
    sArtist2:
        begin
          Songs.Sort(sTitle);
          Songs.Sort(sArtist);
        end;
    sRandom:
        begin
          Songs.Sort(sRandom);
        end;
    sYear:
        begin
          Songs.Sort(sTitle);
          Songs.Sort(sArtist);
          Songs.Sort(sYear);
        end;

  end; // case
  Log.BenchmarkEnd(3);
  Log.LogBenchmark('====> Sort Songs', 3);

  Log.BenchmarkStart(3);
  Letter := ' ';
  SS := '';
  Order := 0;
  CatNumber := 0;

  //Songs leeren
  SetLength (CatSongs.Song, 0);

  for S := Low(Songs.SongSort) to High(Songs.SongSort) do
  begin
    if (Ini.Tabs = 1) then
    begin
      if (Ini.Sorting = sEdition) and
        (CompareText(SS, Songs.SongSort[S].Edition[0]) <> 0) then
      begin
        // add Category Button
        Inc(Order);
        SS := Songs.SongSort[S].Edition[0];
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist :=  SS;
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

        //CatNumber Patch
        if (SS <> '') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sGenre) and
        (CompareText(SS, Songs.SongSort[S].Genre[0]) <> 0) then
      begin
        // add Genre Button
        Inc(Order);
        SS := Songs.SongSort[S].Genre[0];
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := SS;
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

        //CatNumber Patch
        if (SS <> '') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sLanguage) and
        (CompareText(SS, Songs.SongSort[S].Language) <> 0) then
      begin
        // add Language Button
        Inc(Order);
        SS := Songs.SongSort[S].Language;
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := SS;
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

        //CatNumber Patch
        if (SS <> '') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sTitle) and
        (Length(Songs.SongSort[S].Title)>=1) and
        (Letter <> UpCase(Songs.SongSort[S].Title[1])) then
      begin
        // add a letter Category Button
        Inc(Order);
        Letter := UpCase(Songs.SongSort[S].Title[1]);
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

        //CatNumber Patch
        if (Letter <> ' ') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sArtist) and
        (Length(Songs.SongSort[S].Artist)>=1) and
        (Letter <> UpCase(Songs.SongSort[S].Artist[1])) then
      begin
        // add a letter Category Button
        Inc(Order);
        Letter := UpCase(Songs.SongSort[S].Artist[1]);
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

        //CatNumber Patch
        if (Letter <> ' ') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sFolder) and
        (CompareText(SS, Songs.SongSort[S].Folder) <> 0) then
      begin
        // 0.5.0: add folder tab
        Inc(Order);
        SS := Songs.SongSort[S].Folder;
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := SS;
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

        //CatNumber Patch
        if (SS <> '') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sTitle2) AND
        (Length(Songs.SongSort[S].Title)>=1) then
      begin
        if (ord(Songs.SongSort[S].Title[1]) > 47) and
          (ord(Songs.SongSort[S].Title[1]) < 58) then
          Letter2 := '#'
        else
          Letter2 := UpCase(Songs.SongSort[S].Title[1]);

        if (Letter <> Letter2) then
        begin
          // add a letter Category Button
          Inc(Order);
          Letter := Letter2;
          CatLen := Length(CatSongs.Song);
          SetLength(CatSongs.Song, CatLen+1);
          CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
          CatSongs.Song[CatLen].Main := true;
          CatSongs.Song[CatLen].OrderTyp := 0;

          CatSongs.Song[CatLen].OrderNum := Order;

          CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

          //CatNumber Patch
          if (Letter <> ' ') then
          begin
            if (CatLen - CatNumber - 1>=0) then
              Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
            CatNumber := 0;
          end;

          CatSongs.Song[CatLen].Visible := true;
        end;
      end else if (Ini.Sorting = sArtist2) AND
        (Length(Songs.SongSort[S].Artist)>=1) then
      begin
        if (ord(Songs.SongSort[S].Artist[1]) > 47) and
          (ord(Songs.SongSort[S].Artist[1]) < 58) then
          Letter2 := '#'
        else
          Letter2 := UpCase(Songs.SongSort[S].Artist[1]);

        if (Letter <> Letter2) then
        begin
          // add a letter Category Button
          Inc(Order);
          Letter := Letter2;
          CatLen := Length(CatSongs.Song);
          SetLength(CatSongs.Song, CatLen+1);
          CatSongs.Song[CatLen].Artist := '[' + Letter + ']';
          CatSongs.Song[CatLen].Main := true;
          CatSongs.Song[CatLen].OrderTyp := 0;
          CatSongs.Song[CatLen].OrderNum := Order;

          CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

          //CatNumber Patch
          if (Letter <> ' ') then
          begin
            if (CatLen - CatNumber - 1>=0) then
              Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
            CatNumber := 0;
          end;

          CatSongs.Song[CatLen].Visible := true;
        end;
      end else if (Ini.Sorting = sRandom) and
        (Length(Songs.SongSort[S].Artist)>=1) and
        (Letter <> 'R') then
      begin
        // add a letter Category Button
        Inc(Order);
        Letter := 'R';
        CatLen := Length(CatSongs.Song);
        SetLength(CatSongs.Song, CatLen+1);
        CatSongs.Song[CatLen].Artist := '[RANDOM]';
        CatSongs.Song[CatLen].Main := true;
        CatSongs.Song[CatLen].OrderTyp := 0;
        CatSongs.Song[CatLen].OrderNum := Order;

        CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, Letter);

        //CatNumber Patch
        if (Letter <> ' ') then
        begin
          if (CatLen - CatNumber - 1>=0) then
            Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
          CatNumber := 0;
        end;

        CatSongs.Song[CatLen].Visible := true;
      end else if (Ini.Sorting = sYear) then
      begin
        I := Songs.SongSort[S].Year;
        if (I <> -1) then
          tempstr := IntToStr(Trunc(I/10)*10) + '-' + IntToStr(Trunc(I/10)*10+9)
        else
          tempstr := 'undefined';

        if (tempstr <> SS) then
        begin
          Inc(Order);
          SS := tempstr;
          CatLen := Length(CatSongs.Song);
          SetLength(CatSongs.Song, CatLen+1);
          CatSongs.Song[CatLen].Artist := SS;
          CatSongs.Song[CatLen].Main := true;
          CatSongs.Song[CatLen].OrderTyp := 0;
          CatSongs.Song[CatLen].OrderNum := Order;

          CatSongs.Song[CatLen].Cover := CatCovers.GetCover(Ini.Sorting, SS);

          //CatNumber Patch
          if (SS <> '') then
          begin
            if (CatLen - CatNumber - 1>=0) then
              Song[CatLen - CatNumber - 1].CatNumber := CatNumber;//Set CatNumber of Categroy
            CatNumber := 0;
          end;

          CatSongs.Song[CatLen].Visible := true;
        end;
      end;
    end;

    CatLen := Length(CatSongs.Song);
    
    if (CatLen>0) and CatSongs.Song[CatLen-1].Main then
      CatSongs.Song[CatLen-1].CoverTex.TexNum := -1;

    SetLength(CatSongs.Song, CatLen+1);

    Inc (CatNumber); //Increase Number in Cat

    CatSongs.Song[CatLen] := Songs.SongSort[S];
    CatSongs.Song[CatLen].OrderNum := Order; // assigns category
    CatSongs.Song[CatLen].CatNumber := CatNumber;

    if (Ini.Tabs = 0) then
    begin
      CatSongs.Song[CatLen].Visible := true;
      CatSongs.Song[CatLen].Main := false;
    end else if (Ini.Tabs = 1) then
      CatSongs.Song[CatLen].Visible := false;
  end;
  
  //CatNumber Patch - Set CatNumber of Last Category
  if (Ini.Tabs = 1) And (high(Song) >=1) then
    Song[CatLen - CatNumber].CatNumber := CatNumber;//Set CatNumber of Categroy
  //CatCount Patch
  CatCount := Order;

  Log.BenchmarkEnd(3);
  Log.LogBenchmark('====> Build Cat-Structure', 3);
end;

procedure TCatSongs.ShowCategory(Index: integer);
var
  S:    integer; // song
begin
  CatNumShow := Index;
  for S := 0 to high(CatSongs.Song) do
  begin
    if (CatSongs.Song[S].OrderNum = Index) AND (Not CatSongs.Song[S].Main) then
    begin
      if ((ScreenSong.Mode<>smNormal) and (not CatSongs.Song[S].isDuet)) or
         (ScreenSong.Mode=smNormal) then
        CatSongs.Song[S].Visible := true
      else
        CatSongs.Song[S].Visible := false;
    end else
      CatSongs.Song[S].Visible := false;
  end;
end;

procedure TCatSongs.HideCategory(Index: integer); // hides all songs in category
var
  S:    integer; // song
begin
  for S := 0 to high(CatSongs.Song) do
  begin
    if not CatSongs.Song[S].Main then
      CatSongs.Song[S].Visible := false // hides all at now
  end;
end;

procedure TCatSongs.ClickCategoryButton(Index: integer);
var
  Num:    integer;
begin
  Num := CatSongs.Song[Index].OrderNum;
  if Num <> CatNumShow then
  begin
    ShowCategory(Num);
  end else
  begin
    ShowCategoryList;
  end;
end;

function TCatSongs.NumCatSongs(Cat: integer): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Length(CatSongs.Song)-1 do
  begin
    if (CatSongs.Song[I].OrderNum = Cat) AND (Not CatSongs.Song[I].Main) then
    begin
      if ((ScreenSong.Mode<>smNormal) and (not CatSongs.Song[I].isDuet)) or
         (ScreenSong.Mode=smNormal) then
        inc(Result);
    end;
  end;
end;

function TCatSongs.NumSongs(): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Length(CatSongs.Song)-1 do
  begin
    if (Not CatSongs.Song[I].Main) then
    begin
      if ((ScreenSong.Mode<>smNormal) and (not CatSongs.Song[I].isDuet)) or
         (ScreenSong.Mode=smNormal) then
        inc(Result);
    end;
  end;
end;

function TCatSongs.NumVisibleCats(): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Length(CatSongs.Song)-1 do
  begin
    if CatSongs.Song[I].Main and CatSongs.Song[I].Visible then
    begin
      inc(Result);
    end;
  end;
end;

//Hide Categorys when in Category Hack
procedure TCatSongs.ShowCategoryList;
var
  S:    integer;
  vis:  boolean;
begin
  //Hide All Songs Show All Cats
  for S := 0 to high(CatSongs.Song) do
  begin
    vis := false;

    if CatSongs.Song[S].Main then
    begin
      if (ScreenSong.Mode=smNormal) then
        Vis := true
      else if (NumCatSongs(CatSongs.Song[S].OrderNum)>0) then
        Vis := true;
    end;

    CatSongs.Song[S].Visible := vis;
  end;
  CatSongs.Selected := CatNumShow; //Show last shown Category
  CatNumShow := -1;
end;
//Hide Categorys when in Category Hack End

//Wrong song selected when tabs on bug
function TCatSongs.FindNextVisible(SearchFrom:integer): integer;//Find next Visible Song
var
  I: Integer;
  begin
  Result := -1;
  I := SearchFrom + 1;
  while not CatSongs.Song[I].Visible do
    begin
    Inc (I);
    if (I>high(CatSongs.Song)) then
      I := low(CatSongs.Song);
    if (I = SearchFrom) then //Make One Round and no song found->quit
      break;
    end;
  end;
//Wrong song selected when tabs on bug End

function TCatSongs.VisibleSongs: integer;
var
  S:    integer; // song
begin
  Result := 0;
  for S := 0 to high(CatSongs.Song) do
    if CatSongs.Song[S].Visible = true then Inc(Result);
end;

function TCatSongs.VisibleIndex(Index: integer): integer;
var
  S:    integer; // song
begin
  Result := 0;
  for S := 0 to Index-1 do
    if CatSongs.Song[S].Visible = true then Inc(Result);
end;

function TCatSongs.SetFilter(FilterStr: String; const fType: Byte): Cardinal;
var
  I, J: Integer;
  cString: String;
  SearchStr: Array of String;
begin
  {fType: 0: All
          1: Title
          2: Artist
          3: Duet}
  FilterStr := Trim(FilterStr);
  if FilterStr<>'' then
  begin
    Result := 0;
    //Create Search Array
    SetLength(SearchStr, 1);
    I := Pos (' ', FilterStr);
    While (I <> 0) do
    begin
      SetLength (SearchStr, Length(SearchStr) + 1);
      cString := Copy(FilterStr, 1, I-1);
      if (cString <> ' ') AND (cString <> '') then
        SearchStr[High(SearchStr)-1] := cString;
      Delete (FilterStr, 1, I);

      I := Pos (' ', FilterStr);
    end;
    //Copy last Word
    if (FilterStr <> ' ') AND (FilterStr <> '') then
      SearchStr[High(SearchStr)] := FilterStr;

    for I:=0 to High(Song) do begin
      if not Song[i].Main then
      begin
        case fType of
          0: cString := Song[I].Artist + ' ' + Song[i].Title + ' ' + Song[i].Folder;
          1: cString := Song[I].Title;
          2: cString := Song[I].Artist;
        end;
        Song[i].Visible:=True;
        //Look for every Searched Word
        For J := 0 to High(SearchStr) do
        begin
          Song[i].Visible := Song[i].Visible AND AnsiContainsText(cString, SearchStr[J])
        end;
        if Song[i].Visible then
          Inc(Result);
      end
      else
        Song[i].Visible:=False;
    end;
    CatNumShow := -2;
  end else if (fType<>3) then
  begin
    for i:=0 to High(Song) do
    begin
      Song[i].Visible:=(Ini.Tabs=1)=Song[i].Main;
      if (ScreenSong.Mode<>smNormal) and Song[i].isDuet then
        Song[i].Visible := false;
      CatNumShow := -1;
    end;
    Result := 0;
  end else
  begin
    Result := 0;
    for i:=0 to High(Song) do
    begin
      Song[i].Visible := false;
      if not Song[i].Main and Song[i].isDuet then
      begin
        Song[i].Visible:=true;
        inc(Result);
      end;
      CatNumShow := -2;
    end;
  end;
           
end;

end.