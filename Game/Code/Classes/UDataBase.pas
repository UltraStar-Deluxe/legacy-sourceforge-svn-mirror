unit UDataBase;

interface

uses USongs, SQLiteTable3;

//--------------------
//DataBaseSystem - Class including all DB Methods
//--------------------
type
  THandicapResult = record
    P1m: real;
    P2m: real;
  end;

  TStatResult = record
    Case Typ: Byte of
      0: (Singer:     ShortString;    //best scores
          Score:      Word;
          Difficulty: Byte;
          SongArtist: ShortString;
          SongTitle:  ShortString;
          Date:       ShortString);

      1: (Player:     ShortString;    //best singers
          AverageScore: Word;
          SungTimes:    Word);

      2: (Artist: ShortString;        //most sung song
          Title:  ShortString;
          TimesSung:  Word);

      3: (ArtistName:   ShortString;  //most sung band
          TimesSungtot: Word);
  end;
  AStatResult = Array of TStatResult;
  
  TDataBaseSystem = class
    private
      ScoreDB: TSqliteDatabase;
      sFilename: string;
    public


    property Filename: String read sFilename;
    
    Destructor Free;

    Procedure Init(const Filename: string);
    procedure ReadScore(var Song: TSong; max, sum: integer);
    procedure AddScore(var Song: TSong; Level: integer; Name: string; Score: integer; TimeStamp: integer);
    procedure WriteScore(var Song: TSong);

    Function  GetStats(var Stats: AStatResult; const Typ, Count: Byte; const Page: Cardinal; const Reversed: Boolean): Boolean;
    Function  GetTotalEntrys(const Typ: Byte): Cardinal;
    function  FormatDate(time_stamp: integer): String;
    function  GetHandicap(P1: string; P2: string): THandicapResult; //for Handicap-Mode
    function  GetAspect(Artist, Title: string; def: integer): integer;
    procedure SetAspect(Artist, Title: string; aspect: integer);
  end;

var
  DataBase: TDataBaseSystem;

implementation

uses IniFiles, SysUtils, DateUtils, ULanguage;

function  TDataBaseSystem.GetHandicap(P1: string; P2: string): THandicapResult;
const
  min = 3;
var
  P1m, P2m: real;
  P1c, P2c: integer;

  tP1, tP2: string;
begin
  if not Assigned(ScoreDB) then
    Exit;

  //init
  Result.P1m := 1;
  Result.P2m := 1;
  P1m := 0;
  P2m := 0;

  try
    tP1 := StringReplace(P1,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tP2 := StringReplace(P2,'"','""',[rfReplaceAll, rfIgnoreCase]);

    P1c := ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `US_Scores` '+
      'WHERE `Player` = "' + tP1 + '";');
    P2c := ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `US_Scores` '+
      'WHERE `Player` = "' + tP2 + '";');

    if (P1c>min) and (P2c>min) then
    begin
      P1m := ScoreDB.GetTableFloat('SELECT AVG(`Score`) FROM `US_Scores` '+
        'WHERE `Score` IN ('+
        'SELECT Score FROM `US_Scores` '+
        'WHERE `Player` = "' + tP1 + '" ORDER BY `rowid` DESC LIMIT 10);');

      P2m := ScoreDB.GetTableFloat('SELECT AVG(`Score`) FROM `US_Scores` '+
        'WHERE `Score` IN ('+
        'SELECT Score FROM `US_Scores` '+
        'WHERE `Player` = "' + tP2 + '" ORDER BY `rowid` DESC LIMIT 10);');
    end;
  except
    P1m := 0;
    P2m := 0;
  end;

  if (P1m>0) and (P2m>0) then
  begin
    if (P1m>=P2m) then
      Result.P1m := 1-((P1m-P2m)/10000)/1.75
    else
      Result.P2m := 1-((P2m-P1m)/10000)/1.75
  end;
end;

function TDataBaseSystem.GetAspect(Artist, Title: string; def: integer): integer;
var
  ID:               Integer;
  tArtist, tTitle:  string;

begin
  try
    tArtist := StringReplace(Artist,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tTitle := StringReplace(Title,'"','""',[rfReplaceAll, rfIgnoreCase]);

    ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' +
      tArtist + '" AND `Title` = "' + tTitle + '"');

    if ID = 0 then //Song doesn't exist -> Create
    begin
      ScoreDB.ExecSQL ('INSERT INTO `US_Songs` ( `ID` , `Artist` , `Title` , `TimesPlayed`, `Aspect` ) '+
              'VALUES (NULL , "' + tArtist + '", "' + tTitle + '", "0", "'+ IntToStr(def) +'");');
      Result := def;
    end else
    begin
      Result := ScoreDB.GetTableValue('SELECT `Aspect` FROM `US_Songs` WHERE `ID` = "' +
        IntToStr(ID) + '"');
    end;
  except
    Result := def;
  end;
end;

procedure TDataBaseSystem.SetAspect(Artist, Title: string; aspect: integer);
var
  ID:               Integer;
  tArtist, tTitle:  string;

begin
  try
    tArtist := StringReplace(Artist,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tTitle := StringReplace(Title,'"','""',[rfReplaceAll, rfIgnoreCase]);

    ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' +
      tArtist + '" AND `Title` = "' + tTitle + '"');

    if ID = 0 then //Song doesn't exist -> Create
    begin
      ScoreDB.ExecSQL ('INSERT INTO `US_Songs` ( `ID` , `Artist` , `Title` , `TimesPlayed`, `Aspect` ) '+
              'VALUES (NULL , "' + tArtist + '", "' + tTitle + '", "0", "'+ IntToStr(Aspect) +'");');
    end else
    begin

      //Create new Entry
      ScoreDB.ExecSQL ('UPDATE `Us_Songs` SET `Aspect` = "' + IntToStr(aspect) + '" WHERE `ID` = "' +
        IntToStr(ID) + '"');
    end;
  except

  end;
end;

(**
 * Format a UNIX-Timestamp into DATE (If 0 then '')
 *)
function TDataBaseSystem.FormatDate(time_stamp: integer): String;
var
  Year, Month, Day: word;
begin
  Result:='';
  try
    if time_stamp<>0 then
    begin
      DecodeDate(UnixToDateTime(time_stamp), Year, Month, Day);
      Result := Format(Language.Translate('STAT_FORMAT_DATE'), [Day, Month, Year]);
    end;
  except
    on E: EConvertError do
    //Log.LogError('Error Parsing FormatString "STAT_FORMAT_DATE": ' + E.Message); TODO
  end;
end;



//--------------------
//Create - Opens Database and Create Tables if not Exist
//--------------------

Procedure TDataBaseSystem.Init(const Filename: string);
begin
  //Open Database
  ScoreDB := TSqliteDatabase.Create(Filename);
  sFilename := Filename;

  try
    //Look for Tables => When not exist Create them
    if not ScoreDB.TableExists('US_Scores') then
      ScoreDB.execsql('CREATE TABLE `US_Scores` (`SongID` INT( 11 ) NOT NULL , '+
        '`Difficulty` INT( 1 ) NOT NULL , `Player` VARCHAR( 150 ) NOT NULL , '+
        '`Score` INT( 5 ) NOT NULL , `Date` INT NULL)');

    if not ScoreDB.TableExists('US_Songs') then
      ScoreDB.execsql('CREATE TABLE `US_Songs` (`ID` INTEGER PRIMARY KEY, '+
        '`Artist` VARCHAR( 255 ) NOT NULL , `Title` VARCHAR( 255 ) NOT NULL , `TimesPlayed` int(5) NOT NULL );');

    //add column date to cUS-Scores
    if not ScoreDB.ContainsColumn('US_Scores', 'Date') then
    begin
      //Log.LogInfo('adding column date to "' + cUS_Scores + '"', 'TDataBaseSystem.Init'); TODO
      ScoreDB.ExecSQL('ALTER TABLE ' + '`US_Scores`' + ' ADD COLUMN `Date` INT NULL');
    end;

    //add column aspect to cUS-Songs
    if not ScoreDB.ContainsColumn('US_Songs', 'Aspect') then
    begin
      //0=acoStretch; 1=acoCrop; 2=acoLetterBox
      ScoreDB.ExecSQL('ALTER TABLE ' + '`US_Songs`' + ' ADD COLUMN `Aspect` INT NULL');
    end;
  finally
  //ScoreDB.Free;
  end;

end;

//--------------------
//Free - Frees Database
//--------------------
Destructor TDataBaseSystem.Free;
begin
  ScoreDB.Free;
end;

//--------------------
//ReadScore - Read Scores into SongArray
//
//sum:
//  0=never
//  1=only, if more then max entries (dynamic)
//  2=always
//--------------------
procedure TDataBaseSystem.ReadScore(var Song: TSong; max, sum: integer);
var
  TableData:    TSqliteTable;
  Difficulty:   Byte;
  I:            integer;
  PlayerListed: boolean;
  DateStr:      string;
  num:          array[0..2] of integer; //num entries easy, medium, hard
  tArtist,
  tTitle:       string;

begin
  if not Assigned(ScoreDB) then
    Exit;

  try
    tArtist := StringReplace(Song.Artist,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tTitle := StringReplace(Song.Title,'"','""',[rfReplaceAll, rfIgnoreCase]);

    //count num entries
    if(sum=1) then
    begin
      num[0] := ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `US_Scores` '+
        'WHERE `Difficulty` = 0 and '+
        '`SongID` = (SELECT `ID` FROM `us_songs` WHERE `Artist` = "' +
        tArtist + '" AND `Title` = "' + tTitle +
        '" LIMIT 1);');

      num[1] := ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `US_Scores` '+
        'WHERE `Difficulty` = 1 and '+
        '`SongID` = (SELECT `ID` FROM `us_songs` WHERE `Artist` = "' +
        tArtist + '" AND `Title` = "' + tTitle +
        '" LIMIT 1);');

      num[2] := ScoreDB.GetTableValue('SELECT COUNT(`SongID`) FROM `US_Scores` '+
        'WHERE `Difficulty` = 2 and '+
        '`SongID` = (SELECT `ID` FROM `us_songs` WHERE `Artist` = "' +
        tArtist + '" AND `Title` = "' + tTitle +
        '" LIMIT 1);');
    end;

    //Search Song in DB
    TableData := ScoreDB.GetTable('SELECT `Difficulty`, `Player`, `Score`, `Date` '+
      'FROM `us_scores` WHERE '+
      '`SongID` = (SELECT `ID` FROM `us_songs` WHERE `Artist` = "' +
      tArtist + '" AND `Title` = "' + tTitle +
      '" LIMIT 1) ORDER BY `Score` DESC;');
    //Empty Old Scores
    SetLength (Song.Score[0], 0); //easy
    SetLength (Song.Score[1], 0); //medium
    SetLength (Song.Score[2], 0); //hard

   // Go through all Entrys
    while (not TableData.EOF) do
    begin
      // Add one Entry to Array
      Difficulty := StrToInt(TableData.FieldAsString(TableData.FieldIndex['Difficulty']));
      if ((Difficulty >= 0) and (Difficulty <= 2)) and
         (Length(Song.Score[Difficulty]) < max) then
      begin
        //filter player
        PlayerListed:=false;
        if (Length(Song.Score[Difficulty])>0) then
        begin
          for I := 0 to Length(Song.Score[Difficulty]) - 1 do
          begin
            if (Song.Score[Difficulty, I].Name = TableData.FieldAsString(TableData.FieldIndex['Player'])) then
            begin
              PlayerListed:=true;
              break;
            end;
          end;
        end;

        if (sum=0) or
          ((sum=1) and (num[Difficulty]<=max)) or
          ((sum=1) and (num[Difficulty]>max) and not PlayerListed) or
          ((sum=2) and not PlayerListed) then
        begin
          SetLength(Song.Score[Difficulty], Length(Song.Score[Difficulty]) + 1);

          Song.Score[Difficulty, High(Song.Score[Difficulty])].Name  :=
            TableData.FieldAsString(TableData.FieldIndex['Player']);
          Song.Score[Difficulty, High(Song.Score[Difficulty])].Score :=
            StrtoInt(TableData.FieldAsString(TableData.FieldIndex['Score']));
          DateStr := TableData.FieldAsString(TableData.FieldIndex['Date']);
          if DateStr<>'' then
            Song.Score[Difficulty, High(Song.Score[Difficulty])].Date :=
              FormatDate(StrToInt(DateStr))
          else
            Song.Score[Difficulty, High(Song.Score[Difficulty])].Date := '??.??.20??';
        end;
      end;

      TableData.Next;
    end; // while

  except
    for Difficulty := 0 to 2 do
    begin
      SetLength(Song.Score[Difficulty], 1);
      Song.Score[Difficulty, 1].Name := 'Error Reading ScoreDB';
    end;
  end;

end;

//--------------------
//AddScore - Add one new Score to DB
//--------------------
procedure TDataBaseSystem.AddScore(var Song: TSong; Level: integer; Name: string; Score: integer; TimeStamp: integer);
var
  ID:                     Integer;
  tArtist, tTitle, tName: string;

begin
  //Prevent 0 Scores from being added
  if (Score > 0) then
  begin
    tArtist := StringReplace(Song.Artist,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tTitle := StringReplace(Song.Title,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tName := StringReplace(Name,'"','""',[rfReplaceAll, rfIgnoreCase]);

    try //todo : wrapper shouldn't throw exceptions at all - this fixed a wine bug, thanks linnex! (11.11.07)
      ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + tArtist +
        '" AND `Title` = "' + tTitle + '"');
      if ID = 0 then //Song doesn't exist -> Create
      begin
        ScoreDB.ExecSQL ('INSERT INTO `US_Songs` ( `ID` , `Artist` , `Title` , `TimesPlayed` ) '+
          'VALUES (NULL , "' + tArtist + '", "' + tTitle + '", "0");');
        ID := ScoreDB.GetTableValue('SELECT `ID` FROM `US_Songs` WHERE `Artist` = "' + tArtist +
          '" AND `Title` = "' + tTitle + '"');

        if ID = 0 then //Could not Create Table
          exit;
      end;

      //Create new Entry
      ScoreDB.ExecSQL('INSERT INTO `US_Scores` ( `SongID` , `Difficulty` , `Player` , `Score` , `Date` ) '+
        'VALUES ("' + InttoStr(ID) + '", "' + InttoStr(Level) + '", "' +
        tName + '", "' + InttoStr(Score) + '","' + InttoStr(TimeStamp) + '");');
    except

    end;
  end;
end;

//--------------------
//WriteScore - Not needed with new System; But used for Increment Played Count
//--------------------
procedure TDataBaseSystem.WriteScore(var Song: TSong);
var
  tArtist, tTitle:  string;
begin
  try
    tArtist := StringReplace(Song.Artist,'"','""',[rfReplaceAll, rfIgnoreCase]);
    tTitle := StringReplace(Song.Title,'"','""',[rfReplaceAll, rfIgnoreCase]);
    //Increase TimesPlayed
    ScoreDB.ExecSQL ('UPDATE `us_songs` SET `TimesPlayed` = `TimesPlayed` + "1" WHERE `Title` = "' +
      tTitle + '" AND `Artist` = "' + tArtist + '";');
  except

  end;
end;

//--------------------
//GetStats - Write some Stats to Array, Returns True if Chossen Page has Entrys
//Case Typ of
//0 - Best Scores
//1 - Best Singers
//2 - Most sung Songs
//3 - Most popular Band
//--------------------
Function TDataBaseSystem.GetStats(var Stats: AStatResult; const Typ, Count: Byte; const Page: Cardinal; const Reversed: Boolean): Boolean;
var
  Query: String;
  TableData: TSqliteTable;
  DateStr: String;
begin
  Result := False;

  if (Length(Stats) < Count) then
    Exit;

  {Todo:
    Add Prevention that only Players with more than 5 Scores are Selected at Typ 2}

  //Create Query
  Case Typ of
    0: Query := 'SELECT `Player` , `Difficulty` , `Score` , `Artist` , `Title` , `Date` '+
      'FROM `US_Scores` INNER JOIN `US_Songs` ON (`SongID` = `ID`) ORDER BY `Score`';
    1: Query := 'SELECT `Player` , ROUND (Sum(`Score`) / COUNT(`Score`)), '+
      'COUNT(`rowid`) FROM `US_Scores` GROUP BY `Player` '+
      'ORDER BY (Sum(`Score`) / COUNT(`Score`))';
    2: Query := 'SELECT `Artist` , `Title` , `TimesPlayed` '+
      'FROM `US_Songs` WHERE `TimesPlayed` > 0 ORDER BY `TimesPlayed`';
    3: Query := 'SELECT `Artist` , Sum(`TimesPlayed`) '+
      'FROM `US_Songs` WHERE `TimesPlayed` > 0 GROUP BY `Artist` ORDER BY Sum(`TimesPlayed`)';
  end;

  //Add Order Direction
  If Reversed then
    Query := Query + ' ASC'
  else
    Query := Query + ' DESC';

  //Add Limit
  Query := Query + ' LIMIT ' + InttoStr(Count * Page) + ', ' + InttoStr(Count) + ';';

  //Execute Query
  //try
    TableData := ScoreDB.GetTable(Query);
  {except
    exit;
  end;}

  //if Result empty -> Exit
  if (TableData.RowCount < 1) then
    exit;

  //Copy Result to Stats Array
  while not TableData.Eof do
  begin
    Stats[TableData.Row].Typ := Typ;

    Case Typ of
      0:begin
          Stats[TableData.Row].Singer := TableData.Fields[0];

          Stats[TableData.Row].Difficulty := StrtoIntDef(TableData.Fields[1], 0);

          Stats[TableData.Row].Score := StrtoIntDef(TableData.Fields[2], 0){TableData.FieldAsInteger(2)};
          Stats[TableData.Row].SongArtist := TableData.Fields[3];
          Stats[TableData.Row].SongTitle := TableData.Fields[4];
          DateStr := TableData.Fields[5];
          if DateStr<>'' then
            Stats[TableData.Row].Date := FormatDate(StrToInt(DateStr))
          else
            Stats[TableData.Row].Date := '??.??.20??';
        end;

        1:begin
          Stats[TableData.Row].Player := TableData.Fields[0];
          Stats[TableData.Row].AverageScore := StrtoIntDef(TableData.Fields[1], 0);
          Stats[TableData.Row].SungTimes := StrtoIntDef(TableData.Fields[2], 0);
        end;

        2:begin
          Stats[TableData.Row].Artist := TableData.Fields[0];
          Stats[TableData.Row].Title  := TableData.Fields[1];
          Stats[TableData.Row].TimesSung  := StrtoIntDef(TableData.Fields[2], 0);
        end;

        3:begin
          Stats[TableData.Row].ArtistName := TableData.Fields[0];
          Stats[TableData.Row].TimesSungtot := StrtoIntDef(TableData.Fields[1], 0);
        end;

    end;

    TableData.Next;
  end;

  Result := True;
end;

//--------------------
//GetTotalEntrys - Get Total Num of entrys for a Stats Query
//--------------------
Function  TDataBaseSystem.GetTotalEntrys(const Typ: Byte): Cardinal;
var Query: String;
begin
  //Create Query
  Case Typ of
    0: Query := 'SELECT COUNT(`SongID`) FROM `US_Scores`;';
    1: Query := 'SELECT COUNT(DISTINCT `Player`) FROM `US_Scores`;';
    2: Query := 'SELECT COUNT(`ID`) FROM `US_Songs` '+
      'WHERE `TimesPlayed` > 0;';
    3: Query := 'SELECT COUNT(DISTINCT `Artist`) FROM `US_Songs` '+
      'WHERE `TimesPlayed` > 0;';
  end;

  Result := ScoreDB.GetTableValue(Query);
end;

end.