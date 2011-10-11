unit UPartyM2;

interface

uses
  ModiSDK, UDatabase;

type
  TRoundInfo = record
    Winner: byte;
    Plugin: word;
    Medley: boolean;
    MedleySurprise: boolean;
    PluginNr: Integer;
    Player1: byte;
    Player2: byte;
    ScoreP:     Integer;
    ScoreN:     Integer;
  end;

  TPair = record
    Player1: byte;
    Player2: byte;
    played: integer;
    origin: integer;
  end;

  TSongsPlayed = record
    cat: integer;
    Played: array of integer;
  end;

  PlayerOrderArray = array[0..8] of byte;

  TPartyPlugin = record
    ID:          byte;
    TimesPlayed: byte;
    Medley:      boolean;
    MedleySurprise: boolean;
    Selected:    boolean;
    Name:        string;
    Desc:        string;
  end;

  TPartySessionM2 = class
  private
    Pairs: array of TPair;
    SongsPlayed: array of TSongsPlayed;
    function GetRandomPlugin(var medley: boolean; var surprise: boolean; var nr: integer): byte;

  public
    Rounds:   array of TRoundInfo;

    Plugins:  array of TPartyPlugin;

    Handicap: THandicapResult;
    HandicapMode: boolean;

    NumRounds: byte;
    CurRound: byte;
    Players:  TPlayersInfo;
    Teams: TTeamInfo;
    Order: PlayerOrderArray;
    Option_Plugins: boolean;
    ID_DUELL: Byte;
    constructor Create;

    procedure GenScores;
    procedure StartNewParty(NumPlayers: byte; nRounds: byte);
    procedure StartRound;
    procedure EndRound;
    procedure BuildOrder;
    procedure AddSongPlayed(cat: Integer; SongNr: integer);
    procedure ResetSongsPlayed(cat: Integer);
    function  SongPlayed(cat: Integer; SongNr: integer): boolean;
    function  GetSongsPlayed(cat: Integer): integer;
    function  GetCatIndex(cat: Integer): Integer;
  end;

var
  PartySessionM2: TPartySessionM2;

implementation

uses
  UDLLManager,
  UGraphic,
  ULanguage,
  UMain,
  UIni,
  Math,
  ULog;

constructor TPartySessionM2.Create;
begin
  inherited;
end;

function  TPartySessionM2.SongPlayed(cat: integer; SongNr: integer): boolean;
var
  i: integer;
  played :boolean;
  catIndex: integer;
begin
  played := false;
  catIndex:=GetCatIndex(cat);
  for i := 0 to Length(PartySessionM2.SongsPlayed[catIndex].Played) - 1 do
  begin
    if (SongNr=PartySessionM2.SongsPlayed[catIndex].Played[i]) then
    begin
      played:=true;
      break;
    end;
  end;
  Result:=played;
end;

function TPartySessionM2.GetSongsPlayed(cat: integer): Integer;
var
  CatIndex: integer;
begin
  CatIndex := GetCatIndex(cat);
  Result := Length(PartySessionM2.SongsPlayed[CatIndex].Played);
end;

procedure TPartySessionM2.ResetSongsPlayed(cat: Integer);
var
  CatIndex: integer;
begin
  CatIndex := GetCatIndex(cat);
  SetLength(SongsPlayed[CatIndex].Played, 0);
end;


procedure TPartySessionM2.AddSongPlayed(cat: integer; SongNr: integer);
var
  catIndex: integer;
begin
  catIndex:=GetCatIndex(cat);
  SetLength(PartySessionM2.SongsPlayed[catIndex].Played, Length(PartySessionM2.SongsPlayed[catIndex].Played)+1);
  PartySessionM2.SongsPlayed[catIndex].Played[Length(PartySessionM2.SongsPlayed[catIndex].Played)-1]:=SongNr;
end;

function TPartySessionM2.GetCatIndex(cat: Integer): Integer;
var
  i: integer;
  found: boolean;
begin
  found:=false;
  Result := 0;
  for i := 0 to length(SongsPlayed) - 1 do
  begin
    if SongsPlayed[i].cat = cat then
    begin
      Result:=i;
      found:=true;
      break;
    end;
  end;
  if not found then
  begin
    SetLength(SongsPlayed, Length(SongsPlayed)+1);
    SongsPlayed[Length(SongsPlayed)-1].cat:=cat;
    Result:=Length(SongsPlayed)-1;
  end;
end;

procedure TPartySessionM2.BuildOrder();
var
  I,J:    integer;
  cache:  byte;
begin
  for I := 0 to Players.NumPlayer-2 do
  begin
    for J := I+1 to Players.NumPlayer - 1 do
    begin
      if (Players.Playerinfo[Order[I]].Points < Players.Playerinfo[Order[J]].Points) then
      begin
        cache:=Order[I];
        Order[I]:=Order[J];
        Order[J]:=cache;
      end else if (Players.Playerinfo[Order[I]].Points = Players.Playerinfo[Order[J]].Points) and
        ((Players.Playerinfo[Order[I]].ScoreP-Players.Playerinfo[Order[I]].ScoreN) <
        (Players.Playerinfo[Order[J]].ScoreP - Players.Playerinfo[Order[J]].ScoreN)) then
      begin
        cache:=Order[I];
        Order[I]:=Order[J];
        Order[J]:=cache;
      end;
    end;
  end;
end;


//----------
//StartNewParty - Reset and prepares for new party
//----------
procedure TPartySessionM2.StartNewParty(NumPlayers: byte; nRounds: byte);
type
  TFields = record
    x: byte;
    y: byte;
  end;

  TPlayer = record
    Player: byte;
    played: integer;
  end;

  TRN = record
    rounds: array of integer;
  end;

  //if the player has played the last 2 rounds
  function HasPlayed(Pair: TPair; ar: array of TFields; r: integer; num: integer): boolean;
  begin
    //first round
    if r<2 then
      Result:=false

    //exactly the same combination in last round
    else if (ar[r-2].x=Pair.Player1) and (ar[r-2].y=Pair.Player2) then
      Result:=true

    //only two Players => there are no other combinations
    else if num<3 then
      Result:=false

    //reversed combination before
    else if (ar[r-2].x=Pair.Player2) and (ar[r-2].y=Pair.Player1) then
      Result:=true

    //it is the second round
    else if r<3 then
      Result:=false

    else if (num > 3) and (
      (ar[r-2].x=Pair.Player1) or (ar[r-2].y=Pair.Player1) or
      (ar[r-2].x=Pair.Player2) or (ar[r-2].y=Pair.Player2)) then
      Result := true
    //
    else if
      ((ar[r-2].x = Pair.Player1) and (ar[r-3].x = Pair.Player1)) or
      ((ar[r-2].x = Pair.Player1) and (ar[r-3].y = Pair.Player1)) or
      ((ar[r-2].y = Pair.Player1) and (ar[r-3].x = Pair.Player1)) or
      ((ar[r-2].y = Pair.Player1) and (ar[r-3].y = Pair.Player1)) or
      ((ar[r-2].x = Pair.Player2) and (ar[r-3].x = Pair.Player2)) or
      ((ar[r-2].x = Pair.Player2) and (ar[r-3].y = Pair.Player2)) or
      ((ar[r-2].y = Pair.Player2) and (ar[r-3].x = Pair.Player2)) or
      ((ar[r-2].y = Pair.Player2) and (ar[r-3].y = Pair.Player2))then

      Result := true
    else
      Result := false;
  end;

var
  Player: array of TPlayer;
  PlayerOrder: array of integer;
  Len:      integer;
  I, J, K:     integer;
  arr:  array of TFields;
  temp_pairs: array of TPair;
  pairs_season: array of boolean;
  season: integer;
  num: integer;
  max_played: integer;
  max_flag: boolean;
  must_sing: integer;
  DuelRatioFactor: integer;

  //debug
  rn: array of TRN;

begin
  //Set current round to 1
  CurRound := 255;
  NumRounds:= nRounds;
  for I := 0 to 8 do
  begin
    Order[I]:=I;
  end;

  //build all possible pairs
  SetLength(Pairs, 0);
  SetLength(Player, NumPlayers);
  num:=0;

  for I := 0 to NumPlayers - 1 do
  begin
    Player[I].Player:=I;
    Player[I].played:=0;
  end;

  for I := 1 to NumPlayers - 1 do
  begin
    for J := I+1 to NumPlayers do
    begin
      SetLength(Pairs, Length(Pairs)+1);
      Pairs[Length(Pairs)-1].Player1:=I-1;
      Pairs[Length(Pairs)-1].Player2:=J-1;
      Pairs[Length(Pairs)-1].played:=0;
      Pairs[Length(Pairs)-1].origin:=num;
      inc(num);
      SetLength(Pairs, Length(Pairs)+1);
      Pairs[Length(Pairs)-1].Player1:=J-1;
      Pairs[Length(Pairs)-1].Player2:=I-1;
      Pairs[Length(Pairs)-1].played:=0;
      Pairs[Length(Pairs)-1].origin:=num;
      inc(num);
    end;
  end;

  //build the playlist
  SetLength (arr, 0);
  SetLength (arr, NumRounds);
  SetLength (pairs_season, (NumPlayers*NumPlayers-NumPlayers) div 2);

  for I := 0 to NumRounds - 1 do
  begin
    //filter pairs
    season:=Floor((I)/(NumPlayers*NumPlayers-NumPlayers))+1; //complete season

    SetLength(temp_pairs, 0);

    max_flag := false;
    max_played := 0;


    for K := 0 to NumPlayers - 1 do
    begin
      if (max_played<Player[K].played) then
        max_played := Player[K].played;
    end;

    J := 0;
    for K := 0 to NumPlayers - 1 do
    begin
      if (max_played>Player[K].played) then
        inc(J);
    end;

    must_sing := -1;
    if J>1 then
      max_flag := true
    else if J=1 then
    begin
      for K := 0 to NumPlayers - 1 do
      begin
        if (max_played>Player[K].played) then
          must_sing:=K;
      end;
    end;

    for J := 0 to Length(Pairs) - 1 do
    begin
      K := Floor(J / 2);

      if (not HasPlayed(Pairs[J], arr, I+1, NumPlayers)) and
        (Pairs[J].played<season) and not pairs_season[K] then
      begin
        if (not max_flag and (must_sing=-1)) or
          ((must_sing>=0) and (
          (Pairs[J].Player1=must_sing) or
          (Pairs[J].Player2=must_sing))) or
          (max_flag and (
          (Player[Pairs[J].Player1].played<max_played) and
          (Player[Pairs[J].Player2].played<max_played))) then
        begin
          SetLength(temp_pairs, Length(temp_pairs)+1);
          temp_pairs[Length(temp_pairs)-1]:=Pairs[J];
        end;
      end;
    end;

    //first fallback
    if Length(temp_pairs)=0 then
    begin
      for J := 0 to Length(Pairs) - 1 do
      begin
        K := Floor(J / 2);

        if (max_flag and (
          (Player[Pairs[J].Player1].played<max_played) and
          (Player[Pairs[J].Player2].played<max_played))) and
          not pairs_season[K] then
        begin
          SetLength(temp_pairs, Length(temp_pairs)+1);
          temp_pairs[Length(temp_pairs)-1]:=Pairs[J];
        end;
      end;
    end;

    //second fallback
    if Length(temp_pairs)=0 then
    begin
      for J := 0 to Length(Pairs) - 1 do
      begin
        K := Floor(J / 2);
        if (not HasPlayed(Pairs[J], arr, I+1, NumPlayers)) and
          (Pairs[J].played<season) and not pairs_season[K] then
        begin
          SetLength(temp_pairs, Length(temp_pairs)+1);
          temp_pairs[Length(temp_pairs)-1]:=Pairs[J];
        end;
      end;
    end;

    //last fallback
    if Length(temp_pairs)=0 then
    begin
      for J := 0 to Length(Pairs) - 1 do
      begin
        K := Floor(J / 2);
        if not pairs_season[K] then
        begin
          SetLength(temp_pairs, Length(temp_pairs)+1);
          temp_pairs[Length(temp_pairs)-1]:=Pairs[J];
        end;
      end;
    end;

    num:=Random(Length(temp_pairs));

    inc(Pairs[temp_pairs[num].origin].played);
    J := Floor(temp_pairs[num].origin / 2);
    pairs_season[J] := true;
    arr[I].x:=temp_pairs[num].Player1;
    arr[I].y:=temp_pairs[num].Player2;

    inc(Player[arr[I].x].played);
    inc(Player[arr[I].y].played);

    K := 0;
    for J := 0 to Length(pairs_season) - 1 do
    begin
      if pairs_season[J] then
        Inc(K);
    end;

    if K = Length(pairs_season) then
    begin
      for J := 0 to Length(pairs_season) - 1 do
      begin
        pairs_season[J] := false;
      end;
    end;
  end;

  //debug
  SetLength(rn, NumRounds+1);
  SetLength(rn[0].rounds, NumPlayers);
  for I := 1 to NumRounds do
  begin
    SetLength(rn[I].rounds, NumPlayers);
    for J := 0 to NumPlayers - 1 do
    begin
      if (arr[I-1].x = J) or (arr[I-1].y = J) then
        rn[I].rounds[J] := rn[I-1].rounds[J] + 1
      else
        rn[I].rounds[J] := rn[I-1].rounds[J];
    end;
  end;

  Players.NumPlayer := NumPlayers;

  //Set player attributes
  for I := 0 to NumPlayers-1 do
  begin
    Players.Playerinfo[I].Points := 0;
    Players.Playerinfo[I].ScoreP:= 0;
    Players.Playerinfo[I].ScoreN:= 0;
    Players.Playerinfo[I].Wins:= 0;
    Players.Playerinfo[I].Defeats:= 0;
    Players.Playerinfo[I].Draws:= 0;
    Players.Playerinfo[I].Joker:= 0;
  end;

  //Set rounds
  if (Length(Plugins) >= 1) then
  begin
    //set DuelRatioFactor
    DuelRatioFactor := Round((Ini.DuelRatio*Length(Plugins)-10)/(10-Ini.DuelRatio));

    for I := 0 to Length(Plugins) - 1 do
    begin
      if (Plugins[I].ID = ID_DUELL) and
        not Plugins[I].Medley and not Plugins[I].MedleySurprise then
      begin
        for K := 1 to DuelRatioFactor do
        begin
          len := Length(Plugins);
          SetLength(Plugins, len+1);
          Plugins[len] := Plugins[I];
        end;
      end;
    end;

    for I := 0 to NumRounds - 1 do
    begin
      SetLength (Rounds, I+1);
      PartySessionM2.Rounds[I].Plugin := GetRandomPlugin(PartySessionM2.Rounds[I].Medley,
        PartySessionM2.Rounds[I].MedleySurprise,
        PartySessionM2.Rounds[I].PluginNr);

      PartySessionM2.Rounds[I].Player1:=arr[I].x;
      PartySessionM2.Rounds[I].Player2:=arr[I].y;
      PartySessionM2.Rounds[I].ScoreP:=0;
      PartySessionM2.Rounds[I].ScoreN:=0;
    end;
  end
  else
    SetLength (Rounds, 0);

  //Reset SongsPlayed-Array
  SetLength(PartySessionM2.SongsPlayed, 0);
  SetLength(ScreenSong.MedleyPlayed, 0);
end;

//----------
// Returns a number of a random plugin (New Version)
//----------
function TPartySessionM2.GetRandomPlugin(var medley: boolean; var surprise: boolean; var nr: integer): byte;
Type
  TOrder = record
    ID:     integer;
    medley: boolean;
    surprise: boolean;
    Nr:     integer;
  end;

var
  I, K:         Integer;
  plugin_order: array of TOrder;
  min, len:     integer;

begin
  //search for min played
  min := high(min);
  for I := 0 to Length(Plugins) - 1 do
  begin
    if Plugins[I].TimesPlayed < min then
      min := Plugins[I].TimesPlayed;
  end;

  //fill plugin list
  SetLength(plugin_order, 0);
  for I := 0 to Length(Plugins) - 1 do
  begin
    if (Plugins[I].TimesPlayed = min) then
    begin
      len := Length(plugin_order);
      SetLength(plugin_order, len+1);
      plugin_order[len].ID := Plugins[I].ID;
      plugin_order[len].medley := Plugins[I].Medley;
      plugin_order[len].surprise := Plugins[I].MedleySurprise;
      plugin_order[len].Nr := I;
    end;
  end;

  K := random(Length(plugin_order));
  Inc(Plugins[plugin_order[K].Nr].TimesPlayed);

  medley := plugin_order[K].medley;
  surprise := plugin_order[K].surprise;
  nr := plugin_order[K].Nr;
  Result := plugin_order[K].ID;
end;

{**
 * Prepares ScreenSingModiM2 for next round and loads plugin
 *}
procedure TPartySessionM2.StartRound;
begin
  if ((CurRound < Length(Rounds)) or (CurRound = high(CurRound))) then
  begin
    // Increase Current Round but not beyond its limit
    // CurRound is set to 255 to begin with!
    // Ugly solution if you ask me.
    if CurRound < Length(Rounds) then
      Inc(CurRound)
    else
      CurRound := 0;
    BuildOrder;

    if CurRound< Length(Rounds) then
    begin
      Rounds[CurRound].Winner := 255;
      DllMan.LoadPlugin(Rounds[CurRound].Plugin);

      //Select Players
      Teams.NumTeams:=2;

      Teams.Teaminfo[0].Name := 'T1';
      Teams.Teaminfo[0].Score:=0;
      Teams.Teaminfo[0].Joker:=5;
      Teams.Teaminfo[0].CurPlayer:=0;
      Teams.Teaminfo[0].NumPlayers:=1;
      Teams.Teaminfo[0].Playerinfo[0].Name:=Players.Playerinfo[Rounds[CurRound].Player1].Name;
      Teams.Teaminfo[0].Playerinfo[0].TimesPlayed:=0;

      Teams.Teaminfo[1].Name := 'T2';
      Teams.Teaminfo[1].Score:=0;
      Teams.Teaminfo[1].Joker:=0;
      Teams.Teaminfo[1].CurPlayer:=0;
      Teams.Teaminfo[1].NumPlayers:=1;
      Teams.Teaminfo[1].Playerinfo[0].Name:=Players.Playerinfo[Rounds[CurRound].Player2].Name;
      Teams.Teaminfo[1].Playerinfo[0].TimesPlayed:=0;

      if HandicapMode then
        Handicap := DataBase.GetHandicap(Players.Playerinfo[Rounds[CurRound].Player1].Name,
          Players.Playerinfo[Rounds[CurRound].Player2].Name)
      else
      begin
        Handicap.P1m := 1;
        Handicap.P2m := 1;
      end;

      //Set ScreenSingModi Variables
      ScreenSingModi.TeamInfo := Teams;
    end;
  end;

end;

//----------
//EndRound - Get Winner from ScreenSingModi and Save Data to RoundArray
//----------
procedure TPartySessionM2.EndRound;
begin
  //Copy Winner
  if Rounds[CurRound].Medley then
  begin
    if (PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[0].ScoreTotalI>
      PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[1].ScoreTotalI) then
      Rounds[CurRound].Winner := 1
    else if (PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[0].ScoreTotalI<
      PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[1].ScoreTotalI) then
      Rounds[CurRound].Winner := 2
    else
      Rounds[CurRound].Winner := 0;

    ScreenSingModi.PlayerInfo.Playerinfo[0].Score := PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[0].ScoreTotalI;
    ScreenSingModi.PlayerInfo.Playerinfo[1].Score := PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[1].ScoreTotalI;

  end else
    Rounds[CurRound].Winner := ScreenSingModi.Winner;

  //Set Scores
  GenScores;

  //Update Player Stats

end;

//----------
//GenScores - increase scores for current round
//----------
procedure TPartySessionM2.GenScores;
begin
  //Filter "Hau den Lukas"
  if (DLLMan.Plugins[Rounds[CurRound].Plugin].Name='PLUGIN_HAUDENLUKAS_NAME') then
  begin
    ScreenSingModi.PlayerInfo.Playerinfo[0].Score := ScreenSingModi.PlayerInfo.Playerinfo[0].Score*1000;
    ScreenSingModi.PlayerInfo.Playerinfo[1].Score := ScreenSingModi.PlayerInfo.Playerinfo[1].Score*1000;
  end;


  //Copy Scores to Rounds
  Rounds[CurRound].ScoreP:=round(ScreenSingModi.PlayerInfo.Playerinfo[0].Score*Handicap.P1m);
  Rounds[CurRound].ScoreN:=round(ScreenSingModi.PlayerInfo.Playerinfo[1].Score*Handicap.P2m);

  //Update PlayerStats
  Players.Playerinfo[Rounds[CurRound].Player1].ScoreP := Rounds[CurRound].ScoreP +
    Players.Playerinfo[Rounds[CurRound].Player1].ScoreP;
  Players.Playerinfo[Rounds[CurRound].Player1].ScoreN := Rounds[CurRound].ScoreN +
    Players.Playerinfo[Rounds[CurRound].Player1].ScoreN;

  Players.Playerinfo[Rounds[CurRound].Player2].ScoreP := Rounds[CurRound].ScoreN +
    Players.Playerinfo[Rounds[CurRound].Player2].ScoreP;
  Players.Playerinfo[Rounds[CurRound].Player2].ScoreN := Rounds[CurRound].ScoreP +
    Players.Playerinfo[Rounds[CurRound].Player2].ScoreN;

  inc(Players.Playerinfo[Rounds[CurRound].Player1].NumPlayed);
  inc(Players.Playerinfo[Rounds[CurRound].Player2].NumPlayed);

  if (HandicapMode and (Rounds[CurRound].ScoreP>Rounds[CurRound].ScoreN)) or
    (not HandicapMode and (Rounds[CurRound].Winner = 1)) then //Player1 is the winner
  begin
    inc(Players.Playerinfo[Rounds[CurRound].Player1].Wins);
    inc(Players.Playerinfo[Rounds[CurRound].Player2].Defeats);

    inc(Players.Playerinfo[Rounds[CurRound].Player1].Points);
    inc(Players.Playerinfo[Rounds[CurRound].Player1].Points);
  end else if (HandicapMode and (Rounds[CurRound].ScoreP<Rounds[CurRound].ScoreN)) or
    (not HandicapMode and (Rounds[CurRound].Winner = 2))  then  //Player2 is the winner
  begin
    inc(Players.Playerinfo[Rounds[CurRound].Player2].Wins);
    inc(Players.Playerinfo[Rounds[CurRound].Player1].Defeats);

    inc(Players.Playerinfo[Rounds[CurRound].Player2].Points);
    inc(Players.Playerinfo[Rounds[CurRound].Player2].Points);
  end else //Draw
  begin
    inc(Players.Playerinfo[Rounds[CurRound].Player1].Draws);
    inc(Players.Playerinfo[Rounds[CurRound].Player2].Draws);

    inc(Players.Playerinfo[Rounds[CurRound].Player1].Points);
    inc(Players.Playerinfo[Rounds[CurRound].Player2].Points);
  end;
end;

end.