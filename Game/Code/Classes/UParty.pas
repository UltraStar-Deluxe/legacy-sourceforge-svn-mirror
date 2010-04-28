unit UParty;

interface

uses ModiSDK, UIni;

type
  TRoundInfo = record
    Plugin:   Word;
    PluginNr: Integer;
    Medley:   boolean;
    MedleySurprise: boolean;
    Winner:   Byte;
  end;

  TeamOrderEntry = record
    Teamnum: Byte;
    Score: Byte;
  end;

  TeamOrderArray = Array[0..5] of Byte;

  TPartyPlugin = record
    ID:          byte;
    TimesPlayed: byte;
    Medley:      boolean;
    MedleySurprise: boolean;
    Selected:    boolean;
    Name:        string;
    Desc:        string;
  end;

  TParty_Session = class
  private
    function GetRandomPlayer(Team: Byte): Byte;
    function GetRandomPlugin(var medley: boolean; var surprise: boolean; var nr: integer): byte;
    function IsWinner(Player, Winner: Byte): boolean;
    procedure GenScores;
  public
    Plugins: array of TPartyPlugin;
    Teams: TTeamInfo;
    Rounds: array of TRoundInfo;
    CurRound: Byte;

    constructor Create;

    procedure StartNewParty(NumRounds: Byte);
    procedure StartRound;
    procedure EndRound;
    function  GetTeamOrder: TeamOrderArray;
    function  GetWinnerString(Round: Byte): String;
  end;

var
  PartySession: TParty_Session;
    Placings: Array [0..5] of Byte;

implementation

uses UDLLManager, UGraphic, UMain, ULanguage, ULog;

//----------
//Constructor -  Prepares the Class
//----------
constructor TParty_Session.Create;
begin
// - Nothing in here atm
end;

//----------
// Returns a number of a random plugin (New Version)
//----------
function TParty_Session.GetRandomPlugin(var medley: boolean; var surprise: boolean; var nr: integer): byte;
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

//----------
//StartNewParty - Clears the Class and Prepares for new Party
//----------
procedure TParty_Session.StartNewParty(NumRounds: Byte);
var
  //TeamMode: Boolean;
  //Len:  Integer;
  I, J:  Integer;
  NumMedleys: Integer;

begin
  //Set cur Round to Round 1
  CurRound := 255;

  //Set Rounds
  NumMedleys := 0;
  If (Length(Plugins) >= 1) then
  begin
    SetLength (Rounds, NumRounds);
    For I := 0 to NumRounds-1 do
    begin
      PartySession.Rounds[I].Plugin := GetRandomPlugin(PartySession.Rounds[I].Medley,
        PartySession.Rounds[I].MedleySurprise,
        PartySession.Rounds[I].PluginNr);
      PartySession.Rounds[I].Winner := 255;

      if PartySession.Rounds[I].Medley then
        inc(NumMedleys);
    end;
  end
  else SetLength (Rounds, 0);

  PlayersPlay := Teams.NumTeams;

  //Get Teammode and Set Joker, also set TimesPlayed
  //TeamMode := True;
  For I := 0 to Teams.NumTeams-1 do
  begin
    {if Teams.Teaminfo[I].NumPlayers < 2 then
    begin
      TeamMode := False;
    end;}
    //Set Player Attributes
    For J := 0 to Teams.TeamInfo[I].NumPlayers-1 do
    begin
      Teams.TeamInfo[I].Playerinfo[J].TimesPlayed := 0;
    end;
    Teams.Teaminfo[I].Joker := Round(NumRounds*0.7)+NumMedleys;
    Teams.Teaminfo[I].Score := 0;
  end;

  //Reset Played-array
  SetLength(ScreenSong.MedleyPlayed, 0);
  SetLength(ScreenSong.PartyPlayed, 0);
end;

//----------
//GetRandomPlayer - Gives back a Random Player to Play next Round
//----------
function TParty_Session.GetRandomPlayer(Team: Byte): Byte;
var
  I, R: Integer;
  lowestTP: Byte;
  NumPwithLTP: Byte;
begin
    LowestTP := high(Byte);
    NumPwithLTP := 0;
    Result := 0;

    //Search for Players that have not often played yet
    For I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
    begin
      if (Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed < lowestTP) then
      begin
        lowestTP := Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed;
        NumPwithLTP := 1;
      end
      else if (Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed = lowestTP) then
      begin
        Inc(NumPwithLTP);
      end;
    end;

    //Create Random No
    R := Random(NumPwithLTP);

    //Search for Random Player
    For I := 0 to Teams.Teaminfo[Team].NumPlayers-1 do
    begin
      if Teams.Teaminfo[Team].Playerinfo[I].TimesPlayed = lowestTP then
      begin
        //Player Found
        if (R = 0) then
        begin
          Result := I;
          Break;
        end;
        
        Dec(R);
      end;
    end;
end;

//----------
//StartNextRound - Prepares ScreenSingModi for Next Round And Load Plugin
//----------
procedure TParty_Session.StartRound;
var
  I: Integer;
begin
  if ((CurRound < high(Rounds)) OR (CurRound = high(CurRound))) then
  begin
    //Increase Current Round
    Inc (CurRound);

    Rounds[CurRound].Winner := 255;
    DllMan.LoadPlugin(Rounds[CurRound].Plugin);

    //Select Players
    for I := 0 to Teams.NumTeams-1 do
      Teams.Teaminfo[I].CurPlayer := GetRandomPlayer(I);

    //Set ScreenSingModie Variables
    ScreenSingModi.TeamInfo := Teams;

    //Set 
  end;
end;

//----------
//IsWinner - Returns True if the Players Bit is set in the Winner Byte
//----------
function TParty_Session.IsWinner(Player, Winner: Byte): boolean;
var
  Bit: Byte;
begin
  Case Player of
    0: Bit := 1;
    1: Bit := 2;
    2: Bit := 4;
    3: Bit := 8;
    4: Bit := 16;
    5: Bit := 32;
  end;

  Result := ((Winner AND Bit) = Bit);
end;

//----------
//GenScores - Inc Scores for Cur. Round
//----------
procedure TParty_Session.GenScores;
var
  I: Byte;
begin
  for I := 0 to Teams.NumTeams-1 do
  begin
    if isWinner(I, Rounds[CurRound].Winner) then
      Inc(Teams.Teaminfo[I].Score);
  end;
end;

//----------
//GetWinnerString - Get String with WinnerTeam Name, when there is more than one Winner than Connect with and or ,
//----------
function  TParty_Session.GetWinnerString(Round: Byte): String;
var
  Winners: Array of String;
  I: Integer;
begin
  Result := Language.Translate('PARTY_NOBODY');
  
  if (Round > High(Rounds)) then
    exit;

  if (Rounds[Round].Winner = 0) then
  begin
    exit;
  end;

  if (Rounds[Round].Winner = 255) then
  begin
    Result := Language.Translate('PARTY_NOTPLAYEDYET');
    exit;
  end;

  SetLength(Winners, 0);
  for I := 0 to Teams.NumTeams-1 do
  begin
    if isWinner(I, Rounds[Round].Winner) then
    begin
      SetLength(Winners, Length(Winners) + 1);
      Winners[high(Winners)] := Teams.TeamInfo[I].Name;
    end;
  end;
  Result := Language.Implode(Winners);
end;

//----------
//EndRound - Get Winner from ScreenSingModi and Save Data to RoundArray
//----------
procedure TParty_Session.EndRound;
var
  I, J, MaxScore: Integer;
begin
  //Copy Winner
  if Rounds[CurRound].Medley then
  begin
    Rounds[CurRound].Winner := 0;
    MaxScore := 0;
    for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers-1 do
    begin
      ScreenSingModi.PlayerInfo.Playerinfo[I].Percentage :=
        ScreenSingModi.PlayerInfo.Playerinfo[I].Score div 9999;
      if (ScreenSingModi.PlayerInfo.Playerinfo[I].Score > MaxScore) then
      begin
        MaxScore := ScreenSingModi.PlayerInfo.Playerinfo[I].Score;
        Case I of
          0: Rounds[CurRound].Winner :=  1;
          1: Rounds[CurRound].Winner :=  2;
          2: Rounds[CurRound].Winner :=  4;
          3: Rounds[CurRound].Winner :=  8;
          4: Rounds[CurRound].Winner := 16;
          5: Rounds[CurRound].Winner := 32;
        end;
      end
      else if (ScreenSingModi.PlayerInfo.Playerinfo[I].Score = MaxScore) AND
        (ScreenSingModi.PlayerInfo.Playerinfo[I].Score <> 0) then
      begin
        Case I of
          0: Rounds[CurRound].Winner := Rounds[CurRound].Winner OR 1;
          1: Rounds[CurRound].Winner := Rounds[CurRound].Winner OR 2;
          2: Rounds[CurRound].Winner := Rounds[CurRound].Winner OR 4;
          3: Rounds[CurRound].Winner := Rounds[CurRound].Winner OR 8;
          4: Rounds[CurRound].Winner := Rounds[CurRound].Winner OR 16;
          5: Rounds[CurRound].Winner := Rounds[CurRound].Winner OR 32;
        end;
      end;
    end;

    //When nobody has Points -> Everybody loose
    if (MaxScore = 0) then
      Rounds[CurRound].Winner := 0;
  end else
    Rounds[CurRound].Winner := ScreenSingModi.Winner;

  //Set Scores

  if (Ini.NewPartyPoints = 1) and (Rounds[CurRound].Winner <> 0) then
  begin

    MaxScore := 0;
    for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers-1 do
      if (ScreenSingModi.PlayerInfo.Playerinfo[I].Score > MaxScore) then
        MaxScore := ScreenSingModi.PlayerInfo.Playerinfo[I].Score;


    if (MaxScore > 0) then
    begin
      //New Points
      //Get Placings
      for I := 0 to Teams.NumTeams-1 do
      begin
        Placings[I] := 0;
        for J := 0 to Teams.NumTeams-1 do
          If (ScreenSingModi.PlayerInfo.Playerinfo[J].Score > ScreenSingModi.PlayerInfo.Playerinfo[I].Score) then
            Inc(Placings[I]);
      end;

      for I := 0 to Teams.NumTeams-1 do
        Teams.Teaminfo[I].Score := Teams.Teaminfo[I].Score + ((Placings[I] - (Teams.NumTeams-1)) * -1);
    end
  end
  else
    //Old Points
    GenScores;




  //Increase TimesPlayed 4 all Players
  For I := 0 to Teams.NumTeams-1 do
    Inc(Teams.Teaminfo[I].Playerinfo[Teams.Teaminfo[I].CurPlayer].TimesPlayed);

end;

//----------
//GetTeamOrder - Gives back the Placing of eacb Team [First Position of Array is Teamnum of first placed Team, ...]
//----------
function TParty_Session.GetTeamOrder: TeamOrderArray;
var
  I, J: Integer;
  ATeams: array [0..5] of TeamOrderEntry;
  TempTeam: TeamOrderEntry;
begin
  //Fill Team Array
  For I := 0 to Teams.NumTeams-1 do
  begin
    ATeams[I].Teamnum := I;
    ATeams[I].Score := Teams.Teaminfo[I].Score;
  end;

  //Sort Teams
  for J := 0 to Teams.NumTeams-1 do
    for I := 1 to Teams.NumTeams-1 do
      if ATeams[I].Score > ATeams[I-1].Score then
      begin
        TempTeam    := ATeams[I-1];
        ATeams[I-1] := ATeams[I];
        ATeams[I]   := TempTeam;
      end;

  //Copy to Result
  For I := 0 to Teams.NumTeams-1 do
    Result[I] := ATeams[I].TeamNum;
end;

end.