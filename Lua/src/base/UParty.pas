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

unit UParty;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I switches.inc}

uses
  ULua;

type
  { array holds ids of modes or Party_Round_Random
    its length defines the number of rounds
    it is used as argument for TPartyGame.StartParty }
  ARounds = array of integer;

  { record used by TPartyGame to store round specific data }
  TParty_Round = record
    Mode:   Integer;
    Winner: Integer;
  end;

  { element of APartyTeamRanking returned by TPartyGame.GetTeamRanking }
  TParty_TeamRanking = record
    Team: Integer; //< id of team
    Rank: Integer; //< 1 to Length(Teams) e.g. 1 is for placed first
  end;
  AParty_TeamRanking = Array of TParty_TeamRanking; //< returned by TPartyGame.GetTeamRanking

  TParty_ModeInfo = record
    Name: String; // name of this mode
    Parent: Integer;   // Id of owning plugin

    CanNonParty: Boolean; //< is playable when not in party mode
    CanParty: Boolean;    //< is playable in party mode

    // one bit in the following settings stands for
    // a player or team count
    // PlayerCount = 2 or 4 indicates that the mode is playable with 2 and 3 players per team
    // TeamCount = 1 or 2 or 4 or 8 or 16 or 32 indicates that the mode is playable with 1 to 6 teams
    PlayerCount: Integer;   //< playable with one, two, three etc. players per team
    TeamCount: Integer;     //< playable with one, two, three etc. different teams


    Functions: record // lua functions that will be called at specific events
      BeforeSongSelect: String; // default actions are executed if functions = nil
      AfterSongSelect: String;

      BeforeSing: String;
      OnSing: String;
      AfterSing: String;
    end;
  end;

  { used by TPartyGame to store player specific data }
  TParty_PlayerInfo = record
    Name: String;         //< Playername
    TimesPlayed: Integer; //< How often this Player has Sung
  end;

  { used by TPartyGame to store team specific data }
  TParty_TeamInfo = record
    Name:  String;        //< name of the Team
    Score: Word;          //< current score
    JokersLeft: Integer;  //< jokers this team has left

    NextPlayer: Integer;  //Id of the player that plays the next (the current) song

    Players: array of TParty_PlayerInfo;
  end;

  TPartyGame = class
  private
    bPartyGame: boolean; //< are we playing party or standard mode
    CurRound: Integer;   //< indicates which of the elements of Rounds is played next (at the moment)

    bPartyStarted: Boolean;

    TimesPlayed: array of Integer; //< times every mode was played in current party game (for random mode calculation)

    function IsWinner(Player, Winner: integer): boolean;
    procedure GenScores;
    function GetRandomMode: integer;
    function GetRandomPlayer(Team: integer): integer;

    function CallLua(Parent: Integer; Func: String):Boolean;
  public
    //Teams: TTeamInfo;
    Rounds: array of TParty_Round;    //< holds info which modes are played in this party game (if started)
    Teams: array of TParty_TeamInfo;  //< holds info of teams playing in current round (private for easy manipulation of lua functions)

    Modes: array of TParty_ModeInfo;  //< holds info of registred party modes

    property CurrentRound: Integer read CurRound;

    constructor Create;

    { set the attributes of Info to default values }
    procedure DefaultModeInfo(var Info: TParty_ModeInfo);

    { registers a new mode, returns true on success
      (mode name does not already exist) }
    function RegisterMode(Info: TParty_ModeInfo): Boolean;

    { returns true if modes are available for
      players and teams that are currently set
      up. if there are no teams set up it returns
      if there are any party modes available }
    function ModesAvailable: Boolean;

    { clears all party specific data previously stored }
    procedure Clear;

    { adds a team to the team array, returning its id
      can only be called when game is not already started }
    function AddTeam(Name: String): Integer;

    { adds a player to the player array, returning its id
      can only be called when game is not already started }
    function AddPlayer(Team: Integer; Name: String): Integer;

    { starts a new PartyGame, returns true on success
      before a call of this function teams and players
      has to be added by AddTeam and AddPlayer }

    function StartGame(Rounds: ARounds): Boolean;

    { increases round counter by 1 and clears all round specific information;
      returns the number of the current round or -1 if last round has already
      been played }
    function NextRound: integer;

    { true if in a Party Game (not in standard mode) }
    property PartyGame: Boolean read BPartyGame;


    { returns true if last round was already played }
    function GameFinished: Boolean;

    { call plugins defined function and/or default procedure
      only default procedure is called when no function is defined by plugin
      if plugins function returns true then default is called after plugins
      function was executed}
    procedure CallBeforeSongSelect;
    procedure CallAfterSongSelect;
    procedure CallBeforeSing;
    procedure CallOnSing;
    procedure CallAfterSing;

    { returns an array[1..6] of TParty_TeamRanking.
      the index stands for the placing,
      team is the team number (in the team array)
      rank is correct rank if some teams have the
      same score. 
      }
    function GetTeamRanking: AParty_TeamRanking;

    { returns a string like "Team 1 (and Team 2) win" }
    function GetWinnerString(Round: Integer): String;

    destructor  Destroy;
  end;

const
  { minimal amount of teams for party mode }
  Party_Teams_Min = 2;

  { maximal amount of teams for party mode }
  Party_Teams_Max = 3;

  { minimal amount of players for party mode }
  Party_Players_Min = 1;

  { maximal amount of players for party mode }
  Party_Players_Max = 4;

  { amount of jokers each team gets at the beginning of the game }
  Party_Count_Jokers = 5;

  { to indicate that element (mode) should set randomly in ARounds array }
  Party_Round_Random = -1;
  
  StandardModus = 0; //Modus Id that will be played in non-party mode

var
  Party: TPartyGame;

implementation

uses
  UGraphic,
  ULanguage,
  ULog,
  ULuaCore,
  UDisplay,
  USong,
  SysUtils;

//-------------
// Just the constructor
//-------------
constructor TPartyGame.Create;
begin
  inherited;

  Clear;
end;

destructor TPartyGame.Destroy;
begin
  inherited;
end;

{ clears all party specific data previously stored }
procedure TPartyGame.Clear;
  var
    I: Integer;
begin
  bPartyGame := false; // no party game
  CurRound := low(integer);

  bPartyStarted := false; //game not startet

  SetLength(Teams, 0); //remove team info
  SetLength(Rounds, 0); //remove round info

  // clear times played
  for I := 0 to High(TimesPlayed) do
    TimesPlayed[I] := 0;
end;

{ private: some intelligent randomnes for plugins }
function TPartyGame.GetRandomMode: integer;
var
  LowestTP: integer;
  NumPwithLTP: integer;
  I: integer;
  R: integer;
begin
  Result := 0; //If there are no matching modes, play first modus
  LowestTP := high(Integer);
  NumPwithLTP := 0;

  // search for the plugins less played yet
  for I := 0 to high(Modes) do
  begin
    if (TimesPlayed[I] < lowestTP) then
    begin
      lowestTP := TimesPlayed[I];
      NumPwithLTP := 1;
    end
    else if (TimesPlayed[I] = lowestTP) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  // create random number
  R := Random(NumPwithLTP);

  // select the random mode from the modes with less timesplayed
  for I := 0 to high(Modes) do
  begin
    if (TimesPlayed[I] = lowestTP) { and (((Modis[I].Info.LoadingSettings and MLS_TeamOnly) <> 0) = TeamMode) }then
    begin
      //Plugin found
      if (R = 0) then
      begin
        Result := I;
        Inc(TimesPlayed[I]);
        Break;
      end;

      Dec(R);
    end;
  end;
end;

{ private: GetRandomPlayer - returns a random player
                             that does not play to often ;) }
function TPartyGame.GetRandomPlayer(Team: integer): integer;
var
  I, R: integer;
  lowestTP: Integer;
  NumPwithLTP: Integer;
begin
  LowestTP := high(Integer);
  NumPwithLTP := 0;
  Result := 0;

  // search for players that have less played yet
  for I := 0 to High(Teams[Team].Players) do
  begin
    if (Teams[Team].Players[I].TimesPlayed < lowestTP) then
    begin
      lowestTP := Teams[Team].Players[I].TimesPlayed;
      NumPwithLTP := 1;
    end
    else if (Teams[Team].Players[I].TimesPlayed = lowestTP) then
    begin
      Inc(NumPwithLTP);
    end;
  end;

  // create random number
  R := Random(NumPwithLTP);

  // search for selected random player
  for I := 0 to High(Teams[Team].Players) do
  begin
    if Teams[Team].Players[I].TimesPlayed = lowestTP then
    begin
      if (R = 0) then
      begin // found selected player
        Result := I;
        Break;
      end;

      Dec(R);
    end;
  end;
end;

{ private: returns true if the players bit is set in the winner int }
function TPartyGame.IsWinner(Player, Winner: Integer): boolean;
var
  Bit: byte;
begin
  if (Player < 31) then
  begin
    Bit := 1 shl Player;

    Result := ((Winner and Bit) = Bit);
  end
  else
    Result := False;
end;

//----------
//GenScores - inc scores for cur. round
//----------
procedure TPartyGame.GenScores;
var
  I: byte;
begin
  for I := 0 to High(Teams) do
  begin
    if isWinner(I, Rounds[CurRound].Winner) then
      Inc(Teams[I].Score);
  end;
end;

{ set the attributes of Info to default values }
procedure TPartyGame.DefaultModeInfo(var Info: TParty_ModeInfo);    
begin
  Info.Name := 'undefined';
  Info.Parent := -1; //< not loaded by plugin (e.g. Duell)
  Info.CanNonParty := false;
  Info.CanParty := false;
  Info.PlayerCount := High(Integer); //< no restrictions either on player count
  Info.TeamCount := High(Integer);   //< nor on team count
  Info.Functions.BeforeSongSelect := ''; //< use default functions
  Info.Functions.AfterSongSelect  := '';
  Info.Functions.BeforeSing       := '';
  Info.Functions.OnSing           := '';
  Info.Functions.AfterSing        := '';
end;

{ registers a new mode, returns true on success
  (mode name does not already exist) }
function TPartyGame.RegisterMode(Info: TParty_ModeInfo): Boolean;
  var
    Len: integer;
    LowerName: String;
    I: integer;
begin
  Result := false;

  if (Info.Name <> 'undefined') then
  begin
    // search for a plugin w/ same name
    LowerName := lowercase(Info.Name); // case sensitive search
    for I := 0 to high(Modes) do
      if (LowerName = lowercase(Modes[I].Name)) then
        exit; //< no success (name already exist)

    // add new mode to array and append and clear a new TimesPlayed element
    Len := Length(Modes);
    SetLength(Modes, Len + 1);
    SetLength(TimesPlayed, Len + 1);

    Modes[Len] := Info;
    TimesPlayed[Len] := 0;

    Result := True;
  end;
end;

{ returns true if modes are available for
  players and teams that are currently set
  up. if there are no teams set up it returns
  if there are any party modes available }
function TPartyGame.ModesAvailable: Boolean;
  var
    I, J: Integer;
    CountTeams: Integer;
begin
  CountTeams := Length(Teams);
  if CountTeams = 0 then
  begin
    Result := (Length(Modes) > 0);
  end
  else
  begin
    for I := 0 to High(Modes) do
      if (Modes[I].TeamCount and (1 shl (CountTeams - 1)) <> 0) then
      begin
        Result := True;

        for J := 0 to High(Teams) do
          Result := Result and (Modes[I].PlayerCount and (1 shl (Length(Teams[J].Players) - 1)) <> 0);

        if Result then
          Exit;
      end;
  end;
end;

{ adds a team to the team array, returning its id
  can only be called when game is not already started }
function TPartyGame.AddTeam(Name: String): Integer;
begin
  Result := -1;
  if (not bPartyStarted) and (Length(Name) > 0) and (Length(Teams) < Party_Teams_Max) then
  begin
    Result := Length(Teams);
    SetLength(Teams, Result + 1);

    Teams[Result].Name := Name;
    Teams[Result].Score := 0;
    Teams[Result].JokersLeft := Party_Count_Jokers;
    Teams[Result].NextPlayer := -1;
  end;
end;

{ adds a player to the player array, returning its id
  can only be called when game is not already started }
function TPartyGame.AddPlayer(Team: Integer; Name: String): Integer;
begin
  Result := -1;

  if (not bPartyStarted) and (Team >= 0) and (Team <= High(Teams)) and (Length(Teams[Team].Players) < Party_Players_Max) and (Length(Name) > 0) then
  begin
    // append element to players array
    Result := Length(Teams[Team].Players);
    SetLength(Teams[Team].Players, Result + 1);

    // fill w/ data
    Teams[Team].Players[Result].Name := Name;
    Teams[Team].Players[Result].TimesPlayed := 0;
  end;
end;

{ starts a new PartyGame, returns true on success
  before a call of this function teams and players
  has to be added by AddTeam and AddPlayer }
function TPartyGame.StartGame(Rounds: ARounds): Boolean;
  var
    I: integer;
begin
  Result := false;

  if (not bPartyStarted) and (Length(Rounds) > 0) and (Length(Teams) >= Party_Teams_Min) then
  begin
    // check teams for minimal player count
    for I := 0 to High(Teams) do
      if (Length(Teams[I].Players) < Party_Players_Min) then
        exit;

    // create rounds array
    SetLength(Self.Rounds, Length(Rounds));

    for I := 0 to High(Rounds) do
    begin
      // copy round or select a random round
      if (Rounds[I] <> Party_Round_Random) and (Rounds[I] >= 0) and (Rounds[I] <= High(Modes)) then
        Self.Rounds[I].Mode := Rounds[I]
      else
        Self.Rounds[I].Mode := GetRandomMode;

      Self.Rounds[I].Winner := -1; // -1 indicates not played yet
    end;

    // get the party started!11
    bPartyStarted := true;
    bPartyGame := true;
    CurRound := low(integer); //< set not to -1 to indicate that party game is not finished

    // first round
    NextRound;

    Result := True;
  end;
end;

{ increases round counter by 1 and clears all round specific information;
  returns the number of the current round or -1 if last round has already
  been played }
function TPartyGame.NextRound: integer;
  var I: Integer;
begin
  // some lines concerning the previous round
  if (CurRound >= 0) then
  begin
    GenScores;
  end;

  // increase round counter
  Inc(CurRound);
  if (CurRound < -1) then // we start first round
    CurRound := 0;

  if (CurRound > High(Rounds)) then
    CurRound := -1; //< last round played

  Result := CurRound;

  // some lines concerning the next round
  if (CurRound >= 0) then
  begin
    // select player
    for I := 0 to High(Teams) do
      Teams[I].NextPlayer := GetRandomPlayer(I);
      
    Rounds[CurRound].Winner := 0; //< reset winner
  end;
end;

{ returns true if last round was already played }
function TPartyGame.GameFinished: Boolean;
begin
  Result := (bPartyStarted and (CurRound = -1));
end;

{ private: calls the specified function Func from lua plugin Parent
           if both exist.
           return true if default function should be called
           (function or plugin does not exist, or function returns
           true) }
function TPartyGame.CallLua(Parent: Integer; Func: String):Boolean;
  var
    P: TLuaPlugin;
begin
  // call default function by default
  Result := true;

  // check for core plugin and empty function name
  if (Parent >= 0) and (Length(Func) > 0) then
  begin
    // get plugin that registred the mode
    P := LuaCore.GetPluginById(Parent);

    if (P <> nil) then
    begin
      if (P.CallFunctionByName(Func, 0, 1)) then
        // check result
        Result := (lua_toboolean(P.LuaState, 1));
    end;
  end;
end;

{ call plugins defined function and/or default procedure
  only default procedure is called when no function is defined by plugin
  if plugins function returns true then default is called after plugins
  function was executed}
procedure TPartyGame.CallBeforeSongSelect;
begin
  if (CurRound >= 0) then
  begin
    // we set screen song to party mode
    // plugin should not have to do this if it
    // don't want default procedure to be executed
    ScreenSong.Mode := smPartyMode;

    with Modes[Rounds[CurRound].Mode] do
      if (CallLua(Parent, Functions.BeforeSongSelect)) then
      begin // execute default function:
        // display song select screen
        Display.FadeTo(@ScreenSong);
      end;
  end;
end;

procedure TPartyGame.CallAfterSongSelect;   
begin
  if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      if (CallLua(Parent, Functions.AfterSongSelect)) then
      begin // execute default function:

        // display sing screen
        ScreenSong.StartSong;
      end;
  end;
end;

procedure TPartyGame.CallBeforeSing; 
begin
  if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      if (CallLua(Parent, Functions.BeforeSing)) then
      begin // execute default function:

        //nothing atm
        { to-do : compartmentalize TSingScreen.OnShow into
                  functions for init of a specific part of
                  sing screen.
                  these functions should be called here before
                  sing screen is shown, or it should be called
                  by plugin if it wants to define a custom
                  singscreen start up. } 
      end;
  end;
end;

procedure TPartyGame.CallOnSing;
begin
  if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      if (CallLua(Parent, Functions.OnSing)) then
      begin // execute default function:

        //nothing atm
      end;
  end;
end;

procedure TPartyGame.CallAfterSing;
begin
  if (CurRound >= 0) then
  begin
    with Modes[Rounds[CurRound].Mode] do
      if (CallLua(Parent, Functions.AfterSing)) then
      begin // execute default function:

        // display party score screen
        Display.FadeTo(@ScreenPartyScore);
      end;
  end;
end;

{ returns an array[1..6] of integer. the index stands for the placing,
  value is the team number (in the team array) }
function TPartyGame.GetTeamRanking: AParty_TeamRanking;
  var
    I, J: Integer;
    Temp: TParty_TeamRanking;
    Rank: Integer;
begin
  SetLength(Result, Length(Teams));

  // fill ranking array
  for I := 0 to High(Result) do
  begin
    Result[I].Team := I;
    Result[I].Rank := 0;
  end;

  // bubble sort by score
  J := High(Result);
  repeat
    for I := 0 to J - 1 do
      if (Teams[Result[I].Team].Score > Teams[Result[I+1].Team].Score) then
      begin
        Temp := Result[I];
        Result[I] := Result[I+1];
        Result[I+1] := Temp;
      end;
    Dec(J);
  until J <= 0;

  // set rank field
  Rank := 1; //first rank has id 1
  for I := 0 to High(Result) do
  begin
    Result[I].Rank := Rank;

    if (I < High(Result)) and (Teams[Result[I].Team].Score <> Teams[Result[I+1].Team].Score) then
      Inc(Rank); // next rank if next team has different score 
  end; 
end;

{ returns a string like "Team 1 (and Team 2) win" }
function TPartyGame.GetWinnerString(Round: Integer): String;
var
  Winners: array of String;
  I: integer;
begin
  Result := '';

  if (Round >= 0) and (Round <= High(Rounds)) then
  begin
    if (Rounds[Round].Winner <> 0) then
    begin
      if (Rounds[Round].Winner = -1) then
      begin
        Result := Language.Translate('PARTY_NOTPLAYEDYET');
      end
      else
      begin
        SetLength(Winners, 0);
        for I := 0 to High(Teams) do
        begin
          if isWinner(I, Rounds[Round].Winner) then
          begin
            SetLength(Winners, Length(Winners) + 1);
            Winners[high(Winners)] := Teams[I].Name;
          end;
        end;
        Result := Language.Implode(Winners);
      end;
    end;
  end;


  if (Length(Result) = 0) then
    Result := Language.Translate('PARTY_NOBODY');
end;

{ //----------
// CallModiDeInit - calls DeInitProc and ends the round
//----------
function TPartySession.CallModiDeInit(wParam: TwParam; lParam: TlParam): integer;
var
  I: integer;
  MaxScore: word;
begin
  if (bPartyMode) then
  begin
    //Get Winner Byte!
    if (@Modis[Rounds[CurRound].Modi].Info.ModiDeInit <> nil) then //get winners from plugin
      Rounds[CurRound].Winner := Modis[Rounds[CurRound].Modi].Info.ModiDeInit(Modis[Rounds[CurRound].Modi].Info.ID)
    else
    begin //Create winners by score :/
      Rounds[CurRound].Winner := 0;
      MaxScore := 0;
      for I := 0 to Teams.NumTeams-1 do
      begin
        // to-do : recode percentage stuff
        //PlayerInfo.Playerinfo[I].Percentage := PlayerInfo.Playerinfo[I].Score div 9999;
        if (Player[I].ScoreTotalInt > MaxScore) then
        begin
          MaxScore := Player[I].ScoreTotalInt;
          Rounds[CurRound].Winner := 1 shl I;
        end
        else if (Player[I].ScoreTotalInt = MaxScore) and (Player[I].ScoreTotalInt <> 0) then
        begin
          Rounds[CurRound].Winner := Rounds[CurRound].Winner or (1 shl I);
        end;
      end;


      //When nobody has points -> everybody looses
      if (MaxScore = 0) then
        Rounds[CurRound].Winner := 0;

    end;

    //Generate the scores
    GenScores;

    //Inc players TimesPlayed
    if ((Modis[Rounds[CurRound-1].Modi].Info.LoadingSettings and MLS_IncTP) = MLS_IncTP) then
    begin
      for I := 0 to Teams.NumTeams-1 do
        Inc(Teams.TeamInfo[I].Playerinfo[Teams.TeamInfo[I].CurPlayer].TimesPlayed);
    end;
  end
  else if (@Modis[Rounds[CurRound].Modi].Info.ModiDeInit <> nil) then
    Modis[Rounds[CurRound].Modi].Info.ModiDeInit(Modis[Rounds[CurRound].Modi].Info.ID);

  // FIXME: return a valid result
  Result := 0;
end;

//----------
// GetTeamOrder - returns team order. Structure: Bits 1..3: Team at place1; Bits 4..6: Team at place2 ...
//----------
function  TPartySession.GetTeamOrder(wParam: TwParam; lParam: TlParam): integer;
var
  I, J: integer;
  ATeams: array [0..5] of TeamOrderEntry;
  TempTeam: TeamOrderEntry;
begin
  // to-do : PartyMode: Write this in another way, so that teams with the same score get the same place
  //Fill Team array
  for I := 0 to Teams.NumTeams-1 do
  begin
    ATeams[I].Teamnum := I;
    ATeams[I].Score := Teams.Teaminfo[I].Score;
  end;

  //Sort teams
  for J := 0 to Teams.NumTeams-1 do
    for I := 1 to Teams.NumTeams-1 do
      if ATeams[I].Score > ATeams[I-1].Score then
      begin
        TempTeam    := ATeams[I-1];
        ATeams[I-1] := ATeams[I];
        ATeams[I]   := TempTeam;
      end;

  //Copy to Result
  Result := 0;
  for I := 0 to Teams.NumTeams-1 do
    Result := Result or (ATeams[I].TeamNum Shl I*3);
end; }

end.
