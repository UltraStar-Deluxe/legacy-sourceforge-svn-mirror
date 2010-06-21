unit UScreenPartyNewRound;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPartyNewRound = class(TMenu)
    private
      ScreenRound:  Integer;
      procedure Update;
    public
      //Texts:
      TextRound: array [1..7] of Cardinal;
      TextWinner: array [1..7] of Cardinal;

      TextNextRound: Cardinal;
      TextNextRoundNo: Cardinal;
      TextNextPlayer1: Cardinal;
      TextNextPlayer2: Cardinal;
      TextNextPlayer3: Cardinal;

      //Statics
      StaticRound: array [1..7] of Cardinal;

      //Scores
      TextScoreTeam1: Cardinal;
      TextScoreTeam2: Cardinal;
      TextScoreTeam3: Cardinal;
      TextNameTeam1: Cardinal;
      TextNameTeam2: Cardinal;
      TextNameTeam3: Cardinal;

      TextTeam1Players: Cardinal;
      TextTeam2Players: Cardinal;
      TextTeam3Players: Cardinal;

      StaticTeam1: Cardinal;
      StaticTeam2: Cardinal;
      StaticTeam3: Cardinal;
      StaticNextPlayer1: Cardinal;
      StaticNextPlayer2: Cardinal;
      StaticNextPlayer3: Cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_014';   //for help system
  
implementation

uses UGraphic, UMain, UIni, UTexture, UParty, UDLLManager, ULanguage, UHelp, ULog;

function TScreenPartyNewRound.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Music.PlayBack;
          CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
        end;

      SDLK_RETURN:
        begin
          Music.PlayStart;
          if DLLMan.Selected.LoadSong then
          begin
            //Select PartyMode ScreenSong
            ScreenSong.Mode := smParty;
            FadeTo(@ScreenSong);
          end
          else
          begin
            FadeTo(@ScreenSingModi);
          end;
        end;

      SDLK_UP:
        begin
         if ScreenRound>=1 then
             dec(ScreenRound);
          Update;
        end;

      SDLK_DOWN:
        begin
          if ScreenRound<=(Length(PartySession.Rounds)-7) then
             inc(ScreenRound);
          Update;
        end;
    end;
  end;
end;

constructor TScreenPartyNewRound.Create;
{var
  I:    integer;}
begin
  inherited Create;

  TextRound[1] := AddText (Theme.PartyNewRound.TextRound1);
  TextRound[2] := AddText (Theme.PartyNewRound.TextRound2);
  TextRound[3] := AddText (Theme.PartyNewRound.TextRound3);
  TextRound[4] := AddText (Theme.PartyNewRound.TextRound4);
  TextRound[5] := AddText (Theme.PartyNewRound.TextRound5);
  TextRound[6] := AddText (Theme.PartyNewRound.TextRound6);
  TextRound[7] := AddText (Theme.PartyNewRound.TextRound7);

  TextWinner[1] := AddText (Theme.PartyNewRound.TextWinner1);
  TextWinner[2] := AddText (Theme.PartyNewRound.TextWinner2);
  TextWinner[3] := AddText (Theme.PartyNewRound.TextWinner3);
  TextWinner[4] := AddText (Theme.PartyNewRound.TextWinner4);
  TextWinner[5] := AddText (Theme.PartyNewRound.TextWinner5);
  TextWinner[6] := AddText (Theme.PartyNewRound.TextWinner6);
  TextWinner[7] := AddText (Theme.PartyNewRound.TextWinner7);

  TextNextRound := AddText (Theme.PartyNewRound.TextNextRound);
  TextNextRoundNo := AddText (Theme.PartyNewRound.TextNextRoundNo);
  TextNextPlayer1 := AddText (Theme.PartyNewRound.TextNextPlayer1);
  TextNextPlayer2 := AddText (Theme.PartyNewRound.TextNextPlayer2);
  TextNextPlayer3 := AddText (Theme.PartyNewRound.TextNextPlayer3);

  StaticRound[1] := AddStatic (Theme.PartyNewRound.StaticRound1);
  StaticRound[2] := AddStatic (Theme.PartyNewRound.StaticRound2);
  StaticRound[3] := AddStatic (Theme.PartyNewRound.StaticRound3);
  StaticRound[4] := AddStatic (Theme.PartyNewRound.StaticRound4);
  StaticRound[5] := AddStatic (Theme.PartyNewRound.StaticRound5);
  StaticRound[6] := AddStatic (Theme.PartyNewRound.StaticRound6);
  StaticRound[7] := AddStatic (Theme.PartyNewRound.StaticRound7);

  //Scores
  TextScoreTeam1 := AddText (Theme.PartyNewRound.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyNewRound.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyNewRound.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyNewRound.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyNewRound.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyNewRound.TextNameTeam3);

  //Players
  TextTeam1Players := AddText (Theme.PartyNewRound.TextTeam1Players);
  TextTeam2Players := AddText (Theme.PartyNewRound.TextTeam2Players);
  TextTeam3Players := AddText (Theme.PartyNewRound.TextTeam3Players);

  StaticTeam1 := AddStatic (Theme.PartyNewRound.StaticTeam1);
  StaticTeam2 := AddStatic (Theme.PartyNewRound.StaticTeam2);
  StaticTeam3 := AddStatic (Theme.PartyNewRound.StaticTeam3);
  StaticNextPlayer1 := AddStatic (Theme.PartyNewRound.StaticNextPlayer1);
  StaticNextPlayer2 := AddStatic (Theme.PartyNewRound.StaticNextPlayer2);
  StaticNextPlayer3 := AddStatic (Theme.PartyNewRound.StaticNextPlayer3);

  LoadFromTheme(Theme.PartyNewRound);
end;

procedure TScreenPartyNewRound.onShow;
//var
  //I: Integer;
  function GetTeamPlayers(const Num: Byte): String;
  var
    Players: Array of String;
    J: Byte;
  begin
    if (Num-1 >= PartySession.Teams.NumTeams) then
      exit;

    //Create Players Array
    SetLength(Players, PartySession.Teams.TeamInfo[Num-1].NumPlayers);
    For J := 0 to PartySession.Teams.TeamInfo[Num-1].NumPlayers-1 do
      Players[J] := String(PartySession.Teams.TeamInfo[Num-1].PlayerInfo[J].Name);

    //Implode and Return
    Result := Language.Implode(Players);
  end;
begin
  PartySession.StartRound;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyNewRound)');

  ScreenRound:=PartySession.CurRound-2;
  if ScreenRound<0 then
  begin
    ScreenRound:=PartySession.CurRound-1;
    if ScreenRound<0 then
      ScreenRound:=0;
  end;
  Update;

  //Display Scores
  if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(PartySession.Teams.TeamInfo[0].Score);
    Text[TextNameTeam1].Text := String(PartySession.Teams.TeamInfo[0].Name);
    Text[TextTeam1Players].Text := GetTeamPlayers(1);

    Text[TextScoreTeam1].Visible := True;
    Text[TextNameTeam1].Visible := True;
    Text[TextTeam1Players].Visible := True;
    Static[StaticTeam1].Visible := True;
    Static[StaticNextPlayer1].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := False;
    Text[TextNameTeam1].Visible := False;
    Text[TextTeam1Players].Visible := False;
    Static[StaticTeam1].Visible := False;
    Static[StaticNextPlayer1].Visible := False;
  end;

  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(PartySession.Teams.TeamInfo[1].Score);
    Text[TextNameTeam2].Text := String(PartySession.Teams.TeamInfo[1].Name);
    Text[TextTeam2Players].Text := GetTeamPlayers(2);

    Text[TextScoreTeam2].Visible := True;
    Text[TextNameTeam2].Visible := True;
    Text[TextTeam2Players].Visible := True;
    Static[StaticTeam2].Visible := True;
    Static[StaticNextPlayer2].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := False;
    Text[TextNameTeam2].Visible := False;
    Text[TextTeam2Players].Visible := False;
    Static[StaticTeam2].Visible := False;
    Static[StaticNextPlayer2].Visible := False;
  end;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(PartySession.Teams.TeamInfo[2].Score);
    Text[TextNameTeam3].Text := String(PartySession.Teams.TeamInfo[2].Name);
    Text[TextTeam3Players].Text := GetTeamPlayers(3);

    Text[TextScoreTeam3].Visible := True;
    Text[TextNameTeam3].Visible := True;
    Text[TextTeam3Players].Visible := True;
    Static[StaticTeam3].Visible := True;
    Static[StaticNextPlayer3].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := False;
    Text[TextNameTeam3].Visible := False;
    Text[TextTeam3Players].Visible := False;
    Static[StaticTeam3].Visible := False;
    Static[StaticNextPlayer3].Visible := False;
  end;

  //nextRound Texts
  Text[TextNextRound].Text := PartySession.Plugins[PartySession.Rounds[PartySession.CurRound].PluginNr].Desc;

  Text[TextNextRoundNo].Text := InttoStr(PartySession.CurRound + 1);
  if (PartySession.Teams.NumTeams >= 1) then
  begin
    Text[TextNextPlayer1].Text := PartySession.Teams.Teaminfo[0].Playerinfo[PartySession.Teams.Teaminfo[0].CurPlayer].Name;
    Text[TextNextPlayer1].Visible := True;
  end
  else
    Text[TextNextPlayer1].Visible := False;
    
  if (PartySession.Teams.NumTeams >= 2) then
  begin
    Text[TextNextPlayer2].Text := PartySession.Teams.Teaminfo[1].Playerinfo[PartySession.Teams.Teaminfo[1].CurPlayer].Name;
    Text[TextNextPlayer2].Visible := True;
  end
  else
    Text[TextNextPlayer2].Visible := False;

  if (PartySession.Teams.NumTeams >= 3) then
  begin
    Text[TextNextPlayer3].Text := PartySession.Teams.Teaminfo[2].Playerinfo[PartySession.Teams.Teaminfo[2].CurPlayer].Name;
    Text[TextNextPlayer3].Visible := True;
  end
  else
    Text[TextNextPlayer3].Visible := False;

end;

procedure TScreenPartyNewRound.Update;
var
  N, R: Integer;
  T: Integer;
  NumRounds: Integer;

begin
  //N:=0;
  //current round-number
  R:=PartySession.CurRound;

  //Set Visibility of Round Infos
  NumRounds := Length(PartySession.Rounds);

  N:=ScreenRound;

  if ((NumRounds-7)<N) then
  begin
    N:=NumRounds-7;
    ScreenRound:=N;
  end;

  if (N<0) then
  begin
    N:=0;
    ScreenRound:=0;
  end;

  for T := 1 to 7 do
  begin
    if (NumRounds >= T) then
    begin
      Static[StaticRound[T]].Visible := true;
      Text[TextRound[T]].Visible := true;
      if (N+T-1<R) then
        Text[TextWinner[T]].Visible := true
      else
        Text[TextWinner[T]].Visible := false;

      Text[TextRound[T]].Text := IntToStr(N+T)+') '+ PartySession.Plugins[PartySession.Rounds[N+T-1].PluginNr].Name;
      Text[TextWinner[T]].Text := PartySession.GetWinnerString(N+T-1);
    end else
    begin
      Static[StaticRound[T]].Visible := false;
      Text[TextRound[T]].Visible := false;
      Text[TextWinner[T]].Visible := false;
    end;
  end;
end;


procedure TScreenPartyNewRound.SetAnimationProgress(Progress: real);
begin
  {Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress; }
end;

end.
