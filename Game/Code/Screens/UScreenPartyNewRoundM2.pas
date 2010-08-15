unit UScreenPartyNewRoundM2;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPartyNewRoundM2 = class(TMenu)
    public
      //Texts:
      TextRound: array [1..9] of cardinal;
      TextWinner: array [1..9] of cardinal;

      TextNextRound: cardinal;

      TextNextPlayer1: cardinal;
      TextNextPlayer2: cardinal;
      TextHandicap:    cardinal;

      //Statics
      StaticRound: array [1..9] of cardinal;
      StaticTable: array [1..9] of cardinal;

      StaticNextPlayer1: cardinal;
      StaticNextPlayer2: cardinal;
      StaticHandicap: cardinal;

      TextTableName: array[1..9,1..7] of cardinal;

      ScreenRound:  Integer;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function FillNulls(number: integer): String;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
      procedure Update;
  end;

const
  ID='ID_019';   //for help system
  
implementation

uses UGraphic, UMain, UIni, UTexture, UPartyM2, UDLLManager, ULanguage, ULog, UHelp;

function TScreenPartyNewRoundM2.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          if(PartySessionM2.CurRound<Length(PartySessionM2.Rounds)) then
          begin
            Music.PlayStart;
            //Select PartyMode M2 ScreenSong
            //Select PartyMode ScreenSong
            ScreenSong.Mode := smChallenge;
            FadeTo(@ScreenSong);
          end else
          begin
            Music.PlayBack;
            CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
          end;
        end;
      SDLK_DOWN:
        begin
          if ScreenRound<=(Length(PartySessionM2.Rounds)-9) then
             inc(ScreenRound);
          Update;
        end;
      SDLK_UP:
        begin
          if ScreenRound>=1 then
             dec(ScreenRound);
          Update;
        end;
      SDLK_RIGHT:
        begin
          if ScreenRound<=(Length(PartySessionM2.Rounds)-9) then
             inc(ScreenRound);
          Update;
        end;
      SDLK_LEFT:
        begin
          if ScreenRound>=1 then
             dec(ScreenRound);
          Update;
        end;
    end;
  end;
end;

constructor TScreenPartyNewRoundM2.Create;
var
  I, J:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.PartyNewRoundM2);

  for I := 1 to 9 do
  begin
    TextRound[I] := AddText (Theme.PartyNewRoundM2.TextRound[I]);
    TextWinner[I] := AddText (Theme.PartyNewRoundM2.TextWinner[I]);
    StaticRound[I] := AddStatic (Theme.PartyNewRoundM2.StaticRound[I]);
    StaticTable[I] := AddStatic (Theme.PartyNewRoundM2.StaticTable[I]);
  end;

  TextNextRound := AddText (Theme.PartyNewRoundM2.TextNextRound);
  TextNextPlayer1 := AddText (Theme.PartyNewRoundM2.TextNextPlayer1);
  TextNextPlayer2 := AddText (Theme.PartyNewRoundM2.TextNextPlayer2);
  TextHandicap := AddText (Theme.PartyNewRoundM2.TextHandicap);

  StaticNextPlayer1 := AddStatic (Theme.PartyNewRoundM2.StaticNextPlayer1);
  StaticNextPlayer2 := AddStatic (Theme.PartyNewRoundM2.StaticNextPlayer2);
  StaticHandicap := AddStatic (Theme.PartyNewRoundM2.StaticHandicap);

  //Table
  for I := 1 to 9 do
  begin
    for J := 1 to 7 do
    begin
      TextTableName[I,J] := AddText (Theme.PartyNewRoundM2.TextTableName[I,J]);
    end;
  end;
end;

procedure TScreenPartyNewRoundM2.onShow;
var
  x,y: Integer;
begin
  inherited;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyNewRoundM2)');

  PartySessionM2.StartRound;
  ScreenRound:=PartySessionM2.CurRound-2;
  if ScreenRound<0 then
  begin
    ScreenRound:=PartySessionM2.CurRound-1;
    if ScreenRound<0 then
      ScreenRound:=0;
  end;
  Update;

  //Display Scores
  for x := 1 to 9 do
  begin
    if x<=PartySessionM2.Players.NumPlayer then
    begin
      Static[StaticTable[x]].Visible := true;
      Text[TextTableName[x,1]].Text := IntToStr(x);
      Text[TextTableName[x,2]].Text := PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].Name;
      Text[TextTableName[x,3]].Text := IntToStr(PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].Wins);
      Text[TextTableName[x,4]].Text := IntToStr(PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].Draws);
      Text[TextTableName[x,5]].Text := IntToStr(PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].Defeats);
      Text[TextTableName[x,6]].Text := IntToStr(PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].ScoreP -
                                                PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].ScoreN);
      Text[TextTableName[x,7]].Text := IntToStr(PartySessionM2.Players.Playerinfo[PartySessionM2.Order[x-1]].Points);
    end else
    begin
      for y := 1 to 7 do
      begin
        Text[TextTableName[x,y]].Text := '';
        Static[StaticTable[x]].Visible := false;
      end;
    end;
  end;

  //nextRound Texts
  if PartySessionM2.CurRound<Length(PartySessionM2.Rounds) then
  begin
    if PartySessionM2.Option_Plugins then
    begin
      Text[TextNextRound].Text := Language.Translate('PARTY_ROUND') + ' ' + IntToStr(PartySessionM2.CurRound + 1) +
        ': ' + PartySessionM2.Plugins[PartySessionM2.Rounds[PartySessionM2.CurRound].PluginNr].Desc;
    end else
      Text[TextNextRound].Text := Language.Translate('PARTY_ROUND') + ' ' + IntToStr(PartySessionM2.CurRound + 1);

    Text[TextNextPlayer1].Text := PartySessionM2.Players.Playerinfo[PartySessionM2.Rounds[PartySessionM2.CurRound].Player1].Name;
    Text[TextNextPlayer1].Visible := true;

    Text[TextNextPlayer2].Text := PartySessionM2.Players.Playerinfo[PartySessionM2.Rounds[PartySessionM2.CurRound].Player2].Name;
    Text[TextNextPlayer2].Visible := true;

    Static[StaticNextPlayer1].Visible := true;
    Static[StaticNextPlayer2].Visible := true;

    //Handicap-mode
    if PartySessionM2.HandicapMode then
    begin
      Text[TextHandicap].Text := '[' + FormatFloat('#0.00', PartySessionM2.Handicap.P1m) +
        ' : ' + FormatFloat('#0.00', PartySessionM2.Handicap.P2m) + ']';
      Text[TextHandicap].visible := true;
      Static[StaticHandicap].visible := true;
    end else
    begin
      Text[TextHandicap].visible := false;
      Static[StaticHandicap].visible := false;
    end;
  end else
  begin
    Text[TextNextRound].Text := Language.Translate('PARTY_ROUNDM2_END');

    Text[TextNextPlayer1].Visible := false;
    Text[TextNextPlayer2].Visible := false;

    Static[StaticNextPlayer1].Visible := false;
    Static[StaticNextPlayer2].Visible := false;
  end;
end;

procedure TScreenPartyNewRoundM2.Update;
var
  N, R: Integer;
  T: Integer;
  NumRounds: Integer;
begin
  //current round-number
  R:=PartySessionM2.CurRound;

  //Set Visibility of Round Infos
  NumRounds := Length(PartySessionM2.Rounds);

  N:=ScreenRound;

  if ((NumRounds-9)<N) then
  begin
    N:=NumRounds-9;
    ScreenRound:=N;
  end;

  if (N<0) then
  begin
    N:=0;
    ScreenRound:=0;
  end;

  for T := 1 to 9 do
  begin
    if (NumRounds >= T) then
    begin
      Static[StaticRound[T]].Visible := true;
      Text[TextRound[T]].Visible := true;
      if (N+T-1<R) then
        Text[TextWinner[T]].Visible := true
      else
        Text[TextWinner[T]].Visible := false;

      //Texts:
      if (N+T-1<R) then
        Text[TextRound[T]].Text := IntToStr(N+T)+ ') ' + PartySessionM2.Players.Playerinfo[PartySessionM2.Rounds[N+T-1].Player1].Name +
          ' - ' + PartySessionM2.Players.Playerinfo[PartySessionM2.Rounds[N+T-1].Player2].Name
      else
      begin
        Text[TextRound[T]].Text := IntToStr(N+T)+ ') ';
        Text[TextRound[T]].Text := Text[TextRound[T]].Text +
          PartySessionM2.Plugins[PartySessionM2.Rounds[N+T-1].PluginNr].Name
      end;

      Text[TextWinner[T]].Text :=FillNulls(PartySessionM2.Rounds[N+T-1].ScoreP) +
        ':' + FillNulls(PartySessionM2.Rounds[N+T-1].ScoreN);
    end else
    begin
      Static[StaticRound[T]].Visible := false;
      Text[TextRound[T]].Visible := false;
      Text[TextWinner[T]].Visible := false;
    end;
  end;
end;

function TScreenPartyNewRoundM2.FillNulls(number: integer): String;
begin
  if number<10 then
    Result := '000' + IntToStr(number)
  else if number<100 then
    Result := '00' + IntToStr(number)
  else if number <1000 then
    Result := '0' + IntToStr(number)
  else
    Result := IntToStr(number);
end;

procedure TScreenPartyNewRoundM2.SetAnimationProgress(Progress: real);
begin
  {Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress; }
end;

end.