unit UScreenScore;

interface

uses
  UMenu, SDL, SysUtils, UDisplay, UMusic, USongs, UThemes, ULCD, gl;

type
  TScreenScore = class(TMenu)
    const
      ID='ID_022';   //for help system

    public
      TextArtist:   integer;
      TextTitle:    integer;

      TextArtistTitle:  integer;

      StaticMedleyNav:  integer;
      TextMedleyNav:    integer;
      
      TextName:             array[1..6] of integer;
      TextScore:            array[1..6] of integer;

      TextNotes:            array[1..6] of integer;
      TextNotesScore:       array[1..6] of integer;
      TextLineBonus:        array[1..6] of integer;
      TextLineBonusScore:   array[1..6] of integer;
      TextGoldenNotes:      array[1..6] of integer;
      TextGoldenNotesScore: array[1..6] of integer;
      TextTotal:            array[1..6] of integer;
      TextTotalScore:       array[1..6] of integer;

      PlayerStatic:         array[1..6] of array of integer;
      PlayerTexts :         array[1..6] of array of integer;


      StaticBoxLightest:    array[1..6] of integer;
      StaticBoxLight:       array[1..6] of integer;
      StaticBoxDark:        array[1..6] of integer;

      StaticBackLevel:        array[1..6] of integer;
      StaticBackLevelRound:   array[1..6] of integer;
      StaticLevel:            array[1..6] of integer;
      StaticLevelRound:       array[1..6] of integer;

      Animation:    real;
      Fadeout:      boolean;
      ActualRound: integer;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure FillPlayer(Item, P: integer);
      procedure RefreshTexts;
      procedure StartPreview;
  end;

implementation

{{$IFDEF TRANSLATE}
uses UGraphic, UScreenSong, UPartyM2, UMenuStatic, UTime, UMain, UIni, ULanguage, UHelp, ULog;
{{$ELSE}{
uses UGraphic, UScreenSong, UMenuStatic, UTime, UMain, UIni;
{{$ENDIF}
function TScreenScore.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
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
      SDLK_BACKSPACE,
      SDLK_RETURN:
        begin
          if (not Fadeout) then
          begin
//            Music.StopShuffle;
            if ScreenSong.PartyMedley and (ScreenSong.Mode=smChallenge) then
            begin
              ScreenSong.SongIndex := -1;
              Music.FadeStop(Ini.PreviewFading);
              FadeTo(@ScreenPartyNewRoundM2);
            end else if (ScreenSong.Mode <> smMedley) then
              FadeTo(@ScreenTop)
            else
            begin
              FadeTo(@ScreenSong);
            end;
            Fadeout := true;
          end;
        end;

      SDLK_SYSREQ:
        begin
          Display.PrintScreen;
        end;

      SDLK_RIGHT:
        begin
          if ActualRound<Length(PlaylistMedley.Stats)-1 then
          begin
            Music.PlayChange;
            inc(ActualRound);
            RefreshTexts;
            StartPreview;
          end;
        end;

      SDLK_LEFT:
        begin
          if ActualRound>0 then
          begin
            Music.PlayChange;
            dec(ActualRound);
            RefreshTexts;
            StartPreview;
          end;
        end;
    end;
  end;
end;

procedure TScreenScore.RefreshTexts;
begin
  if (ActualRound < Length(PlaylistMedley.Stats)-1) then
  begin
    Text[TextArtist].Text      := IntToStr(ActualRound+1) + '/' +
      IntToStr(Length(PlaylistMedley.Stats)-1) + ': ' +
      PlaylistMedley.Stats[ActualRound].SongArtist;
    Text[TextTitle].Text       := PlaylistMedley.Stats[ActualRound].SongTitle;
    Text[TextTitle].Visible    := true;
    Text[TextArtistTitle].Text := IntToStr(ActualRound+1) + '/' +
      IntToStr(Length(PlaylistMedley.Stats)-1) + ': ' +
      PlaylistMedley.Stats[ActualRound].SongArtist +
      ' - ' + PlaylistMedley.Stats[ActualRound].SongTitle;
  end else
  begin
    if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
    begin
      Text[TextArtist].Text      := Language.Translate('SING_TOTAL');
      Text[TextTitle].Visible    := false;
      Text[TextArtistTitle].Text := Language.Translate('SING_TOTAL');
    end else
    begin
      Text[TextArtist].Text      := PlaylistMedley.Stats[ActualRound].SongArtist;
      Text[TextTitle].Text       := PlaylistMedley.Stats[ActualRound].SongTitle;
      Text[TextTitle].Visible    := true;
      Text[TextArtistTitle].Text := PlaylistMedley.Stats[ActualRound].SongArtist + ' - ' +
        PlaylistMedley.Stats[ActualRound].SongTitle;
    end;
  end;
end;

constructor TScreenScore.Create;
var
  P:    integer;
  I, C:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Score);

  TextArtist := AddText(Theme.Score.TextArtist);
  TextTitle := AddText(Theme.Score.TextTitle);

  TextArtistTitle := AddText(Theme.Score.TextArtistTitle);

  StaticMedleyNav := AddStatic(Theme.Score.StaticMedleyNav);
  TextMedleyNav := AddText(Theme.Score.TextMedleyNav);

  for P := 1 to 6 do begin
    TextName[P] := AddText(Theme.Score.TextName[P]);
    TextScore[P] := AddText(Theme.Score.TextScore[P]);

    TextNotes[P] := AddText(Theme.Score.TextNotes[P]);
    TextNotesScore[P] := AddText(Theme.Score.TextNotesScore[P]);
    TextLineBonus[P] := AddText(Theme.Score.TextLineBonus[P]);
    TextLineBonusScore[P] := AddText(Theme.Score.TextLineBonusScore[P]);
    TextGoldenNotes[P] := AddText(Theme.Score.TextGoldenNotes[P]);
    TextGoldenNotesScore[P] := AddText(Theme.Score.TextGoldenNotesScore[P]);
    TextTotal[P] := AddText(Theme.Score.TextTotal[P]);
    TextTotalScore[P] := AddText(Theme.Score.TextTotalScore[P]);

    SetLength(PlayerStatic[P], Length(Theme.Score.PlayerStatic[P]));

    SetLength(PlayerTexts[P], Length(Theme.Score.PlayerTexts[P]));

    for I := 0 to High(Theme.Score.PlayerStatic[P]) do
      PlayerStatic[P, I] := AddStatic(Theme.Score.PlayerStatic[P, I]);


    //added by mog
    for C := 0 to High(Theme.Score.PlayerTexts[P]) do
      PlayerTexts[P, C] := AddText(Theme.Score.PlayerTexts[P, C]);
    // more skinable now

    StaticBoxLightest[P] := AddStatic(Theme.Score.StaticBoxLightest[P]);
    StaticBoxLight[P] := AddStatic(Theme.Score.StaticBoxLight[P]);
    StaticBoxDark[P] := AddStatic(Theme.Score.StaticBoxDark[P]);

    StaticBackLevel[P] := AddStatic(Theme.Score.StaticBackLevel[P]);
    StaticBackLevelRound[P] := AddStatic(Theme.Score.StaticBackLevelRound[P]);
    StaticLevel[P] := AddStatic(Theme.Score.StaticLevel[P]);
    StaticLevelRound[P] := AddStatic(Theme.Score.StaticLevelRound[P]);
  end;
end;

procedure TScreenScore.onShow;
var
  P:    integer;  // player
  PP:   integer;  // another player variable
  S:    string;
  I:    integer;
  Skip: integer;
  V:    array[1..6] of boolean; // visibility array
begin
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenScore)');

  // Singstar
  Fadeout := false;
  ActualRound:=0;
  Text[TextArtist].Text := AktSong.Artist;
  Text[TextTitle].Text := AktSong.Title;
  Text[TextArtistTitle].Text := AktSong.Artist + ' - ' + AktSong.Title;

  // set visibility
  case PlayersPlay of
    1:  begin
          V[1] := true;
          V[2] := false;
          V[3] := false;
          V[4] := false;
          V[5] := false;
          V[6] := false;
        end;
    2, 4:  begin
          V[1] := false;
          V[2] := true;
          V[3] := true;
          V[4] := false;
          V[5] := false;
          V[6] := false;
        end;
    3, 6:  begin
          V[1] := false;
          V[2] := false;
          V[3] := false;
          V[4] := true;
          V[5] := true;
          V[6] := true;
        end;
  end;

  for P := 1 to 6 do
  begin
    Text[TextName[P]].Visible := V[P];
    Text[TextScore[P]].Visible := V[P];

    Text[TextNotes[P]].Visible := V[P];
    Text[TextNotesScore[P]].Visible := V[P];
    Text[TextLineBonus[P]].Visible := V[P];
    Text[TextLineBonusScore[P]].Visible := V[P];
    Text[TextGoldenNotes[P]].Visible := V[P];
    Text[TextGoldenNotesScore[P]].Visible := V[P];
    Text[TextTotal[P]].Visible := V[P];
    Text[TextTotalScore[P]].Visible := V[P];

    for I := 0 to high(PlayerStatic[P]) do
      Static[PlayerStatic[P, I]].Visible := V[P];

    for I := 0 to high(PlayerTexts[P]) do
      Text[PlayerTexts[P, I]].Visible := V[P];

    Static[StaticBoxLightest[P]].Visible := V[P];
    Static[StaticBoxLight[P]].Visible := V[P];
    Static[StaticBoxDark[P]].Visible := V[P];

    Static[StaticBackLevel[P]].Visible := V[P];
    Static[StaticBackLevelRound[P]].Visible := V[P];
    Static[StaticLevel[P]].Visible := V[P];
    Static[StaticLevelRound[P]].Visible := V[P];
  end;

  {
  if PlayersPlay <= 3 then begin // only for 1 screen mode
    for P := 0 to PlayersPlay-1 do begin
      case PlayersPlay of
        1:  PP := 1;
        2:  PP := P + 2;
        3:  PP := P + 4;
      end;

      //Replaced this whole thing with one Procedure call
      FillPlayer(PP, P);

    end; // for
  end; // if  }

  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    Static[StaticMedleyNav].Visible := true;
    Text[TextMedleyNav].Visible := true;
  end else
  begin
    Static[StaticMedleyNav].Visible := false;
    Text[TextMedleyNav].Visible := false;
  end;

  RefreshTexts;

  LCD.HideCursor;
  LCD.Clear;
  LCD.WriteText(1, Ini.Name[0]);
  LCD.WriteText(2, 'Score: ' + Text[TextTotalScore[1]].Text);

  StartPreview;
end;

function TScreenScore.Draw: boolean;
var
{  Min:    real;
  Max:    real;
  Wsp:    real;
  Wsp2:   real;
  Pet:    integer;}

  Item:   integer;
  P:      integer;
  C:      integer;
begin
  if PlayersPlay <= 3 then begin // only for 1 screen mode
    for P := 0 to PlayersPlay-1 do begin
      case PlayersPlay of
        1:  Item := 1;
        2:  Item := P + 2;
        3:  Item := P + 4;
      end;

      //Replaced this whole thing with one Procedure call
      FillPlayer(Item, P);

    end; // for
  end; // if

  // 0.5.0: try also use 4 players screen with nicks
  if PlayersPlay = 4 then begin
    for Item := 2 to 3 do begin
      if ScreenAct = 1 then P := Item-2;
      if ScreenAct = 2 then P := Item;

      FillPlayer(Item, P);
    end;
  end;


  // Singstar - let it be...... with 6 statics
  if PlayersPlay = 6 then begin
    for Item := 4 to 6 do begin
      if ScreenAct = 1 then P := Item-4;
      if ScreenAct = 2 then P := Item-1;

      FillPlayer(Item, P);

    end;
  end;

  inherited Draw;
end;

procedure TScreenScore.FillPlayer(Item, P: integer);
var
  S:    string;
  Lev:  real;
  MaxH: real; // maximum height of score bar
  Wsp:  real;
begin
  Text[TextName[Item]].Text := Ini.Name[P];

  Text[PlayerTexts[Item, 0]].Text := 'P' + IntToStr(P+1);

  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    Player[P] := PlaylistMedley.Stats[ActualRound].Player[P];
  end;

  case (Player[P].ScoreTotalI) of
    0..2000:        Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_TONE_DEAF');
    2010..4000:     Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_AMATEUR');
    4010..6000:     Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_RISING_STAR');
    6010..8000:     Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_LEAD_SINGER');
    8010..9000:     Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_HIT_ARTIST');
    9010..9800:     Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_SUPERSTAR');
    9810..10000:    Text[TextScore[Item]].Text := Language.Translate('SING_SCORE_ULTRASTAR');
  end;


  S := IntToStr(Player[P].ScoreI);
  while (Length(S)<4) do S := '0' + S;
    Text[TextNotesScore[Item]].Text := S;

  S := IntToStr(Player[P].ScoreLineI);
  while (Length(S)<4) do S := '0' + S;
    Text[TextLineBonusScore[Item]].Text := S;

  S := IntToStr(Player[P].ScoreGoldenI);
  while (Length(S)<4) do S := '0' + S;
    Text[TextGoldenNotesScore[Item]].Text := S;

  S := IntToStr(Player[P].ScoreTotalI);
  while (Length(S)<5) do S := '0' + S;
    Text[TextTotalScore[Item]].Text := S;

  // Level bar length


  Lev := Player[P].ScoreTotalI / 10000;
  MaxH := Static[StaticBackLevel[Item]].Texture.H + Static[StaticBackLevelRound[Item]].Texture.H / 2;

  // developer note (Polish):
  // w sumie np. 120 pix
  // ten static moze miec 100 pix
  // wlacza sie od 20 pix i rosnie do 120 pix
  // wiec wysokosc = wyznaczona ilosc - 20
  // nie moze byc mniejsze od 0
  // Lev * MaxH = total number of pixels to draw
  Static[StaticLevel[Item]].Visible := true;
  Static[StaticLevel[Item]].Texture.H := Lev * MaxH - Static[StaticBackLevelRound[Item]].Texture.H / 2;
  if Static[StaticLevel[Item]].Texture.H < 0 then Static[StaticLevel[Item]].Visible := false;

  // Y doesn't change and depend on the back texture coordinate
  Static[StaticLevel[Item]].Texture.Y := Static[StaticBackLevel[Item]].Texture.Y + Static[StaticBackLevel[Item]].Texture.H  - Static[StaticLevel[Item]].Texture.H;

  // we modify LevelRound texture by changing it's Y. TexY1 and TexY2 change when the height to draw is lower than 20
  if Lev * MaxH < Static[StaticBackLevelRound[Item]].Texture.H / 2 then begin
    // when it's lower than 20 => we move TexY1 and TexY2 higher to show only part of this texture
    Static[StaticLevelRound[Item]].Texture.Y := Static[StaticBackLevel[Item]].Texture.Y + Static[StaticBackLevel[Item]].Texture.H - Static[StaticBackLevelRound[Item]].Texture.H;
    // - 0.25 when points = 0
    // - 0 wnen there are more points
    // if Lev * MaxH = Static[StaticBackLevelRound[Item]].Texture.H / 2) then we do not change it
    // if Lev * MaxH = 0 then we substract 0.25
    // we substract (0.25 - 0.25 * (Lev * MaxH)/Static[StaticBackLevelRound[Item]].Texture.H / 2)
    Wsp := Lev * MaxH / (Static[StaticBackLevelRound[Item]].Texture.H / 2);
    Static[StaticLevelRound[Item]].Texture.TexY1 := Static[StaticBackLevelRound[Item]].Texture.TexY1 - 0.25 + 0.25 * Wsp;
    Static[StaticLevelRound[Item]].Texture.TexY2 := Static[StaticBackLevelRound[Item]].Texture.TexY2 - 0.25 + 0.25 * Wsp;
  end else begin
    // when it's higher or equal 20 => full texture is being shown
    Static[StaticLevelRound[Item]].Texture.TexY1 := Static[StaticBackLevelRound[Item]].Texture.TexY1;
    Static[StaticLevelRound[Item]].Texture.TexY2 := Static[StaticBackLevelRound[Item]].Texture.TexY2;
    Static[StaticLevelRound[Item]].Texture.Y := Static[StaticLevel[Item]].Texture.Y - Static[StaticBackLevelRound[Item]].Texture.H;
  end;

  //Load Colors of Player Buttons and Nicks
  LoadColor(
    Text[TextName[Item]].ColR,
    Text[TextName[Item]].ColG,
    Text[TextName[Item]].ColB,
    'P' + IntToStr(P+1) + 'Dark');

  LoadColor(
    Static[StaticBoxLightest[Item]].Texture.ColR,
    Static[StaticBoxLightest[Item]].Texture.ColG,
    Static[StaticBoxLightest[Item]].Texture.ColB,
    'P' + IntToStr(P+1) + 'Lightest');

  LoadColor(
    Static[StaticBoxLight[Item]].Texture.ColR,
    Static[StaticBoxLight[Item]].Texture.ColG,
    Static[StaticBoxLight[Item]].Texture.ColB,
    'P' + IntToStr(P+1) + 'Light');

  LoadColor(
    Static[StaticBoxDark[Item]].Texture.ColR,
    Static[StaticBoxDark[Item]].Texture.ColG,
    Static[StaticBoxDark[Item]].Texture.ColB,
    'P' + IntToStr(P+1) + 'Dark');

  {StaticBackLevel[Item]
  StaticLevelRound[Item]
  and another static has to be colored here to.
  S/o please do this ^^}
end;

//Procedure Change current played Preview
procedure TScreenSCore.StartPreview;
var
  select:   integer;
  changed:  boolean;
begin
  //When Music Preview is avtivated -> then Change Music
  if (Ini.PreviewVolume <> 0) then
  begin
    changed := false;
    if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
    begin
      if (ActualRound<Length(PlaylistMedley.Stats)-1) and (ScreenSong.SongIndex <> PlaylistMedley.Song[ActualRound])  then
      begin
        select := PlaylistMedley.Song[ActualRound];
        changed := true;
        ScreenSong.SongIndex := select;
      end;
    end else
    begin
      select := ScreenSong.Interaction;
      ScreenSong.SongIndex := select;
      changed := true;
    end;

    if changed then
    begin
      Music.Close;
      if Music.Open(CatSongs.Song[select].Path + CatSongs.Song[select].Mp3) then
      begin
        if (CatSongs.Song[select].PreviewStart>0) then
          Music.MoveTo(CatSongs.Song[select].PreviewStart)
        else
          Music.MoveTo(Music.Length / 4);

        //If Song Fading is activated then don't Play directly, and Set Volume to Null, else Play normal
        if (Ini.PreviewFading = 0) then
        begin
          Music.SetMusicVolume (Ini.PreviewVolume * 10);
          Music.Play;
        end else
        begin
          Music.Fade(0, Ini.PreviewVolume*10, Ini.PreviewFading);
          Music.Play;
        end;
      end;
    end;
  end;
end;

end.
