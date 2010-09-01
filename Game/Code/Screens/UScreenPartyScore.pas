unit UScreenPartyScore;

interface

uses
  UMenu, SDL, UDisplay, UMusic, SysUtils, UThemes;

type
  THandler = record
    changed:  boolean;
    change_time:  real;
  end;
  
  TScreenPartyScore = class(TMenu)
    public
      TextScoreTeam1:    Cardinal;
      TextScoreTeam2:    Cardinal;
      TextScoreTeam3:    Cardinal;
      TextNameTeam1:     Cardinal;
      TextNameTeam2:     Cardinal;
      TextNameTeam3:     Cardinal;
      StaticTeam1:       Cardinal;
      StaticTeam1BG:     Cardinal;
      StaticTeam1Deco:   Cardinal;
      StaticTeam2:       Cardinal;
      StaticTeam2BG:     Cardinal;
      StaticTeam2Deco:   Cardinal;
      StaticTeam3:       Cardinal;
      StaticTeam3BG:     Cardinal;
      StaticTeam3Deco:   Cardinal;
      TextWinner:        Cardinal;

      DecoTex:          Array[0..5] of Integer;
      DecoColor:        Array[0..5] of Record
                                        R, G, B: Real;
                        end;

      MaxScore:         Word;

      ActualRound:      integer;
      Voice:            integer;
      Fadeout:          boolean;

      MP3VolumeHandler: THandler;
      
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure SetAnimationProgress(Progress: real); override;
      procedure StartPreview;
      procedure StartVoice;
  end;

const
  ID='ID_017';   //for help system

implementation

uses UGraphic, UDraw, UTime, UMain, UParty, USongs, UScreenSingModi, ULanguage, UTexture, UIni, USkins, UHelp, ULog;

function TScreenPartyScore.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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

      //MP3-Volume Up
      SDLK_PAGEUP:
        begin
          if (ScreenSong.MP3Volume<100) then
          begin
            ScreenSong.MP3Volume := ScreenSong.MP3Volume+5;
            Music.SetMusicVolume(ScreenSong.MP3Volume);
          end;
          MP3VolumeHandler.changed := true;
          MP3VolumeHandler.change_time := 0;
        end;

      //MP3-Volume Down
      SDLK_PAGEDOWN:
        begin
          if (ScreenSong.MP3Volume>0) then
          begin
            ScreenSong.MP3Volume := ScreenSong.MP3Volume-5;
            Music.SetMusicVolume(ScreenSong.MP3Volume);
          end;
          MP3VolumeHandler.changed := true;
          MP3VolumeHandler.change_time := 0;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_RETURN :
        begin
          if (not Fadeout) then
          begin
            Music.PlayStart;

            if (Ini.SavePlayback=1) then
              Music.VoicesClose;

            ScreenSong.SongIndex := -1;
            Music.FadeStop(Ini.PreviewFading);

            if (PartySession.CurRound < High(PartySession.Rounds)) then
              FadeTo(@ScreenPartyNewRound)
            else
            begin
              PartySession.EndRound;
              FadeTo(@ScreenPartyWin);
            end;
            Fadeout := true;
          end;
        end;

      SDLK_RIGHT:
        begin
          if ActualRound<Length(PlaylistMedley.Stats)-1 then
          begin
            Music.PlayChange;
            inc(ActualRound);
            //RefreshTexts;
            if not (Ini.SavePlayback=1) then
              StartPreview
            else
              StartVoice;
          end;
        end;

      SDLK_LEFT:
        begin
          if ActualRound>0 then
          begin
            Music.PlayChange;
            dec(ActualRound);
            //RefreshTexts;
            if not (Ini.SavePlayback=1) then
              StartPreview
            else
              StartVoice;
          end;
        end;
    end;
  end;
end;

constructor TScreenPartyScore.Create;
var
  //I:    integer;
  Tex:  TTexture;
  R, G, B: Real;
  Color: Integer;
begin
  inherited Create;

  TextScoreTeam1 := AddText (Theme.PartyScore.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyScore.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyScore.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyScore.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyScore.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyScore.TextNameTeam3);

  StaticTeam1 := AddStatic (Theme.PartyScore.StaticTeam1);
  StaticTeam1BG := AddStatic (Theme.PartyScore.StaticTeam1BG);
  StaticTeam1Deco := AddStatic (Theme.PartyScore.StaticTeam1Deco);
  StaticTeam2 := AddStatic (Theme.PartyScore.StaticTeam2);
  StaticTeam2BG := AddStatic (Theme.PartyScore.StaticTeam2BG);
  StaticTeam2Deco := AddStatic (Theme.PartyScore.StaticTeam2Deco);
  StaticTeam3 := AddStatic (Theme.PartyScore.StaticTeam3);
  StaticTeam3BG := AddStatic (Theme.PartyScore.StaticTeam3BG);
  StaticTeam3Deco := AddStatic (Theme.PartyScore.StaticTeam3Deco);

  TextWinner := AddText (Theme.PartyScore.TextWinner);

  //Load Deco Textures
  if Theme.PartyScore.DecoTextures.ChangeTextures then
  begin
    //Get Color
    LoadColor(R, G, B, Theme.PartyScore.DecoTextures.FirstColor);
    Color := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    DecoColor[0].R := R;
    DecoColor[0].G := G;
    DecoColor[0].B := B;

    //Load Texture
    Tex := Texture.LoadTexture(pchar(Skin.GetTextureFileName(Theme.PartyScore.DecoTextures.FirstTexture)),  'JPG', PChar(Theme.PartyScore.DecoTextures.FirstTyp), Color);
    DecoTex[0] := Tex.TexNum;

    //Get Second Color
    LoadColor(R, G, B, Theme.PartyScore.DecoTextures.SecondColor);
    Color := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    DecoColor[1].R := R;
    DecoColor[1].G := G;
    DecoColor[1].B := B;

    //Load Second Texture
    Tex := Texture.LoadTexture(pchar(Skin.GetTextureFileName(Theme.PartyScore.DecoTextures.SecondTexture)),  'JPG', PChar(Theme.PartyScore.DecoTextures.SecondTyp), Color);
    DecoTex[1] := Tex.TexNum;

    //Get Third Color
    LoadColor(R, G, B, Theme.PartyScore.DecoTextures.ThirdColor);
    Color := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    DecoColor[2].R := R;
    DecoColor[2].G := G;
    DecoColor[2].B := B;

    //Load Third Texture
    Tex := Texture.LoadTexture(pchar(Skin.GetTextureFileName(Theme.PartyScore.DecoTextures.ThirdTexture)),  'JPG', PChar(Theme.PartyScore.DecoTextures.ThirdTyp), Color);
    DecoTex[2] := Tex.TexNum;
  end;

  LoadFromTheme(Theme.PartyScore);
end;

procedure TScreenPartyScore.onShow;
var
  I, J: Integer;
  Placings: Array [0..5] of Byte;
begin
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyScore)');

  if Music.VocalRemoverActivated() then
    Music.DisableVocalRemover;

  ActualRound := 0;
  Fadeout := false;
    
  //Get Maxscore
  MaxScore := 0;
  for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
  begin
    if (ScreenSingModi.PlayerInfo.Playerinfo[I].Score > MaxScore) then
      MaxScore := ScreenSingModi.PlayerInfo.Playerinfo[I].Score;
  end;

  //Get Placings
  for I := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
  begin
    Placings[I] := 0;
    for J := 0 to ScreenSingModi.PlayerInfo.NumPlayers - 1 do
      If (ScreenSingModi.PlayerInfo.Playerinfo[J].Score > ScreenSingModi.PlayerInfo.Playerinfo[I].Score) then
        Inc(Placings[I]);
  end;


  //Set Static Length
  Static[StaticTeam1].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  Static[StaticTeam2].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  Static[StaticTeam3].Texture.ScaleW := ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;

  //fix: prevents static from drawn out of bounds.
  if Static[StaticTeam1].Texture.ScaleW > 99 then Static[StaticTeam1].Texture.ScaleW := 99;
  if Static[StaticTeam2].Texture.ScaleW > 99 then Static[StaticTeam2].Texture.ScaleW := 99;
  if Static[StaticTeam3].Texture.ScaleW > 99 then Static[StaticTeam3].Texture.ScaleW := 99;

  //End Last Round
  PartySession.EndRound;

  //Set Winnertext
  Text[TextWinner].Text := Format(Language.Translate('PARTY_SCORE_WINS'), [PartySession.GetWinnerString(PartySession.CurRound)]);

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[0].Score);
    Text[TextNameTeam1].Text := String(ScreenSingModi.TeamInfo.Teaminfo[0].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      Static[StaticTeam1Deco].Texture.TexNum := DecoTex[Placings[0]];
      Static[StaticTeam1Deco].Texture.ColR := DecoColor[Placings[0]].R;
      Static[StaticTeam1Deco].Texture.ColG := DecoColor[Placings[0]].G;
      Static[StaticTeam1Deco].Texture.ColB := DecoColor[Placings[0]].B;
    end;

    Text[TextScoreTeam1].Visible := True;
    Text[TextNameTeam1].Visible := True;
    Static[StaticTeam1].Visible := True;
    Static[StaticTeam1BG].Visible := True;
    Static[StaticTeam1Deco].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := False;
    Text[TextNameTeam1].Visible := False;
    Static[StaticTeam1].Visible := False;
    Static[StaticTeam1BG].Visible := False;
    Static[StaticTeam1Deco].Visible := False;
  end;

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[1].Score);
    Text[TextNameTeam2].Text := String(ScreenSingModi.TeamInfo.Teaminfo[1].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      Static[StaticTeam2Deco].Texture.TexNum := DecoTex[Placings[1]];
      Static[StaticTeam2Deco].Texture.ColR := DecoColor[Placings[1]].R;
      Static[StaticTeam2Deco].Texture.ColG := DecoColor[Placings[1]].G;
      Static[StaticTeam2Deco].Texture.ColB := DecoColor[Placings[1]].B;
    end;

    Text[TextScoreTeam2].Visible := True;
    Text[TextNameTeam2].Visible := True;
    Static[StaticTeam2].Visible := True;
    Static[StaticTeam2BG].Visible := True;
    Static[StaticTeam2Deco].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := False;
    Text[TextNameTeam2].Visible := False;
    Static[StaticTeam2].Visible := False;
    Static[StaticTeam2BG].Visible := False;
    Static[StaticTeam2Deco].Visible := False;
  end;

  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(ScreenSingModi.PlayerInfo.Playerinfo[2].Score);
    Text[TextNameTeam3].Text := String(ScreenSingModi.TeamInfo.Teaminfo[2].Name);

    //Set Deco Texture
    if Theme.PartyScore.DecoTextures.ChangeTextures then
    begin
      Static[StaticTeam3Deco].Texture.TexNum := DecoTex[Placings[2]];
      Static[StaticTeam3Deco].Texture.ColR := DecoColor[Placings[2]].R;
      Static[StaticTeam3Deco].Texture.ColG := DecoColor[Placings[2]].G;
      Static[StaticTeam3Deco].Texture.ColB := DecoColor[Placings[2]].B;
    end;

    Text[TextScoreTeam3].Visible := True;
    Text[TextNameTeam3].Visible := True;
    Static[StaticTeam3].Visible := True;
    Static[StaticTeam3BG].Visible := True;
    Static[StaticTeam3Deco].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := False;
    Text[TextNameTeam3].Visible := False;
    Static[StaticTeam3].Visible := False;
    Static[StaticTeam3BG].Visible := False;
    Static[StaticTeam3Deco].Visible := False;
  end;

  MP3VolumeHandler.changed := false;
  if not (Ini.SavePlayback=1) then
    StartPreview
  else
  begin
    Voice := -1;
    StartVoice;
  end;
end;

function TScreenPartyScore.Draw: boolean;
begin
  inherited Draw;

  if MP3VolumeHandler.changed and (MP3VolumeHandler.change_time+TimeSkip<3) then
  begin
    MP3VolumeHandler.change_time := MP3VolumeHandler.change_time + TimeSkip;
    DrawVolumeBar(10, 530, 780, 12, ScreenSong.MP3Volume);
  end else
    MP3VolumeHandler.changed := false;
end;

procedure TScreenPartyScore.StartPreview;
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
          Music.SetMusicVolume (ScreenSong.MP3Volume);
          Music.Play;
        end else
        begin
          Music.Fade(0, ScreenSong.MP3Volume, Ini.PreviewFading);
          Music.Play;
        end;
      end;
    end;
  end;
end;

procedure TScreenPartyScore.StartVoice;
var
  changed:  boolean;
  files:    array of string;
  I:        integer;

begin
  //Music.Close;
  //ScreenSong.SongIndex := -1;
  changed := false;
  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    if (ActualRound<Length(PlaylistMedley.Stats)-1) and (Voice <> ActualRound)  then
    begin
      Voice := ActualRound;
      changed := true;
      SetLength(files, PlaylistMedley.NumPlayer);
      for I := 0 to Length(files) - 1 do
        files[I] := PlaylistMedley.Stats[Voice].Player[I].VoiceFile;
    end;
  end else
  begin
    Voice := 0;
    changed := true;
    SetLength(files, PlayersPlay);
    for I := 0 to Length(files) - 1 do
      files[I] := Player[I].VoiceFile;
  end;

  if changed then
  begin
    Music.VoicesClose;
    if (Music.VoicesOpen(files)>0) then
      Music.VoicesPlay;
  end;
end;

procedure TScreenPartyScore.SetAnimationProgress(Progress: real);
begin
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 1) then
    Static[StaticTeam1].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[0].Percentage / 100;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 2) then
    Static[StaticTeam2].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[1].Percentage / 100;
  if (ScreenSingModi.PlayerInfo.NumPlayers >= 3) then
    Static[StaticTeam3].Texture.ScaleW := Progress * ScreenSingModi.PlayerInfo.Playerinfo[2].Percentage / 100;
end;

end.
