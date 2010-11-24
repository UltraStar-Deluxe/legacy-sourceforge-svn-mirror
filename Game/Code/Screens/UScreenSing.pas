unit UScreenSing;

interface

uses UMenu, UMusic, SDL, SysUtils, UFiles, UTime, USongs, UIni, ULog, UTexture, ULyrics,
  TextGL, gl, BASS, UThemes, UGraphicClasses, UVideo;

type
  THandler = record
    changed:  boolean;
    change_time:  real;
  end;

  TPlayerSkin = record
    StaticP:        real;
    StaticScoreBG : real;
    TextP:          real;
    TextPScore:     real;
  end;

  TSaveCoords = record
    Singbar:  array[1..6] of real;
    PlayerS:  array[1..6] of TPlayerSkin;
  end;

  TScreenSing = class(TMenu)
    protected
      PauseTime: Real;
      WebCam: boolean; //Try WebCam?
      paused: boolean; //Pause Mod
    public
      NumEmptySentences: array [0..1] of integer;
      //TextTime:           integer;
      MP3Volume:        integer;
      MP3VolumeHandler: THandler;

      //Lyric bars
      StaticLyricBar:     integer;
      StaticLyricDuetBar: integer;

      //TimeBar mod
      StaticTimeProgress:  integer;
      TextTimeText:        integer;
      //eoa TimeBar mod

      SaveCoords:         TSaveCoords;

      StaticP1:           integer;
      StaticP1ScoreBG:    integer;
      TextP1:             integer;
      TextP1Score:        integer;

      Alpha:  TAlpha;

      //moveable singbar mod
      StaticP1SingBar:         integer;
      StaticP1ThreePSingBar:   integer;
      StaticP1TwoPSingBar:     integer;
      StaticP2RSingBar:        integer;
      StaticP2MSingBar:        integer;
      StaticP3SingBar:         integer;

      StaticP3FourPSingbar:    integer;
      StaticP4FourPSingbar:    integer;
      StaticP4SixPSingbar:     integer;
      StaticP5Singbar:         integer;
      StaticP6Singbar:         integer;
      //eoa moveable singbar

      //Added for ps3 skin
      //shown when game is in 2/4 player modus
      StaticP1TwoP:           integer;
      StaticP1TwoPScoreBG:    integer;
      TextP1TwoP:             integer;
      TextP1TwoPScore:        integer;
      //shown when game is in 3/6 player modus
      StaticP1ThreeP:           integer;
      StaticP1ThreePScoreBG:    integer;
      TextP1ThreeP:             integer;
      TextP1ThreePScore:        integer;
      //eoa

      StaticP2R:          integer;
      StaticP2RScoreBG:   integer;
      TextP2R:            integer;
      TextP2RScore:       integer;

      StaticP2M:          integer;
      StaticP2MScoreBG:   integer;
      TextP2M:            integer;
      TextP2MScore:       integer;

      StaticP3R:          integer;
      StaticP3RScoreBG:   integer;
      TextP3R:            integer;
      TextP3RScore:       integer;

      // 4 and 6 player on one screen
      P4Mode:               boolean;
      StaticP3FourP:        integer;
      StaticP3FourPScoreBG: integer;
      TextP3FourP:          integer;
      TextP3FourPScore:     integer;

      StaticP4FourP:        integer;
      StaticP4FourPScoreBG: integer;
      TextP4FourP:          integer;
      TextP4FourPScore:     integer;

      StaticP4SixP:         integer;
      StaticP4SixPScoreBG:  integer;
      TextP4SixP:           integer;
      TextP4SixPScore:      integer;

      StaticP5:           integer;
      StaticP5ScoreBG:    integer;
      TextP5:             integer;
      TextP5Score:        integer;

      StaticP6:           integer;
      StaticP6ScoreBG:    integer;
      TextP6:             integer;
      TextP6Score:        integer;

      Tex_Background:     TTexture;
      FadeOut:            boolean;
      LyricMain:          array [0..1] of TLyric;
      LyricSub:           array [0..1] of TLyric;

      //VideoAspect
      VideoAspectText:    integer;
      VideoAspectStatic:  integer;
      AspectHandler:      THandler;

      //for Medley
      SongNameStatic:     integer;
      SongNameText:       integer;
      MedleyHandler:      THandler;

      MedleyStart, MedleyEnd: real;
      StartNote, EndNote:     TPos;

      constructor Create; override;
      procedure onShow; override;
      procedure onShowFinish; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      function GetHelpID(): string;

      procedure Finish; virtual;
      procedure Pause; //Pause Mod(Toggles Pause)

      procedure SetVis();

      //OnSentenceEnd for LineBonus + Singbar
      procedure onSentenceEnd(CP: integer; S: Cardinal);
      //OnSentenceChange (for Golden Notes)
      procedure onSentenceChange(CP: integer; S: Cardinal);

      procedure SongError();
      procedure LoadNextSong;
      procedure UpdateMedleyStats(medley_end: boolean);
      procedure DrawMedleyCountdown();

      procedure SetLyricFontMain(Lyric: TLyric);
      procedure SetLyricFontSub(Lyric: TLyric);
  end;

const
  ID='ID_023';   //for help system

implementation
uses UGraphic, UDataBase, UDraw, UMain, Classes, URecord, ULanguage, UHelp, math,
  UPartyM2, UParty, UWebCam;


function TScreenSing.GetHelpID(): string;
begin
  Result := ID;
end;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSing.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      //MP3-Volume Up
      SDLK_PAGEUP:
        begin
          if (MP3Volume<100) then
          begin
            MP3Volume := MP3Volume+5;
            Music.SetMusicVolume(MP3Volume);
          end;
          MP3VolumeHandler.changed := true;
          MP3VolumeHandler.change_time := 0;
        end;

      //MP3-Volume Down
      SDLK_PAGEDOWN:
        begin
          if (MP3Volume>0) then
          begin
            MP3Volume := MP3Volume-5;
            Music.SetMusicVolume(MP3Volume);
          end;
          MP3VolumeHandler.changed := true;
          MP3VolumeHandler.change_time := 0;
        end;

      SDLK_K:
        begin
          if Music.VocalRemoverActivated() then
            Music.DisableVocalRemover
          else
            Music.EnableVocalRemover;
        end;

      SDLK_N:
        begin
          if (ScreenSong.Mode <> smNormal) then
            Exit;
            
          Inc(ShowNotes);
          if (ShowNotes>4) then
            ShowNotes:=0;
        end;

      SDLK_TAB:
        begin
          if not paused then
            Pause;
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_A:
        begin
          ToggleAspectCorrection;
          AspectHandler.changed := true;
          AspectHandler.change_time := Czas.Teraz;
          Static[VideoAspectStatic].Visible := true;
          case UVideo.fAspectCorrection of
            acoStretch: Text[VideoAspectText].Text := Language.Translate('VIDEO_ASPECT_STRETCH');
            acoCrop: Text[VideoAspectText].Text := Language.Translate('VIDEO_ASPECT_CROP');
            acoLetterBox: Text[VideoAspectText].Text := Language.Translate('VIDEO_ASPECT_LETTER_BOX');
          end;
          DataBase.SetAspect(AktSong.Artist, AktSong.Title, integer(UVideo.fAspectCorrection));
          Text[VideoAspectText].Visible := true;
        end;
      SDLK_Q:
        begin
          //When not ask before Exit then Finish now
          if (Ini.AskbeforeDel <> 1) then
            Finish
          //else just Pause and let the Popup make the Work  
          else if not paused then
            Pause;
          
          Result := false;
        end;
      SDLK_ESCAPE:
        begin
          //Record Sound Hack:
          //Sound[0].BufferLong
          if (ScreenSong.Mode=smMedley) or ScreenSong.PartyMedley then
            PlaylistMedley.CurrentMedleySong:=PlaylistMedley.NumMedleySongs+1;

          Finish;
          FadeOut := true;
          Music.PlayBack;

          if ScreenSong.Mode<>smParty then
            FadeTo(@ScreenScore);
        end;

      SDLK_P://Pause Mod
        begin
          Pause;
        end;

      SDLK_S:
        begin
          if (ScreenSong.Mode = smNormal) then
            Ini.PossibleScore := (Ini.PossibleScore+1) mod 4;
          //Ini.Save;
        end;

      SDLK_W:
        begin
          if WebCam then
            WebCam := false
          else
            WebCam := wStartWebCam();
        end;
        
      SDLK_RETURN:
        begin
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN :
        begin
        end;
      SDLK_UP :
        begin
        end;
    end;
  end;
end;

//Pause Mod
procedure TScreenSing.Pause;
begin
  if not paused then  //Pause einschalten
  begin
    PauseTime := Czas.Teraz;
    Paused := true;
    //stop Music
    Music.Pause;
    if (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then //Video
      acTogglePause; //Video
  end
  else            //Pause ausschalten
  begin
    Czas.Teraz := PauseTime; //Position of Notes
    Music.MoveTo (PauseTime + Ini.LipSync*0.01);//Position of Music
    Music.Play; //Play Music
    if (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then //Video
      acTogglePause;
    Paused := false;
  end;
end;
//Pause Mod End

constructor TScreenSing.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.Sing);

  StaticLyricBar := AddStatic(Theme.Sing.StaticLyricBar);
  StaticLyricDuetBar := AddStatic(Theme.Sing.StaticLyricDuetBar);

  //TimeBar mod
    StaticTimeProgress :=  AddStatic(Theme.Sing.StaticTimeProgress);
    TextTimeText       :=  AddText(Theme.Sing.TextTimeText);
  //eoa TimeBar mod

  //moveable singbar mod
  StaticP1SingBar         := AddStatic(Theme.Sing.StaticP1SingBar);
  StaticP1ThreePSingBar   := AddStatic(Theme.Sing.StaticP1ThreePSingBar);
  StaticP1TwoPSingBar     := AddStatic(Theme.Sing.StaticP1TwoPSingBar);
  StaticP2RSingBar        := AddStatic(Theme.Sing.StaticP2RSingBar);
  StaticP2MSingBar        := AddStatic(Theme.Sing.StaticP2MSingBar);
  StaticP3SingBar         := AddStatic(Theme.Sing.StaticP3SingBar);

  StaticP3FourPSingbar    := AddStatic(Theme.Sing.StaticP3FourPSingbar);
  StaticP4FourPSingbar    := AddStatic(Theme.Sing.StaticP4FourPSingbar);
  StaticP4SixPSingbar     := AddStatic(Theme.Sing.StaticP4SixPSingbar);
  StaticP5Singbar         := AddStatic(Theme.Sing.StaticP5Singbar);
  StaticP6Singbar         := AddStatic(Theme.Sing.StaticP6Singbar);

  //save coordinates
  SaveCoords.Singbar[1] := Theme.Sing.StaticP1SingBar.X;
  SaveCoords.Singbar[2] := Theme.Sing.StaticP1TwoPSingBar.X;
  SaveCoords.Singbar[3] := Theme.Sing.StaticP1ThreePSingBar.X;
  SaveCoords.Singbar[4] := Theme.Sing.StaticP2RSingBar.X;
  SaveCoords.Singbar[5] := Theme.Sing.StaticP2MSingBar.X;
  SaveCoords.Singbar[6] := Theme.Sing.StaticP3SingBar.X;
  //eoa moveable singbar

  StaticP1 := AddStatic(Theme.Sing.StaticP1);
  StaticP1ScoreBG := AddStatic(Theme.Sing.StaticP1ScoreBG);
  TextP1 := AddText(Theme.Sing.TextP1);
  TextP1Score := AddText(Theme.Sing.TextP1Score);

  //Added for ps3 skin
  //This one is shown in 2/4P mode
  StaticP1TwoP := AddStatic(Theme.Sing.StaticP1TwoP);
  StaticP1TwoPScoreBG := AddStatic(Theme.Sing.StaticP1TwoPScoreBG);
  TextP1TwoP := AddText(Theme.Sing.TextP1TwoP);
  TextP1TwoPScore := AddText(Theme.Sing.TextP1TwoPScore);

  //This one is shown in 3/6P mode
  StaticP1ThreeP := AddStatic(Theme.Sing.StaticP1ThreeP);
  StaticP1ThreePScoreBG := AddStatic(Theme.Sing.StaticP1ThreePScoreBG);
  TextP1ThreeP := AddText(Theme.Sing.TextP1ThreeP);
  TextP1ThreePScore := AddText(Theme.Sing.TextP1ThreePScore);
  //eoa

  StaticP2R := AddStatic(Theme.Sing.StaticP2R);
  StaticP2RScoreBG := AddStatic(Theme.Sing.StaticP2RScoreBG);
  TextP2R := AddText(Theme.Sing.TextP2R);
  TextP2RScore := AddText(Theme.Sing.TextP2RScore);

  StaticP2M := AddStatic(Theme.Sing.StaticP2M);
  StaticP2MScoreBG := AddStatic(Theme.Sing.StaticP2MScoreBG);
  TextP2M := AddText(Theme.Sing.TextP2M);
  TextP2MScore := AddText(Theme.Sing.TextP2MScore);

  StaticP3R := AddStatic(Theme.Sing.StaticP3R);
  StaticP3RScoreBG := AddStatic(Theme.Sing.StaticP3RScoreBG);
  TextP3R := AddText(Theme.Sing.TextP3R);
  TextP3RScore := AddText(Theme.Sing.TextP3RScore);

  // 4 and 6 player on one screen:
  StaticP3FourP := AddStatic(Theme.Sing.StaticP3FourP);
  StaticP3FourPScoreBG := AddStatic(Theme.Sing.StaticP3FourPScoreBG);
  TextP3FourP := AddText(Theme.Sing.TextP3FourP);
  TextP3FourPScore := AddText(Theme.Sing.TextP3FourPScore);

  StaticP4FourP := AddStatic(Theme.Sing.StaticP4FourP);
  StaticP4FourPScoreBG := AddStatic(Theme.Sing.StaticP4FourPScoreBG);
  TextP4FourP := AddText(Theme.Sing.TextP4FourP);
  TextP4FourPScore := AddText(Theme.Sing.TextP4FourPScore);

  StaticP4SixP := AddStatic(Theme.Sing.StaticP4SixP);
  StaticP4SixPScoreBG := AddStatic(Theme.Sing.StaticP4SixPScoreBG);
  TextP4SixP := AddText(Theme.Sing.TextP4SixP);
  TextP4SixPScore := AddText(Theme.Sing.TextP4SixPScore);

  StaticP5 := AddStatic(Theme.Sing.StaticP5);
  StaticP5ScoreBG := AddStatic(Theme.Sing.StaticP5ScoreBG);
  TextP5 := AddText(Theme.Sing.TextP5);
  TextP5Score := AddText(Theme.Sing.TextP5Score);

  StaticP6 := AddStatic(Theme.Sing.StaticP6);
  StaticP6ScoreBG := AddStatic(Theme.Sing.StaticP6ScoreBG);
  TextP6 := AddText(Theme.Sing.TextP6);
  TextP6Score := AddText(Theme.Sing.TextP6Score);

  //save coordinates
  SaveCoords.PlayerS[1].StaticP := Theme.Sing.StaticP1.X;
  SaveCoords.PlayerS[1].StaticScoreBG := Theme.Sing.StaticP1ScoreBG.X;
  SaveCoords.PlayerS[1].TextP := Theme.Sing.TextP1.X;
  SaveCoords.PlayerS[1].TextPScore := Theme.Sing.TextP1Score.X;

  SaveCoords.PlayerS[2].StaticP := Theme.Sing.StaticP1TwoP.X;
  SaveCoords.PlayerS[2].StaticScoreBG := Theme.Sing.StaticP1TwoPScoreBG.X;
  SaveCoords.PlayerS[2].TextP := Theme.Sing.TextP1TwoP.X;
  SaveCoords.PlayerS[2].TextPScore := Theme.Sing.TextP1TwoPScore.X;

  SaveCoords.PlayerS[3].StaticP := Theme.Sing.StaticP1ThreeP.X;
  SaveCoords.PlayerS[3].StaticScoreBG := Theme.Sing.StaticP1ThreePScoreBG.X;
  SaveCoords.PlayerS[3].TextP := Theme.Sing.TextP1ThreeP.X;
  SaveCoords.PlayerS[3].TextPScore := Theme.Sing.TextP1ThreePScore.X;

  SaveCoords.PlayerS[4].StaticP := Theme.Sing.StaticP2R.X;
  SaveCoords.PlayerS[4].StaticScoreBG := Theme.Sing.StaticP2RScoreBG.X;
  SaveCoords.PlayerS[4].TextP := Theme.Sing.TextP2R.X;
  SaveCoords.PlayerS[4].TextPScore := Theme.Sing.TextP2RScore.X;

  SaveCoords.PlayerS[5].StaticP := Theme.Sing.StaticP2M.X;
  SaveCoords.PlayerS[5].StaticScoreBG := Theme.Sing.StaticP2MScoreBG.X;
  SaveCoords.PlayerS[5].TextP := Theme.Sing.TextP2M.X;
  SaveCoords.PlayerS[5].TextPScore := Theme.Sing.TextP2MScore.X;

  SaveCoords.PlayerS[6].StaticP := Theme.Sing.StaticP3R.X;
  SaveCoords.PlayerS[6].StaticScoreBG := Theme.Sing.StaticP3RScoreBG.X;
  SaveCoords.PlayerS[6].TextP := Theme.Sing.TextP3R.X;
  SaveCoords.PlayerS[6].TextPScore := Theme.Sing.TextP3RScore.X;

  SongNameStatic := AddStatic(Theme.Sing.StaticSongName);
  SongNameText := AddText(Theme.Sing.TextSongName);

  VideoAspectStatic:= AddStatic(Theme.Sing.VideoAspectStatic);
  VideoAspectText:=   AddText(Theme.Sing.VideoAspectText);

  LyricMain[0] := TLyric.Create;
  LyricSub[0] :=  TLyric.Create;
  LyricMain[1] := TLyric.Create;
  LyricSub[1] :=  TLyric.Create;
  UVideo.Init;

  MP3Volume := 100;
  WebCam := false;
end;

procedure TScreenSing.onShow;
var
  NR:       TRecR; //Line Bonus Mod
begin
  Log.LogStatus('Begin', 'onShow');
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSing)');

  if Music.VocalRemoverActivated() then
    Music.DisableVocalRemover;
    
  FadeOut := false; // 0.5.0: early 0.5.0 problems were by this line commented

  AspectHandler.changed := false;
  Text[VideoAspectText].Visible := false;
  Static[VideoAspectStatic].Visible := false;

  MP3VolumeHandler.changed := false;

  ShowNotes := 0;

  //Reset Player Medley stats
  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley  then
  begin
    PlaylistMedley.CurrentMedleySong:=1;

    //max_song_score_medley := round(MAX_SONG_SCORE / NumMedleySongs);
    //max_song_line_bonus_medley := round(MAX_SONG_LINE_BONUS / NumMedleySongs);
    PlaylistMedley.NumPlayer := PlayersPlay;
    SetLength(PlaylistMedley.Stats, 0);
    //max_song_score_medley := round(MAX_SONG_SCORE / PlaylistMedley.NumMedleySongs);
    //max_song_line_bonus_medley := round(MAX_SONG_LINE_BONUS / PlaylistMedley.NumMedleySongs);
  end;

  // prepare players
  SetLength(Player, PlayersPlay);
//  Player[0].ScoreTotalI := 0;

  // positions
  if ((Screens = 1) and (PlayersPlay <= 3)) or (Screens = 2) then
  begin
    P4Mode := false;
    NR.Left := 20;
    NR.Right := 780;

    Theme.Sing.StaticP1SingBar.X := Round(SaveCoords.Singbar[1]);
    Theme.Sing.StaticP1TwoPSingBar.X := Round(SaveCoords.Singbar[2]);
    Theme.Sing.StaticP1ThreePSingBar.X := Round(SaveCoords.Singbar[3]);
    Theme.Sing.StaticP2RSingBar.X := Round(SaveCoords.Singbar[4]);
    Theme.Sing.StaticP2MSingBar.X := Round(SaveCoords.Singbar[5]);
    Theme.Sing.StaticP3SingBar.X := Round(SaveCoords.Singbar[6]);

    Theme.Sing.StaticP1.X := Round(SaveCoords.PlayerS[1].StaticP);
    Theme.Sing.StaticP1ScoreBG.X := Round(SaveCoords.PlayerS[1].StaticScoreBG);
    Theme.Sing.TextP1.X := Round(SaveCoords.PlayerS[1].TextP);
    Theme.Sing.TextP1Score.X := Round(SaveCoords.PlayerS[1].TextPScore);

    Theme.Sing.StaticP1TwoP.X := Round(SaveCoords.PlayerS[2].StaticP);
    Theme.Sing.StaticP1TwoPScoreBG.X := Round(SaveCoords.PlayerS[2].StaticScoreBG);
    Theme.Sing.TextP1TwoP.X := Round(SaveCoords.PlayerS[2].TextP);
    Theme.Sing.TextP1TwoPScore.X := Round(SaveCoords.PlayerS[2].TextPScore);

    Theme.Sing.StaticP1ThreeP.X := Round(SaveCoords.PlayerS[3].StaticP);
    Theme.Sing.StaticP1ThreePScoreBG.X := Round(SaveCoords.PlayerS[3].StaticScoreBG);
    Theme.Sing.TextP1ThreeP.X := Round(SaveCoords.PlayerS[3].TextP);
    Theme.Sing.TextP1ThreePScore.X := Round(SaveCoords.PlayerS[3].TextPScore);

    Theme.Sing.StaticP2R.X := Round(SaveCoords.PlayerS[4].StaticP);
    Theme.Sing.StaticP2RScoreBG.X := Round(SaveCoords.PlayerS[4].StaticScoreBG);
    Theme.Sing.TextP2R.X := Round(SaveCoords.PlayerS[4].TextP);
    Theme.Sing.TextP2RScore.X := Round(SaveCoords.PlayerS[4].TextPScore);

    Theme.Sing.StaticP2M.X := Round(SaveCoords.PlayerS[5].StaticP);
    Theme.Sing.StaticP2MScoreBG.X := Round(SaveCoords.PlayerS[5].StaticScoreBG);
    Theme.Sing.TextP2M.X := Round(SaveCoords.PlayerS[5].TextP);
    Theme.Sing.TextP2MScore.X := Round(SaveCoords.PlayerS[5].TextPScore);

    Theme.Sing.StaticP3R.X := Round(SaveCoords.PlayerS[6].StaticP);
    Theme.Sing.StaticP3RScoreBG.X := Round(SaveCoords.PlayerS[6].StaticScoreBG);
    Theme.Sing.TextP3R.X := Round(SaveCoords.PlayerS[6].TextP);
    Theme.Sing.TextP3RScore.X := Round(SaveCoords.PlayerS[6].TextPScore);
  end else
  begin
    P4Mode := true;

    if (PlayersPlay=6) then
      Ini.PossibleScore := 3;

    NR.Left := 10;
    NR.Right := 390;

    Theme.Sing.StaticP1SingBar.X := Round(SaveCoords.Singbar[1]);
    Theme.Sing.StaticP1TwoPSingBar.X := Round(SaveCoords.Singbar[2]-400);
    Theme.Sing.StaticP1ThreePSingBar.X := Round(SaveCoords.Singbar[3]+Theme.Sing.OFF_P1);
    Theme.Sing.StaticP2RSingBar.X := Round(SaveCoords.Singbar[4]-400);
    Theme.Sing.StaticP2MSingBar.X := Round(SaveCoords.Singbar[5]+Theme.Sing.OFF_P2);
    Theme.Sing.StaticP3SingBar.X := Round(SaveCoords.Singbar[6]+Theme.Sing.OFF_P3);

    Theme.Sing.StaticP1.X := Round(SaveCoords.PlayerS[1].StaticP);
    Theme.Sing.StaticP1ScoreBG.X := Round(SaveCoords.PlayerS[1].StaticScoreBG);
    Theme.Sing.TextP1.X := Round(SaveCoords.PlayerS[1].TextP);
    Theme.Sing.TextP1Score.X := Round(SaveCoords.PlayerS[1].TextPScore);

    Theme.Sing.StaticP1TwoP.X := Round(SaveCoords.PlayerS[2].StaticP);
    Theme.Sing.StaticP1TwoPScoreBG.X := Round(SaveCoords.PlayerS[2].StaticScoreBG-400);
    Theme.Sing.TextP1TwoP.X := Round(SaveCoords.PlayerS[2].TextP);
    Theme.Sing.TextP1TwoPScore.X := Round(SaveCoords.PlayerS[2].TextPScore-400);

    Theme.Sing.StaticP1ThreeP.X := Round(SaveCoords.PlayerS[3].StaticP+Theme.Sing.OFF_P1);
    Theme.Sing.StaticP1ThreePScoreBG.X := Round(SaveCoords.PlayerS[3].StaticScoreBG+Theme.Sing.OFF_P1);
    Theme.Sing.TextP1ThreeP.X := Round(SaveCoords.PlayerS[3].TextP+Theme.Sing.OFF_P1);
    Theme.Sing.TextP1ThreePScore.X := Round(SaveCoords.PlayerS[3].TextPScore+Theme.Sing.OFF_P1);

    Theme.Sing.StaticP2R.X := Round(SaveCoords.PlayerS[4].StaticP);
    Theme.Sing.StaticP2RScoreBG.X := Round(SaveCoords.PlayerS[4].StaticScoreBG-400);
    Theme.Sing.TextP2R.X := Round(SaveCoords.PlayerS[4].TextP);
    Theme.Sing.TextP2RScore.X := Round(SaveCoords.PlayerS[4].TextPScore-400);

    Theme.Sing.StaticP2M.X := Round(SaveCoords.PlayerS[5].StaticP+Theme.Sing.OFF_P2);
    Theme.Sing.StaticP2MScoreBG.X := Round(SaveCoords.PlayerS[5].StaticScoreBG+Theme.Sing.OFF_P2);
    Theme.Sing.TextP2M.X := Round(SaveCoords.PlayerS[5].TextP+Theme.Sing.OFF_P2);
    Theme.Sing.TextP2MScore.X := Round(SaveCoords.PlayerS[5].TextPScore+Theme.Sing.OFF_P2);

    Theme.Sing.StaticP3R.X := Round(SaveCoords.PlayerS[6].StaticP+Theme.Sing.OFF_P3);
    Theme.Sing.StaticP3RScoreBG.X := Round(SaveCoords.PlayerS[6].StaticScoreBG+Theme.Sing.OFF_P3);
    Theme.Sing.TextP3R.X := Round(SaveCoords.PlayerS[6].TextP+Theme.Sing.OFF_P3);
    Theme.Sing.TextP3RScore.X := Round(SaveCoords.PlayerS[6].TextPScore+Theme.Sing.OFF_P3);
  end;

    Static[StaticP1].Texture.X := Theme.Sing.StaticP1.X;
    Static[StaticP1ScoreBG].Texture.X := Theme.Sing.StaticP1ScoreBG.X;
    Text[TextP1].X := Theme.Sing.TextP1.X;
    Text[TextP1Score].X := Theme.Sing.TextP1Score.X;

    Static[StaticP1TwoP].Texture.X := Theme.Sing.StaticP1TwoP.X;
    Static[StaticP1TwoPScoreBG].Texture.X := Theme.Sing.StaticP1TwoPScoreBG.X;
    Text[TextP1TwoP].X := Theme.Sing.TextP1TwoP.X;
    Text[TextP1TwoPScore].X := Theme.Sing.TextP1TwoPScore.X;

    Static[StaticP1ThreeP].Texture.X := Theme.Sing.StaticP1ThreeP.X;
    Static[StaticP1ThreePScoreBG].Texture.X := Theme.Sing.StaticP1ThreePScoreBG.X;
    Text[TextP1ThreeP].X := Theme.Sing.TextP1ThreeP.X;
    Text[TextP1ThreePScore].X := Theme.Sing.TextP1ThreePScore.X;

    Static[StaticP2R].Texture.X := Theme.Sing.StaticP2R.X;
    Static[StaticP2RScoreBG].Texture.X := Theme.Sing.StaticP2RScoreBG.X;
    Text[TextP2R].X := Theme.Sing.TextP2R.X;
    Text[TextP2RScore].X := Theme.Sing.TextP2RScore.X;

    Static[StaticP2M].Texture.X := Theme.Sing.StaticP2M.X;
    Static[StaticP2MScoreBG].Texture.X := Theme.Sing.StaticP2MScoreBG.X;
    Text[TextP2M].X := Theme.Sing.TextP2M.X;
    Text[TextP2MScore].X := Theme.Sing.TextP2MScore.X;

    Static[StaticP3R].Texture.X := Theme.Sing.StaticP3R.X;
    Static[StaticP3RScoreBG].Texture.X := Theme.Sing.StaticP3RScoreBG.X;
    Text[TextP3R].X := Theme.Sing.TextP3R.X;
    Text[TextP3RScore].X := Theme.Sing.TextP3RScore.X;

  NR.Width := NR.Right - NR.Left;
  NR.WMid := NR.Width / 2;
  NR.Mid := NR.Left + NR.WMid;

  SetVis();

  //Set Position of Line Bonus - PhrasenBonus
  if (Ini.LineBonus = 1) then //Show Line Bonus at Scores
  begin
    Case PlayersPlay of
    1: begin
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;
    end;

    2: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1TwoPScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1TwoPScoreBG.X;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1TwoPScore.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.X;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.X;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;
    end;

    3: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1ThreePScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1ThreePScore.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2MScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2MScore.Y + 65;

      //P3
      Player[2].LineBonus_TargetX := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_TargetY := Theme.Sing.TextP3RScore.Y;
      Player[2].LineBonus_StartX  := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_StartY  := Theme.Sing.TextP3RScore.Y + 65;
    end;

    4: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1TwoPScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1TwoPScore.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;

      if P4Mode then
      begin
        //P3
        Player[2].LineBonus_TargetX := Theme.Sing.StaticP3FourPScoreBG.x;
        Player[2].LineBonus_TargetY := Theme.Sing.TextP3FourPScore.Y;
        Player[2].LineBonus_StartX  := Theme.Sing.StaticP3FourPScoreBG.x;
        Player[2].LineBonus_StartY  := Theme.Sing.TextP3FourPScore.Y + 65;

        //P4
        Player[3].LineBonus_TargetX := Theme.Sing.StaticP4FourPScoreBG.x;
        Player[3].LineBonus_TargetY := Theme.Sing.TextP4FourPScore.Y;
        Player[3].LineBonus_StartX  := Theme.Sing.StaticP4FourPScoreBG.x;
        Player[3].LineBonus_StartY  := Theme.Sing.TextP4FourPScore.Y + 65;
      end else
      begin
        //P3
        Player[2].LineBonus_TargetX := Theme.Sing.StaticP1TwoPScoreBG.x;
        Player[2].LineBonus_TargetY := Theme.Sing.TextP1TwoPScore.Y;
        Player[2].LineBonus_StartX  := Theme.Sing.StaticP1TwoPScoreBG.x;
        Player[2].LineBonus_StartY  := Theme.Sing.TextP1TwoPScore.Y + 65;

        //P4
        Player[3].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.x;
        Player[3].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
        Player[3].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.x;
        Player[3].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;
      end;
    end;

    6: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1ThreePScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1ThreePScore.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2MScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2MScore.Y + 65;

      //P3
      Player[2].LineBonus_TargetX := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_TargetY := Theme.Sing.TextP3RScore.Y;
      Player[2].LineBonus_StartX  := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_StartY  := Theme.Sing.TextP3RScore.Y + 65;

      if P4Mode then
      begin
        //P4
        Player[3].LineBonus_TargetX := Theme.Sing.StaticP4SixPScoreBG.x;
        Player[3].LineBonus_TargetY := Theme.Sing.TextP4SixPScore.Y;
        Player[3].LineBonus_StartX  := Theme.Sing.StaticP4SixPScoreBG.x;
        Player[3].LineBonus_StartY  := Theme.Sing.TextP4SixPScore.Y + 65;

        //P5
        Player[4].LineBonus_TargetX := Theme.Sing.StaticP5ScoreBG.x;
        Player[4].LineBonus_TargetY := Theme.Sing.TextP5Score.Y;
        Player[4].LineBonus_StartX  := Theme.Sing.StaticP5ScoreBG.x;
        Player[4].LineBonus_StartY  := Theme.Sing.TextP5Score.Y + 65;

        //P6
        Player[5].LineBonus_TargetX := Theme.Sing.StaticP6ScoreBG.x;
        Player[5].LineBonus_TargetY := Theme.Sing.TextP6Score.Y;
        Player[5].LineBonus_StartX  := Theme.Sing.StaticP6ScoreBG.x;
        Player[5].LineBonus_StartY  := Theme.Sing.TextP6Score.Y + 65;
      end else
      begin
        //P4
        Player[3].LineBonus_TargetX := Theme.Sing.StaticP1ThreePScoreBG.x;
        Player[3].LineBonus_TargetY := Theme.Sing.TextP1ThreePScore.Y;
        Player[3].LineBonus_StartX  := Theme.Sing.StaticP1ThreePScoreBG.x;
        Player[3].LineBonus_StartY  := Theme.Sing.TextP1ThreePScore.Y + 65;

        //P5
        Player[4].LineBonus_TargetX := Theme.Sing.StaticP2MScoreBG.x;
        Player[4].LineBonus_TargetY := Theme.Sing.TextP2MScore.Y;
        Player[4].LineBonus_StartX  := Theme.Sing.StaticP2MScoreBG.x;
        Player[4].LineBonus_StartY  := Theme.Sing.TextP2MScore.Y + 65;

        //P6
        Player[5].LineBonus_TargetX := Theme.Sing.StaticP3RScoreBG.x;
        Player[5].LineBonus_TargetY := Theme.Sing.TextP3RScore.Y;
        Player[5].LineBonus_StartX  := Theme.Sing.StaticP3RScoreBG.x;
        Player[5].LineBonus_StartY  := Theme.Sing.TextP3RScore.Y + 65;
      end;
      end;
    end;
  end else if (Ini.LineBonus = 2) then //Show Line Bonus at Notes
  begin
  //SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
  //SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
  //SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);

  Case PlayersPlay of
    1: begin
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := Skin_P2_NotesB - 105;
    end;

    2: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
    end;

    3: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := 120 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := 120 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := 245 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := 245 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_TargetY := 370 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_StartY  := 370 + 28;
    end;

    4: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;

      if P4Mode then
      begin
        //P3
        Player[2].LineBonus_TargetX := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[2].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
        Player[2].LineBonus_StartX  := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[2].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

        //P4
        Player[3].LineBonus_TargetX := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
        Player[3].LineBonus_StartX  := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
      end else
      begin
        //P3
        Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
        Player[2].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
        Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
        Player[2].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

        //P4
        Player[3].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
        Player[3].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
      end;
    end;

    6: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := 120 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := 120 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := 245 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := 245 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_TargetY := 370 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_StartY  := 370 + 28;

      if P4Mode then
      begin
        //P4
        Player[3].LineBonus_TargetX := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_TargetY := 120 - 65 + 28;
        Player[3].LineBonus_StartX  := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_StartY  := 120 + 28;

        //P5
        Player[4].LineBonus_TargetX := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[4].LineBonus_TargetY := 245 - 65 + 28;
        Player[4].LineBonus_StartX  := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[4].LineBonus_StartY  := 245 + 28;

        //P6
        Player[5].LineBonus_TargetX := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[5].LineBonus_TargetY := 370 - 65 + 28;
        Player[5].LineBonus_StartX  := Round(400 + Nr.Right + 10*ScreenX - 100);
        Player[5].LineBonus_StartY  := 370 + 28;
      end else
      begin
        //P4
        Player[3].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_TargetY := 120 - 65 + 28;
        Player[3].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
        Player[3].LineBonus_StartY  := 120 + 28;

        //P5
        Player[4].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
        Player[4].LineBonus_TargetY := 245 - 65 + 28;
        Player[4].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
        Player[4].LineBonus_StartY  := 245 + 28;

        //P6
        Player[5].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
        Player[5].LineBonus_TargetY := 370 - 65 + 28;
        Player[5].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
        Player[5].LineBonus_StartY  := 370 + 28;
      end;
    end;
  end;
  end;
  //Set Position of Line Bonus - PhrasenBonus End

  if (PlayersPlay=6) then
  begin
    LoadColor(Static[StaticP4SixP].Texture.ColR, Static[StaticP4SixP].Texture.ColG,
      Static[StaticP4SixP].Texture.ColB, 'P4Dark');
    LoadColor(Static[StaticP5].Texture.ColR, Static[StaticP5].Texture.ColG,
      Static[StaticP5].Texture.ColB, 'P5Dark');
    LoadColor(Static[StaticP6].Texture.ColR, Static[StaticP6].Texture.ColG,
      Static[StaticP6].Texture.ColB, 'P6Dark');

    LoadColor(Static[StaticP4SixPScoreBG].Texture.ColR, Static[StaticP4SixPScoreBG].Texture.ColG,
      Static[StaticP4SixPScoreBG].Texture.ColB, 'P4Dark');
    LoadColor(Static[StaticP5ScoreBG].Texture.ColR, Static[StaticP5ScoreBG].Texture.ColG,
      Static[StaticP5ScoreBG].Texture.ColB, 'P5Dark');
    LoadColor(Static[StaticP6ScoreBG].Texture.ColR, Static[StaticP6ScoreBG].Texture.ColG,
      Static[StaticP6ScoreBG].Texture.ColB, 'P6Dark');

    Text[TextP4SixP].Text := 'P4';
    Text[TextP5].Text := 'P5';
    Text[TextP6].Text := 'P6';
  end;

  if (PlayersPlay=4) then
  begin
    LoadColor(Static[StaticP3FourP].Texture.ColR, Static[StaticP3FourP].Texture.ColG,
      Static[StaticP3FourP].Texture.ColB, 'P3Dark');
    LoadColor(Static[StaticP4FourP].Texture.ColR, Static[StaticP4FourP].Texture.ColG,
      Static[StaticP4FourP].Texture.ColB, 'P4Dark');

    LoadColor(Static[StaticP3FourPScoreBG].Texture.ColR, Static[StaticP3FourPScoreBG].Texture.ColG,
      Static[StaticP3FourPScoreBG].Texture.ColB, 'P3Dark');
    LoadColor(Static[StaticP4FourPScoreBG].Texture.ColR, Static[StaticP4FourPScoreBG].Texture.ColG,
      Static[StaticP4FourPScoreBG].Texture.ColB, 'P4Dark');

    Text[TextP3FourP].Text := 'P3';
    Text[TextP4FourP].Text := 'P4';
  end;

  LoadNextSong;

  Log.LogStatus('End', 'onShow');

  if not PerfLog.isActive and (Ini.Debug = 1) then
    PerfLog.StartNewLog;
end;

procedure TScreenSing.SetVis();
var
  V1:       boolean;
  V1TwoP:   boolean; //added for ps3 skin
  V1ThreeP: boolean; //added for ps3 skin
  V2R:      boolean;
  V2M:      boolean;
  V3R:      boolean;
  V3FourP:  boolean;
  V4FourP:  boolean;
  V4SixP:   boolean;
  V5:       boolean;
  V6:       boolean;
  vis:      boolean;
begin
  vis := (ShowNotes<2);

  if not P4Mode and vis then
  begin
    case PlayersPlay of
    1:  begin
          V1       := true;
          V1TwoP   := false;  //added for ps3 skin
          V1ThreeP := false;  //added for ps3 skin
          V2R      := false;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    2:  begin
          V1       := false;
          V1TwoP   := true;  //added for ps3 skin
          V1ThreeP := false; //added for ps3 skin
          V2R      := true;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    3:  begin
          V1       := false;
          V1TwoP   := false; //added for ps3 skin
          V1ThreeP := true;  //added for ps3 skin
          V2R      := false;
          V2M      := true;
          V3R      := true;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    4:  begin // double screen
          V1       := false;
          V1TwoP   := true;  //added for ps3 skin
          V1ThreeP := false; //added for ps3 skin
          V2R      := true;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    6:  begin // double screen
          V1       := false;
          V1TwoP   := false; //added for ps3 skin
          V1ThreeP := true;  //added for ps3 skin
          V2R      := false;
          V2M      := true;
          V3R      := true;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    else begin    //should not happen
          V1       := true;
          V1TwoP   := false;
          V1ThreeP := false;
          V2R      := false;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    end;
  end else if vis then
  begin
    case PlayersPlay of
    1:  begin
          V1       := true;
          V1TwoP   := false;  //added for ps3 skin
          V1ThreeP := false;  //added for ps3 skin
          V2R      := false;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    2:  begin
          V1       := false;
          V1TwoP   := true;  //added for ps3 skin
          V1ThreeP := false; //added for ps3 skin
          V2R      := true;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    3:  begin
          V1       := false;
          V1TwoP   := false; //added for ps3 skin
          V1ThreeP := true;  //added for ps3 skin
          V2R      := false;
          V2M      := true;
          V3R      := true;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    4:  begin
          V1       := false;
          V1TwoP   := true;  //added for ps3 skin
          V1ThreeP := false; //added for ps3 skin
          V2R      := true;
          V2M      := false;
          V3R      := false;
          V3FourP  := true;
          V4FourP  := true;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    6:  begin
          V1       := false;
          V1TwoP   := false; //added for ps3 skin
          V1ThreeP := true;  //added for ps3 skin
          V2R      := false;
          V2M      := true;
          V3R      := true;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := true;
          V5       := true;
          V6       := true;
        end;
    else begin    //should not happen
          V1       := true;
          V1TwoP   := false;
          V1ThreeP := false;
          V2R      := false;
          V2M      := false;
          V3R      := false;
          V3FourP  := false;
          V4FourP  := false;
          V4SixP   := false;
          V5       := false;
          V6       := false;
        end;
    end;
  end;

  if not vis then
  begin
    V1       := false;
    V1TwoP   := false;
    V1ThreeP := false;
    V2R      := false;
    V2M      := false;
    V3R      := false;
    V3FourP  := false;
    V4FourP  := false;
    V4SixP   := false;
    V5       := false;
    V6       := false;
  end;

  //Added for ps3 skin
  //This one is shown in 1P mode
  Static[StaticP1].Visible := V1;
  Static[StaticP1ScoreBG].Visible := V1;
  Text[TextP1].Visible := V1;
  Text[TextP1Score].Visible := V1;

  //This one is shown in 2/4P mode
  Static[StaticP1TwoP].Visible := V1TwoP;
  Static[StaticP1TwoPScoreBG].Visible := V1TwoP;
  Text[TextP1TwoP].Visible := V1TwoP;
  Text[TextP1TwoPScore].Visible := V1TwoP;

  //This one is shown in 3/6P mode
  Static[StaticP1ThreeP].Visible := V1ThreeP and not (PlayersPlay=6);
  Static[StaticP1ThreePScoreBG].Visible := V1ThreeP;
  Text[TextP1ThreeP].Visible := V1ThreeP and not (PlayersPlay=6);
  Text[TextP1ThreePScore].Visible := V1ThreeP;
  //eoa

  Static[StaticP2R].Visible := V2R;
  Static[StaticP2RScoreBG].Visible := V2R;
  Text[TextP2R].Visible := V2R;
  Text[TextP2RScore].Visible := V2R;

  Static[StaticP2M].Visible := V2M and not (PlayersPlay=6);
  Static[StaticP2MScoreBG].Visible := V2M;
  Text[TextP2M].Visible := V2M and not (PlayersPlay=6);
  Text[TextP2MScore].Visible := V2M;

  Static[StaticP3R].Visible := V3R and not (PlayersPlay=6);
  Static[StaticP3RScoreBG].Visible := V3R;
  Text[TextP3R].Visible := V3R and not (PlayersPlay=6);
  Text[TextP3RScore].Visible := V3R;

  Static[StaticP3FourP].Visible := V3FourP;
  Static[StaticP3FourPScoreBG].Visible := V3FourP;
  Text[TextP3FourP].Visible := V3FourP;
  Text[TextP3FourPScore].Visible := V3FourP;

  Static[StaticP4FourP].Visible := V4FourP;
  Static[StaticP4FourPScoreBG].Visible := V4FourP;
  Text[TextP4FourP].Visible := V4FourP;
  Text[TextP4FourPScore].Visible := V4FourP;

  Static[StaticP4SixP].Visible := false and V4SixP;
  Static[StaticP4SixPScoreBG].Visible := V4SixP;
  Text[TextP4SixP].Visible := false and V4SixP;
  Text[TextP4SixPScore].Visible := V4SixP;

  Static[StaticP5].Visible := false and V5;
  Static[StaticP5ScoreBG].Visible := V5;
  Text[TextP5].Visible := false and V5;
  Text[TextP5Score].Visible := V5;

  Static[StaticP6].Visible := false and V6;
  Static[StaticP6ScoreBG].Visible := V6;
  Text[TextP6].Visible := false and V6;
  Text[TextP6Score].Visible := V6;
end;

procedure TScreenSing.SongError;
var
  I, len:  integer;

begin
  if (not ScreenSong.PartyMedley) and (ScreenSong.Mode <> smMedley) then
  begin
    //Error Loading Song -> Go back to Song Screen and Show some Error Message

    FadeTo(@ScreenSong);
    ScreenSong.SongIndex := -1;
    ScreenSong.onShow;
    if (ScreenSong.Mode = smParty) then
      ScreenSong.SelectRandomSong;

    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    Exit;
  end else if (ScreenSong.PartyMedley or (ScreenSong.Mode = smMedley))  then
  begin
    if (PlaylistMedley.CurrentMedleySong<PlaylistMedley.NumMedleySongs) then
    begin
      //Error Loading Song in Medley Mode -> skip actual Medley Song an go on if possible
      len := Length(PlaylistMedley.Song);
      for I := PlaylistMedley.CurrentMedleySong-1 to len - 1 do
        PlaylistMedley.Song[I] := PlaylistMedley.Song[I+1];

      SetLength(PlaylistMedley.Song, Len-1);
      Dec(PlaylistMedley.NumMedleySongs);
      LoadNextSong;
      Exit;
    end else
    begin
      if (PlaylistMedley.NumMedleySongs=1) then
      begin
        //Error Loading Song in Medley Mode -> Go back to Song Screen and Show some Error Message
        FadeTo(@ScreenSong);
        ScreenSong.SongIndex := -1;
        ScreenSong.onShow;
        ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
        Exit;
      end else
      begin
        //Error Loading Song in Medley Mode -> Finish actual round
        len := Length(PlaylistMedley.Song);
        SetLength(PlaylistMedley.Song, len-1);
        Dec(PlaylistMedley.NumMedleySongs);
        Finish;
        Exit;
      end;
    end;
  end;
end;

procedure TScreenSing.SetLyricFontMain(Lyric: TLyric);
begin
  // set custom options
  case Ini.LyricsFont of
    0:
      begin
        Lyric.FontStyle := 0;
        Lyric.Size := 14; // 13
        Lyric.ColR := Skin_FontR;
        Lyric.ColG := Skin_FontG;
        Lyric.ColB := Skin_FontB;
        Lyric.ColSR := 5/255; //26
        Lyric.ColSG := 163/255; //165
        Lyric.ColSB := 210/255;  //220
      end;
    1:
      begin
        Lyric.FontStyle := 2;
        Lyric.Size := 14;
        Lyric.ColR := 0.6;
        Lyric.ColG := 0.6;
        Lyric.ColB := 1;
        Lyric.ColSR := 0.25;
        Lyric.ColSG := 0.25;
        Lyric.ColSB := 1;
      end;
    2:
      begin
        Lyric.FontStyle := 3;
        Lyric.Size := 12;
        Lyric.ColR := 0.6;
        Lyric.ColG := 0.6;
        Lyric.ColB := 1;
        Lyric.ColSR := 0.25;
        Lyric.ColSG := 0.25;
        Lyric.ColSB := 1;
      end;
    end; // case

    case Ini.LyricsEffect of
      0:  Lyric.Style := 1; // 0 - one selected, 1 - selected all to the current
      1:  Lyric.Style := 2;
      2:  Lyric.Style := 3;
      3:  Lyric.Style := 4;
    end; // case
end;

procedure TScreenSing.SetLyricFontSub(Lyric: TLyric);
begin
  // set custom options
  case Ini.LyricsFont of
    0:
      begin
        Lyric.FontStyle := 0;
        Lyric.Size := 14; // 13
        Lyric.ColR := 0.4; //0.6
        Lyric.ColG := 0.4; //0.6
        Lyric.ColB := 0.4; //0.6
      end;
    1:
      begin
        Lyric.FontStyle := 2;
        Lyric.Size := 14;
        Lyric.ColR := 0.8;
        Lyric.ColG := 0.8;
        Lyric.ColB := 0.8;
      end;
    2:
      begin
        Lyric.FontStyle := 3;
        Lyric.Size := 12;
        Lyric.ColR := 0.8;
        Lyric.ColG := 0.8;
        Lyric.ColB := 0.8;
      end;
    end; // case
end;

procedure TScreenSing.LoadNextSong;
var
  P, I:       integer;
  C, N, beat: integer;
  error:      boolean;

begin
  // load notes
  ResetSingTemp;
  PlaylistMedley.ApplausePlayed := false;
  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    if (length(PlaylistMedley.Song)>=PlaylistMedley.CurrentMedleySong) then
    begin
      CatSongs.Selected := PlaylistMedley.Song[PlaylistMedley.CurrentMedleySong-1];
      Music.Open(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].Mp3);
    end else
    begin
      SongError;
      Exit;
    end;
  end else
  begin
    for I := 0 to PlayersPlay - 1 do
      Player[I].VoiceFile := '';
  end;

  if not Music.isOpen then
    Music.Open(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].Mp3);

  AktSong := CatSongs.Song[CatSongs.Selected];
  try
    if not LoadSong(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].FileName, SONG_LOAD_COMPLETE) then
    begin
      SongError;
      Exit;
    end;

  except
    SongError;
    Exit;
  end;
  
  AktSong.Path := CatSongs.Song[CatSongs.Selected].Path;

  error := false;
  for P := 0 to Length(Czesci) - 1 do
  begin
    C := Length(Czesci[P].Czesc)-1;
    N := Length(Czesci[P].Czesc[C].Nuta)-1;
    beat := Czesci[P].Czesc[C].Nuta[N].Start + Czesci[P].Czesc[C].Nuta[N].Dlugosc;
    if (Music.Length < GetTimeFromBeat(beat)) then
      error := true;
  end;

  if error then
  begin
    Log.LogError('Error: TXT is longer than the MP3 in Song: ' + AktSong.Path + AktSong.Filename);
    SongError;
    Exit;
  end;

  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    SetMedleyMode;
    Text[SongNameText].Text := IntToStr(PlaylistMedley.CurrentMedleySong) +
      '/' + IntToStr(PlaylistMedley.NumMedleySongs) + ': ' +
      AktSong.Artist + ' - ' + AktSong.Title;

    //medley start and end timestamps
    StartNote := FindNote(AktSong.Medley.StartBeat - round(AktSong.BPM[0].BPM*AktSong.Medley.FadeIn_time/60));
    MedleyStart := GetTimeFromBeat(Czesci[0].Czesc[StartNote.line].Nuta[0].Start);

    //check Medley-Start
    if (MedleyStart+AktSong.Medley.FadeIn_time*0.5>GetTimeFromBeat(AktSong.Medley.StartBeat)) then
      MedleyStart := GetTimeFromBeat(AktSong.Medley.StartBeat) - AktSong.Medley.FadeIn_time;
    if MedleyStart<0 then
      MedleyStart := 0;

    MedleyEnd := GetTimeFromBeat(AktSong.Medley.EndBeat) + AktSong.Medley.FadeOut_time;
  end;

  // set movie
  if (Ini.MovieSize<2) and (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then
  begin
    acOpenFile(PAnsiChar(AktSong.Path + AktSong.Video));

    if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
      acSkip2(AktSong.VideoGAP, MedleyStart)
    else
      acSkip2(AktSong.VideoGAP, AktSong.Start);

    if (UVideo.VideoOpened) then
      AktSong.VideoLoaded := true;

    UVideo.SetAspectCorrection(TAspectCorrection(
      DataBase.GetAspect(AktSong.Artist, AktSong.Title, Ini.AspectCorrect)));
  end;

  // set background
  if (AktSong.Background <> '')  and (AktSong.VideoLoaded = false) then
    try
      Tex_Background := Texture.LoadTexture(AktSong.Path + AktSong.Background);
    except
      log.LogError('Background could not be loaded: ' + AktSong.Path + AktSong.Background);
      Tex_Background.TexNum := -1;
    end
  else
    Tex_Background.TexNum := -1;

  // play music+timer (I)
  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    Music.MoveTo(MedleyStart);
    Czas.Teraz := MedleyStart;
    Czas.Razem := MedleyEnd;
  end else
  begin
    Music.MoveTo(AktSong.Start);

    Czas.Teraz := AktSong.Start;
    Czas.Razem := Music.Length;
    if (AktSong.Finish > 0) then
      Czas.Razem := AktSong.Finish / 1000;
  end;

  Czas.AktBeat := 0;
  Czas.AktBeatC := 0;
  Czas.AktBeatD := 0;

  // main text
  LyricMain[0].Clear;
  LyricMain[0].X := 400;
  LyricMain[0].Y := Skin_LyricsT;
  LyricMain[0].Scale := 1.4; //1.4
  LyricMain[0].Align := 1;

  // sub text
  LyricSub[0].Clear;
  LyricSub[0].X := 400;
  LyricSub[0].Y := Skin_LyricsT + 35; //42 //40
  LyricSub[0].Align := 1;

  SetLyricFontMain(LyricMain[0]);
  SetLyricFontSub(LyricSub[0]);

  if AktSong.isDuet then
  begin
    // main text
    LyricMain[1].Clear;
    LyricMain[1].X := 400;
    LyricMain[1].Y := Skin_LyricsT;
    LyricMain[0].Y := 5{Skin_LyricsT};
    LyricMain[1].Scale := 1.4;
    LyricMain[1].Align := 1;

    // sub text
    LyricSub[1].Clear;
    LyricSub[1].X := 400;
    LyricSub[0].Y := 5{Skin_LyricsT} + 35;
    LyricSub[1].Y := Skin_LyricsT + 35;
    LyricSub[1].Align := 1;

    SetLyricFontMain(LyricMain[1]);
    SetLyricFontSub(LyricSub[1]);

    Static[StaticLyricDuetBar].Visible := true;
  end else
    Static[StaticLyricDuetBar].Visible := false;

  Czas.OldBeat := -1;


  for P := 0 to High(Player) do
    ClearScores(P);

  // fill texts
  LyricMain[0].AddCzesc(0, 0);
  LyricMain[0].Selected := -1;
  LyricSub[0].AddCzesc(0, 1);
  LyricSub[0].Selected := -1;

  if AktSong.isDuet then
  begin
    // fill texts
    LyricMain[1].AddCzesc(1, 0);
    LyricMain[1].Selected := -1;
    LyricSub[1].AddCzesc(1, 1);
    LyricSub[1].Selected := -1;
  end;

  //Deactivate Pause
  Paused := False;

  //Kill all Stars not Killed yet
  //GoldenStarsTwinkle Mod
  GoldenRec.SentenceChange(0);
  if AktSong.isDuet then
    GoldenRec.SentenceChange(1);
  //GoldenStarsTwinkle Mod End

  //Set Num of Empty Sentences for Phrasen Bonus
  NumEmptySentences[0] := 0;
  NumEmptySentences[1] := 0;

  for I := 0 to Length(Czesci) - 1 do
  begin
    for P := low(Czesci[I].Czesc) to high(Czesci[I].Czesc) do
      if Czesci[I].Czesc[P].TotalNotes = 0 then Inc(NumEmptySentences[I]);
  end;

  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    MedleyHandler.changed := true;
    MedleyHandler.change_time := Czas.Teraz;

    Static[SongNameStatic].Visible := true;
    Text[SongNameText].Visible := true;
  end else
  begin
    Static[SongNameStatic].Visible := false;
    Text[SongNameText].Visible := false;
  end;

  Music.CaptureStart;

  for I := 0 to 3 - 1 do
    Alpha[I] := 0;
  
  if ((ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley)
    and (PlaylistMedley.CurrentMedleySong>1) then
    onShowFinish;
end;

procedure TScreenSing.onShowFinish;
begin
  // play movie (II)
  if AktSong.VideoLoaded then
  begin
    try
      acGetFrame(Czas.Teraz);
    except
      //If an Error occurs Reading Video: prevent Video from being Drawn again and Close Video
      AktSong.VideoLoaded := False;
      Log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
      Log.LogError('Corrupted File: ' + AktSong.Video);
      try
        acClose;
      except

      end;
    end;
  end;

  if not FGrabFrameFlag and (Ini.EnableWebCam=1) then
  begin
    //Display White Activating WebCam Text
    SetFontStyle(2); //Font: Outlined1
    SetFontSize(12);
    SetFontItalic(False);
    SetFontPos (400 - glTextWidth ('Activating Webcam ...')/2, 250); //Position
    glColor4f(1,1,1,1);
    glPrint('Activating Webcam ...');
    SwapBuffers;    
    wStartWebCam();
  end;

  // play music (II)
  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
    Music.Fade(10, MP3Volume, AktSong.Medley.FadeIn_time)
  else
    Music.SetMusicVolume(MP3Volume);

  Music.Play;

  // prepare timer (II)
  CountSkipTimeSet;
end;

function TScreenSing.Draw: boolean;
Const
  dt = 5;

var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  sung:   boolean;
  Flash:  real;
  S:      integer;
  T:      integer;
  I, J, K:integer;
  ab:     real;
  tt:     real;
  lastLine, LastWord:     integer;
  medley_end:             boolean;
  medley_start_applause:  boolean;
  CurTime:                real;
begin
  PerfLog.AddComment('ScreenSing: Start Draw');

  SetVis;
  //ScoreBG Mod
  // set player colors
  if PlayersPlay = 4 then
  begin
    if ScreenAct = 1 then
    begin
      LoadColor(Static[StaticP1TwoP].Texture.ColR, Static[StaticP1TwoP].Texture.ColG,
      Static[StaticP1TwoP].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
      Static[StaticP2R].Texture.ColB, 'P2Dark');

      LoadColor(Static[StaticP1TwoPScoreBG].Texture.ColR, Static[StaticP1TwoPScoreBG].Texture.ColG,
      Static[StaticP1TwoPScoreBG].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2RScoreBG].Texture.ColR, Static[StaticP2RScoreBG].Texture.ColG,
      Static[StaticP2RScoreBG].Texture.ColB, 'P2Dark');
    end;

    if ScreenAct = 2 then
    begin
      LoadColor(Static[StaticP1TwoP].Texture.ColR, Static[StaticP1TwoP].Texture.ColG,
        Static[StaticP1TwoP].Texture.ColB, 'P3Dark');
      LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P4Dark');

      LoadColor(Static[StaticP1TwoPScoreBG].Texture.ColR, Static[StaticP1TwoPScoreBG].Texture.ColG,
        Static[StaticP1TwoPScoreBG].Texture.ColB, 'P3Dark');
      LoadColor(Static[StaticP2RScoreBG].Texture.ColR, Static[StaticP2RScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P4Dark');
    end;
  end;

  if PlayersPlay = 6 then
  begin
    if ScreenAct = 1 then
    begin
      LoadColor(Static[StaticP1ThreeP].Texture.ColR, Static[StaticP1ThreeP].Texture.ColG,
        Static[StaticP1ThreeP].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P2Dark');
      LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
        Static[StaticP3R].Texture.ColB, 'P3Dark');

      LoadColor(Static[StaticP1ThreePScoreBG].Texture.ColR, Static[StaticP1ThreePScoreBG].Texture.ColG,
        Static[StaticP1ThreePScoreBG].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2MScoreBG].Texture.ColR, Static[StaticP2MScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P2Dark');
      LoadColor(Static[StaticP3RScoreBG].Texture.ColR, Static[StaticP3RScoreBG].Texture.ColG,
        Static[StaticP3RScoreBG].Texture.ColB, 'P3Dark');
    end;

    if ScreenAct = 2 then
    begin
      LoadColor(Static[StaticP1ThreeP].Texture.ColR, Static[StaticP1ThreeP].Texture.ColG,
        Static[StaticP1ThreeP].Texture.ColB, 'P4Dark');
      LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P5Dark');
      LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
        Static[StaticP3R].Texture.ColB, 'P6Dark');

      LoadColor(Static[StaticP1ThreePScoreBG].Texture.ColR, Static[StaticP1ThreePScoreBG].Texture.ColG,
        Static[StaticP1ThreePScoreBG].Texture.ColB, 'P4Dark');
      LoadColor(Static[StaticP2MScoreBG].Texture.ColR, Static[StaticP2MScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P5Dark');
      LoadColor(Static[StaticP3RScoreBG].Texture.ColR, Static[StaticP3RScoreBG].Texture.ColG,
        Static[StaticP3RScoreBG].Texture.ColB, 'P6Dark');
    end;
  end;

  // set player names (for 2 screens and only Singstar skin)
  if ScreenAct = 1 then begin
    Text[TextP1].Text := 'P1';
    Text[TextP1TwoP].Text := 'P1'; //added for ps3 skin
    Text[TextP1ThreeP].Text := 'P1'; //added for ps3 skin
    Text[TextP2R].Text := 'P2';
    Text[TextP2M].Text := 'P2';
    Text[TextP3R].Text := 'P3';
  end;

  if ScreenAct = 2 then begin
    case PlayersPlay of
{        1:  begin
              Text[TextP1].Text := 'P2';
            end;
        2:  begin
              Text[TextP1].Text := 'P3';
              Text[TextP2R].Text := 'P4';
            end;
        3:  begin
              Text[TextP1].Text := 'P4';
              Text[TextP2M].Text := 'P5';
              Text[TextP3R].Text := 'P6';
            end;}

      4:  begin
            Text[TextP1TwoP].Text := 'P3';
            Text[TextP2R].Text := 'P4';
          end;
      6:  begin
            Text[TextP1ThreeP].Text := 'P4';
            Text[TextP2M].Text := 'P5';
            Text[TextP3R].Text := 'P6';
          end;
    end; // case
  end; // if
  // stereo

// weird stuff, maybe this is for "dual screen?", but where is player three then?
  {Static[StaticP1].Texture.X := Static[StaticP1].Texture.X + 10*ScreenX;
  Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;

  Text[TextP1].X := Text[TextP1].X + 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X + 10*ScreenX;


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X + 10*ScreenX;
  Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;

  Text[TextP2R].X := Text[TextP2R].X + 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X + 10*ScreenX;
// end of weird stuff

  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X + 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X + 10*ScreenX;}

  // update static menu with time ...
  if ScreenSong.Mode <> smMedley then
    CurTime := Czas.Razem - Czas.Teraz
  else
    CurTime := MedleyEnd - Czas.Teraz;

  Min := Round(CurTime) div 60;
  Sec := Round(CurTime) mod 60;
  Text[TextTimeText].Text := '';
  if Min < 10 then Text[TextTimeText].Text := '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Min) + ':';
  if Sec < 10 then Text[TextTimeText].Text := Text[TextTimeText].Text + '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Sec);

  sung := true;
  for I := 0 to Length(Czesci) - 1 do
  begin
    lastLine := Length(Czesci[I].Czesc)-1;
    lastWord := Length(Czesci[I].Czesc[lastLine].Nuta)-1;

    if (lastLine>=0) and (lastWord>=0) then
    begin
      if (Czas.AktBeatD<(Czesci[I].Czesc[lastLine].Nuta[lastWord].Start+
        Czesci[I].Czesc[lastLine].Nuta[lastWord].Dlugosc)) then
        sung := false;
    end;
  end;

  ScreenSong.SungToEnd := sung;

  // for medley-mode:
  CurTime := Czas.Teraz;
  if (ScreenSong.Mode = smMedley) and (CurTime > MedleyEnd) then
    medley_end := true
  else
    medley_end := false;

  if (ScreenSong.Mode = smMedley) and (CurTime >
    GetTimeFromBeat(AktSong.Medley.EndBeat)) then
    medley_start_applause := true
  else
    medley_start_applause := false;

  // .. and scores
  if PlayersPlay = 1 then
  begin
    Tekst := IntToStr(Player[0].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP1Score].Text := Tekst;
  end;

  if PlayersPlay = 2 then
  begin
    Tekst := IntToStr(Player[0].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP1TwoPScore].Text := Tekst;

    Tekst := IntToStr(Player[1].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP2RScore].Text := Tekst;
  end;

  if PlayersPlay = 3 then
  begin
    Tekst := IntToStr(Player[0].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP1ThreePScore].Text := Tekst;

    Tekst := IntToStr(Player[1].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP2MScore].Text := Tekst;

    Tekst := IntToStr(Player[2].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP3RScore].Text := Tekst;
  end;

  if PlayersPlay = 4 then
  begin
    if ScreenAct = 1 then
    begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1TwoPScore].Text := Tekst;

      Tekst := IntToStr(Player[1].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2RScore].Text := Tekst;
    end;

    if ScreenAct = 2 then
    begin
      Tekst := IntToStr(Player[2].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1TwoPScore].Text := Tekst;

      Tekst := IntToStr(Player[3].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2RScore].Text := Tekst;
    end;

    if P4Mode then
    begin
      Tekst := IntToStr(Player[2].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP3FourPScore].Text := Tekst;

      Tekst := IntToStr(Player[3].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP4FourPScore].Text := Tekst;
    end;
  end;

  if PlayersPlay = 6 then
  begin
    if ScreenAct = 1 then
    begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1ThreePScore].Text := Tekst;

      Tekst := IntToStr(Player[1].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2MScore].Text := Tekst;

      Tekst := IntToStr(Player[2].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP3RScore].Text := Tekst;
    end;

    if ScreenAct = 2 then
    begin
      Tekst := IntToStr(Player[3].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1ThreePScore].Text := Tekst;

      Tekst := IntToStr(Player[4].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2MScore].Text := Tekst;

      Tekst := IntToStr(Player[5].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP3RScore].Text := Tekst;
    end;

    if P4Mode then
    begin
      Tekst := IntToStr(Player[3].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP4SixPScore].Text := Tekst;

      Tekst := IntToStr(Player[4].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP5Score].Text := Tekst;

      Tekst := IntToStr(Player[5].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP6Score].Text := Tekst;
    end;
  end;

  // draw static menu (BG)
  DrawBG;
  //Draw Background
  SingDrawBackground;
  // update and draw movie
  if ShowFinish and AktSong.VideoLoaded then
  begin
    try
      acGetFrame(Music.Position + Ini.LipSync*0.01);
      acDrawGL(ScreenAct, not WebCam); // this only draws
    except
      //If an Error occurs drawing: prevent Video from being Drawn again and Close Video
      AktSong.VideoLoaded := False;
      log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
      Log.LogError('Corrupted File: ' + AktSong.Video);
      try
        acClose;
      except

      end;
    end;
  end;

  wDraw(WebCam, ScreenAct);
  // draw static menu (FG)
  if (ShowNotes<4) then
    DrawFG;

  //Medley Countdown
  if ScreenSong.Mode = smMedley then
    DrawMedleyCountdown;
    
  // check for music finish
  // Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(Czas.Teraz*1000) + ' ' + IntToStr(AktSong.Finish));
  if ShowFinish then
  begin
    if (not Music.Finished) and (not medley_end or (ScreenSong.Mode <> smMedley))
      and ((AktSong.Finish = 0) or (Music.Position + Ini.LipSync*0.01 <= AktSong.Finish)) then
    begin
      //Pause Mod:
      if not Paused then
      begin
        Sing(Self);       // analyze song
        //Update Medley Stats
        if (ScreenSong.Mode = smMedley) and not FadeOut then
          UpdateMedleyStats(medley_start_applause);
      end;
    end else
    begin
      if not FadeOut and ((Screens=1) or (ScreenAct = 2)) then
      begin
        Finish;
        if ScreenSong.Mode = smNormal then
        begin
          FadeOut := true;
          FadeTo(@ScreenScore);
        end;
      end;
    end;
  end;

  for I := 0 to Length(Czesci) - 1 do
  begin
    //init K ...
    K := Czesci[I].Akt;

    //find actual line
    for J := 0 to Czesci[I].High do
    begin
      if Czas.AktBeat >= Czesci[I].Czesc[J].Start then
        K := J;
    end;

    //time diff
    ab := GetTimeFromBeat(Czesci[I].Czesc[K].StartNote) - Czas.Teraz;

    //last line
    if (K = Czesci[I].High) then
      ab := Czas.Teraz - GetTimeFromBeat(Czesci[I].Czesc[K].Nuta[Czesci[I].Czesc[K].HighNut].Start+
        Czesci[I].Czesc[K].Nuta[Czesci[I].Czesc[K].HighNut].Dlugosc);

    //lyric main and other nice things
    if (ab>2.3*dt) or ((K = Czesci[I].High) and (ab>dt)) then
    begin
      if (ab>3.3*dt) or (Alpha[I]<1) or (K = Czesci[I].High) then
        Alpha[I] := Alpha[I]-TimeSkip/dt;
      if (Alpha[I]<0) then
        Alpha[I] := 0;
    end else if (ab>dt) then
    begin
      tt := ab-dt;
      if (tt<0) then
        Alpha[I] := 1
      else
      begin
        if (1-tt/dt>Alpha[I]) or (K = Czesci[I].High)then
          Alpha[I] := 1-tt/dt;
      end;
    end else
      Alpha[I] := 1;

    //lyric sub
    if (K < Czesci[I].High) then
    begin
      ab := GetTimeFromBeat(Czesci[I].Czesc[K+1].StartNote) - Czas.Teraz;

      if (ab>2.5*dt) then
        Alpha[I+2] := 0
      else if (ab>dt) then
        Alpha[I+2] := (1-(ab-dt)/dt)
      else
        Alpha[I+2] := 1;
    end;
  end;

  if not AktSong.isDuet then
  begin
    Alpha[1] := Alpha[0];
    Alpha[3] := Alpha[2];
  end;

  Static[StaticLyricBar].Texture.Alpha := Alpha[1];
  if (ShowNotes<3) then
    Static[StaticLyricBar].Visible := true
  else
    Static[StaticLyricBar].Visible := false;

  if AktSong.isDuet then
  begin
    Static[StaticLyricDuetBar].Texture.Alpha := Alpha[0];

    if (ShowNotes<3) then
      Static[StaticLyricDuetBar].Visible := true
    else
      Static[StaticLyricDuetBar].Visible := false;
  end;


  // draw custom items
  SingDraw(Alpha);  // always draw

  //GoldenNoteStarsTwinkle Mod
  if (ShowNotes=0) then
    GoldenRec.SpawnRec(Alpha);
  //GoldenNoteStarsTwinkle Mod

  // back stereo

  // weird stuff, maybe this is for "dual screen?", but where is player three then?
  {Static[StaticP1].Texture.X := Static[StaticP1].Texture.X - 10*ScreenX;
  Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X - 10*ScreenX;

  Text[TextP1].X := Text[TextP1].X - 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X - 10*ScreenX;


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X - 10*ScreenX;
  Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X - 10*ScreenX;

  Text[TextP2R].X := Text[TextP2R].X - 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X - 10*ScreenX;}
  //weird end

  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X - 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10*ScreenX;

  //Aspect
  if AspectHandler.changed and (Czas.Teraz>AspectHandler.change_time + 3) then
  begin
    AspectHandler.changed:=false;
    Text[VideoAspectText].Visible := false;
    Static[VideoAspectStatic].Visible := false;
  end;

  //Medley
  if MedleyHandler.changed and (Czas.Teraz>MedleyHandler.change_time + AktSong.Medley.FadeIn_time) then
  begin
    MedleyHandler.changed:=false;
    Static[SongNameStatic].Visible := false;
    Text[SongNameText].Visible := false;
  end;

  if MP3VolumeHandler.changed and (MP3VolumeHandler.change_time+TimeSkip<3) then
  begin
    MP3VolumeHandler.change_time := MP3VolumeHandler.change_time + TimeSkip;
    DrawVolumeBar(10, 475, 782, 12, MP3Volume);
  end else
    MP3VolumeHandler.changed := false;


  if (Ini.Debug=1) then
  begin
    glEnable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glColor4f(1, 1, 1, 0.6);
    glBegin(GL_QUADS);
      glVertex2f(0, 180);
      glVertex2f(0, 210);
      glVertex2f(400, 210);
      glVertex2f(400, 180);
    glEnd;
    glDisable(GL_BLEND);

    SetFontStyle(1);
    SetFontItalic(false);
    SetFontSize(8);
    glColor4f(0, 0, 0, 1);

    SetFontPos (5, 184);
    glPrint(PChar('lt: ' + FormatFloat('#0.00', Czas.Teraz) +
      ' mt: ' + FormatFloat('#0.00', Music.Position + Ini.LipSync*0.01) +
      ' dt: ' + FormatFloat('#0.000', Czas.Teraz-(Music.Position+Ini.LipSync*0.01))));
  end;
  PerfLog.AddComment('ScreenSing: End Draw');
end;

procedure TScreenSing.UpdateMedleyStats(medley_end: boolean);
var
  len, num, I : integer;

begin
  len := Length(PlaylistMedley.Stats);
  num := PlaylistMedley.NumPlayer;

  if (PlaylistMedley.CurrentMedleySong>len) and
    (PlaylistMedley.CurrentMedleySong<=PlaylistMedley.NumMedleySongs) then
  begin
    inc(len);
    SetLength(PlaylistMedley.Stats, len);
    SetLength(PlaylistMedley.Stats[len-1].Player, num);
    PlaylistMedley.Stats[len-1].SongArtist := AktSong.Artist;
    PlaylistMedley.Stats[len-1].SongTitle := AktSong.Title;
  end;

  if (PlaylistMedley.CurrentMedleySong<=PlaylistMedley.NumMedleySongs) then
    for I := 0 to num - 1 do
      PlaylistMedley.Stats[len-1].Player[I] := Player[I];

  if medley_end and not PlaylistMedley.ApplausePlayed and
    (PlaylistMedley.CurrentMedleySong<=PlaylistMedley.NumMedleySongs) then
  begin
    PlaylistMedley.ApplausePlayed:=true;
    Music.PlayApplause;
    Music.Fade(MP3Volume, 10, AktSong.Medley.FadeOut_time);
  end;
end;

procedure TScreenSing.Finish;
var
  I, J:     integer;
  len, num: integer;
  points:   string;
  scores:   array of string;
  names:    array of string;
  singmode: string;
  Min, Sec: integer;
  CurTime:  real;

begin
  PerfLog.StopLogging;
  Music.CaptureStop;
  Music.Stop;

  if (Ini.SavePlayback = 1) and not FadeOut then
  begin
    Log.BenchmarkStart(0);

    for I := 0 to PlayersPlay - 1 do
    begin
      points := IntToStr(Player[I].ScoreTotalI);
      while Length(points) < 5 do
        points := '0'+points;

      Player[I].VoiceFile := Log.LogVoice(I, Ini.Name[I], AktSong.Artist, AktSong.Title, points);
    end;

    Log.BenchmarkEnd(0);
    Log.LogBenchmark('Creating files', 0);
  end;

  if (Ini.LogSession=1) and not FadeOut then
  begin
    SetLength(scores, PlayersPlay);
    SetLength(names, PlayersPlay);
    for I := 0 to PlayersPlay - 1 do
    begin
      points := IntToStr(Player[I].ScoreTotalI);
      while Length(points) < 5 do
        points := '0'+points;

      scores[I] := points;
      names[I] := Ini.Name[I];
    end;

    if (ScreenSong.Mode = smParty) then
      singmode := PartySession.Plugins[PartySession.Rounds[PartySession.CurRound].PluginNr].Name
    else if (ScreenSong.Mode = smChallenge) then
      singmode := PartySessionM2.Plugins[PartySessionM2.Rounds[PartySessionM2.CurRound].PluginNr].Name;

    if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
    begin
      if (ScreenSong.Mode = smMedley) then
        singmode := 'Medley';

      if (PlaylistMedley.CurrentMedleySong<=PlaylistMedley.NumMedleySongs) then
        singmode := singmode + ' (' + IntToStr(PlaylistMedley.CurrentMedleySong) +
          '/' + IntToStr(PlaylistMedley.NumMedleySongs) + ')'
      else
        singmode := singmode + ' (' + IntToStr(PlaylistMedley.NumMedleySongs) +
          '/' + IntToStr(PlaylistMedley.NumMedleySongs) + ')';
          
    end else if(ScreenSong.Mode = smNormal) then
    begin
      singmode := 'Normal';
      if AktSong.isDuet then
        singmode := singmode + ' (Duet)';
    end;

    if not ScreenSong.SungToEnd then
    begin
      singmode := singmode + ', abort at: ';

      if ScreenSong.Mode <> smMedley then
        CurTime := Czas.Teraz
      else
        CurTime := Czas.Teraz - MedleyStart;

      Min := Round(CurTime) div 60;
      Sec := Round(CurTime) mod 60;

      if Min < 10 then singmode := singmode + '0';
      singmode := singmode + IntToStr(Min) + ':';
      if Sec < 10 then singmode := singmode + '0';
      singmode := singmode + IntToStr(Sec);
    end;

    Log.LogSession(names, scores, AktSong.Artist, AktSong.Title, singmode);
  end;

  if AktSong.VideoLoaded then
  begin
    acClose;
    AktSong.VideoLoaded := false; // to prevent drawing closed video
  end;

  SetFontItalic (False);

  if (ScreenSong.Mode = smMedley) or ScreenSong.PartyMedley then
  begin
    if not FadeOut then
    begin
      for I := 0 to PlayersPlay - 1 do
          PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[I] := Player[I];

      inc(PlaylistMedley.CurrentMedleySong);
      if PlaylistMedley.CurrentMedleySong<=PlaylistMedley.NumMedleySongs then
      begin
        LoadNextSong;
      end else
      begin
        wStopWebCam();
        Ini.Save;
        //build sums
        len := Length(PlaylistMedley.Stats);
        num := PlaylistMedley.NumPlayer;

        SetLength(PlaylistMedley.Stats, len+1);
        SetLength(PlaylistMedley.Stats[len].Player, num);

        for J := 0 to len - 1 do
        begin
          for I := 0 to num - 1 do
          begin
            PlaylistMedley.Stats[len].Player[I].Score :=
              PlaylistMedley.Stats[len].Player[I].Score +
              PlaylistMedley.Stats[J].Player[I].Score;

            PlaylistMedley.Stats[len].Player[I].ScoreLine :=
              PlaylistMedley.Stats[len].Player[I].ScoreLine +
              PlaylistMedley.Stats[J].Player[I].ScoreLine;

            PlaylistMedley.Stats[len].Player[I].ScoreGolden :=
              PlaylistMedley.Stats[len].Player[I].ScoreGolden +
              PlaylistMedley.Stats[J].Player[I].ScoreGolden;

            PlaylistMedley.Stats[len].Player[I].ScoreI :=
              PlaylistMedley.Stats[len].Player[I].ScoreI +
              PlaylistMedley.Stats[J].Player[I].ScoreI;

            PlaylistMedley.Stats[len].Player[I].ScoreLineI :=
              PlaylistMedley.Stats[len].Player[I].ScoreLineI +
              PlaylistMedley.Stats[J].Player[I].ScoreLineI;

            PlaylistMedley.Stats[len].Player[I].ScoreGoldenI :=
              PlaylistMedley.Stats[len].Player[I].ScoreGoldenI +
              PlaylistMedley.Stats[J].Player[I].ScoreGoldenI;

            PlaylistMedley.Stats[len].Player[I].ScoreTotalI :=
              PlaylistMedley.Stats[len].Player[I].ScoreTotalI +
              PlaylistMedley.Stats[J].Player[I].ScoreTotalI;
          end; //of for I
        end; //of for J

        //build mean on sum
        for I := 0 to num - 1 do
        begin
          PlaylistMedley.Stats[len].Player[I].Score := round(
            PlaylistMedley.Stats[len].Player[I].Score / len);

          PlaylistMedley.Stats[len].Player[I].ScoreLine := round(
            PlaylistMedley.Stats[len].Player[I].ScoreLine / len);

          PlaylistMedley.Stats[len].Player[I].ScoreGolden := round(
            PlaylistMedley.Stats[len].Player[I].ScoreGolden / len);

          PlaylistMedley.Stats[len].Player[I].ScoreI := round(
            PlaylistMedley.Stats[len].Player[I].ScoreI / len);

          PlaylistMedley.Stats[len].Player[I].ScoreLineI := round(
            PlaylistMedley.Stats[len].Player[I].ScoreLineI / len);

          PlaylistMedley.Stats[len].Player[I].ScoreGoldenI := round(
            PlaylistMedley.Stats[len].Player[I].ScoreGoldenI / len);

          PlaylistMedley.Stats[len].Player[I].ScoreTotalI := round(
            PlaylistMedley.Stats[len].Player[I].ScoreTotalI / len);
        end;

        FadeOut:=true;
        if ScreenSong.PartyMedley then
        begin
          for I := 0 to PlayersPlay-1 do
          begin
            Player[I].ScoreTotalI := PlaylistMedley.Stats[Length(PlaylistMedley.Stats)-1].Player[I].ScoreTotalI;
            if(ScreenSong.Mode=smChallenge) then
              ScreenSingModi.TeamInfo.TeamInfo[I].Score:=Player[I].ScoreTotalI;
          end;

          if ScreenSong.Mode=smChallenge then
          begin
            PartySessionM2.Teams:=ScreenSingModi.TeamInfo;
            PartySessionM2.EndRound;
          end;{ else if ScreenSong.Mode = smParty then
          begin
            PartySession.Teams := ScreenSingModi.TeamInfo;
            PartySession.EndRound;
          end; }
        end;

        if (ScreenSong.Mode=smMedley) or (ScreenSong.Mode=smChallenge) then
          FadeTo(@ScreenScore)
        else
          FadeTo(@ScreenPartyScore);
      end;
    end;
  end else
  begin
    wStopWebCam();
    Ini.Save;
    SetLength(PlaylistMedley.Stats, 1);
    SetLength(PlaylistMedley.Stats[0].Player, PlayersPlay);
    for I := 0 to PlayersPlay - 1 do
      PlaylistMedley.Stats[0].Player[I] := Player[I];
    PlaylistMedley.Stats[0].SongArtist := AktSong.Artist;
    PlaylistMedley.Stats[0].SongTitle := AktSong.Title;
  end;
end;

procedure TScreenSing.onSentenceEnd(CP: integer; S: Cardinal);
var
  I: Integer;
  A: Real;
  B: integer; //Max Points for Notes

begin
  //Check for Empty Sentence
  if (Czesci[CP].Czesc[S].TotalNotes<=0) then
    exit;

  //Set Max Note Points
  if (Ini.LineBonus > 0) then
    B :=  9000
  else
    B := 10000;

  for I := 0 to High(Player) do
  begin
    if not AktSong.isDuet or (I mod 2 = CP) then
    begin
      
      A := Player[I].Score + Player[I].ScoreGolden - Player[I].ScoreLast + 2;

      //SingBar Mod
      If (Czesci[CP].Czesc[S].TotalNotes>0) then
      begin
        Player[I].ScorePercentTarget := Player[I].ScorePercentTarget +
          floor(A / (B * Czesci[CP].Czesc[S].TotalNotes / Czesci[CP].Wartosc) * 40 - 26);
        if Player[I].ScorePercentTarget < 0 then Player[I].ScorePercentTarget := 0;
        if Player[I].ScorePercentTarget > 99 then Player[I].ScorePercentTarget := 99;
      //end Singbar Mod
      end;

      //PhrasenBonus - Line Bonus Mod

      //Generate Steps 0 to 8
      A := Floor(A / (B * Czesci[CP].Czesc[S].TotalNotes / Czesci[CP].Wartosc) * 8);

      If (Ini.LineBonus > 0) then
      begin
        //Generate Text
        if A >= 8 then
          Player[I].LineBonus_Text := Theme.Sing.LineBonusText[8]
        else
          Player[I].LineBonus_Text := Theme.Sing.LineBonusText[Floor(A)];

        //PhrasenBonus give Points
        if (Length(Czesci[CP].Czesc) - NumEmptySentences[CP])>0 then
          Player[I].ScoreLine := Player[I].ScoreLine +
            (1000 / (Length(Czesci[CP].Czesc) - NumEmptySentences[CP]) * A / 8);

        Player[I].ScoreLineI := Round(Player[I].ScoreLine / 10) * 10;
        //Update Total Score
        Player[I].ScoreTotalI := Player[I].ScoreI + Player[I].ScoreGoldenI + Player[I].ScoreLineI;

        //Color
        Case Floor(A) of
          0: begin
            Player[I].LineBonus_Color.R := 1;
            Player[I].LineBonus_Color.G := 0;
            Player[I].LineBonus_Color.B := 0;
          end;
          1..3: begin
            Player[I].LineBonus_Color.R := 1;
            Player[I].LineBonus_Color.G := (A * 0.25);
            Player[I].LineBonus_Color.B := 0;
          end;
          4: begin
            Player[I].LineBonus_Color.R := 1;
            Player[I].LineBonus_Color.G := 1;
            Player[I].LineBonus_Color.B := 0;
          end;
          5..7: begin
            Player[I].LineBonus_Color.R := 1-((a-4)*0.25);
            Player[I].LineBonus_Color.G := 1;
            Player[I].LineBonus_Color.B := 0;
          end;
          8: begin
            Player[I].LineBonus_Color.R := 0;
            Player[I].LineBonus_Color.G := 1;
            Player[I].LineBonus_Color.B := 0;
          end;
        End; //Case
        //Player[I].LineBonus_Color.B := 0;
        //Player[I].LineBonus_Color.R := (8-A)/8;
        //Player[I].LineBonus_Color.G := A/10;

        Player[I].LineBonus_PosX  := Player[I].LineBonus_StartX;
        Player[I].LineBonus_PosY  := Player[I].LineBonus_StartY;
        Player[I].LineBonus_Alpha := 0.92;
        Player[I].LineBonus_Visible := True;
        Player[I].LineBonus_Age := 1;
      end;

      //PhrasenBonus - Line Bonus Mod End// }

      //PerfectLineTwinkle Mod (effect) Pt.1
      If (Ini.EffectSing=1) then
      begin
        if A >= 8 then Player[I].LastSentencePerfect := True
        else Player[I].LastSentencePerfect := False;
      end;
      //PerfectLineTwinkle Mod end

      //Refresh LastScore
      Player[I].ScoreLast := Player[I].Score + Player[I].ScoreGolden;
    end else
    begin
      Player[I].LineBonus_Visible := false;
      Player[I].LastSentencePerfect := False;
    end;
  end;

  //PerfectLineTwinkle Mod (effect) Pt.2
  if Ini.EffectSing=1 then
    GoldenRec.SpawnPerfectLineTwinkle;
  //PerfectLineTwinkle Mod end
end;

//Called on Sentence Change S= New Current Sentence
procedure TScreenSing.onSentenceChange(CP: integer; S: Cardinal);
begin
  //GoldenStarsTwinkle Mod
  GoldenRec.SentenceChange(CP);
  //GoldenStarsTwinkle Mod End
end;

procedure TScreenSing.DrawMedleyCountdown();
var
  w, h:           real;
  timeDiff:       real;
  t:              real;
  CountDownText:  string;

begin
  if (Czas.Teraz < GetTimeFromBeat(AktSong.Medley.StartBeat)) then
  begin
    timeDiff := GetTimeFromBeat(AktSong.Medley.StartBeat)-Czas.Teraz+1;
    t := frac(timeDiff);

    glColor4f(0.15, 0.30, 0.6, t);

    h := 100*t*ScreenH/RenderH;
    SetFontStyle(4);
    SetFontItalic(false);
    SetFontSize(h);
    CountDownText := IntToStr(round(timeDiff-t));
    w := glTextWidth(PChar(CountDownText));

    SetFontPos (RenderW/2-w/2, RenderH/2-h/2*3);
    glPrint(PChar(CountDownText));
  end;
end;

end.