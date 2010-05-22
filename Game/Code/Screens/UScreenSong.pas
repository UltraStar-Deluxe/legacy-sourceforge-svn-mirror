unit UScreenSong;

interface

uses
  UMenu,
  TextGL,
  SDL,
  UMusic,
  UDraw,
  UFiles,
  UTime,
  UDisplay,
  USongs,
  SysUtils,
  ULog,
  UThemes,
  UTexture,
  ULanguage,
  UIni,
  UVideo;

type
  TVisArr = array of integer;

  TVidVis = (none, windowed, full);

  THandler = record
    changed:      boolean;
    change_time:  real;
    lastIndex:    integer;
    lastCat:      integer;
    active:       boolean;
  end;

  TScreenSong = class(TMenu)
    private
      SkippedSongs: array of integer;
      ChooseableSongs:  integer;

    public
      MP3Volume:        integer;
      MP3VolumeHandler: THandler;
      TextArtist:   integer;
      TextTitle:    integer;
      TextNumber:   integer;

      TextPlugin:   integer;
      TextP1:       integer;  //for M2-MOD: show actual player-name p1
      TextP2:       integer;  //for M2-MOD: show actual player-name p2

      TextMedley:   array[1..4] of integer;
      TextTop:      array[0..2] of integer;
      StaticTop:    integer;

      FoundCAT: boolean;      //for M2-MOD: a cat is chosen, see whats next...

      SongIndex:    integer; //Index of Song that is playing since UScreenScore...

      //Video Icon Mod
      VideoIcon:    Cardinal;

      VidVis:       TVidVis; //video visiability
      StartTry:     boolean;

      MinSource:    TMedleySource;

      MakeMedley:   boolean;

      //VideoAspect
      AspectHandler:  THandler;

      //Wait timer
      WaitHandler:  THandler;

      //Medley Icons
      MedleyIcon: cardinal;
      CalcMedleyIcon: cardinal;

      TextCat:   integer;
      StaticCat: integer;

      SongCurrent:  real;
      SongTarget:   real;

      HighSpeed:    boolean;
      CoverFull:    boolean;
      CoverTime:    real;
      CoverX:       integer;
      CoverY:       integer;
      CoverW:       integer;
      is_jump:      boolean; // Jump to Song Mod
      is_jump_title:boolean; //Jump to SOng MOd-YTrue if search for Title

      EqualizerBands: array of Byte;
      EqualizerTime: Cardinal;
      EqualizerTime2: Byte;

      ID:           string; //for help-system

      //Party Mod
      Mode:         TSongMode;  //0 = Standard, 1= Go to PartyMode after Selection + Change to Random Song at Show
                                //2 = M2-Mode, 3=Medley-Mode
      PartyMedley:  boolean;
      PartyPlayed:  array of integer; //played in Party Classic
      MedleyPlayed: array of integer; //played in Challenge or Classic Medley-mode

      SungToEnd: boolean; //Song was sung to the end?

      //party Statics (Joker)
      StaticTeam1Joker1: Cardinal;
      StaticTeam1Joker2: Cardinal;
      StaticTeam1Joker3: Cardinal;
      StaticTeam1Joker4: Cardinal;
      StaticTeam1Joker5: Cardinal;

      StaticTeam2Joker1: Cardinal;
      StaticTeam2Joker2: Cardinal;
      StaticTeam2Joker3: Cardinal;
      StaticTeam2Joker4: Cardinal;
      StaticTeam2Joker5: Cardinal;

      StaticTeam3Joker1: Cardinal;
      StaticTeam3Joker2: Cardinal;
      StaticTeam3Joker3: Cardinal;
      StaticTeam3Joker4: Cardinal;
      StaticTeam3Joker5: Cardinal;

      TextNumJokerTeam1: Cardinal;
      TextNumJokerTeam2: Cardinal;
      TextNumJokerTeam3: Cardinal;

      StaticParty:    Array of Cardinal;
      TextParty:      Array of Cardinal;
      StaticNonParty: Array of Cardinal;
      TextNonParty:   Array of Cardinal;
      StaticM2Party: Array of Cardinal;
      TextM2Party:   Array of Cardinal;


      constructor Create; override;
      procedure SetScroll;
      procedure SetScroll1;
      procedure SetScroll2;
      procedure SetScroll3;
      procedure SetScroll4;
      procedure SetScroll5;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure SelectNext;
      procedure SelectPrev;
      procedure SkipTo(Target: Cardinal);
      procedure RandomSongChallenge();
      procedure SkipTo2(Target: Integer); //skipt exactly to the target song nr.
      procedure FixSelected; //Show Wrong Song when Tabs on Fix
      procedure FixSelected2; //Show Wrong Song when Tabs on Fix
      procedure ShowCatTL(Cat: Integer);// Show Cat in Top left
      procedure ShowCatTLCustom(Caption: String);// Show Custom Text in Top left
      procedure HideCatTL;// Show Cat in Tob left
      procedure Refresh(GiveStats: boolean); //Refresh Song Sorting
      procedure DrawEqualizer;
      procedure ChangeMusic;
      procedure LoadTop;
      procedure StartVideoPreview;
      //Party Mode
      procedure SelectRandomSong;
      procedure SetJoker;
      procedure SetStatics;
      //procedures for Menu
      procedure StartSong;
      procedure OpenEditor;
      procedure DoJoker(Team: Byte; SDL_ModState: Word);
      procedure SelectPlayers;

      procedure UnLoadDetailedCover;

      //Extensions
      procedure DrawExtensions;

      //for M2-MOD:
      function SongSkipped(SongNr: integer): boolean;
      function GetSongsSkipped(): Integer;
      procedure DoJokerM2;
      function  getVisibleMedleyArr(MinS: TMedleySource): TVisArr;
      procedure StartMedley(num: integer; MinS: TMedleySource);
      procedure DrawAspect;

      function PartyPlayedSong(SongNr: integer): boolean;
      function PartyPlayedMedley(SongNr: integer): boolean;


  end;

implementation
uses UGraphic, UMain,
  UCovers,
  math,
  gl,
  Windows,
  USkins,
  UHelp,
  UDLLManager,
  UDataBase,
  UParty,
  UPartyM2,
  UPlaylist,
  UScreenSongMenu;

// ***** Public methods ****** //
function  TScreenSong.SongSkipped(SongNr: integer): boolean;
var
  i: integer;
  skipped :boolean;
begin
  skipped := false;
  for i := 0 to Length(SkippedSongs) - 1 do
  begin
    if (SongNr=SkippedSongs[i]) then
    begin
      skipped:=true;
      break;
    end;
  end;
  Result:=skipped;
end;

function TScreenSong.GetSongsSkipped(): Integer;
begin
  Result := Length(SkippedSongs);
end;

function  TScreenSong.PartyPlayedSong(SongNr: integer): boolean;
var
  i: integer;
  played :boolean;
begin
  played := false;
  for i := 0 to Length(PartyPlayed) - 1 do
  begin
    if (SongNr=PartyPlayed[i]) then
    begin
      played:=true;
      break;
    end;
  end;
  Result:=played;
end;

function  TScreenSong.PartyPlayedMedley(SongNr: integer): boolean;
var
  i: integer;
  played :boolean;
begin
  played := false;
  for i := 0 to Length(MedleyPlayed) - 1 do
  begin
    if (SongNr=MedleyPlayed[i]) then
    begin
      played:=true;
      break;
    end;
  end;
  Result:=played;
end;

//Show Wrong Song when Tabs on Fix
procedure TScreenSong.FixSelected;
var I, I2: Integer;
  begin
    if CatSongs.VisibleSongs > 0 then
    begin
      I2:= 0;
      for I := low(CatSongs.Song) to High(Catsongs.Song) do
      begin
        if CatSongs.Song[I].Visible then
          inc(I2);

        if I = Interaction - 1 then
          break;
      end;

      SongCurrent := I2;
      SongTarget  := I2;
    end;
  end;

procedure TScreenSong.FixSelected2;
var I, I2: Integer;
  begin
    if CatSongs.VisibleSongs > 0 then
    begin
      I2:= 0;
      for I := low(CatSongs.Song) to High(Catsongs.Song) do
      begin
        if CatSongs.Song[I].Visible then
          inc(I2);

        if I = Interaction - 1 then
          break;
      end;

      SongTarget  := I2;
    end;
  end;
//Show Wrong Song when Tabs on Fix End

  procedure TScreenSong.ShowCatTLCustom(Caption: String);// Show Custom Text in Top left
  begin
    Text[TextCat].Text := Caption;
    Text[TextCat].Visible := true;
    Static[StaticCat].Visible := False;
  end;

  //Show Cat in Top Left Mod
  procedure TScreenSong.ShowCatTL(Cat: Integer);
    begin
    //Change
    Text[TextCat].Text := CatSongs.Song[Cat].Artist;
    //showmessage(CatSongs.Song[Cat].Path + CatSongs.Song[Cat].Cover);
    //Static[StaticCat].Texture := Texture.GetTexture(Button[Cat].Texture.Name, 'Plain', true);

    Static[StaticCat].Texture := Texture.GetTexture(Button[Cat].Texture.Name, 'Plain', true);
    //Texture.GetTexture(Button[Cat].Texture.Name, 'Plain', false);
    //Button[Cat].
    //Cover


    //Show
    Text[TextCat].Visible := true;
    Static[StaticCat].Visible := True;
    end;

  procedure TScreenSong.HideCatTL;
    begin
    //Hide
    //Text[TextCat].Visible := false;
    Static[StaticCat].Visible := false;
    //New -> Show Text specified in Theme
    Text[TextCat].Visible := True;
    Text[TextCat].Text := Theme.Song.TextCat.Text;
    end;
    //Show Cat in Top Left Mod End


// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSong.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  I:      integer;
  I2:     integer;
  SDL_ModState:  Word;
  Letter: Char;
  VisArr: array of integer;
begin
  Result := true;

  //Song Screen Extensions (Jumpto + Menu)
  if (ScreenSongMenu.Visible) then
  begin
    Result := ScreenSongMenu.ParseInput(PressedKey, ScanCode, PressedDown);
    Exit;
  end
  else if (ScreenSongJumpto.Visible) then
  begin
    Result := ScreenSongJumpto.ParseInput(PressedKey, ScanCode, PressedDown);
    Exit;
  end;

  If (PressedDown) Then
  begin // Key Down

    if (WaitHandler.active) and not (PressedKey IN [SDLK_RETURN, SDLK_TAB, SDLK_F,
      SDLK_A, SDLK_E, SDLK_K, SDLK_M, SDLK_P, SDLK_S, SDLK_V]) then
    begin
      if (Ini.Tabs=1) and not (CatSongs.CatNumShow = -3) then
      begin
        //Search Cat
        for I := WaitHandler.lastIndex downto low(CatSongs.Song) do
        begin
          if CatSongs.Song[I].Main then
            break;
        end;

        //Choose Cat
        CatSongs.ShowCategoryList;
        ShowCatTL (I);

        CatSongs.ClickCategoryButton(I);
      end;

      //Choose Song
      SkipTo2(WaitHandler.lastIndex);


      ChangeMusic;
      SetScroll4;
    end;

    if (Mode = smNormal) and not MakeMedley and (Ini.ShuffleTime>0) then
    begin
      WaitHandler.changed := true;
      WaitHandler.change_time := 0;
      if (WaitHandler.active) then
      begin
        WaitHandler.active := false;
        if (not PressedKey IN [SDLK_RETURN, SDLK_TAB, SDLK_F,
          SDLK_A, SDLK_E, SDLK_K, SDLK_M, SDLK_P, SDLK_S, SDLK_V]) then
          Exit;
      end;
    end;

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    //Jump to Artist/Titel  (not in party-mode)
    if (SDL_ModState and KMOD_LALT <> 0) AND (Mode = smNormal) AND (PressedKey >= SDLK_A) AND (PressedKey <= SDLK_Z) then
    begin
      Letter := UpCase(Chr(ScanCode));
      I2 := Length(CatSongs.Song);

      //Jump To Titel
      if (SDL_ModState = KMOD_LALT or KMOD_LSHIFT) then
      begin
        For I := 1 to high(CatSongs.Song) do
        begin
          if (CatSongs.Song[(I + Interaction) mod I2].Visible) AND
            (Length(CatSongs.Song[(I + Interaction) mod I2].Title)>0) AND
            (UpCase(CatSongs.Song[(I + Interaction) mod I2].Title[1]) = Letter) then
          begin
            SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2));

            Music.PlayChange;

            ChangeMusic;
            SetScroll4;
            //Break and Exit
            Exit;
          end;
        end;
      end
      //Jump to Artist
      else  if (SDL_ModState = KMOD_LALT) then
      begin
        For I := 1 to high(CatSongs.Song) do
        begin
          if (CatSongs.Song[(I + Interaction) mod I2].Visible) AND (Length(CatSongs.Song[(I + Interaction) mod I2].Artist)>0) AND (UpCase(CatSongs.Song[(I + Interaction) mod I2].Artist[1]) = Letter) then
          begin
            SkipTo(CatSongs.VisibleIndex((I + Interaction) mod I2));

            Music.PlayChange;

            ChangeMusic;
            SetScroll4;

            //Break and Exit
            Exit;
          end;
        end;
      end;
      Exit;
    end;

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
        
      SDLK_A:
        begin
          if VidVis = full then

            ToggleAspectCorrection;

            DataBase.SetAspect(CatSongs.Song[Interaction].Artist,
              CatSongs.Song[Interaction].Title, integer(UVideo.fAspectCorrection));

            AspectHandler.changed := true;
            AspectHandler.change_time := Czas.Teraz;
        end;
      SDLK_V:
        begin
          if UVideo.VideoOpened then
          begin
            if VidVis=full then
              VidVis:=windowed
            else begin
              VidVis:=full;
              UVideo.SetAspectCorrection(TAspectCorrection(
                DataBase.GetAspect(CatSongs.Song[Interaction].Artist,
                CatSongs.Song[Interaction].Title, Ini.AspectCorrect)));
              AspectHandler.changed := true;
              AspectHandler.change_time := Czas.Teraz;
            end;

          end;
        end;

      SDLK_TAB:
        begin
          Help.SetHelpID(ID);
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_S:
        begin
          if (SDL_ModState = KMOD_LSHIFT) and not MakeMedley and
            (CatSongs.Song[Interaction].Medley.Source>=msCalculated) and
            (Mode = smNormal)then
            StartMedley(0, msCalculated)
          else if (CatSongs.Song[Interaction].Medley.Source>=msTag) and not MakeMedley and
            (Mode = smNormal) then
            StartMedley(0, msTag);
        end;

      SDLK_D:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(5, msCalculated)
          else if (Mode = smNormal) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(5, msTag);
        end;

      SDLK_F:
        begin
          WaitHandler.change_time := 0;
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT) and MakeMedley then
          begin
            if Length(PlaylistMedley.Song)>0 then
            begin
              SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)-1);
              PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);
            end;

            if Length(PlaylistMedley.Song)=0 then
              MakeMedley := false;
          end else if (Mode = smNormal) and (Length(getVisibleMedleyArr(msCalculated)) > 0) then
          begin
            MakeMedley := true;
            StartMedley(99, msCalculated);
          end;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
        WaitHandler.change_time := 0;
        if (Mode = smNormal) or ((Mode = smChallenge) and not PartyMedley and not FoundCAT) then
        begin
          //On Escape goto Cat-List Hack
          if (Ini.Tabs = 1) AND (CatSongs.CatNumShow <> -1) then
            begin
            //Find Category
            I := Interaction;
            while not catsongs.Song[I].Main  do
              begin
              Dec (I);
              if (I < low(catsongs.Song)) then
                break;
              end;
            if (I<= 1) then
            Interaction := high(catsongs.Song)
            else
            Interaction := I - 1;

            //Stop Music
            Music.Stop;

            //Stop Video
            acClose;
            VidVis := none;
            CatSongs.ShowCategoryList;

            //Show Cat in Top Left Mod
            HideCatTL;


            //Show Wrong Song when Tabs on Fix
            SelectNext;
            FixSelected;
            //SelectPrev;
            //CatSongs.Song[0].Visible := False;
            end
          else
          begin
          //On Escape goto Cat-List Hack End
            //Tabs off and in Search or Playlist -> Go back to Song view
            if (CatSongs.CatNumShow < -1) then
            begin
              //Atm: Set Empty Filter
              CatSongs.SetFilter('', 0);

              //Show Cat in Top Left Mod
              HideCatTL;
              Interaction := 0;

              //Show Wrong Song when Tabs on Fix
              SelectNext;
              FixSelected;

              ChangeMusic;
            end
            else if (Mode = smNormal) then
            begin
              Music.Stop;
              Music.PlayBack;
              acClose;
              VidVis := none;
              FadeTo(@ScreenMain);
            end;

          end;
        end
        //When in party Mode then Ask before Close
        else if (Mode = smParty) then
        begin
          Music.PlayBack;
          CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
        end;
        end;
      SDLK_RETURN:
        begin
          if Length(Songs.Song) > 0 then
          begin
//            PortWriteB($378, 0);
            if CatSongs.Song[Interaction].Main then
            begin // clicked on Category Button

              //Show Cat in Top Left Mod
              ShowCatTL (Interaction);

              //I := CatSongs.VisibleIndex(Interaction);
              CatSongs.ClickCategoryButton(Interaction);
              {I2 := CatSongs.VisibleIndex(Interaction);
              SongCurrent := SongCurrent - I + I2;
              SongTarget := SongTarget - I + I2; }

//              if I<>I2 then beep;
              //  SetScroll4;

              //Show Wrong Song when Tabs on Fix
              SelectNext;
              FixSelected;

              //Play Music:
              ChangeMusic;

            end else
            begin // clicked on song
              if (Mode = smNormal) then //Normal Mode -> Start Song
              begin
                if MakeMedley then
                begin
                  Mode := smMedley;
                  Music.Stop;
                  //Do the Action that is specified in Ini
                  case Ini.OnSongClick of
                    0: FadeTo(@ScreenSing);
                    1: SelectPlayers;
                    2: FadeTo(@ScreenSing);
                  end;
                end else
                begin
                  WaitHandler.changed := false;
                  CatSongs.Selected := Interaction;
                  //Do the Action that is specified in Ini
                  case Ini.OnSongClick of
                    0: StartSong;
                    1: SelectPlayers;
                    2:begin
                      VidVis := windowed;
                      If (CatSongs.CatNumShow = -3) then
                        ScreenSongMenu.MenuShow(SM_Playlist)
                      else
                        ScreenSongMenu.MenuShow(SM_Main);
                    end;
                  end;
                end;
              end
              else if (Mode = smParty) and not PartyMedley then //PartyMode classic -> Show Menu
              begin
                VidVis := windowed;
                if (Ini.PartyPopup = 1) then
                  ScreenSongMenu.MenuShow(SM_Party_Main)
                else begin
                  SetLength(PartyPlayed, Length(PartyPlayed)+1);
                  PartyPlayed[Length(PartyPlayed)-1] := Interaction;
                  ScreenSong.StartSong;
                end;
              end else if (Mode = smChallenge) and not PartyMedley then
              begin
                PartySessionM2.AddSongPlayed(CatSongs.CatNumShow, Interaction);
                ScreenSong.StartSong;
              end else if PartyMedley then
              begin
                StartMedley(5, MinSource);
              end;
            end;
          end;
        end;

      SDLK_M: //Show SongMenu
        begin
          if (Length(Songs.Song) > 0) and (Mode <> smChallenge) then begin //not in M2-Mode
            if (Mode = smNormal) then begin
              WaitHandler.changed := false;
              CatSongs.Selected := Interaction;
              if not CatSongs.Song[Interaction].Main then begin // clicked on Song
                if CatSongs.CatNumShow = -3 then
                  ScreenSongMenu.MenuShow(SM_Playlist)
                else
                  ScreenSongMenu.MenuShow(SM_Main);
              end
              else
              begin
                ScreenSongMenu.MenuShow(SM_Playlist_Load);
              end;
            end //Party Mode -> Show Party Menu
            else ScreenSongMenu.MenuShow(SM_Party_Main);
          end;
        end;

      SDLK_P: //Show Playlist Menu
        begin
          if (Length(Songs.Song) > 0) AND (Mode = smNormal) then
          begin //not in party-modes
            WaitHandler.changed := false;
            CatSongs.Selected := Interaction;
            ScreenSongMenu.MenuShow(SM_Playlist_Load);
          end;
        end;

      SDLK_J: //Show Jumpto Menu / Joker
        begin
          if (Length(Songs.Song) > 0) AND (Mode = smNormal) then //not in party-modes
          begin
            VidVis := windowed;
            WaitHandler.changed := false;
            CatSongs.Selected := Interaction;
            ScreenSongJumpto.Visible := True;
          end else if (Mode=smChallenge) and not PartyMedley then  //M2-MOD-mode
            DoJokerM2
          else if PartyMedley and (Mode=smChallenge) then
          begin
            DoJoker(0, SDL_ModState)
          end;
        end;

      SDLK_DOWN:
        begin
          if (Mode = smNormal) or ((Mode = smChallenge) and not FoundCAT) then
          begin
            //Only Change Cat when not in Playlist or Search Mode
            if (CatSongs.CatNumShow > -2) then
            begin
              //Cat Change Hack
              if Ini.Tabs = 1 then
              begin
                I := Interaction;
                if I <= 0 then I := 1;

                while not catsongs.Song[I].Main do
                begin
                  Inc (I);
                  if (I > high(catsongs.Song)) then
                    I := low(catsongs.Song);
                end;

                Interaction := I;

                //Show Cat in Top Left Mod
                ShowCatTL (Interaction);

                CatSongs.ClickCategoryButton(Interaction);
                SelectNext;
                FixSelected;

                //Play Music:
                Music.PlayChange;
                ChangeMusic;

              end;

            //
            //Cat Change Hack End}
            end;
          end;
        end;
      SDLK_UP:
        begin
          if (Mode = smNormal) or ((Mode = smChallenge) and not FoundCAT) then
          begin
            //Only Change Cat when not in Playlist or Search Mode
            if (CatSongs.CatNumShow > -2) then
            begin
              //Cat Change Hack
              if Ini.Tabs = 1 then
              begin
                I := Interaction;
                I2 := 0;
                if I <= 0 then I := 1;

                while not catsongs.Song[I].Main or (I2 = 0) do
                begin
                  if catsongs.Song[I].Main then
                    Inc(I2);
                  Dec (I);
                  if (I < low(catsongs.Song)) then
                    I := high(catsongs.Song);
                end;

                Interaction := I;

                //Show Cat in Top Left Mod
                ShowCatTL (I);

                CatSongs.ClickCategoryButton(I);
                SelectNext;
                FixSelected;

                //Play Music:
                Music.PlayChange;
                ChangeMusic;
              end;
            end;
            //Cat Change Hack End}
          end;
        end;

      SDLK_RIGHT:
        begin
          if (Length(Songs.Song) > 0) AND
            ((Mode = smNormal) or ((Mode = smChallenge) and CatSongs.Song[Interaction].Main)) then
          begin
            Music.PlayChange;
            SelectNext;
            ChangeMusic;
            SetScroll4;
          end;
        end;

      SDLK_LEFT:
        begin
          if (Length(Songs.Song) > 0) AND
            ((Mode = smNormal) or ((Mode = smChallenge) and CatSongs.Song[Interaction].Main)) then
          begin
            Music.PlayChange;
            SelectPrev;
            ChangeMusic;
            SetScroll4;
          end;
        end;

      SDLK_E:
        begin
          if (Mode = smNormal) then
          begin
            WaitHandler.changed := false;
            OpenEditor;
          end;
        end;

      SDLK_R:
        begin
          if (Length(Songs.Song) > 0) and (Mode = smNormal) then
          begin
            if (SDL_ModState = KMOD_LSHIFT) AND (Ini.Tabs = 1) then
            //Random Category
            begin
              SetLength(VisArr, 0);

              //Search Cat
              for I2 := Interaction downto low(CatSongs.Song) do
              begin
                if CatSongs.Song[I2].Main then
                  break;
              end;

              for I := 0 to Length(CatSongs.Song) - 1 do
              begin
                if CatSongs.Song[I].Main and not (I=I2) then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end;

              if (Length(VisArr)>0) then
              begin
                I2 := Random(Length(VisArr));
                I2 := VisArr[I2];

                //Search Cat
                for I := I2 downto low(CatSongs.Song) do
                begin
                  if CatSongs.Song[I].Main then
                    break;
                end;
                //In I ist jetzt die Kategorie in I2 der Song
                //I is the CatNum, I2 is the No of the Song within this Cat

                //Choose Cat
                CatSongs.ShowCategoryList;

                //Show Cat in Top Left Mod
                ShowCatTL (I);

                //CatSongs.ClickCategoryButton(I);
                //SelectNext;

                //Choose Song
                SkipTo2(I);
              end;
            end else if (SDL_ModState = KMOD_LCTRL) AND (Ini.Tabs = 1) then
            //random in All Categorys
            begin
              SetLength(VisArr, 0);

              for I := 0 to Length(CatSongs.Song) - 1 do
              begin
                if not CatSongs.Song[I].Main and (I<>Interaction)then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end;


              if (Length(VisArr)>0) then
              begin
                I2 := Random(Length(VisArr));
                I2 := VisArr[I2];

                //Search Cat
                for I := I2 downto low(CatSongs.Song) do
                begin
                  if CatSongs.Song[I].Main then
                    break;
                end;

                //Choose Cat
                CatSongs.ShowCategoryList;

                //Show Cat in Top Left Mod
                ShowCatTL (I);

                CatSongs.ClickCategoryButton(I);
                //SelectNext;

                //Choose Song
                SkipTo2(I2);
              end;
            end else //Random in one Category
            begin
              SetLength(VisArr, 0);

              for I := 0 to Length(CatSongs.Song) - 1 do
              begin
                if CatSongs.Song[I].Visible and not (I=Interaction)then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end;


              if (Length(VisArr)>0) then
              begin
                I := Random(Length(VisArr));
                I := VisArr[I];

                //Choose Song
                SkipTo2(I);
              end;
              //old: SkipTo(Random(CatSongs.VisibleSongs));
            end;
            Music.PlayChange;
            ChangeMusic;
            SetScroll4;
          end else if (Mode = smChallenge) and not PartyMedley then //M2-MOD-mode
            DoJokerM2
          else if (Mode = smChallenge) and PartyMedley then
            DoJoker(0, SDL_ModState);
        end;

      SDLK_T:
        begin
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            //Change Sorting
            if (Ini.Sorting<Length(ISorting)-1) then
              Inc(Ini.Sorting)
            else
              Ini.Sorting := 0;
          end else
          begin
            //Change Tabs (on/off)
            if (Ini.Tabs=1) then
              Ini.Tabs := 0
            else
              Ini.Tabs := 1;
          end;

          Refresh(false);
          PlaylistMan.LoadPlayLists;
          OnShow;
        end;

      SDLK_1:
        begin //Joker Team 1
          if (Mode = smParty) AND (PartySession.Teams.NumTeams >= 1) AND (PartySession.Teams.Teaminfo[0].Joker > 0) then
          begin
            //Joker spielen
            DoJoker(0, SDL_ModState);
          end else if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(10, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(10, msTag);
        end;

      SDLK_2:
        begin //Joker Team 2
          if (Mode = smParty) AND (PartySession.Teams.NumTeams >= 2) AND (PartySession.Teams.Teaminfo[1].Joker > 0) then
          begin
            //Joker spielen
            DoJoker(1, SDL_ModState);
          end else if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(20, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(20, msTag);
        end;

      SDLK_3:
        begin //Joker Team 3
          if (Mode = smParty) AND (PartySession.Teams.NumTeams >= 3) AND (PartySession.Teams.Teaminfo[2].Joker > 0) then
          begin
            //Joker spielen
            DoJoker(2, SDL_ModState);
          end else if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(30, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(30, msTag);
        end;

      SDLK_4:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(40, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(40, msTag);
        end;

      SDLK_5:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(50, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(50, msTag);
        end;

      SDLK_6:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(60, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(60, msTag);
        end;

      SDLK_7:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(70, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(70, msTag);
        end;

      SDLK_8:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(80, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(80, msTag);
        end;

      SDLK_9:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(90, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(90, msTag);
        end;

      //stress test :>
      SDLK_0:
        begin
          if (Mode = smNormal) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and not MakeMedley and
            (length(getVisibleMedleyArr(msCalculated))>0) then
            StartMedley(10000, msCalculated)
          else if (Mode = smNormal) and (SDL_ModState = KMOD_LCTRL) and (Length(getVisibleMedleyArr(msTag)) > 0)
            and not MakeMedley then
            StartMedley(10000, msTag);
        end;
    end;
  end;
end;

constructor TScreenSong.Create;
var
  Pet:    integer;
  I:      integer;

begin
  inherited Create;

  LoadFromTheme(Theme.Song);

  TextArtist := AddText(Theme.Song.TextArtist);
  TextTitle :=  AddText(Theme.Song.TextTitle);
  TextNumber := AddText(Theme.Song.TextNumber);

  TextPlugin := AddText(Theme.Song.TextPlugin);

  for I := 1 to 4 do
    TextMedley[I] := AddText(Theme.Song.TextMedley[I]);

  for I := 0 to 2 do
    TextTop[I] := AddText(Theme.Song.TextTop[I]);

  StaticTop := AddStatic(Theme.Song.StaticTop);
  Static[StaticTop].Texture.Alpha := 0.4;

  //for M2-MOD-mode:
  TextP1 := AddText(Theme.Song.TextP1);
  TextP2 := AddText(Theme.Song.TextP2);

  //Show Cat in Top Left mod
  TextCat := AddText(Theme.Song.TextCat);
  StaticCat :=  AddStatic(Theme.Song.StaticCat);

  //Show Video Icon Mod
  VideoIcon := AddStatic(Theme.Song.VideoIcon);

  //Meldey Icons
  MedleyIcon := AddStatic(Theme.Song.MedleyIcon);
  CalcMedleyIcon := AddStatic(Theme.Song.CalculatedMedleyIcon);

  //Party Mode
  StaticTeam1Joker1 := AddStatic(Theme.Song.StaticTeam1Joker1);
  StaticTeam1Joker2 := AddStatic(Theme.Song.StaticTeam1Joker2);
  StaticTeam1Joker3 := AddStatic(Theme.Song.StaticTeam1Joker3);
  StaticTeam1Joker4 := AddStatic(Theme.Song.StaticTeam1Joker4);
  StaticTeam1Joker5 := AddStatic(Theme.Song.StaticTeam1Joker5);

  StaticTeam2Joker1 := AddStatic(Theme.Song.StaticTeam2Joker1);
  StaticTeam2Joker2 := AddStatic(Theme.Song.StaticTeam2Joker2);
  StaticTeam2Joker3 := AddStatic(Theme.Song.StaticTeam2Joker3);
  StaticTeam2Joker4 := AddStatic(Theme.Song.StaticTeam2Joker4);
  StaticTeam2Joker5 := AddStatic(Theme.Song.StaticTeam2Joker5);

  StaticTeam3Joker1 := AddStatic(Theme.Song.StaticTeam3Joker1);
  StaticTeam3Joker2 := AddStatic(Theme.Song.StaticTeam3Joker2);
  StaticTeam3Joker3 := AddStatic(Theme.Song.StaticTeam3Joker3);
  StaticTeam3Joker4 := AddStatic(Theme.Song.StaticTeam3Joker4);
  StaticTeam3Joker5 := AddStatic(Theme.Song.StaticTeam3Joker5);

  TextNumJokerTeam1 := AddText(Theme.Song.SongTextPartyTeam1NumJoker);
  TextNumJokerTeam2 := AddText(Theme.Song.SongTextPartyTeam2NumJoker);
  TextNumJokerTeam3 := AddText(Theme.Song.SongTextPartyTeam3NumJoker);

  //Load Party and NonParty specific Statics and Texts
  SetLength(StaticParty, Length(Theme.Song.StaticParty));
  for I := 0 to High(Theme.Song.StaticParty) do
    StaticParty[I] := AddStatic(Theme.Song.StaticParty[I]);

  SetLength(TextParty, Length(Theme.Song.TextParty));
  for I := 0 to High(Theme.Song.TextParty) do
    TextParty[I] := AddText(Theme.Song.TextParty[I]);

  SetLength(StaticNonParty, Length(Theme.Song.StaticNonParty));
  for I := 0 to High(Theme.Song.StaticNonParty) do
    StaticNonParty[I] := AddStatic(Theme.Song.StaticNonParty[I]);

  SetLength(TextNonParty, Length(Theme.Song.TextNonParty));
  for I := 0 to High(Theme.Song.TextNonParty) do
    TextNonParty[I] := AddText(Theme.Song.TextNonParty[I]);

  SetLength(StaticM2Party, Length(Theme.Song.StaticM2Party));
  for I := 0 to High(Theme.Song.StaticM2Party) do
    StaticM2Party[I] := AddStatic(Theme.Song.StaticM2Party[I]);

  SetLength(TextM2Party, Length(Theme.Song.TextM2Party));
  for I := 0 to High(Theme.Song.TextM2Party) do
    TextM2Party[I] := AddText(Theme.Song.TextM2Party[I]);

  // Song List
//  Songs.LoadSongList; // moved to the UltraStar unit
  
  //Refresh;


  // Randomize Patch
  Randomize;
  //Equalizer
  SetLength(EqualizerBands, Theme.Song.Equalizer.Bands);
  
  //ClearArray
  For I := low(EqualizerBands) to high(EqualizerBands) do
    EqualizerBands[I] := 3;

  MP3Volume := Ini.PreviewVolume * 10;
end;

procedure TScreenSong.SetScroll;
var
  VS, B, I: Integer;
  VisArr:   array of Integer;

begin
  VS := CatSongs.VisibleSongs;
  if VS > 0 then
  begin
    //Set Positions
    Case Theme.Song.Cover.Style of
      3: SetScroll3;
      5:begin
          if VS > 5 then
            SetScroll5
          else
            SetScroll4;
        end;
      else SetScroll4;
    end;
    //Set Visibility of Video Icon
    Static[VideoIcon].Visible := (CatSongs.Song[Interaction].Video <> '');

    // Set visibility of medley icons
    if Mode=smNormal then
    begin
      Static[MedleyIcon].Visible := (CatSongs.Song[Interaction].Medley.Source = msTag);
      Static[CalcMedleyIcon].Visible := (CatSongs.Song[Interaction].Medley.Source = msCalculated);
    end else
    begin
      Static[MedleyIcon].Visible := false;
      Static[CalcMedleyIcon].Visible := false;
    end;

    //Set Texts:
    Text[TextArtist].Text := CatSongs.Song[Interaction].Artist;
    Text[TextTitle].Text  :=  CatSongs.Song[Interaction].Title;
    if (Mode=smNormal) then
    begin
      if (Ini.Tabs = 1) And (CatSongs.CatNumShow = -1) then
      begin
        Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].OrderNum) + '/' + IntToStr(CatSongs.CatCount);
        Text[TextTitle].Text  := '(' + IntToStr(CatSongs.Song[Interaction].CatNumber) + ' ' + Language.Translate('SING_SONGS_IN_CAT') + ')';
      end else if (CatSongs.CatNumShow = -2) then
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
      else if (CatSongs.CatNumShow = -3) then
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
      else if (Ini.Tabs = 1) then
        Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].CatNumber) + '/' +
          IntToStr(CatSongs.Song[Interaction - CatSongs.Song[Interaction].CatNumber].CatNumber)
      else
        Text[TextNumber].Text := IntToStr(Interaction+1) + '/' + IntToStr(Length(CatSongs.Song));
    end else if (Mode=smChallenge) and not PartyMedley then
    begin
      if (Ini.Tabs = 1) and (CatSongs.CatNumShow = -1) then
      begin
        Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].OrderNum) + '/' + IntToStr(CatSongs.CatCount);
        Text[TextTitle].Text  := '(' +
          IntToStr(CatSongs.Song[Interaction].CatNumber - PartySessionM2.GetSongsPlayed(CatSongs.Song[Interaction].OrderNum)) +
          '/' +
          IntToStr(CatSongs.Song[Interaction].CatNumber) + ' ' + Language.Translate('SING_SONGS_IN_CAT') + ')'; //AND HERE!
      end else if (CatSongs.CatNumShow = -2) then
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS)
      else if (CatSongs.CatNumShow = -3) then
      begin
        ChooseableSongs := VS - PartySessionM2.GetSongsPlayed(CatSongs.CatNumShow) - GetSongsSkipped();
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS) +
          ' (' + IntToStr(ChooseableSongs) + ')';
      end else if (Ini.Tabs = 1) then
      begin
        ChooseableSongs:=CatSongs.Song[Interaction - CatSongs.Song[Interaction].CatNumber].CatNumber -
          PartySessionM2.GetSongsPlayed(CatSongs.CatNumShow) - GetSongsSkipped();

        Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].CatNumber) + '/' +
          IntToStr(CatSongs.Song[Interaction - CatSongs.Song[Interaction].CatNumber].CatNumber) + ' (' +
          IntToStr(ChooseableSongs) + ')'; //HERE!
      end else
      begin
        ChooseableSongs:=Length(CatSongs.Song)-PartySessionM2.GetSongsPlayed(CatSongs.CatNumShow)-GetSongsSkipped();
        Text[TextNumber].Text := IntToStr(Interaction+1) + '/' + IntToStr(Length(CatSongs.Song)) + ' (' +
          IntToStr(ChooseableSongs) + ')'; //HERE!
      end
    end else if PartyMedley then//PartyMedley
    begin
      SetLength(VisArr, 0);

      if (CatSongs.CatNumShow = -2) then
      begin
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS);
      end
      else if (CatSongs.CatNumShow = -3) then //Playlist!
      begin
        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if CatSongs.Song[I].Visible and not PartyPlayedMedley(I) and
            not SongSkipped(I) and (CatSongs.Song[I].Medley.Source>=MinSource) then
          begin
            SetLength(VisArr, Length(VisArr)+1);
            VisArr[Length(VisArr)-1] := I;
          end;
        end;
        ChooseableSongs := Length(VisArr);
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS) +
          ' (' + IntToStr(ChooseableSongs) + ')'; //HERE!
      end else if (Ini.Tabs = 1) then
      begin
        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if not CatSongs.Song[I].Main and not PartyPlayedMedley(I) and
            not SongSkipped(I) and (CatSongs.Song[I].Medley.Source>=MinSource) then
          begin
            if (PlaylistMan.Mode=0) or ((PlaylistMan.Mode<>0) and CatSongs.Song[I].Visible) then
            begin
              SetLength(VisArr, Length(VisArr)+1);
              VisArr[Length(VisArr)-1] := I;
            end;
          end;
        end;
        ChooseableSongs := Length(VisArr);

        Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].CatNumber) + '/' +
          IntToStr(CatSongs.Song[Interaction - CatSongs.Song[Interaction].CatNumber].CatNumber) + ' (' +
          IntToStr(ChooseableSongs) + ')'; //HERE!
      end else
      begin
        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if CatSongs.Song[I].Visible and not PartyPlayedMedley(I) and
            not SongSkipped(I) and (CatSongs.Song[I].Medley.Source>=MinSource) then
          begin
            SetLength(VisArr, Length(VisArr)+1);
            VisArr[Length(VisArr)-1] := I;
          end;
        end;
        ChooseableSongs := Length(VisArr);
        Text[TextNumber].Text := IntToStr(Interaction+1) + '/' + IntToStr(Length(CatSongs.Song)) + ' (' +
          IntToStr(ChooseableSongs) + ')'; //HERE!
      end;
    end else if (Mode=smParty) then//Party
    begin
      SetLength(VisArr, 0);

      if (CatSongs.CatNumShow = -2) then
      begin
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS);
      end
      else if (CatSongs.CatNumShow = -3) then //Playlist!
      begin
        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if CatSongs.Song[I].Visible and not PartyPlayedSong(I) and not SongSkipped(I) then
          begin
            SetLength(VisArr, Length(VisArr)+1);
            VisArr[Length(VisArr)-1] := I;
          end;
        end;
        ChooseableSongs := Length(VisArr);
        Text[TextNumber].Text := IntToStr(CatSongs.VisibleIndex(Interaction)+1) + '/' + IntToStr(VS) +
          ' (' + IntToStr(ChooseableSongs) + ')'; //HERE!
      end else if (Ini.Tabs = 1) then
      begin
        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if not CatSongs.Song[I].Main and not PartyPlayedSong(I) and not SongSkipped(I) then
          begin
            if (PlaylistMan.Mode=0) or ((PlaylistMan.Mode<>0) and CatSongs.Song[I].Visible) then
            begin
              SetLength(VisArr, Length(VisArr)+1);
              VisArr[Length(VisArr)-1] := I;
            end;
          end;
        end;
        ChooseableSongs := Length(VisArr);

        Text[TextNumber].Text := IntToStr(CatSongs.Song[Interaction].CatNumber) + '/' +
          IntToStr(CatSongs.Song[Interaction - CatSongs.Song[Interaction].CatNumber].CatNumber) + ' (' +
          IntToStr(ChooseableSongs) + ')'; //HERE!
      end else
      begin
        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if CatSongs.Song[I].Visible and not PartyPlayedSong(I) and not SongSkipped(I) then
          begin
            SetLength(VisArr, Length(VisArr)+1);
            VisArr[Length(VisArr)-1] := I;
          end;
        end;
        ChooseableSongs := Length(VisArr);
        Text[TextNumber].Text := IntToStr(Interaction+1) + '/' + IntToStr(Length(CatSongs.Song)) + ' (' +
          IntToStr(ChooseableSongs) + ')'; //HERE!
      end;
    end;
  end else
  begin
    Text[TextNumber].Text := '0/0';
    Text[TextArtist].Text := '';
    Text[TextTitle].Text  := '';
    for B := 0 to High(Button) do
    Button[B].Visible := False;

  end;
end;

procedure TScreenSong.SetScroll1;
var
  B:      integer;    // button
  //BMin:   integer;    // button min
  //BMax:   integer;    // button max
  Src:    integer;
//  Dst:    integer;
  Count:  integer;    // Dst is not used. Count is used.
  Ready:  boolean;

  VisCount: integer;  // count of visible (or selectable) buttons
  VisInt:   integer;  // visible position of interacted button
  Typ:      integer;  // 0 when all songs fits the screen
  Placed:   integer;  // number of placed visible buttons
begin
//  Src := 0;
//  Dst := -1;
  Count := 1;
  Typ := 0;
  Ready := false;
  Placed := 0;

  VisCount := 0;
  for B := 0 to High(Button) do
    if CatSongs.Song[B].Visible then Inc(VisCount);

  VisInt := 0;
  for B := 0 to Interaction-1 do
    if CatSongs.Song[B].Visible then Inc(VisInt);


  if VisCount <= 6 then begin
    Typ := 0;
  end else begin
    if VisInt <= 3 then begin
      Typ := 1;
      Count := 7;
      Ready := true;
    end;

    if (VisCount - VisInt) <= 3 then begin
      Typ := 2;
      Count := 7;
      Ready := true;
    end;

    if not Ready then begin
      Typ := 3;
      Src := Interaction;
    end;
  end;



  // hide all buttons
  for B := 0 to High(Button) do begin
    Button[B].Visible := false;
    Button[B].Selectable := CatSongs.Song[B].Visible;
  end;

{  for B := Src to Dst do begin
//    Button[B].Visible := true;
    Button[B].Visible := CatSongs.Song[B].Visible;
    Button[B].Selectable := Button[B].Visible;
    Button[B].Y := 140 + (B-Src) * 60;
  end;}


  if Typ = 0 then begin
    for B := 0 to High(Button) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (Placed) * 60;
        Inc(Placed);
      end;
    end;
  end;

  if Typ = 1 then begin
    B := 0;
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Inc(B);
    end;
  end;

  if Typ = 2 then begin
    B := High(Button);
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (6-Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Dec(B);
    end;
  end;

  if Typ = 3 then begin
    B := Src;
    Count := 4;
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (3+Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Inc(B);
    end;

    B := Src-1;
    Placed := 0;
    Count := 3;
    while (Count > 0) do begin
      if CatSongs.Song[B].Visible then begin
        Button[B].Visible := true;
        Button[B].Y := 140 + (2-Placed) * 60;
        Inc(Placed);
        Dec(Count);
      end;
      Dec(B);
    end;

  end;

  if Length(Button) > 0 then
    Static[1].Texture.Y := Button[Interaction].Y - 5; // selection texture
end;

procedure TScreenSong.SetScroll2;
var
  B:      integer;
  //Wsp:    integer; // wspolczynnik przesuniecia wzgledem srodka ekranu
  //Wsp2:   real;
begin
  // liniowe
  for B := 0 to High(Button) do
    Button[B].X := 300 + (B - Interaction) * 260;

  if Length(Button) >= 3 then begin
    if Interaction = 0 then
      Button[High(Button)].X := 300 - 260;

    if Interaction = High(Button) then
      Button[0].X := 300 + 260;
  end;

  // kolowe
{  for B := 0 to High(Button) do begin
    Wsp := (B - Interaction); // 0 dla srodka, -1 dla lewego, +1 dla prawego itd.
    Wsp2 := Wsp / Length(Button);
    Button[B].X := 300 + 10000 * sin(2*pi*Wsp2);
//    Button[B].Y := 140 + 50 * ;
  end;}
end;

procedure TScreenSong.SetScroll3; // with slide
var
  B:      integer;
  //Wsp:    integer; // wspolczynnik przesuniecia wzgledem srodka ekranu
  //Wsp2:   real;
begin
  SongTarget := Interaction;

  // liniowe
  for B := 0 to High(Button) do
  begin
    Button[B].X := 300 + (B - SongCurrent) * 260;
    if (Button[B].X < -Button[B].W) OR (Button[B].X > 800) then
      Button[B].Visible := False
    else
      Button[B].Visible := True;
  end;

{  if Length(Button) >= 3 then begin
    if Interaction = 0 then
      Button[High(Button)].X := 300 - 260;

    if Interaction = High(Button) then
      Button[0].X := 300 + 260;
  end;}

  // kolowe
{  for B := 0 to High(Button) do begin
    Wsp := (B - Interaction); // 0 dla srodka, -1 dla lewego, +1 dla prawego itd.
    Wsp2 := Wsp / Length(Button);
    Button[B].X := 300 + 10000 * sin(2*pi*Wsp2);
//    Button[B].Y := 140 + 50 * ;
  end;}
end;

procedure TScreenSong.SetScroll4; // rotate
var
  B:      integer;
  Wsp:    real;
  Z, Z2:      real;
  VS:     integer;
begin
  VS := CatSongs.VisibleSongs; // 0.5.0 (I): cached, very important

  // kolowe
  for B := 0 to High(Button) do begin
    Button[B].Visible := CatSongs.Song[B].Visible; // nowe
    if Button[B].Visible then begin // 0.5.0 optimization for 1000 songs - updates only visible songs, hiding in tabs becomes useful for maintaing good speed

    Wsp := 2 * pi * (CatSongs.VisibleIndex(B) - SongCurrent) /  VS {CatSongs.VisibleSongs};// 0.5.0 (II): takes another 16ms

    Z := (1 + cos(Wsp)) / 2;
    Z2 := (1 + 2*Z) / 3;


    Button[B].X := Theme.Song.Cover.X + (0.185 * Theme.Song.Cover.H * VS * sin(Wsp)) * Z2 - ((Button[B].H - Theme.Song.Cover.H)/2); // 0.5.0 (I): 2 times faster by not calling CatSongs.VisibleSongs
    Button[B].Z := Z / 2 + 0.3;

    Button[B].W := Theme.Song.Cover.H * Z2;

//    Button[B].Y := {50 +} 140 + 50 - 50 * Z2;
    Button[B].Y := Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Abs(Button[B].H)) * 0.7 ;
    Button[B].H := Button[B].W;
    end;
  end;
end;
{ old version
procedure TScreenSong.SetScroll5; // rotate
var
  B:      integer;
  Angle:    real;
  Pos:    Real;
  VS:     integer;
  diff:     real;
  X:        Real;
begin
  VS := CatSongs.VisibleSongs; // cache Visible Songs

  //Change Pos of all Buttons
  for B := low(Button) to high(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible; //Adjust Visability
    if Button[B].Visible then //Only Change Pos for Visible Buttons
    begin
      Pos := (CatSongs.VisibleIndex(B) - SongCurrent);
      if (Pos < -VS/2) then
        Pos := Pos + VS
      else if (Pos > VS/2) then
        Pos := Pos - VS;

      if (Abs(Pos) < 2.5) then //fixed Positions
      begin
      Angle := Pi * (Pos / 5);
      //Button[B].Visible := False;

      Button[B].H := Abs(Theme.Song.Cover.H * cos(Angle*0.8));//Power(Z2, 3);

//      Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
      Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

      Button[B].Z := 0.95 - Abs(Pos) * 0.01;

      Button[B].Y := (Theme.Song.Cover.Y  +
        (Theme.Song.Cover.H - Abs(Theme.Song.Cover.H * cos(Angle))) * 0.5);

      Button[B].W := Button[B].H;

      Diff := (Button[B].H - Theme.Song.Cover.H)/2;


      X := Sin(Angle*1.3)*0.9;

      Button[B].X := Theme.Song.Cover.X + Theme.Song.Cover.W * X - Diff;

      end
      else
      begin //Behind the Front Covers
//      Button[B].Visible := False;
//        if VS/2-abs(Pos)>VS*0.4 then Button[B].Visible := False;

        if Pos < 0 then
          Pos := (Pos - VS/2) /VS
        else
          Pos := (Pos + VS/2) /VS;

        Angle := pi * Pos*2;

        Button[B].Z := (0.4 - Abs(Pos/4)) -0.00001; //z < 0.49999 is behind the cover 1 is in front of the covers

        Button[B].H :=0.6*(Theme.Song.Cover.H-Abs(Theme.Song.Cover.H * cos(Angle/2)*0.8));//Power(Z2, 3);

        Button[B].W := Button[B].H;

        Button[B].Y := Theme.Song.Cover.Y  - (Button[B].H - Theme.Song.Cover.H)*0.75;

//        Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

        //Diff := (Button[B].H - Theme.Song.Cover.H)/2;

        Button[B].X :=  Theme.Song.Cover.X+Theme.Song.Cover.H/2-Button[b].H/2 + (Theme.Song.Cover.H)*sin(Angle/2)*1.52;

      end;

      //Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Button[B].H)/1.5); //Cover at down border of the change field
//      Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Button[B].H) * 0.7);

    end;
  end;
end; }

//new version from 1.1 (copy)
procedure TScreenSong.SetScroll5;
var
  B:      integer;
  Angle:    real;
  Pos:    real;
  VS:     integer;
  Padding:     real;
  X:        real;
  {
  Theme.Song.CoverW: circle radius
  Theme.Song.CoverX: x-pos. of the left edge of the selected cover
  Theme.Song.CoverY: y-pos. of the upper edge of the selected cover
  Theme.Song.CoverH: cover height
  }
begin
  VS := CatSongs.VisibleSongs();

  // Update positions of all buttons
  for B := 0 to High(Button) do
  begin
    Button[B].Visible := CatSongs.Song[B].Visible; // adjust visibility
    if Button[B].Visible then // Only change pos for visible buttons
    begin
      // Pos is the distance to the centered cover in the range [-VS/2..+VS/2]
      Pos := (CatSongs.VisibleIndex(B) - SongCurrent);
      if (Pos < -VS/2) then
        Pos := Pos + VS
      else if (Pos > VS/2) then
        Pos := Pos - VS;

      // Avoid overlapping of the front covers.
      // Use an alternate position for the five front covers. 
      if (Abs(Pos) < 2.5) then
      begin
        Angle := Pi * (Pos / 5); // Range: (-1/4*Pi .. +1/4*Pi)

        Button[B].H := Abs(Theme.Song.Cover.H * cos(Angle*0.8));
        Button[B].W := Button[B].H;

        //Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;

        Padding := (Button[B].H - Theme.Song.Cover.H)/2;
        X := Sin(Angle*1.3) * 0.9;

        Button[B].X := Theme.Song.Cover.X + Theme.Song.Cover.W * X - Padding;
        Button[B].Y := (Theme.Song.Cover.Y  + (Theme.Song.Cover.H - Abs(Theme.Song.Cover.H * cos(Angle))) * 0.5);
        Button[B].Z := 0.95 - Abs(Pos) * 0.01;
      end
      { only draw 3 visible covers in the background
        (the 3 that are on the opposite of the front covers}
      else if (Abs(Pos) > floor(VS/2) - 1.5) then
      begin
        // Transform Pos to range [-1..-3/4, +3/4..+1]
        { the 3 covers at the back will show up in the gap between the
          front cover and its neighbors
          one cover will be hiddenbehind the front cover,
          but this will not be a lack of performance ;) }
        if Pos < 0 then
          Pos := (Pos - 2 + ceil(VS/2))/8 - 0.75
        else
          Pos := (Pos + 2 - floor(VS/2))/8 + 0.75;

        // angle in radians [-2Pi..-Pi, +Pi..+2Pi]
        Angle := 2*Pi * Pos;

        Button[B].H := 0.6*(Theme.Song.Cover.H-Abs(Theme.Song.Cover.H * cos(Angle/2)*0.8));
        Button[B].W := Button[B].H;

        Padding := (Button[B].H - Theme.Song.Cover.H)/2;

        Button[B].X :=  Theme.Song.Cover.X+Theme.Song.Cover.H/2-Button[b].H/2+Theme.Song.Cover.W/320*((Theme.Song.Cover.H)*sin(Angle/2)*1.52);
        Button[B].Y := Theme.Song.Cover.Y  - (Button[B].H - Theme.Song.Cover.H)*0.75;
        Button[B].Z := (0.4 - Abs(Pos/4)) -0.00001; //z < 0.49999 is behind the cover 1 is in front of the covers

        //Button[B].Reflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
        Button[B].DeSelectReflectionspacing := 15 * Button[B].H/Theme.Song.Cover.H;
      end
      { all other covers are not visible }
      else
      Button[B].Visible := false;
    end;
  end;
end;


procedure TScreenSong.onShow;
var
  I:  Integer;

begin
  if Music.VocalRemoverActivated() then
    Music.DisableVocalRemover;

  if SongIndex<>Interaction then
    Music.Stop;

  PartyMedley := false;

  SungToEnd := false;
  if Mode = smMedley then
    Mode := smNormal;

  MakeMedley := false;

  StartTry := false;
  AspectHandler.changed := false;
  MP3VolumeHandler.changed := false;

  SetLength(PlaylistMedley.Song, 0);
  SetLength(SkippedSongs, 0);

  if Ini.Players <= 3 then PlayersPlay := Ini.Players + 1;
  if Ini.Players = 4 then PlayersPlay := 6;

  for I := 0 to 2 do
    Text[TextTop[I]].Visible := false;

  Static[StaticTop].Visible := false;

  //Cat Mod etc
  if (Ini.Tabs = 1) AND (CatSongs.CatNumShow = -1) AND
    (PlaylistMan.Mode=0) then
  begin
    CatSongs.ShowCategoryList;
    //SelectNext;
    //Show Cat in Top Left Mod
    HideCatTL;
  end else if (PlaylistMan.Mode=0) and (Ini.Tabs = 1) AND (CatSongs.CatNumShow = -3) then
  begin
    //Find Category
    I := Interaction;
    while not catsongs.Song[I].Main  do
    begin
      Dec (I);
      if (I < low(catsongs.Song)) then
        break;
    end;
    if (I<= 1) then
      Interaction := high(catsongs.Song)
    else
    Interaction := I - 1;

    //Stop Music
    Music.Stop;

    //Stop Video
    acClose;
    VidVis := none;
    CatSongs.ShowCategoryList;

    //Show Cat in Top Left Mod
    HideCatTL;

    //Show Wrong Song when Tabs on Fix
    SelectNext;
    FixSelected;
  end else if (Mode<>smNormal) and (PlaylistMan.Mode=0) and (CatSongs.CatNumShow < -1) then
  begin
    //Atm: Set Empty Filter
    CatSongs.SetFilter('', 0);

    //Show Cat in Top Left Mod
    HideCatTL;
    Interaction := 0;

    //Show Wrong Song when Tabs on Fix
    SelectNext;
    FixSelected;

    ChangeMusic;
  end;


  //Playlist Mode
  if (Mode = smNormal) then
  begin
    ScreenSongMenu.Visible := False;

    //If Playlist Shown -> Select Next automatically
    if (CatSongs.CatNumShow = -3) then
    begin
      SelectNext;
    end;
    Text[TextP1].visible := false;
    Text[TextP2].visible := false;
    ID := 'ID_024';
    if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSong, smNormal)');
    Text[TextPlugin].Visible := false;
  end else if (Mode = smParty) then   //Party Mode classic
  begin
    //TODO: PartyMedley
    PartyMedley := PartySession.Rounds[PartySession.CurRound].Medley;

    //Show Menu directly in PartyMode
    //But only if selected in Options
    if (Ini.PartyPopup = 1) then
    begin
      ScreenSongMenu.MenuShow(SM_Party_Main);
    end;
    Text[TextP1].visible := false;
    Text[TextP2].visible := false;
    ID := 'ID_025';
    if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSong, smParty)');

    Text[TextPlugin].Visible := true;
    if PartyMedley then
      MinSource := msCalculated;

    Text[TextPlugin].Text := PartySession.Plugins[PartySession.Rounds[PartySession.CurRound].PluginNr].Name;
    SelectRandomSong;
  end else if (Mode = smChallenge) then //M2-MOD
  begin
    PartyMedley := PartySessionM2.Rounds[PartySessionM2.CurRound].Medley;

    ID := 'ID_026';
    if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSong, smChallenge)');

    if not PartyMedley then
    begin
      if (PlaylistMan.Mode=1) then  //One Category Select Category and Select Random Song
      begin
        CatSongs.ShowCategoryList;
        CatSongs.ClickCategoryButton(PlaylistMan.CurPlayList);
        ShowCatTL(PlaylistMan.CurPlayList);
      end else if (PlaylistMan.Mode=2) then
      begin
        PlaylistMan.SetPlayList(PlaylistMan.CurPlayList);
      end;

      RandomSongChallenge;
      //SkipTo(Random(CatSongs.VisibleSongs - PartySessionM2.GetSongsPlayed(CatSongs.CatNumShow)));

      if (Ini.Tabs = 1) and (PlaylistMan.Mode <> 2) and
        (PlaylistMan.Mode <> 1) then
        FoundCAT:=false
      else
        FoundCAT:=true;
    end else
    begin
      MinSource := msCalculated;
      FoundCAT:=true;
      SelectRandomSong;
    end;

    Text[TextP1].Text := 'P1: ' + PartySessionM2.Teams.Teaminfo[0].Playerinfo[0].Name;
    Text[TextP2].Text := 'P2: ' + PartySessionM2.Teams.Teaminfo[1].Playerinfo[0].Name;
    Text[TextP1].visible := true;
    Text[TextP2].visible := true;
    if PartySessionM2.Option_Plugins then
    begin
      Text[TextPlugin].Visible := true;
      Text[TextPlugin].Text := PartySessionM2.Plugins[PartySessionM2.Rounds[PartySessionM2.CurRound].PluginNr].Name;
    end else
      Text[TextPlugin].Visible := false;
  end;

  if Length(CatSongs.Song) > 0 then begin
    if SongIndex<>Interaction then
    begin
      CoverTime := 0;
      ChangeMusic;
    end else
    begin
      CoverTime := 0;
      StartVideoPreview;
      LoadTop;
    end;

    SongIndex := -1;
    SetScroll;
  end;

  SetJoker;
  SetStatics;

  if (Mode = smNormal) and (Ini.ShuffleTime>0) then
  begin
    WaitHandler.changed := true;
    WaitHandler.change_time := 0;
  end;

  WaitHandler.active := false;
end;

procedure TScreenSong.RandomSongChallenge();
var
  VisArr: array of integer;
  I:      integer;
begin
  if (CatSongs.VisibleSongs - PartySessionM2.GetSongsPlayed(CatSongs.CatNumShow))=0 then
    PartySessionM2.ResetSongsPlayed(CatSongs.CatNumShow);

  SetLength(VisArr, 0);
  for I := 0 to Length(CatSongs.Song) - 1 do
  begin
    if not PartyMedley then
    begin
      if CatSongs.Song[I].Visible and not SongSkipped(I) and
        not PartySessionM2.SongPlayed(CatSongs.CatNumShow, I) then
      begin
        SetLength(VisArr, Length(VisArr)+1);
        VisArr[Length(VisArr)-1] := I;
      end;
    end else
    begin
      if CatSongs.Song[I].Visible and not SongSkipped(I) and
        not PartyPlayedMedley(I) and
        (CatSongs.Song[I].Medley.Source >= MinSource) then
      begin
        SetLength(VisArr, Length(VisArr)+1);
        VisArr[Length(VisArr)-1] := I;
      end;
    end;
  end;

  if (Length(VisArr)>0) then
  begin
    I := Random(Length(VisArr));
    I := VisArr[I];

    //Choose Song
    SkipTo2(I);
  end;
end;

procedure TScreenSong.onHide;
begin
  //When Music Fading is activated, Turn Music to 100 %
  If (Ini.PreviewVolume <> 100) or (Ini.PreviewFading <> 0) then
    Music.SetMusicVolume(100);

  //If Preview is deactivated: Load MUsicfile now
  If (Ini.PreviewVolume = 0) then
    Music.Open(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Mp3);

  //When hide then Stop Music (For Party Mode Popup on Exit)
  if (Display.NextScreen <> @ScreenSong) and (Display.NextScreen <> @ScreenSing) and
    (Display.NextScreen <> @ScreenSingModi) and (Music <> nil) then
    Music.Stop;
end;

procedure TScreenSong.DrawExtensions;
begin
  //Draw Song Menu
  if (ScreenSongMenu.Visible) then
  begin
    ScreenSongMenu.Draw;
  end
  else if (ScreenSongJumpto.Visible) then
  begin
    ScreenSongJumpto.Draw;
  end
end;

function TScreenSong.Draw: boolean;
var
  dx:     real;
  dt:     real;
  I, I2, J:   Integer;
  Window: TRectCoords;
  Blend:  real;
  VisArr: array of integer;

begin
  dx := SongTarget-SongCurrent;
  dt := TimeSkip*7;
  if dt > 1 then dt := 1;
  SongCurrent := SongCurrent + dx*dt;

{  if SongCurrent > Catsongs.VisibleSongs then begin
    SongCurrent := SongCurrent - Catsongs.VisibleSongs;
    SongTarget := SongTarget - Catsongs.VisibleSongs;
  end;}

//  Log.BenchmarkStart(5);
  SetScroll;
//  Log.BenchmarkEnd(5);
//  Log.LogBenchmark('SetScroll4', 5);

  //Fading Functions, Only if Covertime is under 5 Seconds
  If (CoverTime < 5) then
  begin
    // 0.5.0: cover fade
    if (CoverTime < 1) and (CoverTime + TimeSkip >= 1) then begin
      // load new texture
      Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false);
      Button[Interaction].Texture.Alpha := 1;
      Button[Interaction].Texture2 := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false);
      Button[Interaction].Texture2.Alpha := 1;
    end;
    {
    //Song Fade
    if (CatSongs.VisibleSongs > 0) AND (Ini.PreviewVolume <> 0) AND (Not CatSongs.Song[Interaction].Main) AND (Ini.PreviewFading <> 0) then
    begin
      //Start Song Fade after a little Time, to prevent Song to be Played on Scrolling
      if (CoverTime < 0.2) and (CoverTime + TimeSkip >= 0.2) then
        Music.Play;

      //Update Song Volume
      if (CoverTime < Ini.PreviewFading) then
        Music.SetMusicVolume(Round (CoverTime * Ini.PreviewVolume / Ini.PreviewFading * 10))
      else
        Music.SetMusicVolume(Ini.PreviewVolume * 10);

    end;
    }

    //Update Fading Time
    CoverTime := CoverTime + TimeSkip;

    //Update Fading Texture
    Button[Interaction].Texture2.Alpha := (CoverTime - 1) * 1.5;
    if Button[Interaction].Texture2.Alpha > 1 then Button[Interaction].Texture2.Alpha := 1;

  end;


  //inherited Draw;
  //heres a little Hack, that causes the Statics
  //are Drawn after the Buttons because of some Blending Problems.
  //This should cause no Problems because all Buttons on this screen
  //Has Z Position.
  //Draw BG
  DrawBG;

  //Medley Playlist
  if Length(PlaylistMedley.Song)>4 then
    J := Length(PlaylistMedley.Song)-4
  else
    J := 0;

  for I := 1 to 4 do
  begin
    if (Length(PlaylistMedley.Song)>=I+J) and (MakeMedley or
      ((Mode=smParty) and not PartySession.Rounds[PartySession.CurRound].MedleySurprise) or
      ((Mode=smChallenge) and not PartySessionM2.Rounds[PartySessionM2.CurRound].MedleySurprise)) then
    begin
      Text[TextMedley[I]].Visible := true;
      Text[TextMedley[I]].Text := IntToStr(I+J)+') ' +
        CatSongs.Song[PlaylistMedley.Song[I-1+J]].Artist + '\n' +
        CatSongs.Song[PlaylistMedley.Song[I-1+J]].Title;
    end else
      Text[TextMedley[I]].Visible := false;
  end;

  //Medley Mode
  if (Mode=smNormal) then
  begin
    if MakeMedley then
    begin
      Text[TextPlugin].Text := 'Medley-Mode';
      Text[TextPlugin].Visible := true;
    end else
      Text[TextPlugin].Visible := false;
  end;

  //Instead of Draw FG Procedure:
  //We draw Buttons for our own
  for I := 0 to Length(Button) - 1 do
    Button[I].Draw;

  // Statics
  for I := 0 to Length(Static) - 1 do
    Static[I].Draw;

  // and texts
  for I := 0 to Length(Text) - 1 do
    Text[I].Draw;



  //Draw Equalizer
  if Theme.Song.Equalizer.Visible then
    DrawEqualizer;

  if (CatSongs.Song[Interaction].Main) or (CatSongs.VisibleSongs = 0) then
  begin
    acClose;
    VidVis := none;
  end;

  if UVideo.VideoOpened then
  begin
    Czas.Teraz := Czas.Teraz + TimeSkip;
    try
      acGetFrame(Czas.Teraz);

      if VidVis=windowed then
      begin
        Window.Left := Button[Interaction].X;
        Window.Right := Button[Interaction].X+Button[Interaction].W;
        Window.Upper := Button[Interaction].Y;
        Window.Lower := Button[Interaction].Y+Button[Interaction].H;
        Window.windowed := true;

        {if CoverTime>=Ini.PreviewFading then
        begin
          glColor4f(0, 0, 0, 1);

          glbegin(gl_quads);
            glVertex2f(Window.Left, Window.Upper);
            glVertex2f(Window.Left, Window.Lower);
            glVertex2f(Window.Right, Window.Lower);
            glVertex2f(Window.Right, Window.Upper);
          glEnd;
        end; }
        SetAspectCorrection(acoCrop);
        Blend := (CoverTime-1.75)/Ini.PreviewFading;
        if Blend<0 then
          Blend := 0
        else if Blend>1 then
          Blend := 1;

        acDrawGLi(ScreenAct, Window, Blend);
      end else if VidVis=full then
      begin
        acDrawGL(ScreenAct);
      end;

      //ResetAspectCorrection;

      if (Czas.Teraz>=Czas.Razem) then
      begin
        acClose;
        VidVis := none;
      end;
    except
      //If an Error occurs drawing: prevent Video from being Drawn again and Close Video
      log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
      Log.LogError('Corrupted File: ' + CatSongs.Song[Interaction].Video);
      try
        acClose;
        VidVis := none;
      except

      end;
    end;
  end else
    StartVideoPreview;

  if (VidVis = full) and AspectHandler.changed and
    (AspectHandler.change_time+3>Czas.Teraz) then
  begin
    DrawAspect;
  end else if AspectHandler.changed and
    (AspectHandler.change_time+3<Czas.Teraz) then
    AspectHandler.changed := false;

  if MP3VolumeHandler.changed and (MP3VolumeHandler.change_time+TimeSkip<3) then
  begin
    MP3VolumeHandler.change_time := MP3VolumeHandler.change_time + TimeSkip;
    DrawVolumeBar(10, 530, 780, 12, MP3Volume);
  end else
    MP3VolumeHandler.changed := false;

  if MakeMedley or PartyMedley or
    ((CatSongs.VisibleSongs > 0) and CatSongs.Song[Interaction].Main) then
  begin
    for I := 0 to 2 do
      Text[TextTop[I]].Visible := false;

    Static[StaticTop].Visible := false;
  end;

  if (Mode = smNormal) and (Ini.ShuffleTime>0) and
    not MakeMedley and not CatSongs.Song[Interaction].Main and
    (Length(CatSongs.Song)-CatSongs.CatCount>1) then
  begin
    if (WaitHandler.changed and
      (((Ini.ShuffleTime<9) and (WaitHandler.change_time + TimeSkip>Ini.ShuffleTime*15))
      or (Music.Finished))) then
    begin
      WaitHandler.change_time := 0;
      if (not WaitHandler.active) then
      begin
        WaitHandler.active := true;
        WaitHandler.lastIndex := Interaction;
        WaitHandler.lastCat := CatSongs.CatNumShow;
      end;

      if(Ini.Tabs<>1) or (CatSongs.CatNumShow = -3) then
      begin
        //Random in one Category
        SetLength(VisArr, 0);

        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if CatSongs.Song[I].Visible and not (I=Interaction)then
          begin
            SetLength(VisArr, Length(VisArr)+1);
            VisArr[Length(VisArr)-1] := I;
          end;
        end;

        if (Length(VisArr)>0) then
        begin
          I := Random(Length(VisArr));
          I := VisArr[I];

          //Choose Song
          SkipTo2(I);
        end;
      end else
      begin
        //random in All Categorys
        SetLength(VisArr, 0);

        for I := 0 to Length(CatSongs.Song) - 1 do
        begin
          if not CatSongs.Song[I].Main and (I<>Interaction)then
          begin
            SetLength(VisArr, Length(VisArr)+1);
            VisArr[Length(VisArr)-1] := I;
          end;
        end;

        if (Length(VisArr)>0) then
        begin
          I2 := Random(Length(VisArr));
          I2 := VisArr[I2];

          //Search Cat
          for I := I2 downto low(CatSongs.Song) do
          begin
            if CatSongs.Song[I].Main then
            break;
          end;

          //Choose Cat
          CatSongs.ShowCategoryList;
          ShowCatTL (I);

          CatSongs.ClickCategoryButton(I);

          //Choose Song
          SkipTo2(I2);
        end;
      end;

      Music.PlayChange;
      ChangeMusic;
      SetScroll4;
    end else
      WaitHandler.change_time := WaitHandler.change_time + TimeSkip;
  end;

  DrawExtensions;
end;

procedure TScreenSong.DrawAspect();
var
  w, h: real;
  str: string;

begin
  //draw quad
  glColor4f(0.7, 0.7, 0.7, 0.6);
  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glVertex2f(270, 20);
    glVertex2f(270, 60);
    glVertex2f(530, 60);
    glVertex2f(530, 20);
  glEnd;
  glDisable(GL_BLEND);

  //print Text
  case UVideo.fAspectCorrection of
    acoStretch: str := Language.Translate('VIDEO_ASPECT_STRETCH');
    acoCrop: str := Language.Translate('VIDEO_ASPECT_CROP');
    acoLetterBox: str := Language.Translate('VIDEO_ASPECT_LETTER_BOX');
    else
      str := 'error';
  end;

  glColor4f(1, 1, 1, 1);

  h := 12;
  SetFontStyle(1);
  SetFontItalic(false);
  SetFontSize(h);
  w := glTextWidth(PChar(str));

  SetFontPos (RenderW/2-w/2, 21);
  glPrint(PChar(str));
end;

procedure TScreenSong.SelectNext;
var
  Skip, Skip2:   integer;
  //I:      integer;
  VS:     Integer;
begin
  VS := CatSongs.VisibleSongs;

  if VS > 0 then
  begin
    UnLoadDetailedCover;

    Skip := 1;
    Skip2:= 0;
     
    if (Mode=smChallenge) and not PartyMedley then
    begin
      while (not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible or
        PartySessionM2.SongPlayed(CatSongs.CatNumShow, (Interaction + Skip + Skip2) mod Length(Interactions)) or
        SongSkipped((Interaction + Skip + Skip2) mod Length(Interactions))) do
      begin
        if not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible then
          Inc(Skip)
        else
          Inc(Skip2);
      end;
    end else if PartyMedley then
    begin
      while (not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible or
        (CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Medley.Source < MinSource) or
        PartyPlayedMedley((Interaction + Skip + Skip2) mod Length(Interactions)) or
        SongSkipped((Interaction + Skip + Skip2) mod Length(Interactions))) do
      begin
        if not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible then
          Inc(Skip)
        else
          Inc(Skip2);
      end;
    end else if (Mode=smParty) then
    begin
      while (not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible or
        PartyPlayedSong((Interaction + Skip + Skip2) mod Length(Interactions)) or
        SongSkipped((Interaction + Skip + Skip2) mod Length(Interactions))) do
      begin
        if not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible then
          Inc(Skip)
        else
          Inc(Skip2);
      end;
    end else
    begin
      while (not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible or
        SongSkipped((Interaction + Skip + Skip2) mod Length(Interactions))) do
      begin
        if not CatSongs.Song[(Interaction + Skip + Skip2) mod Length(Interactions)].Visible then
          Inc(Skip)
        else
          Inc(Skip2);
      end;
    end;


    SongTarget := SongTarget + 1 + Skip2;//Skip;
    Interaction := (Interaction + Skip + Skip2) mod Length(Interactions);

    // try to keep all at the beginning
    if SongTarget > VS-1 then
    begin
      SongTarget := SongTarget - VS;
      SongCurrent := SongCurrent - VS;
    end;

  end;
      // Interaction -> Button, ktorego okladke przeczytamy
      //  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false); // 0.5.0: show uncached texture
end;

procedure TScreenSong.SelectPrev;
var
  Skip:   integer;
  VS:     Integer;
begin
  VS := CatSongs.VisibleSongs;

  if VS > 0 then
  begin
    UnLoadDetailedCover;

    Skip := 1;

    while (not CatSongs.Song[(Interaction - Skip + Length(Interactions)) mod Length(Interactions)].Visible) do Inc(Skip);
    SongTarget := SongTarget - 1;//Skip;

    Interaction := (Interaction - Skip + Length(Interactions)) mod Length(Interactions);

    // try to keep all at the beginning
    if SongTarget < 0 then begin
      SongTarget := SongTarget + CatSongs.VisibleSongs;
      SongCurrent := SongCurrent + CatSongs.VisibleSongs;
    end;

  //  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', false); // 0.5.0: show uncached texture
  end;
end;

//Procedure Change current played Preview
procedure TScreenSong.ChangeMusic;
begin
  //When Music Preview is avtivated -> then Change Music
  if (Ini.PreviewVolume <> 0) then
  begin
    if (NOT CatSongs.Song[Interaction].Main) AND(CatSongs.VisibleSongs > 0) then
    begin
      Music.Close;
      acClose;
      if Music.Open(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Mp3) then
      begin
        if (CatSongs.Song[Interaction].PreviewStart>0) then
          Music.MoveTo(CatSongs.Song[Interaction].PreviewStart)
        else
          Music.MoveTo(Music.Length / 4);

        StartVideoPreview;
        //If Song Fading is activated then don't Play directly, and Set Volume to Null, else Play normal
        if (Ini.PreviewFading = 0) then
        begin
          Music.SetMusicVolume (MP3Volume);
          Music.Play;
        end else
        begin
          Music.Fade(0, MP3Volume, Ini.PreviewFading);
          Music.Play;
        end;
      end;
    end else
      Music.Stop;
  end;

  LoadTop;
end;

procedure TScreenSong.LoadTop;
var
  I: integer;
begin
  //Load Top 3
  if (NOT CatSongs.Song[Interaction].Main) AND (CatSongs.VisibleSongs > 0) and
    not MakeMedley and not PartyMedley then
  begin
    AktSong := CatSongs.Song[Interaction];
    DataBase.ReadScore(AktSong, 3, {Ini.SumPlayers}0);

    for I := 0 to 2 do
    begin
      Text[TextTop[I]].Text := IntToStr(I+1)+'. ';
    end;

    if Length(AktSong.Score[Ini.Difficulty])>0 then
      Static[StaticTop].Visible := true
    else
      Static[StaticTop].Visible := false;

    for I := 0 to Length(AktSong.Score[Ini.Difficulty])-1 do
    begin
      Text[TextTop[I]].Visible := true;

      Text[TextTop[I]].Text := Text[TextTop[I]].Text + AktSong.Score[Ini.Difficulty, I].Name + '\n' +
        AktSong.Score[Ini.Difficulty, I].Date + ' (' + IntToStr(AktSong.Score[Ini.Difficulty, I].Score) + ')';
    end;

    for I := Length(AktSong.Score[Ini.Difficulty]) to 2 do
      Text[TextTop[I]].Visible := false;
  end else
  begin
    for I := 0 to 2 do
      Text[TextTop[I]].Visible := false;

    Static[StaticTop].Visible := false;
  end;
end;

procedure TScreenSong.StartVideoPreview;
begin
  if (Ini.PreviewVolume <> 0) and (Ini.MovieSize < 3) then
  begin
    if (NOT CatSongs.Song[Interaction].Main) AND (CatSongs.VisibleSongs > 0) then
    begin
      if CoverTime<0.75 then
      begin
        acClose;
        VidVis := none;
        StartTry := true;
        AspectHandler.changed := false;
      end else if (Ini.MoviePreview=1) and StartTry then
      begin
        if (CatSongs.Song[Interaction].Video <> '') and
          FileExists(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Video) then
        begin
          acOpenFile(PAnsiChar(CatSongs.Song[Interaction].Path + CatSongs.Song[Interaction].Video));

          acSkip2(CatSongs.Song[Interaction].VideoGAP, Music.Position);
          Czas.Teraz := Music.Position;
          Czas.Razem := Music.Length;
          StartTry := false;
          try
            acGetFrame(Czas.Teraz);
            VidVis := windowed;
          except
            //If an Error occurs Reading Video: prevent Video from being Drawn again and Close Video
            Log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
            Log.LogError('Corrupted File: ' + CatSongs.Song[Interaction].Video);
            CatSongs.Song[Interaction].Video := ''; //dirt fix
            try
              acClose;
              VidVis := none;
            except

            end;
          end;
        end else
          VidVis := none;
      end;
    end;
  end;
end;


procedure TScreenSong.SkipTo(Target: Cardinal); // 0.5.0
var
  I:      integer;
  
begin
  UnLoadDetailedCover;

  Interaction := High(CatSongs.Song);
  SongTarget := 0;

  for I := 1 to Target+1 do
    SelectNext;

  FixSelected2;
end;

procedure TScreenSong.SkipTo2(Target: Integer); //new
begin
  UnLoadDetailedCover;

  Interaction := High(CatSongs.Song);
  SongTarget := 0;

  while Interaction<>Target do
  begin
    SelectNext;
  end;

  FixSelected2;
end;

procedure TScreenSong.DrawEqualizer;
var
  Data: TFFTData; //Audio Data
  I, J: Integer;
  Res: byte;
  A, B: Integer;
  PosX, PosY: Integer;
  Pos: Real;
begin
if (not Music.Finished) AND (Theme.Song.Equalizer.Length > 0) then
begin


  A := GetTickCount div 44;

  if (A <> EqualizerTime) then
  begin
    EqualizerTime := A;
    Data := Music.GetFFTData;

    B:=0;
    Pos := 0;
    Res := ceil(92/Theme.Song.Equalizer.Bands);//How much channels are used for one Band

    //Change Lengths
    for I := 0 to (Res * Theme.Song.Equalizer.Bands - 1) do
    begin
      A := floor(I/Res);

      if (A<>B) then //Band changed
      begin
        if (Pos <= Theme.Song.Equalizer.Length) then
        begin
          if ((Pos < EqualizerBands[B]) AND (EqualizerBands[B]>1)) then
            EqualizerBands[B] := EqualizerBands[B] - 1
          else
            EqualizerBands[B] := floor(Pos);
        end
        else
          EqualizerBands[B] := 1;

        B := A;
        Pos := 0;
      end;

      if I > 35 then
        Data[i] := Data[i] * 8
      else if I > 11 then
        Data[i] := Data[i] * 4.5
      else
        Data[i] := Data[i] * 1.1;

      if (Data[i] >= 1) then
        Data[i] := 0.9999999999999;

      if Data[i]*Theme.Song.Equalizer.Length > Pos then
        Pos := Data[i]*Theme.Song.Equalizer.Length;
    end;

    //Change Last Band
    if (EqualizerBands[B] <= Theme.Song.Equalizer.Length) then
    begin
      if ((Pos < EqualizerBands[B]) AND (EqualizerBands[B]>1)) then
        EqualizerBands[B] := EqualizerBands[B] - 1
      else
        EqualizerBands[B] := floor(Pos)
    end
    else
      EqualizerBands[B] := 1;
  end;

  //Draw every Channel
  glColor4f(Theme.Song.Equalizer.ColR, Theme.Song.Equalizer.ColG, Theme.Song.Equalizer.ColB, Theme.Song.Equalizer.Alpha); //Set Color
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  PosY := Theme.Song.Equalizer.Y;
  PosX := Theme.Song.Equalizer.X;

  For I := 0 to Theme.Song.Equalizer.Bands-1 do
  begin
    if Theme.Song.Equalizer.Direction then
      PosY := Theme.Song.Equalizer.Y //+ (Theme.Song.Equalizer.H + Theme.Song.Equalizer.Space) * Theme.Song.Equalizer.Length
    else
      PosX := Theme.Song.Equalizer.X;
    //Draw for every visible quad
    for J := 1 to EqualizerBands[I] do
    begin
      glBegin(GL_QUADS);
        glVertex3f(PosX, PosY, Theme.Song.Equalizer.Z);
        glVertex3f(PosX, PosY+Theme.Song.Equalizer.H, Theme.Song.Equalizer.Z);
        glVertex3f(PosX+Theme.Song.Equalizer.W, PosY+Theme.Song.Equalizer.H, Theme.Song.Equalizer.Z);
        glVertex3f(PosX+Theme.Song.Equalizer.W, PosY, Theme.Song.Equalizer.Z);
      glEnd;

      if Theme.Song.Equalizer.Direction then //Vertically
        PosY := PosY - Theme.Song.Equalizer.H - Theme.Song.Equalizer.Space
      else //Horizontally
        PosX := PosX + Theme.Song.Equalizer.W + Theme.Song.Equalizer.Space;
    end;
    if Theme.Song.Equalizer.Direction then //Horizontally
      PosX := PosX + Theme.Song.Equalizer.W + Theme.Song.Equalizer.Space
    else //Vertically
      PosY := PosY + Theme.Song.Equalizer.H + Theme.Song.Equalizer.Space;
  end;
end;
end;

Procedure TScreenSong.SelectRandomSong;
var
  I, I2: Integer;
  VisArr: array of Integer;

begin
  if PartyMedley then
  begin
    if (Length(getVisibleMedleyArr(MinSource))<=0) then
    begin
      if (GetSongsSkipped()>0) then
        SetLength(SkippedSongs, 0);

      if (Length(getVisibleMedleyArr(MinSource))<=0) then
        SetLength(MedleyPlayed, 0);
    end;
  end else
  begin
    if (CatSongs.VisibleSongs-Length(PartyPlayed)<=0) then
    begin
      if (GetSongsSkipped()>0) then
        SetLength(SkippedSongs, 0);

      if (CatSongs.VisibleSongs-Length(PartyPlayed)<=0) then
        SetLength(PartyPlayed, 0);
    end;
  end;

  Case PlaylistMan.Mode of
      0:  //All Songs Just Select Random Song
        begin
          //When Tabs are activated then use Tab Method
          if (Ini.Tabs = 1) then
          begin
            SetLength(VisArr, 0);
            for I := 0 to Length(CatSongs.Song) - 1 do
            begin
              if not PartyMedley then
              begin
                if not CatSongs.Song[I].Main and not SongSkipped(I) and not PartyPlayedSong(I) then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end else
              begin
                if not CatSongs.Song[I].Main and not SongSkipped(I) and not PartyPlayedMedley(I) and
                  (CatSongs.Song[I].Medley.Source >= MinSource) then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end;
            end;

            if (Length(VisArr)>0) then
            begin
              I2 := Random(Length(VisArr));
              I2 := VisArr[I2];

              //Search Cat
              for I := I2 downto low(CatSongs.Song) do
              begin
                if CatSongs.Song[I].Main then
                  break;
              end;
              //In I ist jetzt die Kategorie in I2 der Song
              //I is the CatNum, I2 is the No of the Song within this Cat

              //Choose Cat
              CatSongs.ShowCategoryList;

              //Show Cat in Top Left Mod
              ShowCatTL (I);

              CatSongs.ClickCategoryButton(I);
              //SelectNext;

              //Choose Song
              SkipTo2(I2);
            end;
          end
          //When Tabs are deactivated use easy Method
          else
          begin
            SetLength(VisArr, 0);
            for I := 0 to Length(CatSongs.Song) - 1 do
            begin
              if not PartyMedley then
              begin
                if not CatSongs.Song[I].Main and not SongSkipped(I) and not PartyPlayedSong(I) then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end else
              begin
                if not CatSongs.Song[I].Main and not SongSkipped(I) and not PartyPlayedMedley(I) and
                  (CatSongs.Song[I].Medley.Source >= MinSource) then
                begin
                  SetLength(VisArr, Length(VisArr)+1);
                  VisArr[Length(VisArr)-1] := I;
                end;
              end;
            end;
            if (Length(VisArr)>0) then
            begin
              I2 := Random(Length(VisArr));
              I2 := VisArr[I2];

              //Search Cat
              for I := I2 downto low(CatSongs.Song) do
              begin
                if CatSongs.Song[I].Main then
                  break;
              end;
              //Choose Song
              SkipTo2(I2);
            end;
          end;
        end;
      1:  //One Category Select Category and Select Random Song
        begin
          CatSongs.ShowCategoryList;
          CatSongs.ClickCategoryButton(PlaylistMan.CurPlayList);
          ShowCatTL(PlaylistMan.CurPlayList);

          SetLength(VisArr, 0);
          for I := 0 to Length(CatSongs.Song) - 1 do
          begin
            if not PartyMedley then
            begin
              if CatSongs.Song[I].Visible and not SongSkipped(I) and not PartyPlayedSong(I) then
              begin
                SetLength(VisArr, Length(VisArr)+1);
                VisArr[Length(VisArr)-1] := I;
              end;
            end else
            begin
              if CatSongs.Song[I].Visible and not SongSkipped(I) and not PartyPlayedMedley(I) and
                (CatSongs.Song[I].Medley.Source >= MinSource) then
              begin
                SetLength(VisArr, Length(VisArr)+1);
                VisArr[Length(VisArr)-1] := I;
              end;
            end;
          end;
          if (Length(VisArr)>0) then
            begin
              I2 := Random(Length(VisArr));
              I2 := VisArr[I2];

              //Search Cat
              for I := I2 downto low(CatSongs.Song) do
              begin
                if CatSongs.Song[I].Main then
                  break;
              end;
              //Choose Song
              SkipTo2(I2);
            end;
        end;
      2:  //Playlist: Select Playlist and Select Random Song
        begin
          PlaylistMan.SetPlayList(PlaylistMan.CurPlayList);

          SetLength(VisArr, 0);
          for I := 0 to Length(CatSongs.Song) - 1 do
          begin
            if not PartyMedley then
            begin
              if CatSongs.Song[I].Visible and not SongSkipped(I) and not PartyPlayedSong(I) then
              begin
                SetLength(VisArr, Length(VisArr)+1);
                VisArr[Length(VisArr)-1] := I;
              end;
            end else
            begin
              if CatSongs.Song[I].Visible and not SongSkipped(I) and not PartyPlayedMedley(I) and
                (CatSongs.Song[I].Medley.Source >= MinSource) then
              begin
                SetLength(VisArr, Length(VisArr)+1);
                VisArr[Length(VisArr)-1] := I;
              end;
            end;
          end;
          if (Length(VisArr)>0) then
          begin
            I2 := Random(Length(VisArr));
            I2 := VisArr[I2];

            //Search Cat
            for I := I2 downto low(CatSongs.Song) do
            begin
              if CatSongs.Song[I].Main then
                break;
            end;
            //Choose Song
            SkipTo2(I2);
          end;

          FixSelected2;
        end;
  end;

  Music.PlayChange;
  ChangeMusic;
  SetScroll;
end;

//do Joker in M2-MOD mode
procedure TScreenSong.DoJokerM2;
begin
  if (PartySessionM2.Teams.Teaminfo[0].Joker>0) then
  begin
    if (((not CatSongs.Song[Interaction].Main) or (Ini.Tabs=0)) and (ChooseableSongs>1)) then
    begin
      if (FoundCAT) then Dec(PartySessionM2.Teams.Teaminfo[0].Joker);
        FoundCAT:=true;

      SetLength(SkippedSongs, Length(SkippedSongs)+1);
      SkippedSongs[Length(SkippedSongs)-1] := Interaction;
    end;

    if (ChooseableSongs > 1) then
    begin
      RandomSongChallenge;
      //SkipTo(Random(CatSongs.VisibleSongs - PartySessionM2.GetSongsPlayed(CatSongs.CatNumShow) - GetSongsSkipped()));
      SetJoker;

      Music.PlayChange;
      ChangeMusic;
      SetScroll4;
    end;
  end;
end;

procedure TScreenSong.SetJoker;
begin
  //If Party Mode
  if Mode = smParty then //Show Joker that are available
  begin
    if (PartySession.Teams.NumTeams >= 1) then
    begin
      Static[StaticTeam1Joker1].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 1);
      if PartySession.Teams.Teaminfo[0].Joker > 5 then
      begin
        Text[TextNumJokerTeam1].Text := 'x'+IntToStr(PartySession.Teams.Teaminfo[0].Joker);
        Text[TextNumJokerTeam1].Visible := true;

        Static[StaticTeam1Joker2].Visible := False;
        Static[StaticTeam1Joker3].Visible := False;
        Static[StaticTeam1Joker4].Visible := False;
        Static[StaticTeam1Joker5].Visible := False;
      end else
      begin
        Text[TextNumJokerTeam1].Visible := false;
        Static[StaticTeam1Joker2].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 2);
        Static[StaticTeam1Joker3].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 3);
        Static[StaticTeam1Joker4].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 4);
        Static[StaticTeam1Joker5].Visible := (PartySession.Teams.Teaminfo[0].Joker >= 5);
      end;
    end
    else
    begin
      Static[StaticTeam1Joker1].Visible := False;
      Static[StaticTeam1Joker2].Visible := False;
      Static[StaticTeam1Joker3].Visible := False;
      Static[StaticTeam1Joker4].Visible := False;
      Static[StaticTeam1Joker5].Visible := False;
      Text[TextNumJokerTeam1].Visible := false;
    end;

    if (PartySession.Teams.NumTeams >= 2) then
    begin
      Static[StaticTeam2Joker1].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 1);

      if PartySession.Teams.Teaminfo[1].Joker > 5 then
      begin
        Text[TextNumJokerTeam2].Text := 'x'+IntToStr(PartySession.Teams.Teaminfo[1].Joker);
        Text[TextNumJokerTeam2].Visible := true;

        Static[StaticTeam2Joker2].Visible := False;
        Static[StaticTeam2Joker3].Visible := False;
        Static[StaticTeam2Joker4].Visible := False;
        Static[StaticTeam2Joker5].Visible := False;
      end else
      begin
        Text[TextNumJokerTeam2].Visible := false;
        Static[StaticTeam2Joker2].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 2);
        Static[StaticTeam2Joker3].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 3);
        Static[StaticTeam2Joker4].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 4);
        Static[StaticTeam2Joker5].Visible := (PartySession.Teams.Teaminfo[1].Joker >= 5);
      end;
    end
    else
    begin
      Static[StaticTeam2Joker1].Visible := False;
      Static[StaticTeam2Joker2].Visible := False;
      Static[StaticTeam2Joker3].Visible := False;
      Static[StaticTeam2Joker4].Visible := False;
      Static[StaticTeam2Joker5].Visible := False;
      Text[TextNumJokerTeam2].Visible := false;
    end;

    if (PartySession.Teams.NumTeams >= 3) then
    begin
      Static[StaticTeam3Joker1].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 1);

      if PartySession.Teams.Teaminfo[2].Joker > 5 then
      begin
        Text[TextNumJokerTeam3].Text := 'x'+IntToStr(PartySession.Teams.Teaminfo[2].Joker);
        Text[TextNumJokerTeam3].Visible := true;

        Static[StaticTeam3Joker2].Visible := False;
        Static[StaticTeam3Joker3].Visible := False;
        Static[StaticTeam3Joker4].Visible := False;
        Static[StaticTeam3Joker5].Visible := False;
      end else
      begin
        Text[TextNumJokerTeam3].Visible := false;
        Static[StaticTeam3Joker2].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 2);
        Static[StaticTeam3Joker3].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 3);
        Static[StaticTeam3Joker4].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 4);
        Static[StaticTeam3Joker5].Visible := (PartySession.Teams.Teaminfo[2].Joker >= 5);
      end;
    end
    else
    begin
      Static[StaticTeam3Joker1].Visible := False;
      Static[StaticTeam3Joker2].Visible := False;
      Static[StaticTeam3Joker3].Visible := False;
      Static[StaticTeam3Joker4].Visible := False;
      Static[StaticTeam3Joker5].Visible := False;
      Text[TextNumJokerTeam3].Visible := false;
    end;
  end
  else if (Mode = smChallenge) then  //M2-MOD-mode
  begin
    if (PartySessionM2.Teams.NumTeams >= 1) then
    begin
      Static[StaticTeam1Joker1].Visible := (PartySessionM2.Teams.Teaminfo[0].Joker >= 1);
      Static[StaticTeam1Joker2].Visible := (PartySessionM2.Teams.Teaminfo[0].Joker >= 2);
      Static[StaticTeam1Joker3].Visible := (PartySessionM2.Teams.Teaminfo[0].Joker >= 3);
      Static[StaticTeam1Joker4].Visible := (PartySessionM2.Teams.Teaminfo[0].Joker >= 4);
      Static[StaticTeam1Joker5].Visible := (PartySessionM2.Teams.Teaminfo[0].Joker >= 5);
      Text[TextNumJokerTeam1].Visible := false;
    end
    else
    begin
      Static[StaticTeam1Joker1].Visible := false;
      Static[StaticTeam1Joker2].Visible := false;
      Static[StaticTeam1Joker3].Visible := false;
      Static[StaticTeam1Joker4].Visible := false;
      Static[StaticTeam1Joker5].Visible := false;
      Text[TextNumJokerTeam1].Visible := false;
    end;

    if (PartySessionM2.Teams.NumTeams >= 2) then
    begin
      Static[StaticTeam2Joker1].Visible := (PartySessionM2.Teams.Teaminfo[1].Joker >= 1);
      Static[StaticTeam2Joker2].Visible := (PartySessionM2.Teams.Teaminfo[1].Joker >= 2);
      Static[StaticTeam2Joker3].Visible := (PartySessionM2.Teams.Teaminfo[1].Joker >= 3);
      Static[StaticTeam2Joker4].Visible := (PartySessionM2.Teams.Teaminfo[1].Joker >= 4);
      Static[StaticTeam2Joker5].Visible := (PartySessionM2.Teams.Teaminfo[1].Joker >= 5);
      Text[TextNumJokerTeam2].Visible := false;
    end
    else
    begin
      Static[StaticTeam2Joker1].Visible := false;
      Static[StaticTeam2Joker2].Visible := false;
      Static[StaticTeam2Joker3].Visible := false;
      Static[StaticTeam2Joker4].Visible := false;
      Static[StaticTeam2Joker5].Visible := false;
      Text[TextNumJokerTeam2].Visible := false;
    end;

    if (PartySessionM2.Teams.NumTeams >= 3) then
    begin
      Static[StaticTeam3Joker1].Visible := (PartySessionM2.Teams.Teaminfo[2].Joker >= 1);
      Static[StaticTeam3Joker2].Visible := (PartySessionM2.Teams.Teaminfo[2].Joker >= 2);
      Static[StaticTeam3Joker3].Visible := (PartySessionM2.Teams.Teaminfo[2].Joker >= 3);
      Static[StaticTeam3Joker4].Visible := (PartySessionM2.Teams.Teaminfo[2].Joker >= 4);
      Static[StaticTeam3Joker5].Visible := (PartySessionM2.Teams.Teaminfo[2].Joker >= 5);
      Text[TextNumJokerTeam3].Visible := false;
    end
    else
    begin
      Static[StaticTeam3Joker1].Visible := false;
      Static[StaticTeam3Joker2].Visible := false;
      Static[StaticTeam3Joker3].Visible := false;
      Static[StaticTeam3Joker4].Visible := false;
      Static[StaticTeam3Joker5].Visible := false;
      Text[TextNumJokerTeam3].Visible := false;
    end;
  end else
  begin //Hide all
    Static[StaticTeam1Joker1].Visible := False;
    Static[StaticTeam1Joker2].Visible := False;
    Static[StaticTeam1Joker3].Visible := False;
    Static[StaticTeam1Joker4].Visible := False;
    Static[StaticTeam1Joker5].Visible := False;
    Text[TextNumJokerTeam1].Visible := false;

    Static[StaticTeam2Joker1].Visible := False;
    Static[StaticTeam2Joker2].Visible := False;
    Static[StaticTeam2Joker3].Visible := False;
    Static[StaticTeam2Joker4].Visible := False;
    Static[StaticTeam2Joker5].Visible := False;
    Text[TextNumJokerTeam2].Visible := false;

    Static[StaticTeam3Joker1].Visible := False;
    Static[StaticTeam3Joker2].Visible := False;
    Static[StaticTeam3Joker3].Visible := False;
    Static[StaticTeam3Joker4].Visible := False;
    Static[StaticTeam3Joker5].Visible := False;
    Text[TextNumJokerTeam3].Visible := false;
  end;
end;

procedure TScreenSong.SetStatics;
var
  I:       Integer;
  Visible: Boolean;
begin
  //Set Visibility of Party M2 Statics and Text
  Visible := (Mode = smChallenge);

  For I := 0 to high(StaticM2Party) do
    Static[StaticM2Party[I]].Visible := Visible;

  For I := 0 to high(TextM2Party) do
    Text[TextM2Party[I]].Visible := Visible;

  //Set Visibility of Party Statics and Text
  Visible := (Mode = smParty);

  For I := 0 to high(StaticParty) do
    Static[StaticParty[I]].Visible := Visible;

  For I := 0 to high(TextParty) do
    Text[TextParty[I]].Visible := Visible;

  //Set Visibility of Non Party Statics and Text
  Visible := (Mode = smNormal);

  For I := 0 to high(StaticNonParty) do
    Static[StaticNonParty[I]].Visible := Visible;

  For I := 0 to high(TextNonParty) do
    Text[TextNonParty[I]].Visible := Visible;

end;

//Procedures for Menu

procedure TScreenSong.StartSong;
begin
  WaitHandler.changed := false;
  CatSongs.Selected := Interaction;
  Music.Stop;
  //Party Mode
  if (Mode = smParty) or (Mode = smChallenge) then
  begin
    FadeTo(@ScreenSingModi);
  end
  else
  begin
    FadeTo(@ScreenSing);
  end;
end;

procedure TScreenSong.SelectPlayers;
begin
  CatSongs.Selected := Interaction;
  Music.Stop;
  acClose;
  VidVis := none;
  ScreenName.Goto_SingScreen := True;
  FadeTo(@ScreenName);
end;

procedure TScreenSong.OpenEditor;
begin
  if (Length(Songs.Song) > 0) and (not CatSongs.Song[Interaction].Main) AND (Mode = smNormal) then begin
    Music.Stop;
    acClose;
    VidVis := none;
    Music.PlayStart;
    ScreenEditSub.Path := CatSongs.Song[Interaction].Path;
    ScreenEditSub.FileName := CatSongs.Song[Interaction].FileName;
    ScreenEditSub.SongIndex := Interaction;
    FadeTo(@ScreenEditSub);
  end;
end;

//Team No of Team (0-5)
procedure TScreenSong.DoJoker (Team: Byte; SDL_ModState: Word);
begin
  if not PartyMedley and (ChooseableSongs>1) and
    (Mode = smParty) AND (PartySession.Teams.NumTeams >= Team + 1) AND (PartySession.Teams.Teaminfo[Team].Joker > 0) then
  begin
    //Joker spielen
    if (SDL_ModState <> KMOD_LALT) then
      Dec(PartySession.Teams.Teaminfo[Team].Joker);

    SetLength(SkippedSongs, Length(SkippedSongs)+1);
    SkippedSongs[Length(SkippedSongs)-1] := Interaction;

    //SetLength(PartyPlayed, Length(PartyPlayed)+1);
    //PartyPlayed[Length(PartyPlayed)-1] := Interaction;

    SelectRandomSong;
    SetJoker;
  end;

  if PartyMedley and (Mode=smChallenge) and (PartySessionM2.Teams.Teaminfo[0].Joker>0) then
  begin
    if (ChooseableSongs>1) then
    begin
      Dec(PartySessionM2.Teams.Teaminfo[0].Joker);

      SetLength(SkippedSongs, Length(SkippedSongs)+1);
      SkippedSongs[Length(SkippedSongs)-1] := Interaction;

      //SetLength(MedleyPlayed, Length(MedleyPlayed)+1);
      //MedleyPlayed[Length(MedleyPlayed)-1] := Interaction;

      SelectRandomSong;
      SetJoker;
    end;
  end;

  if PartyMedley and (Mode=smParty) and (PartySession.Teams.Teaminfo[Team].Joker>0) then
  begin
    if (ChooseableSongs>1) then
    begin
      if (SDL_ModState <> KMOD_LALT) then
        Dec(PartySession.Teams.Teaminfo[Team].Joker);

      SetLength(SkippedSongs, Length(SkippedSongs)+1);
      SkippedSongs[Length(SkippedSongs)-1] := Interaction;

      //SetLength(MedleyPlayed, Length(MedleyPlayed)+1);
      //MedleyPlayed[Length(MedleyPlayed)-1] := Interaction;

      SelectRandomSong;
      SetJoker;
    end;
  end;

end;

//Detailed Cover Unloading. Unloads the Detailed, uncached Cover of the cur. Song
procedure TScreenSong.UnLoadDetailedCover;
begin
  CoverTime := 0;

  Button[Interaction].Texture := Texture.GetTexture(Button[Interaction].Texture.Name, 'Plain', true); // 0.5.0: show cached texture
  Button[Interaction].Texture2.Alpha := 0;

  if Button[Interaction].Texture.Name <> Skin.GetTextureFileName('SongCover') then
    Texture.UnloadTexture(Button[Interaction].Texture.Name, false);
end;

procedure TScreenSong.Refresh(GiveStats: boolean);
var
  Pet:    integer;
  I:      integer;
Label CreateSongButtons;

begin
  ClearButtons();
  CatSongs.Refresh;

  if (length(CatSongs.Song) > 0) then
  begin
    //Set Length of Button Array one Time Instead of one time for every Song
    SetButtonLength(Length(CatSongs.Song));

    I := 0;
    Pet := 0;
    CreateSongButtons:

    try
      for Pet := I to High(CatSongs.Song) do
      begin // creating all buttons
        // new
        Texture.Limit := 512;// 256 0.4.2 value, 512 in 0.5.0

        if not FileExists(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover) then
          CatSongs.Song[Pet].Cover := ''; // 0.5.0: if cover not found then show 'no cover'

        if CatSongs.Song[Pet].Cover = '' then
          AddButton(300 + Pet*250, 140, 200, 200, Skin.GetTextureFileName('SongCover'), 'JPG', 'Plain', Theme.Song.Cover.Reflections)
        else begin
          // cache texture if there is a need to this
          if not Covers.CoverExists(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover) then begin
            Texture.CreateCacheMipmap := true;
            Texture.GetTexture(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover, 'Plain', true); // preloads textures and creates cache mipmap
            Texture.CreateCacheMipmap := false;

            // puts this texture to the cache file
            Covers.AddCover(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover);

            // unload full size texture
            Texture.UnloadTexture(CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover, false);

            // we should also add mipmap texture by calling createtexture and use mipmap cache as data source
          end;

          // and now load it from cache file (small place for the optimization by eliminating reading it from file, but not here)
          AddButton(300 + Pet*250, 140, 200, 200, CatSongs.Song[Pet].Path + CatSongs.Song[Pet].Cover, 'JPG', 'Plain', Theme.Song.Cover.Reflections);
        end;
        Texture.Limit := 1024*1024;
        I := -1;
        if GiveStats then
        begin
          if (Pet mod 10 = 0) then
            UpdateScreenLoading('Songs: '+IntToStr(Pet));
        end;
        
      end;
    except
      //When Error is reported the First time for this Song
      if (I <> Pet) then
      begin
        //Some Error reporting:
        Log.LogError('Could not load Cover: ' + CatSongs.Song[Pet].Cover);

        //Change Cover to NoCover and Continue Loading
        CatSongs.Song[Pet].Cover := '';
        I := Pet;
      end
      else //when Error occurs Multiple Times(NoSong Cover is damaged), then start loading next Song
      begin
        Log.LogError('NoCover Cover is damaged!');
        try
          AddButton(300 + Pet*250, 140, 200, 200, '', 'JPG', 'Plain', Theme.Song.Cover.Reflections);
        except
          Messagebox(0, PChar('No Cover Image is damage. Could not Workaround Song Loading, Ultrastar will exit now.'), PChar(Language.Translate('US_VERSION')), MB_ICONERROR or MB_OK);
          Halt;
        end;
        I := Pet + 1;
      end;
    end;

    if (I <> -1) then
      GoTo CreateSongButtons;

  end;

  FixSelected;
end;

function TScreenSong.getVisibleMedleyArr(MinS: TMedleySource): TVisArr;
var
  I: integer;
  res: TVisArr;
begin
  SetLength(res, 0);
  if CatSongs.Song[Interaction].main then
  begin
    for I := 0 to Length(CatSongs.Song) - 1 do
    begin
      if not CatSongs.Song[I].main and (CatSongs.Song[I].Medley.Source >= MinS) then
      begin
        SetLength(res, Length(res)+1);
        res[Length(res)-1] := I;
      end;
    end;
  end else begin
    if PartyMedley then
    begin
      for I := 0 to Length(CatSongs.Song) - 1 do
      begin
        if (CatSongs.Song[I].Medley.Source >= MinS) and not PartyPlayedMedley(I)
          and not SongSkipped(I) then
        begin
          if (PlaylistMan.Mode=0) or ((PlaylistMan.Mode<>0) and CatSongs.Song[I].Visible)  then
          begin
            SetLength(res, Length(res)+1);
            res[Length(res)-1] := I;
          end;
        end;
      end;
    end else
    begin
      for I := 0 to Length(CatSongs.Song) - 1 do
      begin
        if CatSongs.Song[I].Visible and (CatSongs.Song[I].Medley.Source >= MinS) then
        begin
          SetLength(res, Length(res)+1);
          res[Length(res)-1] := I;
        end;
      end;
    end;
  end;
  Result := res;
end;

//start Medley round
procedure TScreenSong.StartMedley(num: integer; MinS: TMedleySource);
  procedure AddSong(SongNr: integer);
  begin
    SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)+1);
    PlaylistMedley.Song[Length(PlaylistMedley.Song)-1] := SongNr;
  end;

  function SongAdded(SongNr: integer): boolean;
  var
    i: integer;
    skipped :boolean;
  begin
    skipped := false;
    for i := 0 to Length(PlaylistMedley.Song) - 1 do
    begin
      if (SongNr=PlaylistMedley.Song[i]) then
      begin
        skipped:=true;
        break;
      end;
    end;
    Result:=skipped;
  end;

  function NumSongsAdded(): Integer;
  begin
    Result := Length(PlaylistMedley.Song);
  end;

  function GetNextSongNr(MinS: TMedleySource): integer;
  var
    I, num: integer;
    unused_arr: array of integer;
    visible_arr: TVisArr;
  begin
    SetLength(unused_arr, 0);
    visible_arr := getVisibleMedleyArr(MinS);
    for I := 0 to Length(visible_arr) - 1 do
    begin
      if (not SongAdded(visible_arr[I])) then
      begin
        SetLength(unused_arr, Length(unused_arr)+1);
        unused_arr[Length(unused_arr)-1] := visible_arr[I];
      end;
    end;

    num := random(Length(unused_arr));
    Result := unused_arr[num];
end;

var
  I: integer;
  VS: integer;

begin
  if (num>0) and not PartyMedley and not MakeMedley then
  begin
    VS := Length(getVisibleMedleyArr(MinS));
    if VS < num then
      PlaylistMedley.NumMedleySongs := VS
    else
    PlaylistMedley.NumMedleySongs := num;

    Randomize;
    //set up Playlist Medley
    SetLength(PlaylistMedley.Song, 0);
    for I := 0 to PlaylistMedley.NumMedleySongs - 1 do
    begin
      AddSong(GetNextSongNr(MinS));
    end;
  end else if not PartyMedley and not MakeMedley then //start this song
  begin
    SetLength(PlaylistMedley.Song, 1);
    PlaylistMedley.Song[0] := Interaction;
    PlaylistMedley.NumMedleySongs := 1;
  end else if PartyMedley then //PartyMedley
  begin
    AddSong(Interaction);
    PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);

    //SetLength(SkippedSongs, Length(SkippedSongs)+1);
    //SkippedSongs[Length(SkippedSongs)-1] := Interaction;

    SetLength(MedleyPlayed, Length(MedleyPlayed)+1);
    MedleyPlayed[Length(MedleyPlayed)-1] := Interaction;

    if ((Mode=smParty) and (PartySession.Rounds[PartySession.CurRound].MedleySurprise)) or
      ((Mode=smChallenge) and (PartySessionM2.Rounds[PartySessionM2.CurRound].MedleySurprise)) then
    begin
      VS := Length(getVisibleMedleyArr(MinS));
      if VS < num then
      begin
        SetLength(MedleyPlayed, 0);
        VS := Length(getVisibleMedleyArr(MinS));
        if VS<num then
          PlaylistMedley.NumMedleySongs := VS
        else
          PlaylistMedley.NumMedleySongs := num;
      end else
      PlaylistMedley.NumMedleySongs := num;

      Randomize;

      //set up Playlist Medley
      for I := 1 to PlaylistMedley.NumMedleySongs - 1 do
      begin
        VS := GetNextSongNr(MinS);
        AddSong(VS);

        SetLength(MedleyPlayed, Length(MedleyPlayed)+1);
      MedleyPlayed[Length(MedleyPlayed)-1] := VS;
      end;
    end;
  end else if MakeMedley then
  begin
    if (CatSongs.Song[Interaction].Medley.Source>=MinS) then
    begin
      AddSong(Interaction);
      PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);
    end;
  end;

  if (Mode=smNormal) and not MakeMedley then
  begin
    Mode := smMedley;
    Music.Stop;
    //TODO: how about case 2? menu for medley mode?
    case Ini.OnSongClick of
      0: FadeTo(@ScreenSing);
      1: SelectPlayers;
      2: FadeTo(@ScreenSing);
      {2: begin
         if (CatSongs.CatNumShow = -3) then
           ScreenSongMenu.MenuShow(SM_Playlist)
         else
           ScreenSongMenu.MenuShow(SM_Main);
       end;}
    end;
  end else if PartyMedley then
  begin
    if (PlaylistMedley.NumMedleySongs = num) then
    begin
      Music.Stop;
      FadeTo(@ScreenSingModi);
    end else if (ChooseableSongs=1) then
    begin
      VS := Length(getVisibleMedleyArr(MinS));
      if VS<1 then
      begin
        SetLength(MedleyPlayed, 0);
        VS := Length(getVisibleMedleyArr(MinS));
        if VS<1 then
        begin
          Music.Stop;
          FadeTo(@ScreenSingModi);
        end else
          SelectRandomSong;
      end;
    end else
      SelectRandomSong;
  end else if MakeMedley then
  begin
    if PlaylistMedley.NumMedleySongs=num then
    begin
      Mode := smMedley;
      Music.Stop;
      //TODO: how about case 2? menu for medley mode?
      case Ini.OnSongClick of
        0: FadeTo(@ScreenSing);
        1: SelectPlayers;
        2: FadeTo(@ScreenSing);
        {2: begin
          if (CatSongs.CatNumShow = -3) then
            ScreenSongMenu.MenuShow(SM_Playlist)
          else
            ScreenSongMenu.MenuShow(SM_Main);
        end;}
      end;
    end;
  end;
end;


end.