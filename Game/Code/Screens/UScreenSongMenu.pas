unit UScreenSongMenu;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenSongMenu = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
      ID:       String; //for help-system

    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure MenuShow(sMenu: Byte);
      procedure HandleReturn;
  end;

const
  SM_Main = 1;

  SM_Song = 8 or 1;

  SM_Medley = 16 or 1;

  SM_Sort = 32 or 1;
  
  SM_PlayList = 64 or 1;
  SM_Playlist_Add = 64 or 2;
  SM_Playlist_New = 64 or 3;

  SM_Playlist_DelItem = 64 or 5;

  SM_Playlist_Load = 64 or 8 or 1;
  SM_Playlist_Del  = 64 or 8 or 5;


  SM_Party_Main = 128 or 1;
  SM_Party_Joker = 128 or 2;

var
  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses UGraphic, UMain, UDatabase, UScreenSong, UVideo, ULog, UIni, UTexture, ULanguage, UParty, UPlaylist, USongs, UHelp;

function TScreenSongMenu.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;

begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
      + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    if (CurMenu = SM_Playlist_New) AND (Interaction=0) then
    begin
      if not (ScanCode in [0..31, 127..159]) then
      begin
        Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text + chr(ScanCode);
        Exit;
      end;
      case PressedKey of
        SDLK_BACKSPACE:
          begin
            Button[Interaction].Text[0].DeleteLastL;
            exit;
          end;
      end;
    end;

    case PressedKey of
      SDLK_TAB:
        begin
          Help.SetHelpID(ID);
          ScreenPopupHelp.ShowPopup();
        end;

      //MP3-Volume Up
      SDLK_PAGEUP:
        begin
          if (ScreenSong.MP3Volume<100) then
          begin
            ScreenSong.MP3Volume := ScreenSong.MP3Volume+5;
            Music.SetMusicVolume(ScreenSong.MP3Volume);
          end;
          ScreenSong.MP3VolumeHandler.changed := true;
          ScreenSong.MP3VolumeHandler.change_time := 0;
        end;

      //MP3-Volume Down
      SDLK_PAGEDOWN:
        begin
          if (ScreenSong.MP3Volume>0) then
          begin
            ScreenSong.MP3Volume := ScreenSong.MP3Volume-5;
            Music.SetMusicVolume(ScreenSong.MP3Volume);
          end;
          ScreenSong.MP3VolumeHandler.changed := true;
          ScreenSong.MP3VolumeHandler.change_time := 0;
        end;

      SDLK_V:
        begin
          if UVideo.VideoOpened then
          begin
            if ScreenSong.TargetVidVis=full then
            begin
              ScreenSong.TargetVidVis:=windowed;
              ScreenSong.TargetAspect := acoCrop;
              if not ScreenSong.VidVisHandler.changed then
              begin
                ScreenSong.VidVisHandler.changed := true;
                ScreenSong.VidVisHandler.change_time := 0;
              end;
            end else
            begin
              ScreenSong.TargetVidVis:=full;
              if not ScreenSong.VidVisHandler.changed then
              begin
                ScreenSong.VidVisHandler.changed := true;
                ScreenSong.VidVisHandler.change_time := 0;
              end;
              //UVideo.SetAspectCorrection(TAspectCorrection(
              //  DataBase.GetAspect(CatSongs.Song[Interaction].Artist,
              //  CatSongs.Song[Interaction].Title, Ini.AspectCorrect)));
              ScreenSong.TargetAspect := TAspectCorrection(
                DataBase.GetAspect(CatSongs.Song[Interaction].Artist,
                CatSongs.Song[Interaction].Title, Ini.AspectCorrect));

              ScreenSong.AspectHandler.changed := true;
              ScreenSong.AspectHandler.change_time := Czas.Teraz;
            end;

          end;
        end;
        
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE:
        begin
          Music.PlayBack;
          Visible := False;
          if(ScreenSong.Mode = smNormal) and not ScreenSong.MakeMedley then
          begin
            ScreenSong.WaitHandler.changed := true;
            ScreenSong.WaitHandler.change_time := 0;
          end;
          CatSongs.Selected := Interaction;
        end;

      SDLK_RETURN:
        begin
          HandleReturn;
          if (CurMenu = SM_Playlist_Add) then
            MenuShow(CurMenu);
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT:
        begin
          if (Interaction=3) then
            InteractInc;

          if (CurMenu = SM_Sort) then
            Button[3].Visible := (Ini.Sorting <> SelectValue);

          if (ScreenSong.Mode = smParty) and (PartySession.Rand3) and (ScreenSong.Sel3<=0) then
          begin
            if (ScreenSong.Sel3<1) then
              Inc(ScreenSong.Sel3);
            Music.PlayChange;
            ScreenSong.SelectNext;
            ScreenSong.ChangeMusic;
          end;

          if (CurMenu = SM_Playlist_Add) and (Interaction <> 3) then
          begin
            Music.PlayChange;
            ScreenSong.SelectNext;
            ScreenSong.ChangeMusic;
            MenuShow(CurMenu);
          end else if (CurMenu = SM_Playlist_Add) then
          begin
            MenuShow(CurMenu);
            Interaction := 3;
          end;
        end;
      SDLK_LEFT:
        begin
          if (Interaction=3) then
            InteractDec;

          if (CurMenu = SM_Sort) then
            Button[3].Visible := (Ini.Sorting <> SelectValue);

          if (ScreenSong.Mode = smParty) and (PartySession.Rand3) and (ScreenSong.Sel3>=0) then
          begin
            if (ScreenSong.Sel3>-1) then
              Dec(ScreenSong.Sel3);
            Music.PlayChange;
            ScreenSong.SelectPrev;
            ScreenSong.ChangeMusic;
          end;

          if (CurMenu = SM_Playlist_Add) and (Interaction <> 3) then
          begin
            Music.PlayChange;
            ScreenSong.SelectPrev;
            ScreenSong.ChangeMusic;
            MenuShow(CurMenu);
          end else if (CurMenu = SM_Playlist_Add) then
          begin
            MenuShow(CurMenu);
            Interaction := 3;
          end;
        end;

      SDLK_1:
        begin //Jocker
            //Joker spielen
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(0, SDL_ModState)
            end;
          end;
        end;
      SDLK_2:
        begin //Jocker
            //Joker spielen
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(1, SDL_ModState)
            end;
          end;
        end;
      SDLK_3:
        begin //Jocker
            //Joker spielen
          case CurMenu of
            SM_Party_Main:
            begin
              ScreenSong.DoJoker(2, SDL_ModState)
            end;
          end;
        end;


    end;
  end;
end;

constructor TScreenSongMenu.Create;
{var
  I:    integer;}
begin
  inherited Create;
  
  //Create Dummy SelectSlide Entrys
  SetLength(ISelections, 1);
  ISelections[0] := 'Dummy';

  
  AddText(Theme.SongMenu.TextMenu);

  LoadFromTheme(Theme.SongMenu);

  AddButton(Theme.SongMenu.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.SongMenu.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  AddButton(Theme.SongMenu.Button3);
  if (Length(Button[2].Text) = 0) then
    AddButtonText(14, 20, 'Button 3');

  AddSelectSlide(Theme.SongMenu.SelectSlide3, SelectValue, ISelections);

  AddButton(Theme.SongMenu.Button4);
  if (Length(Button[3].Text) = 0) then
    AddButtonText(14, 20, 'Button 4');


  Interaction := 0;
end;

function TScreenSongMenu.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenSongMenu.onShow;
begin
  
end;

procedure TScreenSongMenu.MenuShow(sMenu: Byte);
var
  I: integer;

begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible
  Case sMenu of
    SM_Main:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_MAIN');

        Button[0].Visible := True;
        Button[1].Visible := (CatSongs.Song[ScreenSong.Interaction].Medley.Source>msNone);
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_SONG');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_MEDLEY');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_SORT');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_HELP');
      end;

    SM_Song:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_SONG');

        Button[0].Visible := True;
        Button[1].Visible := True;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_CHANGEPLAYERS');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
      end;

    SM_Medley:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_MEDLEY');

        Button[0].Visible := True;
        Button[1].Visible := (Length(PlaylistMedley.Song)>0);
        Button[2].Visible := (Length(PlaylistMedley.Song)>0) or
          (CatSongs.Song[ScreenSong.Interaction].Medley.Source > msNone);
        Button[3].Visible := not ScreenSong.MakeMedley;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_MEDLEY_ADD');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_MEDLEY_DELETE');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_MEDLEY_START');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_MEDLEY_START5');
      end;
    SM_Sort:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_SORT');

        Button[0].Visible := True;
        Button[1].Visible := (Length(PlaylistMedley.Song)=0);
        Button[2].Visible := False;
        SelectsS[0].Visible := True;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_SORT_TABS');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_SORT_DUETS');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_SORT_CONFIRM');

        SetLength(ISelections, Length(UIni.ISorting));
        For I := 0 to Length(UIni.ISorting)-1 do
          ISelections[I] := UIni.ISorting[I];

        UpdateSelectSlideOptions(Theme.SongMenu.SelectSlide3, 0, ISelections, SelectValue);
        Button[3].Visible := (Ini.Sorting <> SelectValue);
      end;

    SM_PlayList:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST');

        Button[0].Visible := True;
        Button[1].Visible := True;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        Button[1].Text[0].Text := Language.Translate('SONG_MENU_CHANGEPLAYERS');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_DEL');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_EDIT');
      end;

    SM_Playlist_Add:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_ADD');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := not PlaylistMan.SongExists(ScreenSong.Interaction, SelectValue);
        SelectsS[0].Visible := True;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD_NEW');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_ADD_EXISTING');

        SetLength(ISelections, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(ISelections);

        if (Length(ISelections)>=1) then
        begin
          UpdateSelectSlideOptions(Theme.SongMenu.SelectSlide3, 0, ISelections, SelectValue);
        end
        else
        begin
          Button[3].Visible := False;
          SelectsS[0].Visible := False;
          Button[2].Visible := True;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NOEXISTING');
        end;
      end;

    SM_Playlist_New:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_NEW');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := True;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NEW_UNNAMED');
        Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NEW_CREATE');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Playlist_DelItem:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_DELITEM');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;

    SM_Playlist_Load:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_LOAD');

        //Show Delete Curent Playlist Button when Playlist is opened
        Button[0].Visible := (CatSongs.CatNumShow = -3);

        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := True;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_DELCURRENT');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_LOAD');

        SetLength(ISelections, Length(PlaylistMan.Playlists));
        PlaylistMan.GetNames(ISelections);

        if (Length(ISelections)>=1) then
        begin
          UpdateSelectSlideOptions(Theme.SongMenu.SelectSlide3, 0, ISelections, SelectValue);
          Interaction := 3;
        end
        else
        begin
          Button[3].Visible := False;
          SelectsS[0].Visible := False;
          Button[2].Visible := True;
          Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYLIST_NOEXISTING');
          Interaction := 2;
        end;
      end;

    SM_Playlist_Del:
      begin
        ID := 'ID_032';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PLAYLIST_DEL');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');
      end;


    SM_Party_Main:
      begin
        ID := 'ID_033';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_MAIN');

        Button[0].Visible := True;
        Button[1].Visible := False;
        Button[2].Visible := False;
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := Language.Translate('SONG_MENU_PLAY');
        //Button[1].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
        //Button[2].Text[0].Text := Language.Translate('SONG_MENU_PLAYMODI');
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_JOKER');
      end;

    SM_Party_Joker:
      begin
        ID := 'ID_033';
        CurMenu := sMenu;
        Text[0].Text := Language.Translate('SONG_MENU_NAME_PARTY_JOKER');

        Button[0].Visible := (PartySession.Teams.NumTeams >= 1) AND (PartySession.Teams.Teaminfo[0].Joker > 0);
        Button[1].Visible := (PartySession.Teams.NumTeams >= 2) AND (PartySession.Teams.Teaminfo[1].Joker > 0);
        Button[2].Visible := (PartySession.Teams.NumTeams >= 3) AND (PartySession.Teams.Teaminfo[2].Joker > 0);
        Button[3].Visible := True;
        SelectsS[0].Visible := False;

        Button[0].Text[0].Text := String(PartySession.Teams.Teaminfo[0].Name);
        Button[1].Text[0].Text := String(PartySession.Teams.Teaminfo[1].Name);
        Button[2].Text[0].Text := String(PartySession.Teams.Teaminfo[2].Name);
        Button[3].Text[0].Text := Language.Translate('SONG_MENU_CANCEL');

        //Set right Interaction
        if (not Button[0].Visible) then
        begin
          if (not Button[1].Visible) then
          begin
            if (not Button[2].Visible) then
            begin
              Interaction := 4;
            end
            else Interaction := 2;
          end
          else Interaction := 1;
        end;
        
      end;
  end;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSongMenu)');
end;

procedure TScreenSongMenu.HandleReturn;
begin
  Case CurMenu of
    SM_Main:
      begin
        Case Interaction of
          0: //Button 1
            begin
              MenuShow(SM_Song);
            end;

          1: //Button 2
            begin
              MenuShow(SM_Medley);
            end;

          2: //Button 3
            begin
              MenuShow(SM_Sort);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              Help.SetHelpID(ScreenSong.ID);
              ScreenPopupHelp.ShowPopup();
              Visible := False;
            end;
        end;
      end;

    SM_Song:
      begin
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.StartSong;
              Visible := False;
            end;

          1: //Button 2
            begin
              //Select New Players then Sing:
              ScreenSong.SelectPlayers;
              Visible := False;
            end;

          2: //Button 3
            begin
              //Show add to Playlist Menu
              MenuShow(SM_Playlist_Add);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              ScreenSong.OpenEditor;
              Visible := False;
            end;
        end;
      end;

    SM_Medley:
      begin
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.MakeMedley := true;
              ScreenSong.StartMedley(99, msCalculated);

              Visible := False;
            end;

          1: //Button 2
            begin
              SetLength(PlaylistMedley.Song, Length(PlaylistMedley.Song)-1);
              PlaylistMedley.NumMedleySongs := Length(PlaylistMedley.Song);

              if Length(PlaylistMedley.Song)=0 then
                ScreenSong.MakeMedley := false;

              Visible := False;
            end;

          2: //Button 3
            begin
              if ScreenSong.MakeMedley then
              begin
                ScreenSong.Mode := smMedley;
                Music.Stop;
                //Do the Action that is specified in Ini
                case Ini.OnSongClick of
                  0: FadeTo(@ScreenSing);
                  1: ScreenSong.SelectPlayers;
                  2: FadeTo(@ScreenSing);
                end;
              end else
                ScreenSong.StartMedley(0, msCalculated);
                
              Visible := False;
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              ScreenSong.StartMedley(5, msCalculated);
            end;
        end;
      end;

    SM_Sort:
      begin
        Case Interaction of
          0: //Button 1
            begin
              //Change Tabs (on/off)
              if (Ini.Tabs=1) then
                ScreenSong.ChangeSorting(false, Ini.Sorting)
              else
                ScreenSong.ChangeSorting(true, Ini.Sorting);
            end;

          1: //Button 2
            begin
              ScreenSongJumpto.ToggleDuetFilter;
              Visible := false;
            end;

          3: //Slide
            begin
              //dummy
            end;

          4: //Button 4
            begin
              //Change Sorting
              ScreenSong.ChangeSorting(Ini.Tabs=1, SelectValue);
              Visible := false;
            end;
        end;
      end;

    SM_PlayList:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              ScreenSong.StartSong;
              Visible := False;
            end;

          1: //Button 2
            begin
              //Select New Players then Sing:
              ScreenSong.SelectPlayers;
              Visible := False;
            end;

          2: //Button 3
            begin
              //Show add to Playlist Menu
              MenuShow(SM_Playlist_DelItem);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              ScreenSong.OpenEditor;
              Visible := False;
            end;
        end;
      end;

    SM_Playlist_Add:
      begin
        Case Interaction of
          0: //Button 1
            begin
              MenuShow(SM_Playlist_New);
            end;

          3: //SelectSlide 3
            begin
              //Dummy
            end;

          4: //Button 4
            begin
              PlaylistMan.AddItem(ScreenSong.Interaction, SelectValue);
              ScreenSong.WaitHandler.changed := true;
              ScreenSong.WaitHandler.change_time := 0;
              //Visible := False;
            end;
        end;
      end;

      SM_Playlist_New:
      begin
        Case Interaction of
          0: //Button 1
            begin
              //Nothing, Button for Entering Name
            end;

          2: //Button 3
            begin
              //Create Playlist and Add Song
              PlaylistMan.AddItem(
              ScreenSong.Interaction,
              PlaylistMan.AddPlaylist(Button[0].Text[0].Text));
              ScreenSong.WaitHandler.changed := true;
              ScreenSong.WaitHandler.change_time := 0;
              Visible := False;
            end;

          3: //SelectSlide 3
            begin
              //Cancel -> Go back to Add screen
              MenuShow(SM_Playlist_Add);
            end;

          4: //Button 4
            begin
              ScreenSong.WaitHandler.changed := true;
              ScreenSong.WaitHandler.change_time := 0;
              Visible := False;
            end;
        end;
      end;

    SM_Playlist_DelItem:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Delete
              PlayListMan.DelItem(PlayListMan.GetIndexbySongID(ScreenSong.Interaction));
              ScreenSong.WaitHandler.changed := true;
              ScreenSong.WaitHandler.change_time := 0;
              Visible := False;
            end;

          4: //Button 4
            begin
              MenuShow(SM_Playlist);
            end;
        end;
      end;

    SM_Playlist_Load:
      begin
        Case Interaction of
          0: //Button 1 (Delete Playlist)
            begin
              MenuShow(SM_Playlist_Del);
            end;
          4: //Button 4
            begin
              //Load Playlist
              PlaylistMan.SetPlayList(SelectValue);
              ScreenSong.WaitHandler.changed := true;
              ScreenSong.WaitHandler.change_time := 0;
              Visible := False;
            end;
        end;
      end;

    SM_Playlist_Del:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Delete
              PlayListMan.DelPlaylist(PlaylistMan.CurPlayList);
              ScreenSong.WaitHandler.changed := true;
              ScreenSong.WaitHandler.change_time := 0;
              Visible := False;
            end;

          4: //Button 4
            begin
              MenuShow(SM_Playlist_Load);
            end;
        end;
      end;

    SM_Party_Main:
      begin
        Case Interaction of
          0: //Button 1
            begin
              //Start Singing
              if not ScreenSong.PartyMedley then
              begin
                SetLength(ScreenSong.PartyPlayed, Length(ScreenSong.PartyPlayed)+1);
                ScreenSong.PartyPlayed[Length(ScreenSong.PartyPlayed)-1] := ScreenSong.Interaction;
                ScreenSong.StartSong;
                Visible := False;
              end else
                ScreenSong.StartMedley(5, msCalculated);
            end;

          4: //Button 4
            begin
              //Joker
              MenuShow(SM_Party_Joker);
            end;
        end;
      end;

    SM_Party_Joker:
      begin
        Visible := False;
        Case Interaction of
          0: //Button 1
            begin
              //Joker Team 1
              ScreenSong.DoJoker(0, 0);
            end;

          1: //Button 2
            begin
              //Joker Team 2
              ScreenSong.DoJoker(1, 0);
            end;

          2: //Button 3
            begin
              //Joker Team 3
              ScreenSong.DoJoker(2, 0);
            end;

          4: //Button 4
            begin
              //Cancel... (Fo back to old Menu)
              MenuShow(SM_Party_Main);
            end;
        end;
      end;
  end;
end;

end.
 