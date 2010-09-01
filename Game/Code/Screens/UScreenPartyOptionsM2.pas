unit UScreenPartyOptionsM2;

interface

uses
  UMenu,
  gl,
  glu,
  TextGL,
  SDL,
  UDisplay,
  UMusic,
  UFiles,
  SysUtils,
  UPartyM2,
  UThemes;

type
  TScreenPartyOptionsM2 = class(TMenu)
    public
      ID_DUELL:         Byte;
      SelectLevel:      cardinal;
      SelectPlayList:   cardinal;
      SelectPlayList2:  cardinal;
      NameRounds:       cardinal;
      CountRounds:      cardinal;
      SelectPlayers:    cardinal;
      SelectRounds:     cardinal;
      SelectOptionPlugin:      cardinal;
      SelectOptionHandicap: cardinal;

      MenuPluginOpen:   boolean;

      PluginList:       array of TPartyPlugin;
      SelectedPlugin:   Integer;

      PlayList:  integer;
      PlayList2: integer;
      Rounds:    integer;
      NumPlayer: integer;
      OptionPlugin: integer;
      OptionHandicap: integer;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure SetAnimationProgress(Progress: real); override;
      procedure SetPlaylist2;

      procedure Draw_MenuPlugin;
      function PartyInput_MenuPlugin(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): boolean;
  end;

var
  IPlaylist: array[0..2] of String;
  IPlaylist2: array of String;
  IRounds: array of String;
  IOptionYesNo: array of String;

const
  IPlayers: array[0..7] of String = ('2', '3', '4', '5', '6', '7', '8', '9');
  MAX_ROUNDS: Integer = 100;
  ID='ID_020';   //for help system

implementation

uses
  UGraphic,
  UMain,
  UIni,
  UTexture,
  ULanguage,
  UDLLManager,
  UPlaylist,
  UHelp,
  ULog,
  USongs;

function TScreenPartyOptionsM2.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  I, J, Len: integer;
  DuellAvailible: boolean;

begin
  Result := true;

  if MenuPluginOpen then
  begin
    PartyInput_MenuPlugin(PressedKey, ScanCode, PressedDown);
    Exit;
  end;

  if (PressedDown) then
  begin
    // check special keys
    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Music.PlayBack;
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          if not (OptionPlugin=1) then
          begin
            //Don'T start when Playlist is Selected and there are no Playlists
            if (Playlist = 2) and (Length(PlaylistMan.Playlists) = 0) then
              Exit;

            //check plugins
            SetLength(PartySessionM2.Plugins, 0);
            DuellAvailible := false;
            for I := 0 to Length(PluginList)-1 do
            begin
              //deactivate if not select plugins
              //deselect all Plugins
              if (OptionPlugin=0) then
                PluginList[I].Selected := false;

              //activate only duell plugin
              if (OptionPlugin=0) and (PluginList[I].ID=ID_DUELL) and not PluginList[I].Medley then
              begin
                DuellAvailible := true;
                if OptionPlugin=0 then
                  PluginList[I].Selected := true;
              end;

              if PluginList[I].Selected then
              begin
                Len := Length(PartySessionM2.Plugins);
                SetLength(PartySessionM2.Plugins, Len+1);
                PartySessionM2.Plugins[Len] := PluginList[I];
              end;
            end;

            // Don't start when OptionPlugin=0 and no DUEL-Plugin
            if not DuellAvailible and (OptionPlugin=0) then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS') + ' (Duell)');
              Exit;
            end;

            if Length(PartySessionM2.Plugins)=0 then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
              Exit;
            end;

            //Save OptionPlugin
            if OptionPlugin=0 then
              PartySessionM2.Option_Plugins := false
            else
              PartySessionM2.Option_Plugins := true;

            //Save Option Handicap
            if OptionHandicap=0 then
              PartySessionM2.HandicapMode := false
            else
              PartySessionM2.HandicapMode := true;

            //Save Difficulty
            Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
            Ini.SaveLevel;

            //Save Playlist
            PlaylistMan.Mode := Playlist;
            PlaylistMan.CurPlayList := High(Cardinal);
            //If Category Selected Search Category ID
            if Playlist = 1 then
            begin
              J := -1;
              For I := 0 to high(CatSongs.Song) do
              begin
                if CatSongs.Song[I].Main and (CatSongs.NumCatSongs(CatSongs.Song[I].OrderNum)>0) then
                  Inc(J);

                if J = Playlist2 then
                begin
                  PlaylistMan.CurPlayList := I;
                  Break;
                end;
              end;

              //No Categorys or Invalid Entry
              If PlaylistMan.CurPlayList = High(Cardinal) then
                Exit;
            end
            else
              PlaylistMan.CurPlayList := Playlist2;

            PartySessionM2.ID_DUELL := ID_DUELL;

            //Start Party
            PartySessionM2.StartNewParty(NumPlayer+2, Rounds+1);

            Music.PlayStart;
            //Go to Player Screen
            FadeTo(@ScreenPartyPlayerM2);
          end else
          begin
            MenuPluginOpen := true;
            if not Help.SetHelpID('ID_030') then
              Log.LogError('No Entry for Help-ID ID_030 (ScreenPartyOptionsM2, SelectPlugins)');
          end;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:
        begin
          InteractNext;
          if Interaction=7 then
            MenuPluginOpen := true;
        end;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          Music.PlayOption;
          InteractInc;

          //Change Playlist2 if Playlist is Changed
          if (Interaction = 1) then
          begin
            SetPlaylist2;
          end
          else if(Interaction = 3) then
          begin
            Rounds:=(((NumPlayer+2)*(NumPlayer+2))-NumPlayer-2)-1;
            SelectsS[SelectRounds].SetSelectOpt(Rounds);
          end else if(Interaction = 4) then
          begin
            if  (Rounds+1 + NumPlayer + 1 <= MAX_ROUNDS-1)  then
              Rounds:=Rounds + NumPlayer + 1
            else
              dec(Rounds);

            SelectsS[SelectRounds].SetSelectOpt(Rounds);
          end else if(Interaction = 5) then //Option Plugins
          begin
            //select all Plugins
            for I := 0 to Length(PluginList) - 1 do
            begin
              PluginList[I].Selected := true;
            end;
          end;
        end;
      SDLK_LEFT:
        begin
          Music.PlayOption;
          InteractDec;

          //Change Playlist2 if Playlist is Changed
          if (Interaction = 1) then
          begin
            SetPlaylist2;
          end
          else if(Interaction = 3) then
          begin
            Rounds:=(((NumPlayer+2)*(NumPlayer+2))-NumPlayer-2)-1;
            SelectsS[SelectRounds].SetSelectOpt(Rounds);
          end else if(Interaction = 4) then
          begin
            if  (Rounds+1 - NumPlayer - 1 >= 2)  then
              Rounds:=Rounds - NumPlayer - 1
            else
              inc(Rounds);

            SelectsS[SelectRounds].SetSelectOpt(Rounds);
          end;
        end;
    end;
  end;
end;

function TScreenPartyOptionsM2.PartyInput_MenuPlugin(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): boolean;
var
  I, J, Len: integer;
  DuellAvailible: boolean;

begin
  Result := true;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_ESCAPE:
        begin
          MenuPluginOpen := false;
          if not Help.SetHelpID(ID) then
            Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyOptions)');
        end;
      
      SDLK_RETURN:
        begin
          MenuPluginOpen := false;
          if not Help.SetHelpID(ID) then
            Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyOptions)');
          //Don'T start when Playlist is Selected and there are no Playlists
            if (Playlist = 2) and (Length(PlaylistMan.Playlists) = 0) then
              Exit;

            //check plugins
            SetLength(PartySessionM2.Plugins, 0);
            DuellAvailible := false;
            for I := 0 to Length(PluginList)-1 do
            begin
              if (PluginList[I].ID=ID_DUELL) then
              begin
                DuellAvailible := true;
                if OptionPlugin=0 then
                  PluginList[I].Selected := true;
              end;

              if PluginList[I].Selected then
              begin
                Len := Length(PartySessionM2.Plugins);
                SetLength(PartySessionM2.Plugins, Len+1);
                PartySessionM2.Plugins[Len] := PluginList[I];
              end;
            end;

            // Don't start when OptionPlugin=0 and no DUEL-Plugin
            if not DuellAvailible and (OptionPlugin=0) then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS') + ' (Duell)');
              Exit;
            end;

            if Length(PartySessionM2.Plugins)=0 then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
              Exit;
            end;

            //Save OptionPlugin
            if OptionPlugin=0 then
              PartySessionM2.Option_Plugins := false
            else
              PartySessionM2.Option_Plugins := true;

            //Save Option Handicap
            if OptionHandicap=0 then
              PartySessionM2.HandicapMode := false
            else
              PartySessionM2.HandicapMode := true;

            //Save Difficulty
            Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
            Ini.SaveLevel;

            //Save Playlist
            PlaylistMan.Mode := Playlist;
            PlaylistMan.CurPlayList := High(Cardinal);
            //If Category Selected Search Category ID
            if Playlist = 1 then
            begin
              J := -1;
              For I := 0 to high(CatSongs.Song) do
              begin
                if CatSongs.Song[I].Main then
                  Inc(J);

                if J = Playlist2 then
                begin
                  PlaylistMan.CurPlayList := I;
                  Break;
                end;
              end;

              //No Categorys or Invalid Entry
              If PlaylistMan.CurPlayList = High(Cardinal) then
                Exit;
            end
            else
              PlaylistMan.CurPlayList := Playlist2;

            PartySessionM2.ID_DUELL := ID_DUELL;

            //Start Party
            PartySessionM2.StartNewParty(NumPlayer+2, Rounds+1);

            Music.PlayStart;
            //Go to Player Screen
            FadeTo(@ScreenPartyPlayerM2);
        end;
      
      SDLK_SPACE:
        begin
          Music.PlayOption;
          PluginList[SelectedPlugin].Selected := not PluginList[SelectedPlugin].Selected;
        end;

      SDLK_LEFT:
        begin
          if PluginList[SelectedPlugin].Selected then
          begin
            Music.PlayOption;
            PluginList[SelectedPlugin].Selected := false;
          end;
        end;

      SDLK_RIGHT:
        begin
          if not PluginList[SelectedPlugin].Selected then
          begin
            Music.PlayOption;
            PluginList[SelectedPlugin].Selected := true;
          end;
        end;

      SDLK_UP:
        begin
          if SelectedPlugin>0 then
            Dec(SelectedPlugin);
        end;

      SDLK_DOWN:
        begin
          if SelectedPlugin<Length(PluginList)-1 then
            Inc(SelectedPlugin);
        end;
    end; //of case
  end; //of if
end;

procedure TScreenPartyOptionsM2.Draw_MenuPlugin;
Type
  TRect = record
    left, right, top, bottom: integer;
  end;

Const
  h = 40;

  procedure DrawPlugin(num: integer; Rect: TRect; Plug: TPartyPlugin; selected: boolean);
  var
    text:   PChar;
    name:   string;

  begin
    glColor4f(1, 1, 1, 1);
    glLineWidth(1);
    if selected then
    begin
      glBegin(GL_LINE_LOOP);
        glVertex2f(Rect.left-5, Rect.top+h*num);
        glVertex2f(Rect.right+5, Rect.top+h*num);
        glVertex2f(Rect.right+5, Rect.top+h*(num+1));
        glVertex2f(Rect.left-5, Rect.top+h*(num+1));
      glEnd;
    end;

    SetFontStyle(1);
    SetFontItalic(false);
    SetFontSize(13);

    SetFontPos (Rect.left, Rect.top+ num*h);

    name := Plug.Name;

    text := Addr(name[1]);
    glPrint(text);

    SetFontPos (Rect.left+Round((Rect.right-Rect.left)/4*3), Rect.top+ num*h);
    if Plug.Selected then
      name := Language.Translate('SONG_MENU_YES')
    else
      name := Language.Translate('SONG_MENU_NO');
      
    text := Addr(name[1]);
    glPrint(text);
  end;

  procedure DrawScroll(X, Y, W, H: integer; pos, len: double);
  var
    fY, tY: double;
  begin
    glColor4f(1, 1, 1, 1);

    glLineWidth(1);
    glBegin(GL_LINE_LOOP);
      glVertex2f(X, Y);
      glVertex2f(X+W, Y);
      glVertex2f(X+W, Y+H);
      glVertex2f(X, Y+H);
    glEnd;

    fY := Y+(H-H*len)*pos;
    tY := fY+H*len;
    if tY+0.001>=Y+H then
      tY := Y+H;

    glBegin(GL_QUADS);
      glVertex2f(X, fY);
      glVertex2f(X+W, fY);
      glVertex2f(X+W, tY);
      glVertex2f(X, tY);
    glEnd;
  end;

var
  Rect:   TRect;
  abs:    real;
  ab:     real;
  I:      integer;
  barH:   double;
  pos:    double;

begin
  Rect.left := 150;
  Rect.right := 650;
  Rect.top := 30;
  Rect.bottom := 550;

  abs := 15;

  if Length(PluginList)<=13 then
  begin
    ab := 0;
    barH := 1;
  end else
  begin
    ab := 10;
    barH := h*13/(h*Length(PluginList));
  end;

  glEnable(GL_BLEND);
  glBegin(gl_quads);
    //Background:
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.left-abs, Rect.top-abs);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.right+abs+ab, Rect.top-abs);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.right+abs+ab, Rect.bottom+abs);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.left-abs, Rect.bottom+abs);
  glEnd;
  glDisable(GL_BLEND);

  //Draw MainFrame
  glColor4f(1, 1, 1, 1);

  glLineWidth(1);
  glBegin(GL_LINE_LOOP);
    glVertex2f(Rect.left-abs+1, Rect.top-abs+1);
    glVertex2f(Rect.right+abs-1, Rect.top-abs+1);
    glVertex2f(Rect.right+abs-1, Rect.bottom+abs-1);
    glVertex2f(Rect.left-abs+1, Rect.bottom+abs-1);
  glEnd;

  if barH<1 then
  begin
    pos := SelectedPlugin/(Length(PluginList)-1);

    //draw plugin names and selections
    for I := 0 to 12 do
    begin
      if (SelectedPlugin<=6) then
        DrawPlugin(I, Rect, PluginList[I], I=SelectedPlugin)
      else if ((SelectedPlugin-6)<=(Length(PluginList)-13)) then
        DrawPlugin(I, Rect, PluginList[I+(SelectedPlugin-6)],
          I+(SelectedPlugin-6)=SelectedPlugin)
      else
        DrawPlugin(I, Rect, PluginList[I+(Length(PluginList)-13)],
          I+(Length(PluginList)-13)=SelectedPlugin);
    end;

    DrawScroll(Round(Rect.right+abs-1), Rect.top-round(abs)+1,
      Round(ab), Rect.bottom-Rect.top+round(abs+abs)-2,
      pos, barH);
  end else
  begin
    for I := 0 to Length(PluginList)-1 do
      DrawPlugin(I, Rect, PluginList[I], I=SelectedPlugin)
  end;
end;

constructor TScreenPartyOptionsM2.Create;
var
  I: integer;
begin
  inherited Create;
  //Fill IPlaylist

  SetLength(IOptionYesNo, 2);
  IOptionYesNo[0] := Language.Translate('SONG_MENU_NO');
  IOptionYesNo[1] := Language.Translate('SONG_MENU_YES');

  SetLength(IRounds, Round(MAX_ROUNDS));
  for I := 1 to MAX_ROUNDS do
  begin
    IRounds[I-1]:=IntToStr(I);
  end;

  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');

  //Fill IPlaylist2
  SetLength(IPlaylist2, 1);
  IPlaylist2[0] := '---';

  //Clear all Selects
  NumPlayer := 0;
  Rounds := 0;
  PlayList := 0;
  PlayList2 := 0;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyOptionsM2);

  SelectLevel     := AddSelectSlide (Theme.PartyOptionsM2.SelectLevel, Ini.Difficulty, Theme.ILevel);
  SelectPlayList  := AddSelectSlide (Theme.PartyOptionsM2.SelectPlayList, PlayList, IPlaylist);
  SelectPlayList2 := AddSelectSlide (Theme.PartyOptionsM2.SelectPlayList2, PlayList2, IPlaylist2);
  SelectPlayers   := AddSelectSlide (Theme.PartyOptionsM2.SelectPlayers1, NumPlayer, IPlayers);
  SelectRounds    := AddSelectSlide (Theme.PartyOptionsM2.SelectRounds, Rounds, IRounds);
  SelectOptionPlugin := AddSelectSlide (Theme.PartyOptionsM2.SelectOptionPlugin, OptionPlugin, IOptionYesNo);
  SelectOptionHandicap := AddSelectSlide (Theme.PartyOptionsM2.SelectOptionHandicap, OptionHandicap, IOptionYesNo);

  Interaction := 0;
  Rounds:=1;
  SelectsS[SelectRounds].SetSelectOpt(Rounds);

end;

procedure TScreenPartyOptionsM2.SetPlaylist2;
var
  I: integer;
begin
  case Playlist of
    0:
      begin
        SetLength(IPlaylist2, 1);
        IPlaylist2[0] := '(' + IntToStr(CatSongs.NumSongs()) + ' Songs)';
      end;
    1:
      begin
        SetLength(IPlaylist2, 0);
        for I := 0 to high(CatSongs.Song) do
        begin
          if CatSongs.Song[I].Main and (CatSongs.NumCatSongs(CatSongs.Song[I].OrderNum)>0) then
          begin
            SetLength(IPlaylist2, Length(IPlaylist2) + 1);
            IPlaylist2[high(IPlaylist2)] := CatSongs.Song[I].Artist +
              ' (' + IntToStr(CatSongs.NumCatSongs(CatSongs.Song[I].OrderNum)) + ' Songs)';
          end;
        end;

        if (Length(IPlaylist2) = 0) then
        begin
          SetLength(IPlaylist2, 1);
          IPlaylist2[0] := 'No Categories found';
        end;
      end;
    2:
      begin
        if (Length(PlaylistMan.Playlists) > 0) then
        begin
          SetLength(IPlaylist2, Length(PlaylistMan.Playlists));
          PlaylistMan.GetNamesAndNumSongs(IPlaylist2);
        end
        else
        begin
          SetLength(IPlaylist2, 1);
          IPlaylist2[0] := 'No Playlists found';
        end;
      end;
  end;

  Playlist2 := 0;
  UpdateSelectSlideOptions(Theme.PartyOptionsM2.SelectPlayList2, 2, IPlaylist2, Playlist2);
end;

procedure TScreenPartyOptionsM2.onShow;
var
  Len, I:     Integer;
  ID_BLIND:   Byte;
  ID_NOSCORE: Byte;

begin
  Randomize;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyOptionsM2)');
    
  MenuPluginOpen := false;

  //Fill plugin array
  ID_DUELL := high(Byte);
  ID_BLIND := high(Byte);
  ID_NOSCORE := high(Byte);

  SetLength(PluginList, 0);
  for I := 0 to high(DLLMan.Plugins) do
  begin
    if DLLMan.Plugins[I].Name='PLUGIN_DUELL_NAME' then
      ID_DUELL := I;

    if DLLMan.Plugins[I].Name='PLUGIN_BLIND_NAME' then
      ID_BLIND := I;

    if DLLMan.Plugins[I].Name='PLUGIN_NOSCORE_NAME' then
      ID_NOSCORE := I;

    if (not DLLMan.Plugins[I].TeamModeOnly) then
    begin
      Len := Length(PluginList);
      SetLength(PluginList, Len + 1);
      PluginList[Len].ID := I;
      PluginList[Len].TimesPlayed := 0;
      PluginList[Len].Medley := false;
      PluginList[Len].MedleySurprise := false;
      PluginList[Len].Selected := true;

      PluginList[Len].Name := Language.Translate(DLLMan.Plugins[I].Name);
      PluginList[Len].Desc := Language.Translate(DLLMan.Plugins[I].PluginDesc);
    end;
  end;

  //Add Medley "Plugin" (if Plugin DUELL availible)
  if ID_DUELL<>high(Byte) then
  begin
    Len := Length(PluginList);
    SetLength(PluginList, Len + 1);
    PluginList[Len].ID := ID_DUELL;
    PluginList[Len].TimesPlayed := 0;
    PluginList[Len].Medley := true;
    PluginList[Len].MedleySurprise := false;
    PluginList[Len].Selected := true;
    PluginList[Len].Name := Language.Translate('PLUGIN_MEDLEY_NAME');
    PluginList[Len].Desc := Language.Translate('PLUGIN_MEDLEY_DESC');
  end;

  //Add Medley blind "Plugin" (if Plugin BLIND availible)
  if ID_BLIND<>high(Byte) then
  begin
    Len := Length(PluginList);
    SetLength(PluginList, Len + 1);
    PluginList[Len].ID := ID_BLIND;
    PluginList[Len].TimesPlayed := 0;
    PluginList[Len].Medley := true;
    PluginList[Len].MedleySurprise := false;
    PluginList[Len].Selected := true;
    PluginList[Len].Name := Language.Translate('PLUGIN_MEDLEYBLIND_NAME');
    PluginList[Len].Desc := Language.Translate('PLUGIN_MEDLEYBLIND_DESC');
  end;

  //Add Medley noscore "Plugin" (if Plugin NOSCORE availible)
  if ID_NOSCORE<>high(Byte) then
  begin
    Len := Length(PluginList);
    SetLength(PluginList, Len + 1);
    PluginList[Len].ID := ID_NOSCORE;
    PluginList[Len].TimesPlayed := 0;
    PluginList[Len].Medley := true;
    PluginList[Len].MedleySurprise := false;
    PluginList[Len].Selected := true;
    PluginList[Len].Name := Language.Translate('PLUGIN_MEDLEYNOSCORE_NAME');
    PluginList[Len].Desc := Language.Translate('PLUGIN_MEDLEYNOSCORE_DESC');
  end;

  //Add Medley Surprise "Plugin" (if Plugin DUELL availible)
  if ID_DUELL<>high(Byte) then
  begin
    Len := Length(PluginList);
    SetLength(PluginList, Len + 1);
    PluginList[Len].ID := ID_DUELL;
    PluginList[Len].TimesPlayed := 0;
    PluginList[Len].Medley := true;
    PluginList[Len].MedleySurprise := true;
    PluginList[Len].Selected := true;
    PluginList[Len].Name := Language.Translate('PLUGIN_MEDLEYSURPRISE_NAME');
    PluginList[Len].Desc := Language.Translate('PLUGIN_MEDLEYSURPRISE_DESC');
  end;

  //Add Medley Surprise blind "Plugin" (if Plugin BLIND availible)
  if ID_BLIND<>high(Byte) then
  begin
    Len := Length(PluginList);
    SetLength(PluginList, Len + 1);
    PluginList[Len].ID := ID_BLIND;
    PluginList[Len].TimesPlayed := 0;
    PluginList[Len].Medley := true;
    PluginList[Len].MedleySurprise := true;
    PluginList[Len].Selected := true;
    PluginList[Len].Name := Language.Translate('PLUGIN_MEDLEYSURPRISEBLIND_NAME');
    PluginList[Len].Desc := Language.Translate('PLUGIN_MEDLEYSURPRISEBLIND_DESC');
  end;

  SelectedPlugin := 0;
  ScreenSong.Mode := smChallenge;
  SetPlaylist2;
end;

function TScreenPartyOptionsM2.Draw: boolean;
begin
  // draw static menu
  inherited Draw;

  if MenuPluginOpen then
    Draw_MenuPlugin;

  Result := true;
end;

procedure TScreenPartyOptionsM2.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
