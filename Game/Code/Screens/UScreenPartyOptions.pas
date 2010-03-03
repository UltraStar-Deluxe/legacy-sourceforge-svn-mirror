unit UScreenPartyOptions;

interface

uses
  UMenu, gl, glu, TextGL, SDL, UDisplay, UMusic, UParty, UFiles, SysUtils, UThemes;

type
  TScreenPartyOptions = class(TMenu)
    public
      SelectLevel: Cardinal;
      SelectPlayList: Cardinal;
      SelectPlayList2: Cardinal;
      SelectRounds: Cardinal;
      SelectTeams: Cardinal;
      SelectPlayers1: Cardinal;
      SelectPlayers2: Cardinal;
      SelectPlayers3: Cardinal;

      PluginList:       array of TPartyPlugin;
      SelectedPlugin:   Integer;
      MenuPluginOpen:   boolean;

      PlayList:  Integer;
      PlayList2: Integer;
      Rounds:    Integer;
      NumTeams:  Integer;
      NumPlayer1, NumPlayer2, NumPlayer3: Integer;
      
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
const
  ITeams: array[0..1] of String =('2', '3');
  IPlayers: array[0..3] of String =('1', '2', '3', '4');
  IRounds: array[0..30] of String = ('2', '3', '4', '5', '6', '7', '8', '9', '10',
    '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23', '24', '25', '26', '27', '28', '29', '30',
    '31', '32');
  ID='ID_015';   //for help system

implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UDLLManager, UPlaylist, USongs, UHelp, ULog;

function TScreenPartyOptions.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;

  if MenuPluginOpen then
  begin
    PartyInput_MenuPlugin(PressedKey, ScanCode, PressedDown);
    Exit;
  end;

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
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          MenuPluginOpen := true;
          if not Help.SetHelpID('ID_030') then
            Log.LogError('No Entry for Help-ID ID_030 (ScreenPartyOptions, SelectPlugins)');
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          Music.PlayOption;
          InteractInc;

          //Change Playlist2 if Playlist is Changed
          If (Interaction = 1) then
          begin
            SetPlaylist2;
          end //Change Team3 Players visibility
          Else If (Interaction = 4) then
          begin
              SelectsS[7].Visible := (NumTeams = 1);
          end;
        end;
      SDLK_LEFT:
        begin
          Music.PlayOption;
          InteractDec;

          //Change Playlist2 if Playlist is Changed
          If (Interaction = 1) then
          begin
            SetPlaylist2;
          end //Change Team3 Players visibility
          Else If (Interaction = 4) then
          begin
            SelectsS[7].Visible := (NumTeams = 1);
          end;
        end;
    end;
  end;
end;

function TScreenPartyOptions.PartyInput_MenuPlugin(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): boolean;
var
  I, J, Len:        Integer;
  OnlyMultiPlayer:  boolean;
  SinglePlayerTeams:      boolean;

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
          If (Playlist = 2) and (Length(PlaylistMan.Playlists) = 0) then
            Exit;
          // Don't start when SinglePlayer Teams but only Multiplayer Plugins available
          OnlyMultiPlayer:=true;
          SinglePlayerTeams := ((NumPlayer1 = 0) OR (NumPlayer2 = 0) OR ((NumPlayer3 = 0) AND
            (NumTeams = 1)));

          SetLength(PartySession.Plugins, 0);
          for I := 0 to Length(PluginList)-1 do
          begin
            if PluginList[I].Selected then
            begin
              if PluginList[I].Medley or
                (not DLLMan.Plugins[PluginList[I].ID].TeamModeOnly) or
                (not SinglePlayerTeams and DLLMan.Plugins[PluginList[I].ID].TeamModeOnly) then
              begin
                Len := Length(PartySession.Plugins);
                SetLength(PartySession.Plugins, Len+1);
                PartySession.Plugins[Len] := PluginList[I];
              end;

              if not PluginList[I].Medley then
                OnlyMultiPlayer := (OnlyMultiPlayer AND
                  DLLMan.Plugins[PluginList[I].ID].TeamModeOnly)
              else
                OnlyMultiPlayer := false;
            end;
          end;
          
          if (OnlyMultiPlayer) AND SinglePlayerTeams or
            (Length(PartySession.Plugins)=0) then begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            Exit;
          end;
          //Save Difficulty
          Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
          Ini.SaveLevel;


          //Save Num Teams:
          PartySession.Teams.NumTeams := NumTeams + 2;
          PartySession.Teams.Teaminfo[0].NumPlayers := NumPlayer1+1;
          PartySession.Teams.Teaminfo[1].NumPlayers := NumPlayer2+1;
          PartySession.Teams.Teaminfo[2].NumPlayers := NumPlayer3+1;

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

          //Start Party
          PartySession.StartNewParty(Rounds + 2);

          Music.PlayStart;
          //Go to Player Screen
          FadeTo(@ScreenPartyPlayer);
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

procedure TScreenPartyOptions.Draw_MenuPlugin;
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

function TScreenPartyOptions.Draw: boolean;
begin
  // draw static menu
  inherited Draw;

  if MenuPluginOpen then
    Draw_MenuPlugin;
end;

constructor TScreenPartyOptions.Create;
var
  I:    integer;
begin
  inherited Create;
  //Fill IPlaylist
  IPlaylist[0] := Language.Translate('PARTY_PLAYLIST_ALL');
  IPlaylist[1] := Language.Translate('PARTY_PLAYLIST_CATEGORY');
  IPlaylist[2] := Language.Translate('PARTY_PLAYLIST_PLAYLIST');

  //Fill IPlaylist2
  SetLength(IPlaylist2, 1);
  IPlaylist2[0] := '---';

  //Clear all Selects
  NumTeams := 0;
  NumPlayer1 := 0;
  NumPlayer2 := 0;
  NumPlayer3 := 0;
  Rounds := 5;
  PlayList := 0;
  PlayList2 := 0;

  //Load Screen From Theme
  LoadFromTheme(Theme.PartyOptions);

  SelectLevel := AddSelectSlide (Theme.PartyOptions.SelectLevel, Ini.Difficulty, Theme.ILevel);
  SelectPlayList := AddSelectSlide (Theme.PartyOptions.SelectPlayList, PlayList, IPlaylist);
  SelectPlayList2 := AddSelectSlide (Theme.PartyOptions.SelectPlayList2, PlayList2, IPlaylist2);
  SelectRounds := AddSelectSlide (Theme.PartyOptions.SelectRounds, Rounds, IRounds);
  SelectTeams := AddSelectSlide (Theme.PartyOptions.SelectTeams, NumTeams, ITeams);
  SelectPlayers1 := AddSelectSlide (Theme.PartyOptions.SelectPlayers1, NumPlayer1, IPlayers);
  SelectPlayers2 := AddSelectSlide (Theme.PartyOptions.SelectPlayers2, NumPlayer2, IPlayers);
  SelectPlayers3 := AddSelectSlide (Theme.PartyOptions.SelectPlayers3, NumPlayer3, IPlayers);

  Interaction := 0;

  //Hide Team3 Players
  SelectsS[7].Visible := False;
end;

procedure TScreenPartyOptions.SetPlaylist2;
var I: Integer;
begin
  Case Playlist of
    0:
      begin
        SetLength(IPlaylist2, 1);
        IPlaylist2[0] := '---';
      end;
    1:
      begin
        SetLength(IPlaylist2, 0);
        For I := 0 to high(CatSongs.Song) do
        begin
          If (CatSongs.Song[I].Main) then
          begin
            SetLength(IPlaylist2, Length(IPlaylist2) + 1);
            IPlaylist2[high(IPlaylist2)] := CatSongs.Song[I].Artist;
          end;
        end;

        If (Length(IPlaylist2) = 0) then
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
          PlaylistMan.GetNames(IPlaylist2);
        end
        else
        begin
          SetLength(IPlaylist2, 1);
          IPlaylist2[0] := 'No Playlists found';
        end;
      end;
  end;

  Playlist2 := 0;
  UpdateSelectSlideOptions(Theme.PartyOptions.SelectPlayList2, 2, IPlaylist2, Playlist2);
end;

procedure TScreenPartyOptions.onShow;
var
  Len, I:     Integer;
  ID_DUELL:   Integer;
  ID_BLIND:   Integer;
  ID_NOSCORE: Integer;

begin
  Randomize;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyOptions)');

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
end;

procedure TScreenPartyOptions.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
