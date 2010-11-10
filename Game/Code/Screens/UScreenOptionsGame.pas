unit UScreenOptionsGame;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes, USongs;

type
  TScreenOptionsGame = class(TMenu)
    private
      old_Tabs, old_Sorting: integer;
      old_Language:          integer;

      procedure Leave;
      procedure RefreshSongs;
      procedure RefreshLanguage;
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

const
  ID='ID_008';   //for help system
  
implementation

uses UGraphic, UHelp, ULog, UPlaylist, ULanguage, TextGL, UTexture;

function TScreenOptionsGame.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Leave;
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 7 then
          begin
            Leave;
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 6) then
          begin
            Music.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 6) then
          begin
            Music.PlayOption;
            InteractDec;
          end;
        end;
    end;
  end;
end;

constructor TScreenOptionsGame.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsGame);

  AddSelect(Theme.OptionsGame.SelectPlayers, Ini.Players, IPlayers);
  AddSelect(Theme.OptionsGame.SelectDifficulty, Ini.Difficulty, IDifficulty);
  AddSelectSlide(Theme.OptionsGame.SelectLanguage, Ini.Language, ILanguage);
  AddSelect(Theme.OptionsGame.SelectTabs, Ini.Tabs, ITabs);
  AddSelectSlide(Theme.OptionsGame.SelectSorting, Ini.Sorting, ISorting);
  AddSelectSlide(Theme.OptionsGame.SelectShuffleTime, Ini.ShuffleTime, IShuffleTime);
  AddSelect(Theme.OptionsGame.SelectDebug, Ini.Debug, IDebug);


  AddButton(Theme.OptionsGame.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

end;

//Refresh Songs Patch
procedure TScreenOptionsGame.RefreshSongs;
begin
  if (Ini.Sorting <> old_Sorting) or (Ini.Tabs <> old_Tabs) then
  begin
    ScreenSong.Refresh(false);
    PlaylistMan.LoadPlayLists;
  end;
end;

procedure TScreenOptionsGame.RefreshLanguage;
begin
  if (Ini.Language = old_Language) then
    Exit;

  UGraphic.UnLoadScreens();
  KillFont;

  BuildFont;
  Language.ChangeLanguage(ILanguage[Ini.Language]);
  Help.ChangeLanguage(ILanguage[Ini.Language]);

  Theme.LoadTheme('Themes\' + ITheme[Ini.Theme] + '.ini', Ini.Color);
  UGraphic.LoadScreens( true );
  ScreenSong.Refresh(true);
  PlaylistMan.LoadPlayLists;

  old_language := Ini.Language;
  old_Sorting := Ini.Sorting;
  old_Tabs    := Ini.Tabs;
  ScreenMain.ShowNumErrors := false;
end;

procedure TScreenOptionsGame.onShow;
begin
//  Interaction := 0;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsGame)');

  //Refresh Songs Patch
  old_Sorting := Ini.Sorting;
  old_Tabs    := Ini.Tabs;
  old_Language := Ini.Language;
end;

procedure TScreenOptionsGame.Leave;
begin
  Ini.Save;
  RefreshLanguage;
  RefreshSongs;

  Music.PlayBack;
  FadeTo(@ScreenOptions);
end;

end.