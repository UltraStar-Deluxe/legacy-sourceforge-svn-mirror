unit UScreenOptionsGame;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes, USongs;

type
  TScreenOptionsGame = class(TMenu)
    private
      old_Tabs, old_Sorting: integer;

      procedure Leave;
      procedure RefreshSongs;
      
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
  end;

const
  ID='ID_008';   //for help system
  
implementation

uses UGraphic, UHelp, ULog, UPlaylist;

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
          if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
            Music.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 6) then begin
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
  if (ini.Sorting <> old_Sorting) or (ini.Tabs <> old_Tabs) then
  begin
    ScreenSong.Refresh(false);
    PlaylistMan.LoadPlayLists;
  end;
end;

procedure TScreenOptionsGame.onShow;
begin
//  Interaction := 0;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsGame)');

  //Refresh Songs Patch
  old_Sorting := Ini.Sorting;
  old_Tabs    := Ini.Tabs;
end;

procedure TScreenOptionsGame.Leave;
begin


  Music.PlayBack;
  RefreshSongs;
  FadeTo(@ScreenOptions);
end;

end.