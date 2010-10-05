unit UScreenMain;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes, UHelp;

type
  TScreenMain = class(TMenu)
    private
      IdleTicks:              cardinal;
      procedure StartScreenScredits;
    public
      TextDescription:        integer;
      TextDescriptionLong:    integer;

      ShowNumErrors:          boolean;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function  Draw: boolean; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure InteractInc; override;
      procedure InteractDec; override;
      procedure SetAnimationProgress(Progress: real); override;
      //function Draw: boolean; override;
  end;

const
  ID='ID_003';   //for help system
  
implementation

uses
  Windows,
  UPlaylist,
  UGraphic,
  UMain,
  UIni,
  UTexture,
  USongs,
  Textgl,
  opengl,
  ULanguage,
  UParty,
  UDLLManager,
  UScreenCredits,
  USkins,
  ULog;

procedure TScreenMain.StartScreenScredits;
begin
  //If CreditsScreen is not Created -> Then Create
  If (ScreenCredits = nil) then
  begin
    try
      //Display White Loading Text
      SetFontStyle(2); //Font: Outlined1
      SetFontSize(12);
      SetFontItalic(False);
      SetFontPos (400 - glTextWidth ('Loading Credits ...')/2, 250); //Position
      glColor4f(1,1,1,1);
      glPrint('Loading Credits ...');
      SwapBuffers;

      ScreenCredits    :=       TScreenCredits.Create;
    except
      Log.LogError ('Couldn''t Create Credits Screen');
    end;
  end;

  If (ScreenCredits <> nil) then
  begin
    Music.PlayStart;
    FadeTo(@ScreenCredits);
  end;
end;

function TScreenMain.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;
  I, num:        Integer;

begin
  Result := true;
  IdleTicks := SDL_GetTicks;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

  num := 0; //suitable songs for party-modes
  for i := 0 to Length(Songs.Song) - 1 do
  begin
    if not Songs.Song[i].isDuet then
      inc(num);
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
          Result := False;
        end;

      SDLK_C:
        begin
          if (SDL_ModState = KMOD_LALT) then
            StartScreenScredits;
        end;
      SDLK_M:
        begin
          if SDL_ModState = KMOD_LSHIFT then
          begin
            if (num >= 1) then
            begin
              if (Length(DLLMan.Plugins)>=1) then
              begin
                Music.PlayStart;
                FadeTo(@ScreenPartyOptionsM2);
              end
              else //show error message, No Plugins Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            end
            else //show error message, No Songs Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end else
          begin
            if (num >= 1) then
            begin
              if (Length(DLLMan.Plugins)>=1) then
              begin
                Music.PlayStart;
                FadeTo(@ScreenPartyOptions);
              end
              else //show error message, No Plugins Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            end
            else //show error message, No Songs Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;
        end;

      SDLK_S:
        begin
          Music.PlayStart;
          FadeTo(@ScreenStatMain);
        end;

      SDLK_E:
        begin
          Music.PlayStart;
          FadeTo(@ScreenEdit);
        end;

      SDLK_RETURN:
        begin
          //Solo
          if (Interaction = 0) then
          begin
            if (Length(Songs.Song) >= 1) then
            begin
              Music.PlayStart;
              if (Ini.Players >= 0) and (Ini.Players <= 3) then PlayersPlay := Ini.Players + 1;
              if (Ini.Players = 4) then PlayersPlay := 6;

              ScreenName.Goto_SingScreen := False;
              FadeTo(@ScreenName);
            end
            else //show error message
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;

          //Multi
          if Interaction = 1 then begin
            if (num >= 1) then
            begin
              if (Length(DLLMan.Plugins)>=1) then
              begin
                Music.PlayStart;
                FadeTo(@ScreenPartyOptions);
              end
              else //show error message, No Plugins Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            end
            else //show error message, No Songs Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;

          //Multi M2
          if Interaction = 2 then begin
            if (num >= 1) then
            begin
              if (Length(DLLMan.Plugins)>=1) then
              begin
                Music.PlayStart;
                FadeTo(@ScreenPartyOptionsM2);
              end
              else //show error message, No Plugins Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            end
            else //show error message, No Songs Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;

          //Stats
          if Interaction = 3 then begin
            Music.PlayStart;
            FadeTo(@ScreenStatMain);
          end;

          //Editor
          if Interaction = 4 then begin
            Music.PlayStart;
            FadeTo(@ScreenEdit);
          end;

          //Options
          if Interaction = 5 then begin
            Music.PlayStart;
            FadeTo(@ScreenOptions);
          end;

          //Exit
          if Interaction = 6 then begin
            Result := false;
          end;
        end;
      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractInc;
      SDLK_UP:      InteractDec;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenMain.Create;
begin
  inherited Create;

  //----------------
  //Attention ^^:
  //New Creation Order needed because of LoadFromTheme
  //and Button Collections.
  //At First Custom Texts and Statics
  //Then LoadFromTheme
  //after LoadFromTheme the Buttons and Selects
  //----------------


  TextDescription := AddText(Theme.Main.TextDescription);
  TextDescriptionLong := AddText(Theme.Main.TextDescriptionLong);

  LoadFromTheme(Theme.Main);

  AddButton(Theme.Main.ButtonSolo);
  AddButton(Theme.Main.ButtonMulti);
  AddButton(Theme.Main.ButtonMultiM2);
  AddButton(Theme.Main.ButtonStat);
  AddButton(Theme.Main.ButtonEditor);
  AddButton(Theme.Main.ButtonOptions);
  AddButton(Theme.Main.ButtonExit);

  Interaction := 0;
  ShowNumErrors := true;
end;

procedure TScreenMain.onShow;
var
  J:  integer;
begin
  IdleTicks := SDL_GetTicks;
  ScreenSong.Mode := smNormal;
  ScreenSong.SongIndex := -1;
  PlaylistMan.Mode := 0;
  if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenMain)');

  if (Ini.LoadFaultySongs=0) and ShowNumErrors and (Songs.NumFaultySongs>0) then
  begin
    ScreenPopupCheck.ShowPopup(Format(Language.Translate('MSG_ERROR_SONGLOADING'), [Songs.NumFaultySongs]));
  end else
    ShowNumErrors := false;

  if (Ini.tabs_temp <> Ini.tabs) or (Ini.sorting_temp <> Ini.Sorting)then
  begin
    Ini.Tabs := Ini.tabs_temp;
    Ini.Sorting := Ini.sorting_temp;
    ScreenSong.Refresh(true);
    PlaylistMan.LoadPlayLists;
  end;
end;

function TScreenMain.Draw: boolean;
begin
  Result := inherited Draw;

  if ScreenPopupHelp.Visible or ScreenPopupCheck.Visible or ScreenPopupError.Visible then
    IdleTicks := SDL_GetTicks;

  if (IdleTicks + 30*1000 < SDL_GetTicks) then
  begin
    StartScreenScredits;
  end;
end;

procedure TScreenMain.InteractNext;
begin
  inherited InteractNext;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.InteractPrev;
begin
  inherited InteractPrev;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.InteractDec;
begin
  inherited InteractDec;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.InteractInc;
begin
  inherited InteractInc;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
end;

procedure TScreenMain.SetAnimationProgress(Progress: real);
begin
  Static[0].Texture.ScaleW := Progress;
  Static[0].Texture.ScaleH := Progress;
end;
end.