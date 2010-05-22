unit UScreenOptionsThemes;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsThemes = class(TMenu)
    private
      procedure ReloadTheme;

    public
      AktualTheme:  Integer;
      SkinSelect:   Integer;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure InteractInc; override;
      procedure InteractDec; override;
  end;

const
  ID='ID_013';   //for help system

implementation

uses UGraphic, USkins, UHelp, ULog, UPlaylist;

function TScreenOptionsThemes.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Ini.Save;

          // Reload all screens, after Theme changed
          if(AktualTheme<>Ini.Theme) then
          begin
            UGraphic.UnLoadScreens();
            UGraphic.LoadScreens( true );
            ScreenSong.Refresh(true);
            PlaylistMan.LoadPlayLists;
          end;
          Music.PlayBack;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 3 then
          begin
            Ini.Save;

            // Reload all screens, after Theme changed
            if(AktualTheme<>Ini.Theme) then
            begin
              UGraphic.UnLoadScreens();
              UGraphic.LoadScreens( true );
              ScreenSong.Refresh(true);
              PlaylistMan.LoadPlayLists;
            end;
            Music.PlayBack;
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 2) then
          begin
            Music.PlayOption;
            InteractInc;
          end;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 2) then
          begin
            Music.PlayOption;
            InteractDec;
          end;
        end;
    end;
  end;
end;

procedure TScreenOptionsThemes.InteractInc;
begin
  inherited InteractInc;
  //Update Skins
  if (SelInteraction = 0) then
  begin
    Skin.OnThemeChange;
    UpdateSelectSlideOptions (Theme.OptionsThemes.SelectSkin, SkinSelect, ISkin, Ini.SkinNo);
  end;

  ReloadTheme();
end;

procedure TScreenOptionsThemes.InteractDec;
begin
  inherited InteractDec;
  //Update Skins
  if (SelInteraction = 0) then
  begin
    Skin.OnThemeChange;
    UpdateSelectSlideOptions (Theme.OptionsThemes.SelectSkin, SkinSelect, ISkin, Ini.SkinNo);
  end;

  ReloadTheme();  
end;

constructor TScreenOptionsThemes.Create;
{var
  I:      integer;}
begin
  inherited Create;
  

  LoadFromTheme(Theme.OptionsThemes);

  AddSelectSlide(Theme.OptionsThemes.SelectTheme, Ini.Theme, ITheme);

  SkinSelect := AddSelectSlide(Theme.OptionsThemes.SelectSkin, Ini.SkinNo, ISkin);

  AddSelectSlide(Theme.OptionsThemes.SelectColor, Ini.Color, IColor);

  AddButton(Theme.OptionsThemes.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);
end;

procedure TScreenOptionsThemes.onShow;
begin
  Interaction := 0;
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsThemes)');

  AktualTheme := Ini.Theme;
end;

procedure TScreenOptionsThemes.ReloadTheme;
begin
  Theme.LoadTheme('Themes\' + ITheme[Ini.Theme] + '.ini', Ini.Color);

  ScreenOptionsThemes := TScreenOptionsThemes.create();
  ScreenOptionsThemes.onshow;
  Display.ActualScreen := @ScreenOptionsThemes;

  ScreenOptionsThemes.Interaction    := self.Interaction;
  ScreenOptionsThemes.Draw;


  Display.Draw;
  SwapBuffers;

  ScreenOptionsThemes.AktualTheme := self.AktualTheme;

  freeandnil( self );
end;

end.
