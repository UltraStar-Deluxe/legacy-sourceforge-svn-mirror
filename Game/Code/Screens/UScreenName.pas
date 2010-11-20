unit UScreenName;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenName = class(TMenu)
    public
      Goto_SingScreen: Boolean; //If True then next Screen in SingScreen
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure Refresh;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_004';   //for help system

implementation

uses UGraphic, UMain, UIni, UTexture, UHelp, ULog;

function TScreenName.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  I:    integer;
SDL_ModState:  Word;
begin
  Result := true;

  if not (ScanCode in [0..31, 127..159]) then
  begin
    Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text + chr(ScanCode);
    Exit;
  end;

  If (PressedDown) Then
  begin // Key Down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      // Templates for Names Mod
      SDLK_F1:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[0] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[0];
        end;
      SDLK_F2:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[1] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[1];
        end;
      SDLK_F3:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[2] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[2];
        end;
      SDLK_F4:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[3] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[3];
        end;
      SDLK_F5:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[4] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[4];
        end;
      SDLK_F6:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[5] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[5];
        end;
      SDLK_F7:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[6] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[6];
        end;
      SDLK_F8:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[7] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[7];
        end;
      SDLK_F9:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[8] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[8];
        end;
      SDLK_F10:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[9] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[9];
        end;
      SDLK_F11:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[10] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[10];
        end;
      SDLK_F12:
        if (SDL_ModState = KMOD_LALT) then
        begin
          Ini.NameTemplate[11] := Button[Interaction].Text[0].Text;
        end else
        begin
          Button[Interaction].Text[0].Text := Ini.NameTemplate[11];
        end;


      SDLK_BACKSPACE:
        begin
          Button[Interaction].Text[0].DeleteLastL;
        end;

      SDLK_ESCAPE:
        begin
          Ini.SaveNames;
          Music.PlayBack;
          if GoTo_SingScreen then
            FadeTo(@ScreenSong)
          else
            FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin
          for I := 1 to 6 do
            Ini.Name[I-1] := Button[I-1].Text[0].Text;
          Ini.SaveNames;
          Music.PlayStart;

          if GoTo_SingScreen then
            FadeTo(@ScreenSing)
          else
            FadeTo(@ScreenLevel);

          GoTo_SingScreen := False;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:
        begin
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            if (Ini.Players>0) then
              Dec(Ini.Players);

            if (Ini.Players >= 0) and (Ini.Players <= 3) then PlayersPlay := Ini.Players + 1;
            if (Ini.Players = 4) then PlayersPlay := 6;

            Refresh;
          end else
            InteractNext;
        end;

      SDLK_UP:
        begin
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            if (Ini.Players<4) then
              Inc(Ini.Players);

            if (Ini.Players >= 0) and (Ini.Players <= 3) then PlayersPlay := Ini.Players + 1;
            if (Ini.Players = 4) then PlayersPlay := 6;
            Refresh;
          end else
            InteractPrev;
        end;

      SDLK_RIGHT:
        begin
          InteractNext;
        end;

      SDLK_LEFT:
        begin
          InteractPrev;
        end;

    end;
  end;
end;

constructor TScreenName.Create;
var
  I:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Name);


  for I := 1 to 6 do
    AddButton(Theme.Name.ButtonPlayer[I]);

  Interaction := 0;
end;

procedure TScreenName.onShow;
var
  I:    integer;
begin
  for I := 1 to 6 do
    Button[I-1].Text[0].Text := Ini.Name[I-1];

  Refresh;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenName)');
end;

procedure TScreenName.Refresh;
var
  I:    integer;
begin
  for I := 1 to PlayersPlay do
  begin
    Button[I-1].Visible := true;
    Button[I-1].Selectable := true;
  end;

  for I := PlayersPlay+1 to 6 do
  begin
    Button[I-1].Visible := false;
    Button[I-1].Selectable := false;
  end;
end;

procedure TScreenName.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  for I := 1 to 6 do
    Button[I-1].Texture.ScaleW := Progress;
end;

end.
