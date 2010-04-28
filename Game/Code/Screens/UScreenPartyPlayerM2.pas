unit UScreenPartyPlayerM2;

Interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPartyPlayerM2 = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

const
  ID='ID_021';   //for help system

implementation

uses UGraphic, UMain, UIni, UTexture, UPartyM2, UHelp, ULog;

function TScreenPartyPlayerM2.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  {I,} J:    integer;
  SDL_ModState:  Word;
  procedure IntNext;
  begin
    repeat
      InteractNext;
    until Button[Interaction].Visible;
  end;
  procedure IntPrev;
  begin
    repeat
      InteractPrev;
    until Button[Interaction].Visible;
  end;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);


    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_0..SDLK_9, SDLK_A..SDLK_Z, SDLK_SPACE, SDLK_MINUS, SDLK_EXCLAIM, SDLK_COMMA, SDLK_SLASH, SDLK_ASTERISK, SDLK_QUESTION, SDLK_QUOTE, SDLK_QUOTEDBL:
        begin
          Button[Interaction].Text[0].Text := Button[Interaction].Text[0].Text + chr(ScanCode);
        end;

      // Templates for Names Mod
      SDLK_F1:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[0] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[0];
         end;
      SDLK_F2:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[1] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[1];
         end;
      SDLK_F3:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[2] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[2];
         end;
      SDLK_F4:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[3] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[3];
         end;
      SDLK_F5:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[4] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[4];
         end;
      SDLK_F6:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[5] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[5];
         end;
      SDLK_F7:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[6] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[6];
         end;
      SDLK_F8:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[7] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[7];
         end;
      SDLK_F9:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[8] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[8];
         end;
      SDLK_F10:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[9] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[9];
         end;
      SDLK_F11:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[10] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[10];
         end;
      SDLK_F12:
       if (SDL_ModState = KMOD_LALT) then
         begin
           Ini.NameTemplate[11] := Button[Interaction].Text[0].Text;
         end
         else
         begin
           Button[Interaction].Text[0].Text := Ini.NameTemplate[11];
         end;

      SDLK_BACKSPACE:
        begin
          Button[Interaction].Text[0].DeleteLastL;
        end;

      SDLK_ESCAPE :
        begin
          Ini.SaveNames;
          Music.PlayBack;
          FadeTo(@ScreenPartyOptionsM2);
        end;

      SDLK_RETURN:
        begin
          Ini.SaveNames;
          //Save PlayerNames
          for J := 0 to PartySessionM2.Players.NumPlayer-1 do
          begin
            PartySessionM2.Players.Playerinfo[J].Name := PChar(Button[J].Text[0].Text);
            PartySessionM2.Players.Playerinfo[J].NumPlayed := 0;
          end;

          Music.PlayStart;
          FadeTo(@ScreenPartyNewRoundM2);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    IntNext;
      SDLK_UP:      IntPrev;
      SDLK_RIGHT:   IntNext;
      SDLK_LEFT:    IntPrev;
    end;
  end;
end;

constructor TScreenPartyPlayerM2.Create;
{var
  I:    integer;}
begin
  inherited Create;

  LoadFromTheme(Theme.PartyPlayerM2);

  AddButton(Theme.PartyPlayerM2.Player1Name);
  AddButton(Theme.PartyPlayerM2.Player2Name);
  AddButton(Theme.PartyPlayerM2.Player3Name);
  AddButton(Theme.PartyPlayerM2.Player4Name);
  AddButton(Theme.PartyPlayerM2.Player5Name);
  AddButton(Theme.PartyPlayerM2.Player6Name);
  AddButton(Theme.PartyPlayerM2.Player7Name);
  AddButton(Theme.PartyPlayerM2.Player8Name);
  AddButton(Theme.PartyPlayerM2.Player9Name);

  Interaction := 0;
end;

procedure TScreenPartyPlayerM2.onShow;
var
  I:    integer;
begin
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenPartyPlayerM2)');

  // Templates for Names Mod
  for I := 1 to 9 do
  begin
    Button[I-1].Text[0].Text := Ini.NameTemplate[I-1];
    if (PartySessionM2.Players.NumPlayer >=I) then
    begin
      Button[I-1].Visible := true;
    end else
    begin
      Button[I-1].Visible := false;
    end;
  end;
end;

procedure TScreenPartyPlayerM2.SetAnimationProgress(Progress: real);
var
  I:    integer;
begin
  for I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
