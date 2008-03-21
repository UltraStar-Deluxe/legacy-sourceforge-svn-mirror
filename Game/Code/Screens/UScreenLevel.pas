unit UScreenLevel;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenLevel = class(TMenu)
    public
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic,
     UMain,
     UIni,
     USong,
     UTexture;

function TScreenLevel.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenName);
        end;

      SDLK_RETURN:
        begin
          Ini.Difficulty := Interaction;
          Ini.SaveLevel;
          AudioPlayback.PlaySound(SoundLib.Start);
          //Set Standard Mode
          ScreenSong.Mode := smNormal;
          FadeTo(@ScreenSong);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end;
end;

constructor TScreenLevel.Create;
//var
// I:    integer; // Auto Removed, Unused Variable
begin
  inherited Create;

  LoadFromTheme(Theme.Level);

  AddButton(Theme.Level.ButtonEasy);
  AddButton(Theme.Level.ButtonMedium);
  AddButton(Theme.Level.ButtonHard);

  Interaction := 0;
end;

procedure TScreenLevel.onShow;
begin
  inherited;

  Interaction := Ini.Difficulty;

//  LCD.WriteText(1, '  Choose mode:  ');
//  UpdateLCD;
end;

procedure TScreenLevel.SetAnimationProgress(Progress: real);
begin
  Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress;
end;

end.
