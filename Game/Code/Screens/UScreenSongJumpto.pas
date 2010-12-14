unit UScreenSongJumpto;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes, UVideo;

type
  TScreenSongJumpto = class(TMenu)
    private
      //For ChangeMusic
      LastPlayed:   Integer;
      VisibleBool:  Boolean;
      isDuet:       Boolean;
    public
      VisSongs: Integer;

      constructor Create; override;

      //Visible //Whether the Menu should be Drawn
      //Whether the Menu should be Drawn
      procedure SetVisible(Value: Boolean);
      property Visible: Boolean read VisibleBool write SetVisible;

      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;

      procedure SetTextFound(const Count: Cardinal);
      procedure ToggleDuetFilter();
      procedure SetDuetFilter();
      procedure ResetDuetFilter();
      procedure RefreshDuetFilter();
  end;

var
  IType: Array [0..2] of String;
  SelectType: Integer;

const
  ID='ID_031';   //for help system

implementation

uses UGraphic, UHelp, UMain, UIni, UTexture, ULanguage, UParty, USongs, UScreenSong, ULog;

function TScreenSongJumpto.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;

  if not (ScanCode in [0..31, 127..159]) then
  begin
    if not isDuet then
    begin
      if Interaction = 0 then
      begin
        Button[0].Text[0].Text := Button[0].Text[0].Text + chr(ScanCode);
        SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
      end;
    end;
    Exit;
  end;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
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

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_BACKSPACE:
        begin
          if (Interaction = 0) AND (Length(Button[0].Text[0].Text) > 0) and not isDuet then
          begin
            Button[0].Text[0].DeleteLastL;
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          end else if (Interaction = 0) and isDuet then
          begin
            Button[0].Text[0].Text := '';
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
            isDuet := false;
          end;
        end;

      SDLK_F1:
        begin
          ToggleDuetFilter;
        end;

      SDLK_RETURN,
      SDLK_ESCAPE:
        begin
          Visible := False;
          Music.PlayBack;
          if (VisSongs = 0) AND (Length(Button[0].Text[0].Text) > 0) then
          begin
            ScreenSong.UnLoadDetailedCover;
            Button[0].Text[0].Text := '';
            CatSongs.SetFilter('', 0);
            SetTextFound(0);
          end;
          if(ScreenSong.Mode = smNormal) and not ScreenSong.MakeMedley then
          begin
            ScreenSong.WaitHandler.changed := true;
            ScreenSong.WaitHandler.change_time := 0;
          end;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:
        begin
          {SelectNext;
          Button[0].Text[0].Selected := (Interaction = 0);}
        end;

      SDLK_UP:
        begin
          {SelectPrev;
          Button[0].Text[0].Selected := (Interaction = 0); }
        end;

      SDLK_RIGHT:
        begin
          Interaction := 1;
          InteractInc;
          if (Length(Button[0].Text[0].Text) > 0) then
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          Interaction := 0;
        end;
      SDLK_LEFT:
        begin
          Interaction := 1;
          InteractDec;
          if (Length(Button[0].Text[0].Text) > 0) then
            SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
          Interaction := 0;
        end;
    end;
  end;
end;

constructor TScreenSongJumpto.Create;
{var
  I:    integer;}
begin
  inherited Create;

  AddText(Theme.SongJumpto.TextFound);

  LoadFromTheme(Theme.SongJumpto);

  AddButton(Theme.SongJumpto.ButtonSearchText);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, '');

  SelectType := 0;
  AddSelectSlide(Theme.SongJumpto.SelectSlideType, SelectType, Theme.SongJumpto.IType);


  Interaction := 0;
  LastPlayed  := 0;
  isDuet := false;
end;

procedure TScreenSongJumpto.SetVisible(Value: Boolean);
begin
//If change from unvisible to Visible then OnShow
  if (VisibleBool = False) AND (Value = True) then
    OnShow;

  VisibleBool := Value;
end;

procedure TScreenSongJumpto.onShow;
begin
  //Reset Screen if no Old Search is Displayed
  if (CatSongs.CatNumShow <> -2) then
  begin
    SelectsS[0].SetSelectOpt(0);

    Button[0].Text[0].Text := '';
    Text[0].Text := Theme.SongJumpto.NoSongsFound;
    isDuet := false;
  end;

  //Select Input
  Interaction := 0;
  Button[0].Text[0].Selected := True;

  LastPlayed := ScreenSong.Interaction;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenSongJumpTo)');
end;

function TScreenSongJumpto.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenSongJumpto.SetTextFound(const Count: Cardinal);
begin
  if (Count = 0) then
  begin
    Text[0].Text := Theme.SongJumpto.NoSongsFound;
    if (Length(Button[0].Text[0].Text) = 0) then
      ScreenSong.HideCatTL
    else
      ScreenSong.ShowCatTLCustom(Format(Theme.SongJumpto.CatText, [Button[0].Text[0].Text]));
  end
  else
  begin
    Text[0].Text := Format(Theme.SongJumpto.SongsFound, [Count]);

    //Set CatTopLeftText
    ScreenSong.ShowCatTLCustom(Format(Theme.SongJumpto.CatText, [Button[0].Text[0].Text]));
  end;


  //Set visSongs
  VisSongs := Count;

  //Fix SongSelection
  ScreenSong.Interaction := high(CatSongs.Song);
  ScreenSong.SelectNext;
  ScreenSong.FixSelected;

  //Play Correct Music
  if (ScreenSong.Interaction <> LastPlayed) then
  begin
    LastPlayed := ScreenSong.Interaction;

    ScreenSong.ChangeMusic;
  end;
end;

procedure TScreenSongJumpto.ToggleDuetFilter;
begin
  if not isDuet then
    SetDuetFilter
  else
    ResetDuetFilter;
end;

procedure TScreenSongJumpto.SetDuetFilter;
begin
  isDuet := true;
  Button[0].Text[0].Text := 'Duet Songs';
  SetTextFound(CatSongs.SetFilter('', 3));
end;

procedure TScreenSongJumpto.ResetDuetFilter;
begin
  isDuet := false;
  Button[0].Text[0].Text := '';
  SetTextFound(CatSongs.SetFilter(Button[0].Text[0].Text, SelectType));
end;

procedure TScreenSongJumpto.RefreshDuetFilter;
begin
  if isDuet then
    SetDuetFilter
  else
    ResetDuetFilter;
end;
end.
