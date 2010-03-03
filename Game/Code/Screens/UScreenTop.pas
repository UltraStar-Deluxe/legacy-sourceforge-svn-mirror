unit UScreenTop;

interface

uses
  UMenu, SDL, SysUtils, UDisplay, UMusic, USongs, UThemes, ULCD, ModiSDK;

type
  THandler = record
    changed:  boolean;
    change_time:  real;
  end;

  TScreenTop = class(TMenu)
    public
      MP3VolumeHandler: THandler;

      TextLevel:        integer;
      TextArtistTitle:  integer;

      DifficultyShow:  integer;  //TODO
      TeamInfo : TTeamInfo;      //for M2-MOD

      StaticNumber:     array[1..8] of integer;
      TextNumber:       array[1..8] of integer;
      TextName:         array[1..8] of integer;
      TextScore:        array[1..8] of integer;
      TextDate:         array[1..8] of integer;

      Fadeout:      boolean;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      //procedure DrawScores(difficulty: integer);  TODO
  end;

const
  ID='ID_029';   //for help system
  
implementation

uses UGraphic, UDataBase, UDraw, UMain, UIni, UPartyM2, UTime, DateUtils, UHelp, ULog;

function TScreenTop.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
    case PressedKey of
      //MP3-Volume Up
      SDLK_PAGEUP:
        begin
          if (ScreenSong.MP3Volume<100) then
          begin
            ScreenSong.MP3Volume := ScreenSong.MP3Volume+5;
            Music.SetMusicVolume(ScreenSong.MP3Volume);
          end;
          MP3VolumeHandler.changed := true;
          MP3VolumeHandler.change_time := 0;
        end;

      //MP3-Volume Down
      SDLK_PAGEDOWN:
        begin
          if (ScreenSong.MP3Volume>0) then
          begin
            ScreenSong.MP3Volume := ScreenSong.MP3Volume-5;
            Music.SetMusicVolume(ScreenSong.MP3Volume);
          end;
          MP3VolumeHandler.changed := true;
          MP3VolumeHandler.change_time := 0;
        end;

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
        
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_RETURN:
        begin
          if (Ini.SavePlayback=1) then
            Music.VoicesClose;
            
          if (not Fadeout) then begin
            if(ScreenSong.Mode = smNormal) or (ScreenSong.Mode = smMedley) then
              FadeTo(@ScreenSong)
            else
            begin
              ScreenSong.SongIndex := -1;
              Music.FadeStop(Ini.PreviewFading);
              FadeTo(@ScreenPartyNewRoundM2);
            end;
            Fadeout := true;
          end;
        end;
      SDLK_SYSREQ:
        begin
          Display.PrintScreen;
        end;
    end;
  end;
end;

constructor TScreenTop.Create;
var
  I:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Top);


  TextLevel := AddText(Theme.Top.TextLevel);
  TextArtistTitle := AddText(Theme.Top.TextArtistTitle);

  for I := 0 to 7 do
    StaticNumber[I+1] := AddStatic(Theme.Top.StaticNumber[I]);

  for I := 0 to 7 do
    TextNumber[I+1] := AddText(Theme.Top.TextNumber[I]);
  for I := 0 to 7 do
    TextName[I+1] := AddText(Theme.Top.TextName[I]);
  for I := 0 to 7 do
    TextScore[I+1] := AddText(Theme.Top.TextScore[I]);
  for I := 0 to 7 do
    TextDate[I+1] := AddText(Theme.Top.TextDate[I]);

end;

procedure TScreenTop.onShow;
var
  I:      integer;
  PMax:   integer;
  sung:   boolean;
  TimeStamp: integer;
begin
  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenTop)');

  Fadeout := false;
  sung:=false;
  TimeStamp := DateTimeToUnix(Now());

  PMax := PlayersPlay - 1;

  for I := 0 to PMax do
  begin
    if(ScreenSong.Mode=smChallenge) then
      TeamInfo.TeamInfo[I].Score:=Player[I].ScoreTotalI;

    if (Player[I].ScoreTotalI>100) and ScreenSong.SungToEnd and (ScreenSong.Mode<>smMedley) and
      (not ScreenSong.PartyMedley) then
    begin
      DataBase.AddScore(AktSong, Ini.Difficulty, Ini.Name[I], Player[I].ScoreTotalI, TimeStamp);
      sung := true;
    end;
  end;

  if ScreenSong.Mode=smChallenge then
  begin
    PartySessionM2.Teams:=TeamInfo;
    PartySessionM2.EndRound;
  end;

  if sung then
    DataBase.WriteScore(AktSong);

  DataBase.ReadScore(AktSong, 8, Ini.SumPlayers);

  Text[TextArtistTitle].Text := AktSong.Artist + ' - ' + AktSong.Title;

  for I := 1 to Length(AktSong.Score[Ini.Difficulty]) do begin
    Static[StaticNumber[I]].Visible := true;
    Text[TextNumber[I]].Visible := true;
    Text[TextName[I]].Visible := true;
    Text[TextScore[I]].Visible := true;
    Text[TextDate[I]].Visible := true;

    Text[TextName[I]].Text := AktSong.Score[Ini.Difficulty, I-1].Name;
    Text[TextScore[I]].Text := IntToStr(AktSong.Score[Ini.Difficulty, I-1].Score);
    Text[TextDate[I]].Text := AktSong.Score[Ini.Difficulty, I-1].Date;
  end;

  for I := Length(AktSong.Score[Ini.Difficulty])+1 to 8 do begin
    Static[StaticNumber[I]].Visible := false;
    Text[TextNumber[I]].Visible := false;
    Text[TextName[I]].Visible := false;
    Text[TextScore[I]].Visible := false;
    Text[TextDate[I]].Visible := false;
  end;

  Text[TextLevel].Text := IDifficulty[Ini.Difficulty];

  MP3VolumeHandler.changed := false;
end;

function TScreenTop.Draw: boolean;
//var
{  Min:    real;
  Max:    real;
  Wsp:    real;
  Wsp2:   real;
  Pet:    integer;}

{  Item:   integer;
  P:      integer;
  C:      integer;}
begin
  // Singstar - let it be...... with 6 statics
{  if PlayersPlay = 6 then begin
    for Item := 4 to 6 do begin
      if ScreenAct = 1 then P := Item-4;
      if ScreenAct = 2 then P := Item-1;

      FillPlayer(Item, P);

{      if ScreenAct = 1 then begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P1Dark');
      end;

      if ScreenAct = 2 then begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P4Dark');
      end;}

{    end;
  end;}

  inherited Draw;

  if MP3VolumeHandler.changed and (MP3VolumeHandler.change_time+TimeSkip<3) then
  begin
    MP3VolumeHandler.change_time := MP3VolumeHandler.change_time + TimeSkip;
    DrawVolumeBar(10, 530, 780, 12, ScreenSong.MP3Volume);
  end else
    MP3VolumeHandler.changed := false;
end;

end.
