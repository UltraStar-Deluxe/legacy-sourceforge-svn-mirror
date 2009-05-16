{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UScreenPartyNewRound;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, SysUtils, UThemes;

type
  TScreenPartyNewRound = class(TMenu)
    public
      //Texts:
      TextRound: array [0..6] of Cardinal;

      TextWinner: array [0..6] of Cardinal;

      TextNextRound: Cardinal;
      TextNextRoundNo: Cardinal;
      TextNextPlayer1: Cardinal;
      TextNextPlayer2: Cardinal;
      TextNextPlayer3: Cardinal;

      //Statics
      StaticRound: array [0..6] of Cardinal;

      //Scores
      TextScoreTeam1: Cardinal;
      TextScoreTeam2: Cardinal;
      TextScoreTeam3: Cardinal;
      TextNameTeam1: Cardinal;
      TextNameTeam2: Cardinal;
      TextNameTeam3: Cardinal;

      TextTeam1Players: Cardinal;
      TextTeam2Players: Cardinal;
      TextTeam3Players: Cardinal;

      StaticTeam1: Cardinal;
      StaticTeam2: Cardinal;
      StaticTeam3: Cardinal;
      StaticNextPlayer1: Cardinal;
      StaticNextPlayer2: Cardinal;
      StaticNextPlayer3: Cardinal;   


      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

implementation

uses UGraphic,
     UMain,
     UIni,
     UTexture,
     UParty,
     UDLLManager,
     ULanguage,
     USong,
     ULog;

function TScreenPartyNewRound.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          CheckFadeTo(@ScreenMain,'MSG_END_PARTY');
        end;

      SDLK_RETURN:
        begin
          AudioPlayback.PlaySound(SoundLib.Start);
          Party.CallBeforeSongSelect;
        end;
    end;
  end;
end;

constructor TScreenPartyNewRound.Create;
begin
  inherited Create;

  TextRound[0] := AddText (Theme.PartyNewRound.TextRound1);
  TextRound[1] := AddText (Theme.PartyNewRound.TextRound2);
  TextRound[2] := AddText (Theme.PartyNewRound.TextRound3);
  TextRound[3] := AddText (Theme.PartyNewRound.TextRound4);
  TextRound[4] := AddText (Theme.PartyNewRound.TextRound5);
  TextRound[5] := AddText (Theme.PartyNewRound.TextRound6);
  TextRound[6] := AddText (Theme.PartyNewRound.TextRound7);

  TextWinner[0] := AddText (Theme.PartyNewRound.TextWinner1);
  TextWinner[1] := AddText (Theme.PartyNewRound.TextWinner2);
  TextWinner[2] := AddText (Theme.PartyNewRound.TextWinner3);
  TextWinner[3] := AddText (Theme.PartyNewRound.TextWinner4);
  TextWinner[4] := AddText (Theme.PartyNewRound.TextWinner5);
  TextWinner[5] := AddText (Theme.PartyNewRound.TextWinner6);
  TextWinner[6] := AddText (Theme.PartyNewRound.TextWinner7);

  TextNextRound := AddText (Theme.PartyNewRound.TextNextRound);
  TextNextRoundNo := AddText (Theme.PartyNewRound.TextNextRoundNo);
  TextNextPlayer1 := AddText (Theme.PartyNewRound.TextNextPlayer1);
  TextNextPlayer2 := AddText (Theme.PartyNewRound.TextNextPlayer2);
  TextNextPlayer3 := AddText (Theme.PartyNewRound.TextNextPlayer3);

  StaticRound[0] := AddStatic (Theme.PartyNewRound.StaticRound1);
  StaticRound[1] := AddStatic (Theme.PartyNewRound.StaticRound2);
  StaticRound[2] := AddStatic (Theme.PartyNewRound.StaticRound3);
  StaticRound[3] := AddStatic (Theme.PartyNewRound.StaticRound4);
  StaticRound[4] := AddStatic (Theme.PartyNewRound.StaticRound5);
  StaticRound[5] := AddStatic (Theme.PartyNewRound.StaticRound6);
  StaticRound[6] := AddStatic (Theme.PartyNewRound.StaticRound7);

  //Scores
  TextScoreTeam1 := AddText (Theme.PartyNewRound.TextScoreTeam1);
  TextScoreTeam2 := AddText (Theme.PartyNewRound.TextScoreTeam2);
  TextScoreTeam3 := AddText (Theme.PartyNewRound.TextScoreTeam3);
  TextNameTeam1 := AddText (Theme.PartyNewRound.TextNameTeam1);
  TextNameTeam2 := AddText (Theme.PartyNewRound.TextNameTeam2);
  TextNameTeam3 := AddText (Theme.PartyNewRound.TextNameTeam3);

  //Players
  TextTeam1Players := AddText (Theme.PartyNewRound.TextTeam1Players);
  TextTeam2Players := AddText (Theme.PartyNewRound.TextTeam2Players);
  TextTeam3Players := AddText (Theme.PartyNewRound.TextTeam3Players);

  StaticTeam1 := AddStatic (Theme.PartyNewRound.StaticTeam1);
  StaticTeam2 := AddStatic (Theme.PartyNewRound.StaticTeam2);
  StaticTeam3 := AddStatic (Theme.PartyNewRound.StaticTeam3);
  StaticNextPlayer1 := AddStatic (Theme.PartyNewRound.StaticNextPlayer1);
  StaticNextPlayer2 := AddStatic (Theme.PartyNewRound.StaticNextPlayer2);
  StaticNextPlayer3 := AddStatic (Theme.PartyNewRound.StaticNextPlayer3);

  LoadFromTheme(Theme.PartyNewRound);
end;

procedure TScreenPartyNewRound.onShow;
var
  I: Integer;
  function GetTeamPlayers(const Num: Integer): String;
  var
    Players: Array of String;
    J: Integer;
  begin          
    if (Num > High(Party.Teams)) or (Num < 0) then
      exit;

    //Create Players Array
    SetLength(Players, Length(Party.Teams[Num].Players));
    For J := 0 to High(Party.Teams[Num].Players) do
      Players[J] := Party.Teams[Num].Players[J].Name;

    //Implode and Return
    Result := Language.Implode(Players);
  end;
begin
  inherited;

  //Set Visibility of Round Infos
  for I := 0 to 6 do
  begin
    if (I <= High(Party.Rounds)) then
    begin
      Static[StaticRound[I]].Visible := True;
      Text[TextRound[I]].Visible := True;
      Text[TextWinner[I]].Visible := True;

      // update texts:
      Text[TextRound[I]].Text := Language.Translate('PLUGIN_' + uppercase(Party.Modes[Party.Rounds[I].Mode].Name) + '_NAME');
      Text[TextWinner[I]].Text := Party.GetWinnerString(I);
    end
    else
    begin
      Static[StaticRound[I]].Visible := False;
      Text[TextRound[I]].Visible := False;
      Text[TextWinner[I]].Visible := False;
    end;
  end;


  //Display Scores
  if (Length(Party.Teams) >= 1) then
  begin
    Text[TextScoreTeam1].Text := InttoStr(Party.Teams[0].Score);
    Text[TextNameTeam1].Text := Party.Teams[0].Name;
    Text[TextTeam1Players].Text := GetTeamPlayers(0);

    Text[TextScoreTeam1].Visible := True;
    Text[TextNameTeam1].Visible := True;
    Text[TextTeam1Players].Visible := True;
    Static[StaticTeam1].Visible := True;
    Static[StaticNextPlayer1].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam1].Visible := False;
    Text[TextNameTeam1].Visible := False;
    Text[TextTeam1Players].Visible := False;
    Static[StaticTeam1].Visible := False;
    Static[StaticNextPlayer1].Visible := False;
  end;

  if (Length(Party.Teams) >= 2) then
  begin
    Text[TextScoreTeam2].Text := InttoStr(Party.Teams[1].Score);
    Text[TextNameTeam2].Text := Party.Teams[1].Name;
    Text[TextTeam2Players].Text := GetTeamPlayers(1);

    Text[TextScoreTeam2].Visible := True;
    Text[TextNameTeam2].Visible := True;
    Text[TextTeam2Players].Visible := True;
    Static[StaticTeam2].Visible := True;
    Static[StaticNextPlayer2].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam2].Visible := False;
    Text[TextNameTeam2].Visible := False;
    Text[TextTeam2Players].Visible := False;
    Static[StaticTeam2].Visible := False;
    Static[StaticNextPlayer2].Visible := False;
  end;

  if (Length(Party.Teams) >= 3) then
  begin
    Text[TextScoreTeam3].Text := InttoStr(Party.Teams[2].Score);
    Text[TextNameTeam3].Text := Party.Teams[2].Name;
    Text[TextTeam3Players].Text := GetTeamPlayers(2);

    Text[TextScoreTeam3].Visible := True;
    Text[TextNameTeam3].Visible := True;
    Text[TextTeam3Players].Visible := True;
    Static[StaticTeam3].Visible := True;
    Static[StaticNextPlayer3].Visible := True;
  end
  else
  begin
    Text[TextScoreTeam3].Visible := False;
    Text[TextNameTeam3].Visible := False;
    Text[TextTeam3Players].Visible := False;
    Static[StaticTeam3].Visible := False;
    Static[StaticNextPlayer3].Visible := False;
  end;  

  //nextRound Texts
  Text[TextNextRound].Text := Language.Translate('PLUGIN_' + uppercase(Party.Modes[Party.Rounds[Party.CurrentRound].Mode].Name) + '_DESC');
  Text[TextNextRoundNo].Text := InttoStr(Party.CurrentRound + 1);
  if (Length(Party.Teams) >= 1) then
  begin
    Text[TextNextPlayer1].Text := Party.Teams[0].Players[Party.Teams[0].NextPlayer].Name;
    Text[TextNextPlayer1].Visible := True;
  end
  else
    Text[TextNextPlayer1].Visible := False;

  if (Length(Party.Teams) >= 2) then
  begin
    Text[TextNextPlayer2].Text := Party.Teams[1].Players[Party.Teams[1].NextPlayer].Name;
    Text[TextNextPlayer2].Visible := True;
  end
  else
    Text[TextNextPlayer2].Visible := False;

  if (Length(Party.Teams) >= 3) then
  begin
    Text[TextNextPlayer3].Text := Party.Teams[2].Players[Party.Teams[2].NextPlayer].Name;
    Text[TextNextPlayer3].Visible := True;
  end
  else
    Text[TextNextPlayer3].Visible := False;
end;

procedure TScreenPartyNewRound.SetAnimationProgress(Progress: real);
begin
  {Button[0].Texture.ScaleW := Progress;
  Button[1].Texture.ScaleW := Progress;
  Button[2].Texture.ScaleW := Progress; }
end;

end.
