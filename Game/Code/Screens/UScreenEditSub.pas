unit UScreenEditSub;

interface

uses
  UMenu,
  UVideo,
  TextGL,
  UMusic,
  URecord,
  SDL,
  SysUtils, UFiles, UTime, USongs, UIni, ULog, UTexture, UMenuText,
  ULyrics, Math, gl, UThemes, MidiOut, UHelp;

type
  TVidVis = (none, windowed, full);

  TMedleyNotes = record
    start: TPos;
    end_: TPos;
    Preview: TPos;
    isStart: boolean;   //start beat is declared
    isEnd: boolean;     //end beat is declared
  end;

  TVoicePitch = record
    beat:   integer;
    pitch:  integer;
  end;

  TScreenEditSub = class(TMenu)
    private
      PitchRecOn:   boolean;

      cRB, cGB, cBB:  GLfloat;
      cRR, cGR, cBR:  GLfloat;

      offset:       array[0..1] of integer;

      AktBeat:      integer;
      //Variable is True if no SOng is loaded
      Error:        Boolean;
      MP3Volume:    Integer;
      
      TextNote:     integer;
      TextSentence: integer;
      TextTitle:    integer;
      TextArtist:   integer;
      //TextMp3:      integer;
      TextBPM:      integer;
      TextGAP:      integer;
      TextDebug:    integer;
      TextNStart:   integer;
      TextNDlugosc: integer;
      TextNTon:     integer;
      TextNText:    integer;
      TextVideoGap:integer;
      AktNuta:      array[0..1] of integer;

      CP:           integer; //actual singer: 0 or 1 (for duet)
      EditorLyric:  array[0..1] of TLyric;

      PlaySentence:     boolean;
      PlaySentenceMidi: boolean;
      PlayOneNote:      boolean;
      PlayOneNoteMidi:  boolean;
      PlayOneSentence:  boolean;  //for mp3 and midi

      PlayStopTime: real;
      LastClick:    integer;
      Click:        boolean;
      CopySrcLine:  integer;
      CopySrcCP:    integer;

      MidiOut:      TMidiOutput;
      MidiStart:    real;
      MidiStop:     real;
      MidiTime:     real;
      MidiPos:      real;
      MidiLastNote: integer;


      TextEditMode: boolean;
      BPMEditMode:  boolean;

      MedleyNotes:  TMedleyNotes;

      editText:     string; //backup of current text in text-edit-mode
      noteStart:    integer; //Start note when playing sentence
      lineStart:    integer; //Start line when playing sentence
      cpStart:      integer; //Start singer when playing sentence
      LineChanged:  array[0..1] of boolean;

      VidVis:       TVidVis; //video visiability
      PlayVideo:    boolean;
      StartTry:     boolean;
      PlayTime:     real;

      ActTonePitch: integer;

      procedure DrawPitch(x, y, Width, Height: single; beat: integer);
      procedure StartVideo;
      procedure StartVideoPreview;
      procedure NewBeat;
      procedure ChangeBPM(newBPM: real);
      procedure CzesciDivide;
      procedure CzesciMultiply;
      procedure LyricsCapitalize;
      procedure LyricsCorrectSpaces;
      procedure FixTimings;
      procedure DivideSentence;
      procedure JoinSentence;
      procedure DivideNote;
      procedure DeleteNote;
      procedure DeleteSentence;
      procedure TransposeNote(Transpose: integer);
      procedure ChangeWholeTone(Tone: integer);
      procedure ChangeWholeToneActLine(Tone: integer);
      procedure MoveAllToEnd(Move: integer);
      procedure MoveTextToRight;
      procedure MarkSrc;
      procedure PasteText;
      procedure CopySentence(Src, Dst: integer);
      procedure CopySentences(Src, Dst, Num: integer);
      //Note Name Mod
      function GetNoteName(Note: Integer): String;
      function GetMedleyLength: real; //returns if availible the length of the medley in seconds, else 0
      procedure DrawInfoBar(P, x, y, w, h: integer);
      procedure DrawStatics;
      procedure SelectNextNote;
      procedure SelectPrevNote;
      procedure MakeSingle;
      procedure MakeDuet;
      function DuetCopyLine: boolean;
      procedure DuetMoveLine;
      procedure CopyNote(Pf, Cf, Nf, Pt, Ct, Nt: integer);
      procedure CopyLine(Pf, Cf, Pt, Ct: integer);
      procedure Refresh;
    public
      Pitches:            array of TVoicePitch;
      Tex_Background:     TTexture;
      FadeOut:            boolean;
      Path:               string;
      FileName:           string;
      SongIndex:          integer; //SongIndex from CatSongs.Song
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function ParseInputEditText(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
      function ParseInputEditBPM(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
      function Draw: boolean; override;
      procedure onHide; override;
  end;

const
  ID='ID_001';   //for help system
  NumHalftones = 36;

implementation
uses UGraphic, UDisplay, UDraw, UMain, USkins, ULanguage;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenEditSub.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;
  R:          real;
  SResult:    boolean;
  temp:       integer;

begin
  Result := true;
  PlayOneSentence := false;
  Text[TextDebug].Text := '';

  if TextEditMode then
    Result := ParseInputEditText(PressedKey, ScanCode, PressedDown)
  else if BPMEditMode then
    Result := ParseInputEditBPM(PressedKey, ScanCode, PressedDown)
  else
  begin

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  If (PressedDown) then begin // Key Down
    PitchRecOn := false;
    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_ESCAPE:
        begin
          CheckFadeTo(@ScreenSong,'Do you really want to quit?');
          if (Display.NextScreen <> nil) then
          begin
            Music.Close;
            acClose;
          end;
        end;

      SDLK_Q:
        begin
          //Result := false;
        end;

      SDLK_BACKQUOTE:
        begin
          // Increase Note Length (same as Alt + Right)
          Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
          if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].HighNut then
            Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
        end;

      SDLK_EQUALS:
        begin
          // Increase BPM
          if SDL_ModState = 0 then
            AktSong.BPM[0].BPM := Round((AktSong.BPM[0].BPM * 5) + 1) / 5; // (1/20)
          if SDL_ModState = KMOD_LSHIFT then
            AktSong.BPM[0].BPM := AktSong.BPM[0].BPM + 4; // (1/1)
          if SDL_ModState = KMOD_LCTRL then
            AktSong.BPM[0].BPM := Round((AktSong.BPM[0].BPM * 25) + 1) / 25; // (1/100)
        end;

      SDLK_MINUS:
        begin
          // Decrease BPM
          if SDL_ModState = 0 then
            AktSong.BPM[0].BPM := Round((AktSong.BPM[0].BPM * 5) - 1) / 5;
          if SDL_ModState = KMOD_LSHIFT then
            AktSong.BPM[0].BPM := AktSong.BPM[0].BPM - 4;
          if SDL_ModState = KMOD_LCTRL then
            AktSong.BPM[0].BPM := Round((AktSong.BPM[0].BPM * 25) - 1) / 25;
        end;

      SDLK_0:
        begin
          // Increase GAP
          if SDL_ModState = 0 then
            AktSong.GAP := AktSong.GAP + 10;
          if SDL_ModState = KMOD_LSHIFT then
            AktSong.GAP := AktSong.GAP + 1000;
        end;

      SDLK_9:
        begin
          // Decrease GAP
          if SDL_ModState = 0 then
            AktSong.GAP := AktSong.GAP - 10;
          if SDL_ModState = KMOD_LSHIFT then
            AktSong.GAP := AktSong.GAP - 1000;
        end;

      SDLK_8:
        begin
          temp := 0;
          // Increase VideoGAP
          if SDL_ModState = 0 then
            temp := 1;         //10ms
          if SDL_ModState = KMOD_LSHIFT then
            temp := 10;        //100ms
          if SDL_ModState = KMOD_LCTRL then
            temp := 100;       //1000ms

          AktSong.VideoGap := (round(AktSong.VideoGAP*100) + temp)/100;

          if PlayVideo then
            acSkip2(AktSong.VideoGap, Czas.Teraz);
            //StartVideo;
        end;

      SDLK_7:
        begin
          temp := 0;
          // Decrease VideoGAP
          if SDL_ModState = 0 then
            temp := -1;        //10ms
          if SDL_ModState = KMOD_LSHIFT then
            temp := -10;       //100ms
          if SDL_ModState = KMOD_LCTRL then
            temp := -100;      //1000ms

          AktSong.VideoGap := (round(AktSong.VideoGAP*100) + temp)/100;

          if PlayVideo then
            acSkip2(AktSong.VideoGap, Czas.Teraz);
            //StartVideo;
        end;

      SDLK_KP_PLUS:
        begin
          // Increase tone of all notes
          if SDL_ModState = 0 then
            ChangeWholeTone(1);
          if SDL_ModState = KMOD_LSHIFT then
            ChangeWholeTone(12);
        end;

      SDLK_KP_MINUS:
        begin
          // Decrease tone of all notes
          if SDL_ModState = 0 then
            ChangeWholeTone(-1);
          if SDL_ModState = KMOD_LSHIFT then
            ChangeWholeTone(-12);
        end;

      SDLK_SLASH:
        begin
          if SDL_ModState = 0 then
          begin
            // Insert start of sentece
            if (AktNuta[CP] > 0) then
              DivideSentence;
            {else if AktSong.isDuet and
              (Length(Czesci[(CP+1) mod 2].Czesc[Czesci[CP].Akt].Nuta)>0) then
            begin
              if (Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start >
                Czesci[(CP+1) mod 2].Czesc[Czesci[CP].Akt].Nuta[0].Start+
                Czesci[(CP+1) mod 2].Czesc[Czesci[CP].Akt].Nuta[0].Dlugosc) then
                DivideSentence;
            end;}
          end;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            // Join next sentence with current
            if Czesci[CP].Akt < Czesci[CP].High  then
              JoinSentence;
          end;

          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote;
          end;

        end;


      SDLK_S:
        begin
          //Medley MOD:
          if AktSong.isDuet then
          begin
            AktSong.Medley.Source := msNone;
          end
          else if (MedleyNotes.isStart and MedleyNotes.isEnd) and
            (MedleyNotes.start.line < MedleyNotes.end_.line) and
            (Length(Czesci[0].Czesc)> MedleyNotes.end_.line) and
            (Length(Czesci[0].Czesc[MedleyNotes.end_.line].Nuta)>MedleyNotes.end_.note) and
            (Length(Czesci[0].Czesc[MedleyNotes.start.line].Nuta)>MedleyNotes.start.note) then
          begin
            AktSong.Medley.Source := msTag;
            AktSong.Medley.StartBeat:=Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].Start;
            AktSong.Medley.EndBeat:=Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Start +
              Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Dlugosc;
            AktSong.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;
            AktSong.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
          end else
          begin
            AktSong.Medley.Source := msNone;
            AktSong.Medley.StartBeat:=0;
            AktSong.Medley.EndBeat:=0;
          end;

          // Save Song
          if SDL_ModState = KMOD_LSHIFT then
          begin
            if (AktSong.Medley.Source = msTag) then
            begin
              ScreenPopupError.ShowPopup('Medley with Relative is not supported!');
              Exit;
            end;

            if (AktSong.isDuet) then
            begin
              ScreenPopupError.ShowPopup('Duet with Relative is not supported!');
              Exit;
            end;

            SResult := SaveSong(AktSong, Czesci, Path + FileName, true); //save with relative
          end else
            SResult := SaveSong(AktSong, Czesci, Path + FileName, false);

          if SResult then
          begin
            Text[TextDebug].Text := Language.Translate('INFO_FILE_SAVED');
            CatSongs.Song[SongIndex] := AktSong;
          end else
          begin
            ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
          end;

          Exit;
        end;

      // set Medley tags
      SDLK_A:
        begin
          if AktSong.Relative then
          begin
            ScreenPopupError.ShowPopup('Medley with Relative is not supported!');
            Exit;
          end;

          if AktSong.isDuet then
          begin
            ScreenPopupError.ShowPopup('Medley with Duet is not supported!');
            Exit;
          end;

          if SDL_ModState = KMOD_LSHIFT then //Medley End Note
          begin
            if MedleyNotes.isEnd then
            begin
              if (Czesci[0].Akt=MedleyNotes.end_.line) and (AktNuta[0]=MedleyNotes.end_.note) then
              begin
                MedleyNotes.isEnd := false;
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].IsMedley := false;
              end else
              begin
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].IsMedley := true;
                if (Length(Czesci[0].Czesc)> MedleyNotes.end_.line) and
                  (Length(Czesci[0].Czesc[MedleyNotes.end_.line].Nuta)>MedleyNotes.end_.note) then
                  Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].IsMedley := false;
                MedleyNotes.end_.line := Czesci[0].Akt;
                MedleyNotes.end_.note := AktNuta[0];
              end;
            end else
            begin
              MedleyNotes.isEnd := true;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].IsMedley := true;
              MedleyNotes.end_.line := Czesci[0].Akt;
              MedleyNotes.end_.note := AktNuta[0];
            end;
          end else
          begin        //Medley Start Note
            if MedleyNotes.isStart then
            begin
              if (Czesci[0].Akt=MedleyNotes.start.line) and (AktNuta[0]=MedleyNotes.start.note) then
              begin
                MedleyNotes.isStart := false;
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].IsMedley := false;
              end else
              begin
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].IsMedley := true;
                if (Length(Czesci[0].Czesc)> MedleyNotes.start.line) and
                  (Length(Czesci[0].Czesc[MedleyNotes.start.line].Nuta)>MedleyNotes.start.note) then
                  Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].IsMedley := false;
                MedleyNotes.start.line := Czesci[0].Akt;
                MedleyNotes.start.note := AktNuta[0];
              end;
            end else
            begin
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].IsMedley := true;
              MedleyNotes.isStart := true;
              MedleyNotes.start.line := Czesci[0].Akt;
              MedleyNotes.start.note := AktNuta[0];
            end;
          end;

          //show length of medley
          Text[TextDebug].Text := FormatFloat('MedleyLength: #0.00s', GetMedleyLength);
          Exit;
        end;

      // jump to Medley tags
      SDLK_J:
        begin
          if AktSong.Relative then
          begin
            ScreenPopupError.ShowPopup('Medley with Relative is not supported!');
            Exit;
          end;

          if AktSong.isDuet then
          begin
            ScreenPopupError.ShowPopup('Medley with Duet is not supported!');
            Exit;
          end;

          if (SDL_ModState = KMOD_LSHIFT) and MedleyNotes.IsEnd then //Medley End Note
          begin
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;

            if (Length(Czesci[0].Czesc)> MedleyNotes.end_.line) and
              (Length(Czesci[0].Czesc[MedleyNotes.end_.line].Nuta)>MedleyNotes.end_.note) then
            begin
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].Color := 0;
              Czesci[0].Akt := MedleyNotes.end_.line;
              AktNuta[0] := MedleyNotes.end_.note;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].Color := 2;

              EditorLyric[0].AddCzesc(0, Czesci[0].Akt);
              EditorLyric[0].Selected := AktNuta[0];
            end;
          end else if MedleyNotes.IsStart then
          begin
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;

            if (Length(Czesci[0].Czesc)> MedleyNotes.start.line) and
              (Length(Czesci[0].Czesc[MedleyNotes.start.line].Nuta)>MedleyNotes.start.note) then
            begin
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].Color := 0;
              Czesci[0].Akt := MedleyNotes.start.line;
              AktNuta[0] := MedleyNotes.start.note;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta[0]].Color := 2;

              EditorLyric[0].AddCzesc(0, Czesci[0].Akt);
              EditorLyric[0].Selected := AktNuta[0];
            end;
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            PlayOneNote := false;
            Music.Stop;
            LineChanged[0]:=false;
            LineChanged[1]:=false;

            if (MedleyNotes.isStart and MedleyNotes.isEnd) and
              (MedleyNotes.start.line < MedleyNotes.end_.line) and
              (Length(Czesci[0].Czesc)> MedleyNotes.end_.line) and
              (Length(Czesci[0].Czesc[MedleyNotes.end_.line].Nuta)>MedleyNotes.end_.note) and
              (Length(Czesci[0].Czesc[MedleyNotes.start.line].Nuta)>MedleyNotes.start.note) then
            begin
              R := GetTimeFromBeat(Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].Start);
              if R <= Music.Length then
              begin
                Music.MoveTo(R);

                noteStart := AktNuta[0];
                lineStart := Czesci[0].Akt;
                cpStart := 0;

                PlayStopTime := GetTimeFromBeat(
                  Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Start +
                  Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Dlugosc);
                PlaySentence := true;
                Music.Play;
                LastClick := -100;
              end;
            end;
          end;

          //show length of medley
          Text[TextDebug].Text := FormatFloat('MedleyLength: #0.00s', GetMedleyLength);
          Exit;
        end;

      SDLK_K: //Preview Start
        begin
          if (SDL_ModState = KMOD_LSHIFT) then    //jump to...
          begin
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;

            Refresh;

            CP := MedleyNotes.Preview.CP;
            Czesci[CP].Akt := MedleyNotes.Preview.line;
            if AktSong.isDuet then
              Czesci[(CP+1) mod 2].Akt := Czesci[CP].Akt;

            AktNuta[CP] := MedleyNotes.Preview.note;
            if AktSong.isDuet then
              AktNuta[(CP+1) mod 2] := 0;

            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;

            EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
            EditorLyric[CP].Selected := AktNuta[CP];
            if AktSong.isDuet then
            begin
              EditorLyric[(CP+1) mod 2].AddCzesc((CP+1) mod 2, Czesci[(CP+1) mod 2].Akt);
              EditorLyric[(CP+1) mod 2].Selected := -1;
            end;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
          end else
          begin
            if (CP = MedleyNotes.Preview.CP) and
              (Czesci[CP].Akt = MedleyNotes.Preview.line) and
              (AktNuta[CP] = MedleyNotes.Preview.note) and
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].IsStartPreview then //reset
            begin
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].IsStartPreview := false;
              AktSong.PreviewStart := 0;
            end else //set
            begin
              if (Length(Czesci[MedleyNotes.Preview.CP].Czesc[MedleyNotes.Preview.line].Nuta)>MedleyNotes.Preview.note) then
                Czesci[MedleyNotes.Preview.CP].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].IsStartPreview := false;
              MedleyNotes.Preview.CP := CP;
              MedleyNotes.Preview.line := Czesci[CP].Akt;
              MedleyNotes.Preview.note := AktNuta[CP];
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].IsStartPreview := true;
              AktSong.PreviewStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].start);
            end;
          end;
        end;


      SDLK_D:
        begin
          // Divide lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            CzesciDivide;
            Text[TextDebug].Text := 'BPM and note lengths halfed';
          end;

          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
          begin
            if AktSong.isDuet then
            begin
              MakeSingle;
              Text[TextDebug].Text := 'Converted duet into normal song';
            end else
            begin
              MakeDuet;
              Text[TextDebug].Text := 'Created duet song';
            end;
          end;
        end;

      SDLK_M:
        begin
          // Multiply lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            CzesciMultiply;
            Text[TextDebug].Text := 'BPM and note lengths doubled';
          end;
        end;

      SDLK_N:
        begin
          if (SDL_ModState = KMOD_LSHIFT) then
          begin
            // one line, mp3
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            PlayOneSentence := true;
            Click := false;
            Music.Stop;
            R := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            if R <= Music.Length then
            begin
              Music.MoveTo(R);
              PlayStopTime := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec)+
                Ini.LipSync*0.01 + (120 + Ini.Delay*10) / 1000;
              PlaySentence := true;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
            end;
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            // Play whole file
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Click := false;
            Music.Stop;
            R := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            if R <= Music.Length then
            begin
              Music.MoveTo(R);
              PlayStopTime := Music.Length;
              PlaySentence := true;
              PlayOneSentence := false;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
            end;
          end;

          if PlaySentence then
          begin
            SetLength(Pitches, 0);
            PitchRecOn := true;
            noteStart := AktNuta[CP];
            lineStart := Czesci[CP].Akt;
            cpStart := CP;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            AktNuta[CP] := 0;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
            LineChanged[0]:=false;
            LineChanged[1]:=false;
            PlayVideo := false;
          end;

          if (SDL_ModState = 0) then
          begin
            // Set actual note over pitch detection
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Ton := ActTonePitch;
          end;
        end;

      SDLK_C:
        begin
          // Capitalize letter at the beginning of line
          if SDL_ModState = 0 then
          begin
            LyricsCapitalize;
            Text[TextDebug].Text := Language.Translate('EDITOR_CAPITALIZE_LETTER');
            EditorLyric[CP].Selected := AktNuta[CP];
          end;

          // Correct spaces
          if SDL_ModState = KMOD_LSHIFT then
          begin
            LyricsCorrectSpaces;
            Text[TextDebug].Text := 'Corrected lyric spaces';
          end;

          // Copy sentence
          if SDL_ModState = KMOD_LCTRL then
          begin
            MarkSrc;
            Text[TextDebug].Text := 'Line marked'
          end;
        end;

      SDLK_R:   //reload
        begin
          MidiOut.Close;
          MidiOut.Free;
          Music.Close;
          acClose;

          onShow;
          Text[TextDebug].Text := 'Song reloaded!';
        end;

      SDLK_V:
        begin
          // Paste text
          if SDL_ModState = KMOD_LCTRL then
          begin
            if (Length(Czesci[CopySrcCP].Czesc)>CopySrcLine) then
            begin
              if Czesci[CP].Czesc[Czesci[CP].Akt].IlNut >= Czesci[CopySrcCP].Czesc[CopySrcLine].IlNut then
              begin
                PasteText;
                Text[TextDebug].Text := 'Text pasted';
              end else
                beep;
            end else
              beep;
          end;

          if (SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT) then
          begin
            CopySentence(CopySrcLine, Czesci[CP].Akt);
            Text[TextDebug].Text := 'Line pasted';
          end;

          if SDL_ModState = 0 then
            StartVideo;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            StartVideo;
            Click := true;
          end;
        end;

      SDLK_4:
        begin
          if AktSong.isDuet then //TODO !!!
            Exit;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopySentence(CopySrcLine, Czesci[0].Akt);
            CopySentence(CopySrcLine+1, Czesci[0].Akt+1);
            CopySentence(CopySrcLine+2, Czesci[0].Akt+2);
            CopySentence(CopySrcLine+3, Czesci[0].Akt+3);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopySentences(CopySrcLine, Czesci[0].Akt, 4);
          end;
        end;
      SDLK_5:
        begin
          if AktSong.isDuet then //TODO !!!
            Exit;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
          begin
            CopySentence(CopySrcLine, Czesci[0].Akt);
            CopySentence(CopySrcLine+1, Czesci[0].Akt+1);
            CopySentence(CopySrcLine+2, Czesci[0].Akt+2);
            CopySentence(CopySrcLine+3, Czesci[0].Akt+3);
            CopySentence(CopySrcLine+4, Czesci[0].Akt+4);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
          begin
            CopySentences(CopySrcLine, Czesci[0].Akt, 5);
          end;
        end;

      SDLK_T:
        begin
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;
          // Fixes timings between sentences
          FixTimings;
          Text[TextDebug].Text := Language.Translate('EDITOR_FIX_TIMINGS');
          EditorLyric[CP].Selected := AktNuta[CP];
        end;

      SDLK_F4:
        begin
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;
          // Enter Text Edit Mode
          editText := Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst;
          TextEditMode := true;
        end;

      SDLK_F5:
        begin
          // Enter BPM Edit Mode
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;
          Text[TextBPM].Text := Text[TextBPM].Text + '|';
          BPMEditMode := true;
        end;

      SDLK_P:
        begin
          // one line, mp3 + clicks
          if SDL_ModState = 0 then
          begin
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            PlayOneSentence := true;
            Click := true;
            Music.Stop;
            R := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            if R <= Music.Length then
            begin
              Music.MoveTo(R);
              PlayStopTime := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
              PlaySentence := true;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
            end;
          end;

          // one line, midi
          if SDL_ModState = KMOD_LSHIFT then
          begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            PlayOneSentence := true;
            MidiTime := USTime.GetTime;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
            MidiStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            MidiStop := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);

            LastClick := -100;
          end;

          // one line midi + mp3
          if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then
          begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            PlayOneSentence := true;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            MidiStop := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
            LastClick := -100;

            PlaySentence := true;
            PlayOneNote := false;
            //Click := true;
            Music.Stop;
            Music.MoveTo(GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote)+0{-0.10});
            PlayStopTime := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec)+0;
            Music.Play;
            LastClick := -100;
          end;

          //new: play hole file + LALT
          if SDL_ModState = KMOD_LALT then
          begin
            // Play Sentence
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Click := true;
            Music.Stop;
            R := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            if R <= Music.Length then
            begin
              Music.MoveTo(R);
              PlayStopTime := Music.Length;
              PlaySentence := true;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
            end;
          end;

          if SDL_ModState = KMOD_LSHIFT or KMOD_LALT then
          begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            MidiStop := Music.Length;

            LastClick := -100;
          end;

          if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT then
          begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            MidiStop := Music.Length;
            LastClick := -100;

            PlaySentence := true;
            PlayOneNote := false;
            //Click := true;
            Music.Stop;
            Music.MoveTo(GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote)+0{-0.10});
            PlayStopTime := Music.Length;
            Music.Play;
            LastClick := -100;
          end;

          if PlaySentenceMidi or PlaySentence then
          begin
            noteStart := AktNuta[CP];
            lineStart := Czesci[CP].Akt;
            cpStart := CP;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            AktNuta[CP] := 0;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
            LineChanged[0]:=false;
            LineChanged[1]:=false;
            PlayVideo := false;
          end;

        end;

      SDLK_SPACE:
        begin
          //Thx to f1fth_freed0m for his One Note Midi Playback
          if SDL_ModState = KMOD_LSHIFT then
          begin //Play One Notes Midi [Shift + Space]
            PlaySentenceMidi := false;
            PlayOneNoteMidi := true;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);
            MidiStop := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
            LastClick := -100;
          end

          else if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then
          begin
            //Play One Notes Midi + MP3 [CTRL + Shift + Space]
            PlaySentenceMidi := false;
            PlayOneNoteMidi := true;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);
            MidiStop := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
            LastClick := -100;

            PlaySentence := false;
            PlayOneNote := true;
            Click := true;
            Music.Stop;
            Music.MoveTo(GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start));
            PlayStopTime := (GetTimeFromBeat(
                             Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start +
                             Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc));
            Music.Play;
            LastClick := -100;
          end

          Else
          begin
            // Play One Notes MP3 [Space]
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false; // stop midi
            PlayOneNoteMidi := false;
            PlaySentence := false;
            PlayOneNote := true;
            Click := false;
            Music.Stop;

            Music.MoveTo(GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start));
            PlayStopTime := (GetTimeFromBeat(
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start +
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc));
            Music.Play;
            LastClick := -100;
          end;
        end;
      SDLK_RETURN:
        begin
        end;

      SDLK_LCTRL:
        begin
        end;

      SDLK_DELETE:
        begin
          if SDL_ModState = KMOD_LCTRL then
          begin
            // moves text to right in current sentence
            DeleteNote;
            Text[TextDebug].Text := 'Note deleted';
          end;

          if SDL_ModState = KMOD_LSHIFT then
          begin
            DeleteSentence;
            Text[TextDebug].Text := 'Line deleted';
          end;
        end;

      SDLK_PERIOD:
        begin
          // moves text to right in current sentence
          MoveTextToRight;
        end;

      SDLK_RIGHT:
        begin
          if PlaySentenceMidi or PlaySentence then
          begin
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;

            if (cpStart = CP) and (lineStart = Czesci[CP].Akt) then
              AktNuta[CP] := noteStart;

            Dec(AktNuta[CP]);
            if AktNuta[CP] = -1 then AktNuta[CP] := Czesci[CP].Czesc[Czesci[CP].Akt].HighNut;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
          end;
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // right
          if SDL_ModState = 0 then
          begin
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            Inc(AktNuta[CP]);

            if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].IlNut then AktNuta[CP] := 0;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then
          begin
            if Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc > 1 then
            begin
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
              Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);
              if AktNuta[CP] = 0 then
              begin
                Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Start);
                Inc(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
              end;
              FixTimings;
            end;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);
            if AktNuta[CP] = 0 then
            begin
              Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Start);
              Inc(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            end;
            if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].HighNut then
              Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
            FixTimings;
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then
          begin
            Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
            if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].HighNut then
              Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
            FixTimings;
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(1);
            FixTimings;
          end;
        end;

      SDLK_LEFT:
        begin
          if PlaySentenceMidi or PlaySentence then
          begin
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            if (cpStart = CP) and (lineStart = Czesci[CP].Akt) then
              AktNuta[CP] := noteStart;

            Inc(AktNuta[CP]);
            if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].IlNut then AktNuta[CP] := 0;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
          end;
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // left
          if SDL_ModState = 0 then
          begin
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            Dec(AktNuta[CP]);
            if AktNuta[CP] = -1 then
              AktNuta[CP] := Czesci[CP].Czesc[Czesci[CP].Akt].HighNut;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then
          begin
            Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);
            Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
            if AktNuta[CP] = 0 then begin
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Start);
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            end;
            FixTimings;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then
          begin
            Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);

            // resizing sentences
            if AktNuta[CP] = 0 then
            begin
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Start);
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
            end;

            if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].HighNut then
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
            FixTimings;
          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then
          begin
            if Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc > 1 then
            begin
              Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
              if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].HighNut then
                Dec(Czesci[CP].Czesc[Czesci[CP].Akt].Koniec);
            end;
            FixTimings;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
          begin
            MoveAllToEnd(-1);
            FixTimings;
          end;
        end;

      SDLK_DOWN:
        begin
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // skip to next sentence
          if SDL_ModState = 0 then
          begin
            Refresh;
            Inc(Czesci[CP].Akt);
            AktNuta[CP] := 0;
            //AktNuta[1] := 0;
            if Czesci[CP].Akt > Czesci[CP].High then
              Czesci[CP].Akt := 0;

            SelectNextNote;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LCTRL then
          begin
            TransposeNote(-1);
          end;

          // select singer 2 notes if possible
          if (SDL_ModState = KMOD_LSHIFT) and AktSong.isDuet then
          begin
            if (Length(Czesci[1].Czesc[Czesci[1].Akt].Nuta)>0) then
            begin
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
              CP := 1;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            end;
          end;

          if AktSong.isDuet and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and (CP=0) then
            DuetCopyLine;

          if AktSong.isDuet and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) and (CP=0) then
            DuetMoveLine;
        end;

      SDLK_UP:
        begin
          MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged[0]:=false;
          LineChanged[1]:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // skip to previous sentence
          if SDL_ModState = 0 then
          begin
            Refresh;
            Dec(Czesci[CP].Akt);
            AktNuta[CP] := 0;
            if Czesci[CP].Akt = -1 then
              Czesci[CP].Akt := Czesci[CP].High;

            SelectPrevNote;
          end;

          // increase tone
          if SDL_ModState = KMOD_LCTRL then
          begin
            TransposeNote(1);
          end;

          // select singer 1 notes if possible
          if (SDL_ModState = KMOD_LSHIFT) and AktSong.isDuet then
          begin
            if (Length(Czesci[0].Czesc[Czesci[0].Akt].Nuta)>0) then
            begin
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
              CP := 0;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            end;
          end;

          if AktSong.isDuet and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) and (CP=1) then
            DuetCopyLine;

          if AktSong.isDuet and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) and (CP=1) then
            DuetMoveLine;
        end;

      // Golden Note Patch
      SDLK_G:
        begin
          case Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc of
            0, 1: Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc := 2;
            2:    Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc := 1;
          end; // case
          Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Freestyle := False;
        end;

      // Freestyle Note Patch
      SDLK_F:
        begin
           case Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc of
            0:
            begin;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc := 1;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Freestyle := False;
            end;
            1,2:
            begin;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc := 0;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Freestyle := True;
            end;
          end; // case
          EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
          EditorLyric[CP].Selected := AktNuta[CP];
        end;

      //MP3-Volume Up
      SDLK_PAGEUP:
        begin
          if (SDL_ModState = 0) then
          begin
            if (MP3Volume<100) then
              MP3Volume := MP3Volume+5;
            Music.SetMusicVolume(MP3Volume);
            Text[TextDebug].Text := 'MP3 Volume: ' + IntToStr(MP3Volume) + '%';
          end;

          // Increase tone of all notes
          if (SDL_ModState = KMOD_LCTRL or KMOD_LALT) then
            ChangeWholeTone(1);
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT or KMOD_LALT) then
            ChangeWholeTone(12);

          // Increase tone of all notes of actual line
          if (SDL_ModState = KMOD_LCTRL) then
            ChangeWholeToneActLine(1);
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
            ChangeWholeToneActLine(12);
        end;

      //MP3-Volume Down
      SDLK_PAGEDOWN:
        begin
          if (SDL_ModState = 0) then
          begin
            if (MP3Volume>0) then
              MP3Volume := MP3Volume-5;
            Music.SetMusicVolume(MP3Volume);
            Text[TextDebug].Text := 'MP3 Volume: ' + IntToStr(MP3Volume) + '%';
          end;

          // Decrease tone of all notes
          if (SDL_ModState = KMOD_LCTRL or KMOD_LALT) then
            ChangeWholeTone(-1);
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT or KMOD_LALT) then
            ChangeWholeTone(-12);

          // Decrease tone of all notes of actual line
          if (SDL_ModState = KMOD_LCTRL) then
            ChangeWholeToneActLine(-1);
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
            ChangeWholeToneActLine(-12);
        end;
      end;
    end;
  end; // if
end;

function TScreenEditSub.ParseInputEditText(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  SDL_ModState:  Word;
begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  if Ini.Debug=1 then
    Text[TextDebug].Text := 'PressedKey: ' + IntToStr(PressedKey) + ' ScanCode: ' + IntToStr(ScanCode);
    
  // check normal keys
    if not (ScanCode in [0..31, 127..159]) then //=isPrintable
    begin
      Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst :=
      Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst + chr(ScanCode);

      //EditorLyric[CP].ChangeCurText(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst);
      EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
      EditorLyric[CP].Selected := AktNuta[CP];
      Exit;
    end;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE:
        begin
          Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst := editText;
          EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
          TextEditMode := false;
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          TextEditMode := false;
        end;

      SDLK_BACKSPACE:
        begin
          Delete(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst,
            Length(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst), 1);

          //EditorLyric[CP].ChangeCurText(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst);
          EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
          EditorLyric[CP].Selected := AktNuta[CP];
        end;
      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            Inc(AktNuta[CP]);
            if AktNuta[CP] = Czesci[CP].Czesc[Czesci[CP].Akt].IlNut then
              AktNuta[CP] := 0;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
            editText := Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            Dec(AktNuta[CP]);
            if AktNuta[CP] = -1 then
              AktNuta[CP] := Czesci[CP].Czesc[Czesci[CP].Akt].HighNut;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
            editText := Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst;
          end;
      end;
    end;
  end;
end;

function TScreenEditSub.ParseInputEditBPM(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  //SDL_ModState: Word;
  strBPM:       string;
  temp:         real;

begin
  // used when in Text Edit Mode
  Result := true;

  //SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
  //  + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  // check normal keys
  if (ScanCode in [48..57, 44]) then
  begin
    strBPM := Text[TextBPM].Text;
    Delete(strBPM, Length(strBPM), 1);
    Text[TextBPM].Text := strBPM + chr(ScanCode) + '|';
    Exit;
  end;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE:
        begin
          Text[TextBPM].Text := FloatToStr(AktSong.BPM[0].BPM / 4);
          BPMEditMode := false;
        end;
      SDLK_F5, SDLK_RETURN:
        begin
          strBPM := Text[TextBPM].Text;
          Delete(strBPM, Length(strBPM), 1);
          Temp := StrToFloatDef(strBPM, 0);
          if (temp>0) then
            ChangeBPM(temp*4);

          BPMEditMode := false;
        end;
      
      SDLK_BACKSPACE:
        begin
          strBPM := Text[TextBPM].Text;
          if Length(strBPM)>1 then
          begin
            Delete(strBPM, Length(strBPM)-1, 2);
            Text[TextBPM].Text := strBPM + '|';
          end;
        end;
    end;
  end;
end;

procedure TScreenEditSub.DrawPitch(x, y, Width, Height: single; beat: integer);
var
  x1, y1, x2, y2: single;
  i: integer;
  ToneBoxWidth: real;
  ToneString: string;
  ToneStringWidth, ToneStringHeight: real;
  ToneStringMaxWidth: real;
  ToneStringCenterXOffset: real;

const
  PitchBarInnerHSpacing = 2;
  PitchBarInnerVSpacing = 1;

begin
  // calc tone pitch
  Sound[0].AnalizujBufor;

  // coordinates for black rect
  x1 := x;
  y1 := y;
  x2 := x + Width;
  y2 := y + Height;

  // init blend mode
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  // draw black background-rect
  glColor4f(0, 0, 0, 0.8);
  glBegin(GL_QUADS);
    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
  glEnd();

  // coordinates for tone boxes
  ToneBoxWidth := Width / NumHalftones;
  y1 := y1 + PitchBarInnerVSpacing;
  y2 := y2 - PitchBarInnerVSpacing;

  glBegin(GL_QUADS);
    // draw tone boxes
    for i := 0 to NumHalftones-1 do
    begin
      x1 := x + i * ToneBoxWidth + PitchBarInnerHSpacing;
      x2 := x1 + ToneBoxWidth - 2*PitchBarInnerHSpacing;

      if ((Sound[0].SzczytJest) and
          (Sound[0].Ton = i)) then
      begin
        // highlight current tone-pitch
        glColor3f(1, i / (NumHalftones-1), 0);
        ActTonePitch := i;
      end
      else
      begin
        // grey other tone-pitches
        glColor3f(0.3, i / (NumHalftones-1) * 0.3, 0);
      end;

      glVertex2f(x1, y1);
      glVertex2f(x2, y1);
      glVertex2f(x2, y2);
      glVertex2f(x1, y2);
    end;
  glEnd();

  glDisable(GL_BLEND);

  ///
  // draw the name of the tone
  ///////

  ToneString := Sound[0].GetToneString + '(' + IntToStr(ActTonePitch) + ')';
  ToneStringHeight := 8;

  SetFontSize(ToneStringHeight);

  // center
  // Note: for centering let us assume that G#4 has the max. horizontal extent
  ToneStringWidth := glTextWidth(PChar(ToneString));
  ToneStringMaxWidth := glTextWidth('G#4 (222)');
  ToneStringCenterXOffset := (ToneStringMaxWidth-ToneStringWidth) / 2;

  // draw
  SetFontPos(x-ToneStringWidth-ToneStringCenterXOffset, y-ToneStringHeight/2);
  glColor3f(0, 0, 0);
  glPrint(PChar(ToneString));

  // rec pitches
  if not PitchRecOn then
    Exit;

  i := High(Pitches);
  beat := Floor(GetMidBeat(GetTimeFromBeat(beat) - Ini.LipSync*0.01 - (AktSong.Gap + 120 + Ini.Delay*10) / 1000));

  if (i = -1) or (Pitches[i].beat<beat) then
  begin
    SetLength(Pitches, i+2);
    Pitches[i+1].beat := beat;
    Pitches[i+1].pitch := ActTonePitch;
  end;
  
end;


procedure TScreenEditSub.StartVideo;
var
  R:  real;

begin
  // Play Sentences with Video
  MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
  PlaySentenceMidi := false;
  PlayOneNoteMidi := false;
  Click := false;
  Music.Stop;

  if PlayVideo then
  begin
    CP := cpStart;
    Czesci[CP].Akt := lineStart;
    AktNuta[CP] := noteStart;
  end;

  R := GetTimeFromBeat(Czesci[CP].Czesc[Czesci[CP].Akt].StartNote);
  if R <= Music.Length then
  begin
    Music.MoveTo(R);
    PlayStopTime := Music.Length;
    PlaySentence := true;
    PlayOneNote := false;
    Music.Play;
    LastClick := -100;
  end;

  noteStart := AktNuta[CP];
  lineStart := Czesci[CP].Akt;
  cpStart := CP;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
  AktNuta[CP] := 0;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].Selected := AktNuta[CP];
  LineChanged[0]:=false;
  LineChanged[1]:=false;
  PlayTime := 0;
  PlayVideo := true;
  StartVideoPreview;
end;

procedure TScreenEditSub.StartVideoPreview;
begin
  if (PlayTime<0.2) and PlayVideo then
  begin
    acClose;
    VidVis := none;
    StartTry := true;
  end else if StartTry then
  begin
  if (AktSong.Video <> '') and
    FileExists(AktSong.Path + AktSong.Video) then
    begin
      acOpenFile(PAnsiChar(AktSong.Path + AktSong.Video));

      acSkip2(AktSong.VideoGAP, Music.Position);
      Czas.Teraz := Music.Position;
      Czas.Razem := Music.Length;
      StartTry := false;
      try
        acGetFrame(Czas.Teraz);
        VidVis := windowed;

      except
        //If an Error occurs Reading Video: prevent Video from being Drawn again and Close Video
        Log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
        Log.LogError('Corrupted File: ' + AktSong.Video);
        AktSong.Video := ''; //dirt fix
        try
          acClose;
          VidVis := none;
        except
        end;
      end;
    end else
      VidVis := none;
  end;
end;

procedure TScreenEditSub.NewBeat;
begin
    // click
{    for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
  if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = Czas.AktBeat) then begin
      // old}
//    Music.PlayClick;
end;

procedure TScreenEditSub.ChangeBPM(newBPM: real);
var
  P:    integer;
  C:    integer;
  N:    integer;
  f:    real;

begin                    
  f := newBPM/AktSong.BPM[0].BPM;    //z.B. neu/alt => 1/2 = 0.5 => *0.5
  AktSong.BPM[0].BPM := newBPM;

  for P := 0 to Length(Czesci) - 1 do
  begin
    for C := 0 to Czesci[P].High do
    begin
      Czesci[P].Czesc[C].Start :=    ceil(Czesci[P].Czesc[C].Start *f);
      Czesci[P].Czesc[C].StartNote := ceil(Czesci[P].Czesc[C].StartNote *f);
      if (Length(Czesci[P].Czesc[C].Nuta)>0) then
      begin
        for N := 0 to Czesci[P].Czesc[C].HighNut do
        begin
          Czesci[P].Czesc[C].Nuta[N].Start :=   ceil(Czesci[P].Czesc[C].Nuta[N].Start *f); //round up
          Czesci[P].Czesc[C].Nuta[N].Dlugosc := floor(Czesci[P].Czesc[C].Nuta[N].Dlugosc *f); //round down
          if (Czesci[P].Czesc[C].Nuta[N].Dlugosc=0) then
            Czesci[P].Czesc[C].Nuta[N].Dlugosc := 1;
        end; // N (notes)
        Czesci[P].Czesc[C].Koniec :=    Czesci[P].Czesc[C].Nuta[Czesci[P].Czesc[C].HighNut].Start +
          Czesci[P].Czesc[C].Nuta[Czesci[P].Czesc[C].HighNut].Dlugosc;
      end else
        Czesci[P].Czesc[C].Koniec := round(Czesci[P].Czesc[C].Koniec * f);
    end; // C (lines)
  end;
end;


procedure TScreenEditSub.CzesciDivide;
begin
  ChangeBPM(AktSong.BPM[0].BPM / 2);
end;

procedure TScreenEditSub.CzesciMultiply;
begin
  ChangeBPM(AktSong.BPM[0].BPM * 2);
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  P:    integer;
  C:    integer;
  S:    string;
begin
  for P := 0 to Length(Czesci) - 1 do
  begin
    for C := 0 to Czesci[P].High do
    begin
      if (Length(Czesci[P].Czesc[C].Nuta)>0) then
      begin
        S := AnsiUpperCase(Copy(Czesci[P].Czesc[C].Nuta[0].Tekst, 1, 1));
        S := S + Copy(Czesci[P].Czesc[C].Nuta[0].Tekst, 2, Length(Czesci[P].Czesc[C].Nuta[0].Tekst)-1);
        Czesci[P].Czesc[C].Nuta[0].Tekst := S;
      end;
    end; // C
    EditorLyric[P].AddCzesc(P, Czesci[P].Akt);
  end;
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  P:    integer;
  C:    integer;
  N:    integer;
begin
  for P := 0 to Length(Czesci) - 1 do
  begin
    for C := 0 to Czesci[P].High do
    begin
      if(Length(Czesci[P].Czesc[C].Nuta)>0) then
      begin
        // correct starting spaces in the first word
        while Copy(Czesci[P].Czesc[C].Nuta[0].Tekst, 1, 1) = ' ' do
          Czesci[P].Czesc[C].Nuta[0].Tekst := Copy(Czesci[P].Czesc[C].Nuta[0].Tekst, 2, 100);

        // move spaces on the start to the end of the previous note
        {
        for N := 1 to Czesci[P].Czesc[C].HighNut do
        begin
          while (Copy(Czesci[P].Czesc[C].Nuta[N].Tekst, 1, 1) = ' ') do
          begin
            Czesci[P].Czesc[C].Nuta[N].Tekst := Copy(Czesci[P].Czesc[C].Nuta[N].Tekst, 2, 100);
            Czesci[P].Czesc[C].Nuta[N-1].Tekst := Czesci[P].Czesc[C].Nuta[N-1].Tekst + ' ';
          end;
        end; // N
        }

        // correct '-'  to '- '
        {for N := 0 to Czesci[P].Czesc[C].HighNut do
        begin
          if Czesci[P].Czesc[C].Nuta[N].Tekst = '-' then
            Czesci[P].Czesc[C].Nuta[N].Tekst := '- ';
        end; // N
        }
        // add space to the previous note when the current word is '- '
        {for N := 1 to Czesci[P].Czesc[C].HighNut do
        begin
          if Czesci[P].Czesc[C].Nuta[N].Tekst  = '- ' then
            Czesci[P].Czesc[C].Nuta[N-1].Tekst := Czesci[P].Czesc[C].Nuta[N-1].Tekst + ' ';
        end; // N
        }

        // correct too many spaces at the end of note
        for N := 0 to Czesci[P].Czesc[C].HighNut do
        begin
          while Copy(Czesci[P].Czesc[C].Nuta[N].Tekst, Length(Czesci[P].Czesc[C].Nuta[N].Tekst)-1, 2) = '  ' do
            Czesci[P].Czesc[C].Nuta[N].Tekst := Copy(Czesci[P].Czesc[C].Nuta[N].Tekst, 1, Length(Czesci[P].Czesc[C].Nuta[N].Tekst)-1);
        end; // N

        // and correct if there is no space at the end of sentence
        {N := Czesci[P].Czesc[C].HighNut;
        if Copy(Czesci[P].Czesc[C].Nuta[N].Tekst, Length(Czesci[P].Czesc[C].Nuta[N].Tekst), 1) <> ' ' then
          Czesci[P].Czesc[C].Nuta[N].Tekst := Czesci[P].Czesc[C].Nuta[N].Tekst + ' ';}
      end;
    end; // C
    EditorLyric[P].AddCzesc(P, Czesci[P].Akt);
  end;
end;

procedure TScreenEditSub.FixTimings;
var
  P:    integer;
  C:    integer;
  S:    integer;
  Min:  integer;
  Max:  integer;
  len:  integer;

  function GetMin(L: integer): integer;
  var
    len:  integer;
    min:  integer;
  begin
    Result := low(integer);
    if (Length(Czesci[0].Czesc[L].Nuta)>0) then
    begin
      len := Length(Czesci[0].Czesc[L].Nuta);
      Result := Czesci[0].Czesc[L].Nuta[len-1].Start + Czesci[0].Czesc[L].Nuta[len-1].Dlugosc;
    end;

    if not AktSong.isDuet then
      Exit;

    if (Length(Czesci[1].Czesc[L].Nuta)>0) then
    begin
      len := Length(Czesci[1].Czesc[L].Nuta);
      min := Czesci[1].Czesc[L].Nuta[len-1].Start + Czesci[1].Czesc[L].Nuta[len-1].Dlugosc;
      if (min>Result) then
        Result := min;
    end;
  end;

  function GetMax(L: integer): integer;
  var
    max:  integer;
  begin
    Result := high(integer);
    if (Length(Czesci[0].Czesc[L].Nuta)>0) then
    begin
      Result := Czesci[0].Czesc[L].Nuta[0].Start;
    end;

    if not AktSong.isDuet then
      Exit;

    if (Length(Czesci[1].Czesc[L].Nuta)>0) then
    begin
      max := Czesci[1].Czesc[L].Nuta[0].Start;
      if (max<Result) then
        Result := max;
    end;
  end;

begin
  for P := 0 to Length(Czesci) - 1 do
  begin
    for C := 1 to Length(Czesci[P].Czesc) - 1 do
    begin
      len := Length(Czesci[P].Czesc[C-1].Nuta);
      Min := Czesci[P].Czesc[C-1].Nuta[len-1].Start + Czesci[P].Czesc[C-1].Nuta[len-1].Dlugosc;
      Max := Czesci[P].Czesc[C].Nuta[0].Start;
      case (Max - Min) of
        0:    S := Max;
        1:    S := Max;
        2:    S := Max - 1;
        3:    S := Max - 2;
        else
          S := Min + 2;
      end; // case

      Czesci[P].Czesc[C].Start := S;
      Czesci[P].Czesc[C-1].Koniec := Min;
    end; // for
    C := Length(Czesci[P].Czesc) - 1;
    len := Length(Czesci[P].Czesc[C].Nuta);
    Max := Czesci[P].Czesc[C].Nuta[len-1].Start + Czesci[P].Czesc[C].Nuta[len-1].Dlugosc;
    Czesci[P].Czesc[C].Koniec := Max;
  end;
  {
  //second run for duet mode:
  if not AktSong.isDuet then
    Exit;
  
  for P := 0 to Length(Czesci) - 1 do
  begin
    Czesci[P].Czesc[0].Start := -100;
    for C := 1 to Czesci[P].High do
    begin
      if (Length(Czesci[P].Czesc[C-1].Nuta)=0) then
        Czesci[P].Czesc[C].Start := Czesci[(P+1) mod 2].Czesc[C].Start;
    end;
  end;    }
end;

procedure TScreenEditSub.DivideSentence;
var
  P:      integer;
  C:      integer;
  CStart: integer;
  CNew:   integer;
  CLen:   integer;
  N:      integer;
  NStart: integer;
  NHigh:  integer;
  NNewL:  integer;

  BStart: integer; //start beat

begin
  CStart := Czesci[CP].Akt;
  BStart := Czesci[CP].Czesc[CStart].Nuta[AktNuta[CP]].Start;

  CNew := CStart + 1;

  P := CP;
  {for P := 0 to Length(Czesci) - 1 do
  begin}
    // increase sentence length by 1
    CLen := Length(Czesci[P].Czesc);
    SetLength(Czesci[P].Czesc, CLen + 1);
    Inc(Czesci[P].Ilosc);
    Inc(Czesci[P].High);

    // move needed sentences to one forward. newly has the copy of divided sentence
    for C := CLen-1 downto CStart do
      CopyLine(P, C, P, C+1);
      //Czesci[P].Czesc[C+1] := Czesci[P].Czesc[C];

    // clear and set new sentence
    NStart := -1;
    if (Length(Czesci[P].Czesc[CStart].Nuta)>0) then
    begin
      for N := 0 to Length(Czesci[P].Czesc[CStart].Nuta) - 1 do
      begin
        if Czesci[P].Czesc[CStart].Nuta[N].Start>=BStart then
        begin
          NStart := N;
          break;
        end;
      end;

      if (NStart > -1) then
      begin
        Czesci[P].Czesc[CNew].Start := Czesci[P].Czesc[CStart].Nuta[NStart].Start;
        Czesci[P].Czesc[CNew].StartNote := Czesci[P].Czesc[CStart].Nuta[NStart].Start;
      end;
    end;

    Czesci[P].Czesc[CNew].Lyric := '';
    Czesci[P].Czesc[CNew].LyricWidth := 0;
    Czesci[P].Czesc[CNew].Koniec := 0;
    Czesci[P].Czesc[CNew].BaseNote := 0; // 0.5.0: we modify it later in this procedure
    Czesci[P].Czesc[CNew].IlNut := 0;
    Czesci[P].Czesc[CNew].HighNut := -1;
    SetLength(Czesci[P].Czesc[CNew].Nuta, 0);

    // move right notes to new sentences
    if (NStart > -1) then
    begin
      NHigh := Czesci[P].Czesc[CStart].HighNut;
      for N := NStart to NHigh do
      begin
        NNewL := Czesci[P].Czesc[CNew].IlNut;
        SetLength(Czesci[P].Czesc[CNew].Nuta, NNewL + 1);
        CopyNote(P, CStart, N, P, CNew, NNewL);
        //Czesci[P].Czesc[CNew].Nuta[NNewL] := Czesci[P].Czesc[CStart].Nuta[N];

        // increase sentence counters
        Inc(Czesci[P].Czesc[CNew].IlNut);
        Inc(Czesci[P].Czesc[CNew].HighNut);
        Czesci[P].Czesc[CNew].Koniec := Czesci[P].Czesc[CNew].Nuta[NNewL].Start +
          Czesci[P].Czesc[CNew].Nuta[NNewL].Dlugosc;
      end;

      // clear old notes and set sentence counters
      Czesci[P].Czesc[CStart].HighNut := NStart - 1;
      Czesci[P].Czesc[CStart].IlNut := Czesci[P].Czesc[CStart].HighNut + 1;
      if (NStart>0) then
        Czesci[P].Czesc[CStart].Koniec := Czesci[P].Czesc[CStart].Nuta[NStart-1].Start +
          Czesci[P].Czesc[CStart].Nuta[NStart-1].Dlugosc;

      SetLength(Czesci[P].Czesc[CStart].Nuta, Czesci[P].Czesc[CStart].IlNut);
    end;
    Czesci[P].Akt := Czesci[P].Akt + 1;
    AktNuta[P] := 0;
  //end;

  Refresh;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  EditorLyric[CP].Selected := AktNuta[CP];
end;

procedure TScreenEditSub.JoinSentence;
var
  P:      integer;
  C:      integer;
  N:      integer;
  NStart: integer;
  NDst:   integer;
begin
  P := CP;

  C := Czesci[CP].Akt;
  // set new sentence
  NStart := Czesci[P].Czesc[C].IlNut;
  Czesci[P].Czesc[C].IlNut := Czesci[P].Czesc[C].IlNut + Czesci[P].Czesc[C+1].IlNut;
  Czesci[P].Czesc[C].HighNut := Czesci[P].Czesc[C].HighNut + Czesci[P].Czesc[C+1].IlNut;
  SetLength(Czesci[P].Czesc[C].Nuta, Czesci[P].Czesc[C].IlNut);

  // move right notes to new sentences
  if (Length(Czesci[P].Czesc[C+1].Nuta)>0) then
  begin
    for N := 0 to Czesci[P].Czesc[C+1].HighNut do
    begin
      NDst := NStart + N;
      CopyNote(P, C+1, N, P, C, NDst);
    end;

    //add space before first note of 2. sentence
    if (Copy(Czesci[P].Czesc[C].Nuta[NStart].Tekst, 1, 1) <> ' ') then
      Czesci[P].Czesc[C].Nuta[NStart].Tekst := ' ' + Czesci[P].Czesc[C].Nuta[NStart].Tekst;

    // increase sentence counters
    NDst := Czesci[P].Czesc[C].HighNut;
    Czesci[P].Czesc[C].Koniec := Czesci[P].Czesc[C].Nuta[NDst].Start +
    Czesci[P].Czesc[C].Nuta[NDst].Dlugosc;
  end;
  // move needed sentences to one backward.
  for C := Czesci[P].Akt + 1 to Czesci[P].High - 1 do
    CopyLine(P, C+1, P, C);

  // decrease sentence length by 1
  SetLength(Czesci[P].Czesc, Length(Czesci[P].Czesc) - 1);
  Dec(Czesci[P].Ilosc);
  Dec(Czesci[P].High);

  Refresh;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  EditorLyric[CP].Selected := AktNuta[CP];
end;

procedure TScreenEditSub.DivideNote;
var
  C:    integer;
  N:    integer;
  NLen: integer;
begin
  C := Czesci[CP].Akt;

  NLen := Czesci[CP].Czesc[C].IlNut + 1;
  SetLength(Czesci[CP].Czesc[C].Nuta, NLen);
  Inc(Czesci[CP].Czesc[C].HighNut);
  Inc(Czesci[CP].Czesc[C].IlNut);

  // we copy all notes including selected one
  for N := Czesci[CP].Czesc[C].HighNut downto AktNuta[CP]+1 do
  begin
    Czesci[CP].Czesc[C].Nuta[N] := Czesci[CP].Czesc[C].Nuta[N-1];
  end;

  // me slightly modify new note
  Czesci[CP].Czesc[C].Nuta[AktNuta[CP]].Dlugosc := ceil(Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Dlugosc/2);

  Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Start := Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Start +
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]].Dlugosc;

  Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Dlugosc := Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Dlugosc -
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]].Dlugosc;

  if (Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Dlugosc>0) then
  begin
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Tekst := '~';
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]].Color := 2;
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Color := 0;
  end else
  begin
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Tekst := ' ';
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]].Color := 0;
    Czesci[CP].Czesc[C].Nuta[AktNuta[CP]+1].Color := 2;

    Inc(AktNuta[CP]);
  end;

  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  EditorLyric[CP].Selected := AktNuta[CP];
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
  NLen: integer;
begin
  C := Czesci[CP].Akt;

  //Do Not delete Last Note
  if (Czesci[CP].High > 0) OR (Czesci[CP].Czesc[C].HighNut > 0) then
  begin

    // we copy all notes from the next to the selected one
    for N := AktNuta[CP]+1 to Czesci[CP].Czesc[C].HighNut do
    begin
      Czesci[CP].Czesc[C].Nuta[N-1] := Czesci[CP].Czesc[C].Nuta[N];
    end;

    NLen := Czesci[CP].Czesc[C].IlNut - 1;

    if (NLen > 0) then
    begin
      SetLength(Czesci[CP].Czesc[C].Nuta, NLen);
      Dec(Czesci[CP].Czesc[C].HighNut);
      Dec(Czesci[CP].Czesc[C].IlNut);

      // me slightly modify new note
      if AktNuta[CP] > Czesci[CP].Czesc[C].HighNut then
        Dec(AktNuta[CP]);

    end
    //Last Note of current Sentence Deleted - > Delete Sentence
    else
    begin
      DeleteSentence;
    end;
  end;
  
  Refresh;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].Selected := AktNuta[CP];
end;

procedure TScreenEditSub.DeleteSentence;
var
  Pv, Pt: integer;
  P:      integer;
  C:      integer;
  N:      integer;

begin
  Pv := CP;
  Pt := CP;

  {if AktSong.isDuet then
  begin
    if (Length(Czesci[(CP+1) mod 2].Czesc[Czesci[CP].Akt].Nuta)=0) then
    begin
      Pv := 0;
      Pt := 1;
    end;
  end;  }

  for P := Pv to Pt do
  begin
    C := Czesci[CP].Akt;
    {if (Pv <> Pt) or not AktSong.isDuet then
    begin}
      //Move all Sentences after the current to the Left
      for N := C+1 to Czesci[P].High do
        //Czesci[P].Czesc[N-1] := Czesci[P].Czesc[N];
        CopyLine(P, N, P, N-1);

      //Delete Last Sentence
      SetLength(Czesci[P].Czesc, Czesci[P].High);
      Czesci[P].High := High(Czesci[P].Czesc);
      Czesci[P].Ilosc := Length(Czesci[P].Czesc);

      AktNuta[P] := 0;
      if (C > 0) then
        Czesci[P].Akt := C - 1
      else
        Czesci[P].Akt := 0;
    {end else
    begin
      //delete all notes in that line
      SetLength(Czesci[P].Czesc[C].Nuta, 0);

      //switch to the other line
      CP := (CP+1) mod 2;
      AktNuta[CP] := 0;
      AktNuta[(CP+1) mod 2] := 0;
    end;}
  end;
  Refresh;
  //SelectPrevNote();
  //SelectNextNote();
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
end;

procedure TScreenEditSub.TransposeNote(Transpose: integer);
begin
  if (Length(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta)>0) then
  begin
    Inc(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Ton, Transpose);
  end;
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: integer);
var
  C:  integer;
  N:  integer;
begin

    for C := 0 to Czesci[CP].High do
    begin
      if (Length(Czesci[CP].Czesc[C].Nuta)>0) then
      begin
        Czesci[CP].Czesc[C].BaseNote := Czesci[CP].Czesc[C].BaseNote + Tone;
        for N := 0 to Czesci[CP].Czesc[C].HighNut do
          Czesci[CP].Czesc[C].Nuta[N].Ton := Czesci[CP].Czesc[C].Nuta[N].Ton + Tone;
      end;
    end;
end;

procedure TScreenEditSub.ChangeWholeToneActLine(Tone: integer);
var
  C:  integer;
  N:  integer;

begin
  C := Czesci[CP].Akt;
  if (Length(Czesci[CP].Czesc[C].Nuta)>0) then
  begin
    Czesci[CP].Czesc[C].BaseNote := Czesci[CP].Czesc[C].BaseNote + Tone;
    for N := 0 to Czesci[CP].Czesc[C].HighNut do
      Czesci[CP].Czesc[C].Nuta[N].Ton := Czesci[CP].Czesc[C].Nuta[N].Ton + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: integer);
var
  C:    integer;
  N:    integer;
  NStart: integer;
begin
  for C := Czesci[CP].Akt to Czesci[CP].High do
  begin
    NStart := 0;
    if C = Czesci[CP].Akt then NStart := AktNuta[CP];
    for N := NStart to Czesci[CP].Czesc[C].HighNut do
    begin
      Inc(Czesci[CP].Czesc[C].Nuta[N].Start, Move); // move note start

      if N = 0 then
      begin // fix beginning
        Inc(Czesci[CP].Czesc[C].Start, Move);
        Inc(Czesci[CP].Czesc[C].StartNote, Move);
      end;

      if N = Czesci[CP].Czesc[C].HighNut then // fix ending
        Inc(Czesci[CP].Czesc[C].Koniec, Move);

    end; // for
  end; // for
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
end;

procedure TScreenEditSub.MoveTextToRight;
var
  C:      integer;
  N:      integer;
  NHigh:  integer;
begin
  C := Czesci[CP].Akt;
  NHigh := Czesci[CP].Czesc[C].HighNut;

  // last word
  Czesci[CP].Czesc[C].Nuta[NHigh].Tekst := Czesci[CP].Czesc[C].Nuta[NHigh-1].Tekst +
    Czesci[CP].Czesc[C].Nuta[NHigh].Tekst;

  // other words
  for N := NHigh - 1 downto AktNuta[CP] + 1 do
  begin
    Czesci[CP].Czesc[C].Nuta[N].Tekst := Czesci[CP].Czesc[C].Nuta[N-1].Tekst;
  end; // for
  Czesci[CP].Czesc[C].Nuta[AktNuta[CP]].Tekst := '- ';
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
end;

procedure TScreenEditSub.MarkSrc;
begin
  CopySrcLine := Czesci[CP].Akt;
  CopySrcCP := CP;
end;

procedure TScreenEditSub.PasteText;
var
  C:    integer;
  N:    integer;
begin
  C := Czesci[CP].Akt;

  for N := 0 to Czesci[CopySrcCP].Czesc[CopySrcLine].HighNut do
    Czesci[CP].Czesc[C].Nuta[N].Tekst := Czesci[CopySrcCP].Czesc[CopySrcLine].Nuta[N].Tekst;

  Refresh;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
end;

procedure TScreenEditSub.CopySentence(Src, Dst: integer);
var
  N:      integer;
  Time1:  integer;
  Time2:  integer;
  TD:  integer;
begin
  Time1 := Czesci[CopySrcCP].Czesc[Src].Nuta[0].Start;
  Time2 := Czesci[CP].Czesc[Dst].Nuta[0].Start;
  TD := Time2-Time1;

  SetLength(Czesci[CP].Czesc[Dst].Nuta, Czesci[CopySrcCP].Czesc[Src].IlNut);
  Czesci[CP].Czesc[Dst].IlNut := Czesci[CopySrcCP].Czesc[Src].IlNut;
  Czesci[CP].Czesc[Dst].HighNut := Czesci[CopySrcCP].Czesc[Src].HighNut;
  for N := 0 to Czesci[CopySrcCP].Czesc[Src].HighNut do
  begin
    Czesci[CP].Czesc[Dst].Nuta[N].Tekst := Czesci[CopySrcCP].Czesc[Src].Nuta[N].Tekst;
    Czesci[CP].Czesc[Dst].Nuta[N].Dlugosc := Czesci[CopySrcCP].Czesc[Src].Nuta[N].Dlugosc;
    Czesci[CP].Czesc[Dst].Nuta[N].Ton := Czesci[CopySrcCP].Czesc[Src].Nuta[N].Ton;
    Czesci[CP].Czesc[Dst].Nuta[N].FreeStyle := Czesci[CopySrcCP].Czesc[Src].Nuta[N].FreeStyle;
    Czesci[CP].Czesc[Dst].Nuta[N].Wartosc := Czesci[CopySrcCP].Czesc[Src].Nuta[N].Wartosc;
    Czesci[CP].Czesc[Dst].Nuta[N].Start := Czesci[CopySrcCP].Czesc[Src].Nuta[N].Start + TD;
  end;
  N := Czesci[CopySrcCP].Czesc[Src].HighNut;
  Czesci[CP].Czesc[Dst].Koniec := Czesci[CP].Czesc[Dst].Nuta[N].Start + Czesci[CP].Czesc[Dst].Nuta[N].Dlugosc;

  Refresh;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
end;

procedure TScreenEditSub.CopySentences(Src, Dst, Num: integer);
var
  C:      integer;
begin
  // create place for new sentences
  SetLength(Czesci[0].Czesc, Czesci[0].Ilosc + Num - 1);

  // moves sentences next to the destination
  for C := Czesci[0].High downto Dst + 1 do begin
    Czesci[0].Czesc[C + Num - 1] := Czesci[0].Czesc[C];
  end;

  // prepares new sentences: sets sentence start and create first note
  for C := 1 to Num-1 do begin
    Czesci[0].Czesc[Dst + C].Start := Czesci[0].Czesc[Dst + C - 1].StartNote +
      (Czesci[0].Czesc[Src + C].StartNote - Czesci[0].Czesc[Src + C - 1].StartNote);
    SetLength(Czesci[0].Czesc[Dst + C].Nuta, 1);
    Czesci[0].Czesc[Dst + C].IlNut := 1;
    Czesci[0].Czesc[Dst + C].HighNut := 0;
    Czesci[0].Czesc[Dst + C].Nuta[0].Start := Czesci[0].Czesc[Dst + C].Start;
    Czesci[0].Czesc[Dst + C].Nuta[0].Dlugosc := 1;
    Czesci[0].Czesc[Dst + C].StartNote := Czesci[0].Czesc[Dst + C].Start;
    Czesci[0].Czesc[Dst + C].Koniec := Czesci[0].Czesc[Dst + C].Start + 1;
  end;

  // increase counters
  Czesci[0].Ilosc := Czesci[0].Ilosc + Num - 1;
  Czesci[0].High := Czesci[0].High + Num - 1;

  for C := 0 to Num-1 do
    CopySentence(Src + C, Dst + C);
end;


constructor TScreenEditSub.Create;
begin
  inherited Create;
  SetLength(Player, 1);

  //light blue:
  cRB := 0.9; cGB := 0.95; cBB := 1;

  //light red:
  cRR := 1; cGR := 0.8; cBR := 0.8;

  // Line
  //AddText(500, 573, 1, 7, 0, 0, 0, 'Line:');
  TextSentence := AddText(500, 573, 1, 7, 0, 0, 0, 'Line: 0/0');

  // Note
  //AddText(655, 573, 1, 7, 0, 0, 0, 'Note:');
  TextNote := AddText(655, 573, 1, 7, 0, 0, 0, 'Note: 0/0');

  AddText(10, 10, 0, 8, 0, 0, 0, 'Title:');
  AddText(10, 30, 0, 8, 0, 0, 0, 'Artist:');
  //AddText(10, 50, 0, 8, 0, 0, 0, 'Mp3:');
  AddText(10, 50, 0, 8, 0, 0, 0, 'BPM:');
  AddText(10, 70, 0, 8, 0, 0, 0, 'GAP:');

  TextTitle :=  AddText(80, 10, 0, 8, 0, 0, 0, 'a');
  TextArtist := AddText(80, 30, 0, 8, 0, 0, 0, 'b');
  //TextMp3 :=    AddText(80, 50, 0, 8, 0, 0, 0, 'c');
  TextBPM :=    AddText(80, 50, 0, 8, 0, 0, 0, 'd');
  TextGAP :=    AddText(80, 70, 0, 8, 0, 0, 0, 'e');

  // note info
  AddText(10, 90,  0, 8, 0, 0, 0, 'Start:');
    AddText(300, 90, 0, 8, 0, 0, 0, 'Duration:');

  AddText(10, 110, 0, 8, 0, 0, 0, 'Tone:');
  AddText(10, 130, 0, 8, 0, 0, 0, 'Text:');
    AddText(300, 130,  0, 8, 0, 0, 0, 'VideoGap:');

  TextNStart :=   AddText(80, 90,  0, 8, 0, 0, 0, 'a');
  TextNDlugosc := AddText(400, 90,  0, 8, 0, 0, 0, 'b');
  TextNTon :=     AddText(80, 110,  0, 8, 0, 0, 0, 'c');
  TextNText :=    AddText(80, 130,  0, 8, 0, 0, 0, 'd');
  TextVideoGap :=  AddText(400, 130,  0, 8, 0, 0, 0, 'e');

  // debug
  TextDebug :=  AddText(30, 575, 0, 9, 0, 0, 0, '');

  EditorLyric[0] := TLyric.Create;
  EditorLyric[1] := TLyric.Create;

  offset[0] := 155;
  offset[1] := 525;
end;

procedure TScreenEditSub.SelectNextNote();
begin
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;

  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  EditorLyric[CP].Selected := 0;
end;

procedure TScreenEditSub.SelectPrevNote();
begin
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;

  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  EditorLyric[CP].Selected := 0;
end;

procedure TScreenEditSub.MakeSingle;
begin
  SetLength(Czesci, 1);
  AktSong.isDuet := false;
  CP := 0;
  Refresh;
  AktNuta[CP] := 0;
  Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;

  EditorLyric[0].Y := offset[0]+300;
  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
end;

procedure TScreenEditSub.MakeDuet;
var
  L, I:  integer;

begin
  SetLength(Czesci, 2);

  Czesci[1].Akt := Czesci[0].Akt;
  Czesci[1].High := Czesci[0].High;
  Czesci[1].Ilosc := Czesci[0].Ilosc;
  Czesci[1].Resolution := Czesci[0].Resolution;
  Czesci[1].NotesGAP := Czesci[0].NotesGAP;
  Czesci[1].Wartosc := 0;
  SetLength(Czesci[1].Czesc, Length(Czesci[0].Czesc));

  for L := 0 to Length(Czesci[0].Czesc) - 1 do
    CopyLine(0, L, 1, L);

  AktSong.isDuet := true;

  AktNuta[1] := 0;
  Czesci[1].Akt := 0;

  for I := 0 to Length(Czesci)-1 do
  begin
    EditorLyric[I].Clear;
    EditorLyric[I].X := 400;
    if not AktSong.isDuet and (I=0) then
      EditorLyric[I].Y := offset[I]+300
    else
      EditorLyric[I].Y := offset[I];

    EditorLyric[I].Align := 1;
    EditorLyric[I].Size := 13;
    EditorLyric[I].ColR := 0;
    EditorLyric[I].ColG := 0;
    EditorLyric[I].ColB := 0;
    EditorLyric[I].ColSR := Skin_FontHighlightR;
    EditorLyric[I].ColSG := Skin_FontHighlightG;
    EditorLyric[I].ColSB := Skin_FontHighlightB;
    EditorLyric[I].Style := 0;
    EditorLyric[I].AddCzesc(I, Czesci[I].Akt);
    EditorLyric[I].Selected := 0;
  end;

  //delete medley
  MedleyNotes.isStart := false;
  MedleyNotes.isEnd := false;
  AktSong.Medley.Source := msNone;
end;

function TScreenEditSub.DuetCopyLine: boolean;
var
  LSrc, LDst: integer;
  CSrc, CDst: integer;

  SrcStart:   integer;
  SrcEnd:     integer;

  DstStart:   integer;
  DstEnd:     integer;

  SrcNumN:    integer;
  DstNumN:    integer;

  I, C:       integer;

  CLen:       integer;
begin
  Result := false;

  CSrc := CP;
  CDst := (CP+1) mod 2;
  LSrc := Czesci[CSrc].Akt;
  LDst := -1;

  SrcStart := Czesci[CSrc].Czesc[LSrc].Nuta[0].Start;
  SrcNumN := Length(Czesci[CSrc].Czesc[LSrc].Nuta);
  SrcEnd := Czesci[CSrc].Czesc[LSrc].Nuta[SrcNumN-1].Start + Czesci[CSrc].Czesc[LSrc].Nuta[SrcNumN-1].Dlugosc;

  for I := 0 to Length(Czesci[CDst].Czesc)-1 do
  begin
    DstStart := Czesci[CDst].Czesc[I].Nuta[0].Start;
    DstNumN := Length(Czesci[CDst].Czesc[I].Nuta);
    DstEnd := Czesci[CDst].Czesc[I].Nuta[DstNumN-1].Start + Czesci[CDst].Czesc[I].Nuta[DstNumN-1].Dlugosc;
    if (DstStart<=SrcStart) and (SrcEnd<=DstEnd) then
    begin
      LDst := I;
      break;
    end;

    if (LDst = -1) and (I<Length(Czesci[CDst].Czesc)-1) then
    begin
      DstStart := DstEnd;
      DstEnd := Czesci[CDst].Czesc[I+1].Nuta[0].Start;
      if (DstStart<SrcStart) and (SrcEnd<DstEnd) then
      begin
        CLen := Length(Czesci[CDst].Czesc);
        SetLength(Czesci[CDst].Czesc, CLen + 1);
        Inc(Czesci[CDst].Ilosc);
        Inc(Czesci[CDst].High);

        for C := CLen-1 downto I do
          CopyLine(CDst, C, CDst, C+1);

        SetLength(Czesci[CDst].Czesc[I+1].Nuta, 0);
        LDst := I+1;
        break;
      end;
    end;
  end;

  if (LDst = -1) then
    Exit;

  CopyLine(CSrc, LSrc, CDst, LDst);

  Refresh;
  EditorLyric[CDst].AddCzesc(CDst, Czesci[CDst].Akt);
  EditorLyric[CDst].Selected := 0;
  AktNuta[CDst] := 0;
  Czesci[CSrc].Czesc[LSrc].Nuta[AktNuta[CSrc]].Color := 2;
  Result := true;
end;

procedure TScreenEditSub.CopyLine(Pf, Cf, Pt, Ct: integer);
var
  N:  integer;
begin
  Czesci[Pt].Czesc[Ct].IlNut := Czesci[Pf].Czesc[Cf].IlNut;
  Czesci[Pt].Czesc[Ct].HighNut := Czesci[Pf].Czesc[Cf].HighNut;
  Czesci[Pt].Czesc[Ct].Koniec := Czesci[Pf].Czesc[Cf].Koniec;
  Czesci[Pt].Czesc[Ct].Start := Czesci[Pf].Czesc[Cf].Start;
  Czesci[Pt].Czesc[Ct].BaseNote := Czesci[Pf].Czesc[Cf].BaseNote;
  Czesci[Pt].Czesc[Ct].StartNote := Czesci[Pf].Czesc[Cf].StartNote;

  SetLength(Czesci[Pt].Czesc[Ct].Nuta, Czesci[Pf].Czesc[Cf].IlNut);
  for N := 0 to Czesci[Pf].Czesc[Cf].HighNut do
    CopyNote(Pf, Cf, N, Pt, Ct, N);
end;

procedure TScreenEditSub.CopyNote(Pf, Cf, Nf, Pt, Ct, Nt: integer);
begin
  Czesci[Pt].Czesc[Ct].Nuta[Nt].Color := 0;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].Start := Czesci[Pf].Czesc[Cf].Nuta[Nf].Start;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].Dlugosc := Czesci[Pf].Czesc[Cf].Nuta[Nf].Dlugosc;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].Ton := Czesci[Pf].Czesc[Cf].Nuta[Nf].Ton;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].TonGamy := Czesci[Pf].Czesc[Cf].Nuta[Nf].TonGamy;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].Tekst := Czesci[Pf].Czesc[Cf].Nuta[Nf].Tekst;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].FreeStyle := Czesci[Pf].Czesc[Cf].Nuta[Nf].FreeStyle;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].Wartosc := Czesci[Pf].Czesc[Cf].Nuta[Nf].Wartosc;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].IsMedley := Czesci[Pf].Czesc[Cf].Nuta[Nf].IsMedley;
  Czesci[Pt].Czesc[Ct].Nuta[Nt].IsStartPreview := Czesci[Pf].Czesc[Cf].Nuta[Nf].IsStartPreview;
end;

procedure TScreenEditSub.DuetMoveLine;
begin
  if DuetCopyLine then
    DeleteSentence;
  {Czesci[CP].Czesc[Czesci[CP].Akt].Lyric := '';
  Czesci[CP].Czesc[Czesci[CP].Akt].LyricWidth := 0;
  Czesci[CP].Czesc[Czesci[CP].Akt].HighNut := -1;
  Czesci[CP].Czesc[Czesci[CP].Akt].IlNut := 0;
  Czesci[CP].Czesc[Czesci[CP].Akt].TotalNotes := 0;
  SetLength(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta, 0);

  EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
  EditorLyric[CP].Selected := -1;

  CP := (CP+1) mod 2;}
  //Refresh;
end;

procedure TScreenEditSub.Refresh;
var
  P:  integer;
  L:  integer;
  N:  integer;

begin
  FixTimings;
  LyricsCorrectSpaces;

  if MedleyNotes.isStart and
    ((High(Czesci[0].Czesc)<MedleyNotes.start.line) or
     (High(Czesci[0].Czesc[MedleyNotes.start.line].Nuta)<MedleyNotes.start.note)) then
    MedleyNotes.isStart := false;

  if MedleyNotes.isEnd and
    ((High(Czesci[0].Czesc)<MedleyNotes.end_.line) or
     (High(Czesci[0].Czesc[MedleyNotes.end_.line].Nuta)<MedleyNotes.end_.note)) then
    MedleyNotes.isEnd := false;

  for P := 0 to Length(Czesci) - 1 do
  begin
    Czesci[P].Ilosc := Length(Czesci[P].Czesc);
    Czesci[P].High := Czesci[P].Ilosc-1;
    Czesci[P].Wartosc := 0;

    for L := 0 to Czesci[P].High do
    begin
      with Czesci[P].Czesc[L] do
      begin
        IlNut := Length(Nuta);
        HighNut := IlNut-1;
        TotalNotes := 0;
        BaseNote := 120;

        if (Length(Nuta)>0) then
        begin
          StartNote := Nuta[0].Start;
          for N := 0 to Length(Czesci[P].Czesc[L].Nuta) - 1 do
          begin
            Nuta[N].Color := 0;
            if (MedleyNotes.isStart and (MedleyNotes.start.CP = P) and (MedleyNotes.start.line = L) and
              (MedleyNotes.start.note = N)) or
              (MedleyNotes.isEnd and (MedleyNotes.end_.CP = P) and (MedleyNotes.end_.line = L) and
              (MedleyNotes.end_.note = N)) then
              Nuta[N].IsMedley := true
            else
              Nuta[N].IsMedley := false;

            Nuta[N].IsStartPreview := false;

            Czesci[P].Wartosc := Czesci[P].Wartosc + Nuta[N].Dlugosc * Nuta[N].Wartosc;
            TotalNotes := TotalNotes + Nuta[N].Dlugosc * Nuta[N].Wartosc;

            if (Nuta[N].Ton < BaseNote) then
              BaseNote := Nuta[N].Ton;
          end;
        end else
          BaseNote := 0;
      end;
    end;
  end;

  //set Preview Start
  MedleyNotes.Preview := FindNote(round(GetMidBeat(AktSong.PreviewStart-AktSong.Gap/1000)));
  Czesci[MedleyNotes.Preview.CP].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].IsStartPreview := true;
  AktSong.PreviewStart :=
    GetTimeFromBeat(Czesci[MedleyNotes.Preview.CP].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].start);
end;

procedure TScreenEditSub.onShow;
var
  I:  integer;

begin
  Log.LogStatus('Initializing', 'TEditScreen.onShow');

  try
    ResetSingTemp;
    AktSong := CatSongs.Song[SongIndex];
    Error := not LoadSong(Path + FileName, SONG_LOAD_COMPLETE);
    if not Error and not AktSong.isDuet then
      FindRefrainStart(AktSong);
  except
    Error := True;
  end;

  if Error then
  begin
    //Error Loading Song -> Go back to Song Screen and Show some Error Message
    FadeTo(@ScreenSong);
    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    Exit;
  end
  else
  begin
    Music.CaptureStart;
    Refresh;
    MidiOut := TMidiOutput.Create(nil);
    MidiOut.Open;

    SetLength(Pitches, 0);
    PitchRecOn := false;

    //Set Volume
    MP3Volume := 50;
    Music.SetMusicVolume(MP3Volume);

    CP := 0;

    if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenEditSub)');

    Text[TextTitle].Text :=   AktSong.Title;
    Text[TextArtist].Text :=  AktSong.Artist;
    //Text[TextMp3].Text :=     AktSong.Mp3;

    Czesci[0].Akt := 0;
    AktNuta[0] := 0;
    AktNuta[1] := 0;
    noteStart := 0; //when playing sentence
    lineStart := 0;
    cpStart := 0;

    if AktSong.isDuet then
    begin
      Czesci[1].Akt := 0;
      SelectNextNote;
    end else
      Czesci[0].Czesc[0].Nuta[0].Color := 2;

    if AktSong.Medley.Source <> msNone then
    begin
      MedleyNotes.isStart := true;
      MedleyNotes.isEnd := true;
      MedleyNotes.start := FindNote(AktSong.Medley.StartBeat);
      MedleyNotes.end_ := FindNote(AktSong.Medley.EndBeat);
      Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].IsMedley := true;
      Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].IsMedley := true;
    end;

    //set Preview Start
    MedleyNotes.Preview := FindNote(round(GetMidBeat(AktSong.PreviewStart-AktSong.Gap/1000)));
    Czesci[MedleyNotes.Preview.CP].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].IsStartPreview := true;
    AktSong.PreviewStart :=
      GetTimeFromBeat(Czesci[MedleyNotes.Preview.CP].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].start);

    Music.Open(Path + AktSong.Mp3);

    for I := 0 to Length(Czesci)-1 do
    begin
      EditorLyric[I].Clear;
      EditorLyric[I].X := 400;
      if not AktSong.isDuet and (I=0) then
        EditorLyric[I].Y := offset[I]+300
      else
        EditorLyric[I].Y := offset[I];

      EditorLyric[I].Align := 1;
      EditorLyric[I].Size := 13;
      EditorLyric[I].ColR := 0;
      EditorLyric[I].ColG := 0;
      EditorLyric[I].ColB := 0;
      EditorLyric[I].ColSR := Skin_FontHighlightR;
      EditorLyric[I].ColSG := Skin_FontHighlightG;
      EditorLyric[I].ColSB := Skin_FontHighlightB;
      EditorLyric[I].Style := 0;
      EditorLyric[I].AddCzesc(I, Czesci[I].Akt);
      EditorLyric[I].Selected := 0;
    end;

    NotesH := 7;
    NotesW := 4;

  end;

//  Interaction := 0;
  TextEditMode := false;
  BPMEditMode := false;

  //MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
  MidiLastNote := 0;
  PlaySentenceMidi := false;
  PlayOneNoteMidi := false;
  Music.Stop;
  LineChanged[0]:=false;
  LineChanged[1]:=false;
  PlaySentence := false;
  PlayOneNote := false;

  StartTry := false;
  PlayTime := 0;
  PlayVideo := false;
end;

function TScreenEditSub.Draw: boolean;
var
  //Min:    integer;
  //Sec:    integer;
  //Tekst:  string;
  Pet:    integer;
  PlayClick:  boolean;
  line, note: integer;
  end_:   boolean;

  Window: TRectCoords;
  Blend:  real;

  beat:   integer;
  last:   integer;
begin
  DrawStatics;
  end_ := false;

  glClearColor(1,1,1,1);

  last := LastClick;

  PlayClick := false;
  if PlaySentenceMidi or PlaySentence then
  begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;
    PlayTime := PlayTime + TimeSkip;
    // click
    if PlaySentence then
    begin
      AktBeat := Floor(GetMidBeat(Music.Position - AktSong.GAP / 1000));
      if Music.Position>PlayStopTime then
        end_ := true
      else
        end_ := false;
    end else
    begin
      AktBeat := Floor(GetMidBeat(MidiPos - AktSong.GAP / 1000));
      if MidiPos>MidiStop then
        end_ := true
      else
        end_ := false;
    end;

    if AktBeat <> last then
    begin
      for beat := LastClick+1 to AktBeat do
      begin
        PlayClick := false;
        for line := 0 to Length(Czesci[CP].Czesc) - 1 do
        begin
          for note := 0 to Length(Czesci[CP].Czesc[line].Nuta) - 1 do
          begin
            //line change
            if (Czesci[CP].Czesc[line].Start = beat) and (line <> Czesci[CP].Akt) and
              not end_ and not PlayOneSentence then
            begin
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
              AktNuta[CP] := 0;
              Czesci[CP].Akt := line;
              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 1;
              EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
              EditorLyric[CP].Selected := AktNuta[CP];
              LineChanged[CP] := true;
            end;

            if (Czesci[CP].Czesc[line].Nuta[note].Start = beat) then
            begin
              if not PlayOneSentence or (line=Czesci[CP].Akt) then
              begin
                LastClick := beat;
                PlayClick := true;
              end;
            end;
          end;
        end;

        if AktSong.isDuet and not PlayOneSentence then
        begin
          for line := 0 to Length(Czesci[(CP+1) mod 2].Czesc) - 1 do
          begin
            for note := 0 to Length(Czesci[(CP+1) mod 2].Czesc[line].Nuta) - 1 do
            begin
              //line change
              if (Czesci[(CP+1) mod 2].Czesc[line].Start = beat) and (line <> Czesci[(CP+1) mod 2].Akt) and not end_ then
              begin
                if(Length(Czesci[(CP+1) mod 2].Czesc[Czesci[(CP+1) mod 2].Akt].Nuta)>0) then
                  Czesci[(CP+1) mod 2].Czesc[Czesci[(CP+1) mod 2].Akt].Nuta[AktNuta[(CP+1) mod 2]].Color := 0;
                AktNuta[(CP+1) mod 2] := 0;
                Czesci[(CP+1) mod 2].Akt := line;
                Czesci[(CP+1) mod 2].Czesc[Czesci[(CP+1) mod 2].Akt].Nuta[AktNuta[(CP+1) mod 2]].Color := 1;
                EditorLyric[(CP+1) mod 2].AddCzesc((CP+1) mod 2, Czesci[(CP+1) mod 2].Akt);
                EditorLyric[(CP+1) mod 2].Selected := AktNuta[(CP+1) mod 2];
                LineChanged[(CP+1) mod 2] := true;
              end;
            end;
          end;
        end;


        // midi music
        if PlaySentenceMidi then
        begin
          // stop the music
          if end_ then
          begin
            MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            if (Czesci[CP].Akt = lineStart) then
              AktNuta[CP] := noteStart;

            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
          end;

          // click
          Text[TextDebug].Text := IntToStr(AktBeat);

          if PlayClick then
          begin
            for Pet := 0 to Czesci[CP].Czesc[Czesci[CP].Akt].HighNut do
              if (Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet].Start = beat) then
            begin
              if Pet > 0 then
                MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet-1].Ton + 60, 127);
              MidiOut.PutShort($91, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet].Ton + 60, 127);
              MidiLastNote := Pet;
            end;
          end;
        end; // if PlaySentenceMidi

        // mp3 music
        if PlaySentence then
        begin
          // stop the music
          if end_ then
          begin
            Music.Stop;
            PlaySentence := false;
            PitchRecOn := false;
            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
            if (Czesci[CP].Akt = lineStart) then
              AktNuta[CP] := noteStart;

            Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
            EditorLyric[CP].Selected := AktNuta[CP];
          end;

          if (Click) and (PlaySentence) then
          begin
            Text[TextDebug].Text := IntToStr(AktBeat);
            if PlayClick then
              Music.PlayClick;
          end; // click
        end;

        // move "cursor"
        if (PlaySentence or PlaySentenceMidi) then
        begin
          for line := 0 to Length(Czesci[CP].Czesc) - 1 do
          begin
            for note := 0 to Length(Czesci[CP].Czesc[line].Nuta) - 1 do
            begin
              //note change
              if (Czesci[CP].Czesc[line].Nuta[note].Start = beat) and
                (((note <> AktNuta[CP]) or LineChanged[CP]) and
                (not PlayOneSentence or (line = Czesci[CP].Akt))) then
              begin
                Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 0;
                if not LineChanged[CP] then
                begin
                  AktNuta[CP] := note;
                  Czesci[CP].Akt := line;
                end else
                  LineChanged[CP] := false;

                Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Color := 2;
                EditorLyric[CP].AddCzesc(CP, Czesci[CP].Akt);
                EditorLyric[CP].Selected := AktNuta[CP];
              end;
            end;
          end;

          if AktSong.isDuet and not PlayOneSentence then
          begin
            for line := 0 to Length(Czesci[(CP+1) mod 2].Czesc) - 1 do
            begin
              for note := 0 to Length(Czesci[(CP+1) mod 2].Czesc[line].Nuta) - 1 do
              begin
                //note change
                if (Czesci[(CP+1) mod 2].Czesc[line].Nuta[note].Start = beat) and
                  ((note <> AktNuta[(CP+1) mod 2]) or LineChanged[(CP+1) mod 2]) then
                begin
                  if(Length(Czesci[(CP+1) mod 2].Czesc[Czesci[(CP+1) mod 2].Akt].Nuta)>0) then
                    Czesci[(CP+1) mod 2].Czesc[Czesci[(CP+1) mod 2].Akt].Nuta[AktNuta[(CP+1) mod 2]].Color := 0;
                  if not LineChanged[(CP+1) mod 2] then
                  begin
                    AktNuta[(CP+1) mod 2] := note;
                    Czesci[(CP+1) mod 2].Akt := line;
                  end else
                    LineChanged[(CP+1) mod 2] := false;

                  Czesci[(CP+1) mod 2].Czesc[Czesci[(CP+1) mod 2].Akt].Nuta[AktNuta[(CP+1) mod 2]].Color := 2;
                  EditorLyric[(CP+1) mod 2].AddCzesc((CP+1) mod 2, Czesci[(CP+1) mod 2].Akt);
                  EditorLyric[(CP+1) mod 2].Selected := AktNuta[(CP+1) mod 2];
                end;
              end;
            end;
          end;
        end; //move "cursor"
      end; //for beat
    end; //AktBeat <> last
  end else
  begin
    LineChanged[0]:=false;
    LineChanged[1]:=false;
    PlayVideo := false;
    PlayOneSentence := false;
  end;

  // mp3 music
  if PlayOneNote then
  begin
    // stop the music
    if (Music.Position > PlayStopTime) then
    begin
      Music.Stop;
      PlayOneNote := false;
    end;

    // click
    if (Click) and (PlaySentence) then
    begin
      AktBeat := Floor(GetMidBeat(Music.Position - AktSong.GAP / 1000));
      Text[TextDebug].Text := IntToStr(AktBeat);
      if AktBeat <> LastClick then
      begin
        for beat := LastClick+1 to AktBeat do
        begin
          for Pet := 0 to Czesci[CP].Czesc[Czesci[CP].Akt].HighNut do
          begin
            if (Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet].Start = beat) then
            begin
              Music.PlayClick;
              LastClick := beat;
            end;
          end;
        end; //for beat
      end;
    end; // click
  end; // if PlayOneNote

  // midi music
  if PlayOneNoteMidi then
  begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;
    // stop the music
    if (MidiPos > MidiStop) then
    begin
      MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[MidiLastNote].Ton + 60, 127);
      PlayOneNoteMidi := false;
    end;

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - AktSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if AktBeat <> LastClick then
    begin
      for beat := LastClick+1 to AktBeat do
      begin
        for Pet := 0 to Czesci[CP].Czesc[Czesci[CP].Akt].HighNut do
        begin
          if (Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet].Start = beat) then
          begin
            LastClick := beat;
            if Pet > 0 then
              MidiOut.PutShort($81, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet-1].Ton + 60, 127);
            MidiOut.PutShort($91, Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[Pet].Ton + 60, 127);
            MidiLastNote := Pet;
          end;
        end;
      end; //for beat
    end;
  end; // if PlayOneNoteMidi

  Text[TextSentence].Text := 'Line: ' + IntToStr(Czesci[CP].Akt + 1) + '/' + IntToStr(Czesci[CP].Ilosc);
  Text[TextNote].Text := 'Note: ' + IntToStr(AktNuta[CP] + 1) + '/' + IntToStr(Czesci[CP].Czesc[Czesci[CP].Akt].IlNut);

  // Song info
  if not BPMEditMode then
    Text[TextBPM].Text := FloatToStr(AktSong.BPM[0].BPM / 4);

  Text[TextGAP].Text := FloatToStr(AktSong.GAP);
  Text[TextVideoGap].Text := FloatToStr(AktSong.VideoGap);

  //Error reading Variables when no Song is loaded
  if not Error and (Length(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta)>AktNuta[CP]) then
  begin
    // Note info
    Text[TextNStart].Text :=    IntToStr(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Start);
    Text[TextNDlugosc].Text :=  IntToStr(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Dlugosc);
    Text[TextNTon].Text :=      IntToStr(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Ton) +
      ' ( ' + GetNoteName(Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Ton) + ' ) ' +
      IntToStr(Czesci[CP].Czesc[Czesci[CP].Akt].BaseNote);
    Text[TextNText].Text :=              Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Tekst;

    //F and G and Medley Mod:
    if Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].FreeStyle then
      Text[TextNTon].Text := Text[TextNTon].Text + ' *F*'
    else if Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].Wartosc = 2 then
      Text[TextNTon].Text := Text[TextNTon].Text + ' *G*';

    if MedleyNotes.isStart and (Czesci[CP].Akt = MedleyNotes.start.line)
      and (AktNuta[0] = MedleyNotes.start.note) then
      Text[TextNTon].Text := Text[TextNTon].Text + ' MedleyStart';
    if MedleyNotes.isEnd and (Czesci[CP].Akt = MedleyNotes.end_.line) and
      (AktNuta[0] = MedleyNotes.end_.note) then
      Text[TextNTon].Text := Text[TextNTon].Text + ' MedleyEnd';

    //preview mod
    if Czesci[CP].Czesc[Czesci[CP].Akt].Nuta[AktNuta[CP]].IsStartPreview then
      Text[TextNTon].Text := Text[TextNTon].Text + ' [PreviewStart]';
  end;

  // Text Edit Mode
  if TextEditMode then
    Text[TextNText].Text := Text[TextNText].Text + '|';

  // draw static menu
  inherited Draw;

  // draw notes
  if not AktSong.isDuet then
    SingDrawNoteLines(5, offset[0]+90, 795, 15, 1)
  else
  begin
    SingDrawNoteLines(5, offset[0]+45, 795, 15, 1);
    SingDrawNoteLines(5, offset[1]-140, 795, 15, 1);
  end;

  //Error Drawing when no Song is loaded
  if not Error then
  begin
    if not AktSong.isDuet then
    begin
      SingDrawBeatDelimeters(5, offset[0]+90, 795, 0);
      EditDrawCzesc(5, offset[0]+195, 795, 0, 15);
    end else
    begin
      SingDrawBeatDelimeters(5, offset[0]+45, 795, 0);
      EditDrawCzesc(5, offset[0]+150, 795, 0, 15);
      SingDrawBeatDelimeters(5, offset[1]-140, 795, 1);
      EditDrawCzesc(5, offset[1]-35, 795, 1, 15);
    end;
  end;

  // draw text
  if not AktSong.isDuet then
  begin
    EditorLyric[0].Draw;
    DrawInfoBar(0, 5, offset[0]+250, 790, 15);
  end else
  begin
    EditorLyric[0].Draw;
    DrawInfoBar(0, 5, offset[0]+185, 790, 15);

    EditorLyric[1].Draw;
    DrawInfoBar(1, 5, offset[1]-160, 790, 15);
  end;

  if (CP=1) then
  begin
    glEnable(GL_BLEND);
    glColor4f(0, 0, 0, 0.3);
    //notes
    glbegin(gl_quads);
      glVertex2f(5,   offset[0]+45);
      glVertex2f(5,   offset[0]+180);
      glVertex2f(795, offset[0]+180);
      glVertex2f(795, offset[0]+45);
    glEnd;
    //lyric
    glbegin(gl_quads);
      glVertex2f(5,   offset[0]+5);
      glVertex2f(5,   offset[0]+35);
      glVertex2f(795, offset[0]+35);
      glVertex2f(795, offset[0]+5);
    glEnd;
    glDisable(GL_BLEND);
  end else if AktSong.isDuet then
  begin
    glEnable(GL_BLEND);
    glColor4f(0, 0, 0, 0.3);
    //notes
    glbegin(gl_quads);
      glVertex2f(5,   offset[1]-140);
      glVertex2f(5,   offset[1]-5);
      glVertex2f(795, offset[1]-5);
      glVertex2f(795, offset[1]-140);
    glEnd;
    //lyric
    glbegin(gl_quads);
      glVertex2f(5,   offset[1]+35);
      glVertex2f(5,   offset[1]+5);
      glVertex2f(795, offset[1]+5);
      glVertex2f(795, offset[1]+35);
    glEnd;
    glDisable(GL_BLEND);
  end;

  if UVideo.VideoOpened and PlayVideo then
  begin
    Czas.Teraz := Czas.Teraz + TimeSkip;
    try
      acGetFrame(Czas.Teraz);

      if VidVis=windowed then
      begin
        Window.Left := 570;
        Window.Right := 790;
        Window.Upper := 10;
        Window.Lower := 145;
        Window.Reflection := false;
        Window.TargetAspect := acoCrop;
        Window.windowed := true;

        SetAspectCorrection(acoCrop);
        Blend := (PlayTime-0.2);
        if Blend<0 then
          Blend := 0
        else if Blend>1 then
          Blend := 1;

        acDrawGLi(ScreenAct, Window, Blend, true);
      end else if VidVis=full then
      begin
        acDrawGL(ScreenAct, true);
      end;

      if (Czas.Teraz>=Czas.Razem) then
      begin
        acClose;
        VidVis := none;
      end;
    except
      //If an Error occurs drawing: prevent Video from being Drawn again and Close Video
      log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
      Log.LogError('Corrupted File: ' + AktSong.Video);
      try
        acClose;
        VidVis := none;
      except

      end;
    end;
  end else
  begin
    DrawPitch(400, 75, 390, 15, AktBeat);
    StartVideoPreview;
  end;
end;

procedure TScreenEditSub.DrawStatics;
var
  x, y, w, h: Integer;

  procedure DrawBorder(x, y, w, h: real);
  begin
    glColor4f(0, 0, 0, 1);
    glLineWidth(2);
    glBegin(GL_LINE_LOOP);
      glVertex2f(x-1, y-1);
      glVertex2f(x+w+1, y-1);
      glVertex2f(x+w+1, y+h+1);
      glVertex2f(x-1, y+h+1);
    glEnd;
  end;
begin
  glDisable(GL_BLEND);

  //bg
  x := 0;
  y := 0;
  w := 800;
  h := 600;
  glColor4f(0.3, 0.5, 0.6, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  // line bg
  if (CP=0) then
    glColor4f(cRB, cGB, cBB, 1)
  else
    glColor4f(cRR, cGR, cBR, 1);
  x := 650;
  y := 570;
  w := 145;
  h := 25;
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;
  DrawBorder(x, y, w, h);


  // note bg
  if (CP=0) then
    glColor4f(cRB, cGB, cBB, 1)
  else
    glColor4f(cRR, cGR, cBR, 1);
  x := 495;
  y := 570;
  w := 145;
  h := 25;
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;
  DrawBorder(x, y, w, h);

  // some borders:

  //info box
  x := 5;
  y := 5;
  w := 790;
  if AktSong.isDuet then
    h := 145
  else
    h := 150;

  glColor4f(0.95, 0.95, 0.95, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;
  DrawBorder(x, y, w, h);

  //notes singer 1
  x := 5;
  if AktSong.isDuet then
    y := 200
  else
    y := 245;

  w := 790;
  h := 135;
  glColor4f(cRB, cGB, cBB, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;
  DrawBorder(x, y, w, h);

  //notes singer 2
  if AktSong.isDuet then
  begin
    x := 5;
    y := 385;
    w := 790;
    h := 135;
    glColor4f(cRR, cGR, cBR, 1);
    glbegin(gl_quads);
      glVertex2f(x, y);
      glVertex2f(x, y+h);
      glVertex2f(x+w, y+h);
      glVertex2f(x+w, y);
    glEnd;
    DrawBorder(x, y, w, h);
  end;

  //lyric singer 1
  x := 5;
  if AktSong.isDuet then
    y := 160
  else
    y := 460;

  w := 790;
  h := 30;
  glColor4f(cRB, cGB, cBB, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;
  DrawBorder(x, y, w, h);

  //lyric singer 2
  if AktSong.isDuet then
  begin
    x := 5;
    y := 530;
    w := 790;
    h := 30;
    glColor4f(cRR, cGR, cBR, 1);
      glbegin(gl_quads);
      glVertex2f(x, y);
      glVertex2f(x, y+h);
      glVertex2f(x+w, y+h);
      glVertex2f(x+w, y);
    glEnd;
    DrawBorder(x, y, w, h);
  end;

  glLineWidth(1);
end;

procedure TScreenEditSub.DrawInfoBar(P, x, y, w, h: integer);
var
  start, end_:  integer;
  start2:       integer;
  ww:           integer;

  pos:          real;
  br:           real;

  line:         integer;
  numLines:     integer;

  function FindStart(): integer;
  var
    I:      integer;
    start:  integer;
  begin
    start := High(integer);

    for I := 0 to Length(Czesci[0].Czesc) - 1 do
    begin
      if (Length(Czesci[0].Czesc[I].Nuta)>0) then
      begin
        if(start > Czesci[0].Czesc[I].Nuta[0].Start) then
          start := Czesci[0].Czesc[I].Nuta[0].Start;
      end;
    end;

    Result := start;

    if not AktSong.isDuet then
      Exit;

    for I := 0 to Length(Czesci[1].Czesc) - 1 do
    begin
      if (Length(Czesci[1].Czesc[I].Nuta)>0) then
      begin
        if(start > Czesci[1].Czesc[I].Nuta[0].Start) then
          start := Czesci[1].Czesc[I].Nuta[0].Start;
      end;
    end;

    Result := start;
  end;

  function FindEnd(): integer;
  var
    I:      integer;
    end_:   integer;
    h:      integer;
  begin
    end_ := Low(integer);

    for I := 0 to Length(Czesci[0].Czesc) - 1 do
    begin
      if (Length(Czesci[0].Czesc[I].Nuta)>0) then
      begin
        h := Length(Czesci[0].Czesc[I].Nuta)-1;
        if(end_ < Czesci[0].Czesc[I].Nuta[h].Start + Czesci[0].Czesc[I].Nuta[h].Dlugosc) then
          end_ := Czesci[0].Czesc[I].Nuta[h].Start + Czesci[0].Czesc[I].Nuta[h].Dlugosc;
      end;
    end;

    Result := end_;

    if not AktSong.isDuet then
      Exit;

    for I := 0 to Length(Czesci[1].Czesc) - 1 do
    begin
      if (Length(Czesci[1].Czesc[I].Nuta)>0) then
      begin
        h := Length(Czesci[1].Czesc[I].Nuta)-1;
        if(end_ < Czesci[1].Czesc[I].Nuta[h].Start + Czesci[1].Czesc[I].Nuta[h].Dlugosc) then
          end_ := Czesci[1].Czesc[I].Nuta[h].Start + Czesci[1].Czesc[I].Nuta[h].Dlugosc;
      end;
    end;

    Result := end_;
  end;
begin
  numLines := Length(Czesci[P].Czesc);

  if(numLines=0) then
    Exit;

  start2 := FindStart;
  end_ := FindEnd;
  ww := end_ - start2;

  glColor4f(0, 0, 0, 1);
  glDisable(GL_BLEND);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  if (P=0) then
    glColor4f(cRB, cGB, cBB, 1)
  else
    glColor4f(cRR, cGR, cBR, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;


  for line := 0 to numLines - 1 do
  begin
    if (line = Czesci[P].Akt) and not (PlaySentence or PlaySentenceMidi) then
      glColor4f(0.4, 0.4, 0, 1)
    else
      glColor4f(1, 0.6, 0, 1);

    if (Length(Czesci[P].Czesc[line].Nuta)>0) then
    begin
      start := Czesci[P].Czesc[line].Nuta[0].Start;
      end_ := Czesci[P].Czesc[line].Nuta[Czesci[P].Czesc[line].HighNut].Start+
        Czesci[P].Czesc[line].Nuta[Czesci[P].Czesc[line].HighNut].Dlugosc;

      pos := (start-start2)/ww*w;
      br := (end_-start)/ww*w;

      glbegin(gl_quads);
        glVertex2f(x+pos, y);
        glVertex2f(x+pos, y+h);
        glVertex2f(x+pos+br, y+h);
        glVertex2f(x+pos+br, y);
      glEnd;
    end;
  end;


  if(PlaySentence or PlaySentenceMidi) then
  begin
    glColor4f(1, 0, 0, 1);
    pos := (AktBeat-start2)/ww*w;
    br := 1;

    glbegin(gl_quads);
      glVertex2f(x+pos, y);
      glVertex2f(x+pos, y+h);
      glVertex2f(x+pos+br, y+h);
      glVertex2f(x+pos+br, y);
    glEnd;

    if (Length(Czesci[P].Czesc[Czesci[P].Akt].Nuta)>0) then
    begin
      start := Czesci[P].Czesc[Czesci[P].Akt].Nuta[0].Start;
      end_ := Czesci[P].Czesc[Czesci[P].Akt].Nuta[Czesci[P].Czesc[Czesci[P].Akt].HighNut].Start+
        Czesci[P].Czesc[Czesci[P].Akt].Nuta[Czesci[P].Czesc[Czesci[P].Akt].HighNut].Dlugosc;

      pos := (start-start2)/ww*w;
      br := (end_-start)/ww*w;

      glColor4f(0, 0, 0, 0.5);

      glEnable(GL_BLEND);
      glbegin(gl_quads);
        glVertex2f(x+pos, y);
        glVertex2f(x+pos, y+h);
        glVertex2f(x+pos+br, y+h);
        glVertex2f(x+pos+br, y);
      glEnd;
      glDisable(GL_BLEND);
    end;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    if (Length(Czesci[P].Czesc[Czesci[P].Akt].Nuta)>0) then
    begin
      pos := (Czesci[P].Czesc[Czesci[P].Akt].Nuta[AktNuta[P]].Start-start2)/ww*w;
      br := Czesci[P].Czesc[Czesci[P].Akt].Nuta[AktNuta[P]].Dlugosc/ww*w;
      if (br<1) then
        br := 1;

      glbegin(gl_quads);
        glVertex2f(x+pos, y);
        glVertex2f(x+pos, y+h);
        glVertex2f(x+pos+br, y+h);
        glVertex2f(x+pos+br, y);
      glEnd;
    end;
  end;
end;

procedure TScreenEditSub.onHide;
begin
  MidiOut.Close;
  MidiOut.Free;
  Music.CaptureStop;
end;

function TScreenEditSub.GetNoteName(Note: Integer): String;
var N1, N2: Integer;
begin
  if (Note > 0) then
  begin
    N1 := Note mod 12;
    N2 := Note div 12;
  end
  else
  begin
    N1 := (Note + (-Trunc(Note/12)+1)*12) mod 12;
    N2 := -1;
  end;



  case N1 of
    0: Result := 'c';
    1: Result := 'c#';
    2: Result := 'd';
    3: Result := 'd#';
    4: Result := 'e';
    5: Result := 'f';
    6: Result := 'f#';
    7: Result := 'g';
    8: Result := 'g#';
    9: Result := 'a';
    10: Result := 'b';
    11: Result := 'h';
  end;

  case N2 of
    0: Result := UpperCase(Result); //Normal Uppercase Note, 1: Normal lowercase Note
    2: Result := Result + '''';     //One Striped
    3: Result := Result + '''''';   //Two Striped
    4: Result := Result + ''''''''; //etc.
    5: Result := Result + '''''''''';
    6: Result := Result + '''''''''''';
    7: Result := Result + '''''''''''''';
  end;
end;

function TScreenEditSub.GetMedleyLength: real;
begin
  if MedleyNotes.isStart and MedleyNotes.isEnd then
  begin
    Result := GetTimeFromBeat(
      Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Start +
      Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Dlugosc) -
      GetTimeFromBeat(Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].Start);
  end else
    Result := 0;
end;

end.