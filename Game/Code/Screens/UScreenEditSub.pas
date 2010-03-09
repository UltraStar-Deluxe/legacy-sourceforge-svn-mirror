unit UScreenEditSub;

interface

uses UMenu, UVideo, TextGL, UMusic, SDL, SysUtils, UFiles, UTime, USongs, UIni, ULog, USmpeg, UTexture, UMenuText,
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

  TScreenEditSub = class(TMenu)
    private
      AktBeat:      integer;
      //Variable is True if no SOng is loaded
      Error:        Boolean;
      MP3Volume:    Integer;
      
      TextNote:     integer;
      TextSentence: integer;
      TextTitle:    integer;
      TextArtist:   integer;
      TextMp3:      integer;
      TextBPM:      integer;
      TextGAP:      integer;
      TextDebug:    integer;
      TextNStart:   integer;
      TextNDlugosc: integer;
      TextNTon:     integer;
      TextNText:    integer;
      TextVideoGap:integer;
      AktNuta:      integer;

      PlaySentence:     boolean;
      PlaySentenceMidi: boolean;
      PlayOneNote:      boolean;
      PlayOneNoteMidi:  boolean;

      PlayStopTime: real;
      LastClick:    integer;
      Click:        boolean;
      CopySrc:      integer;

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
      LineChanged:  boolean;

      VidVis:       TVidVis; //video visiability
      PlayVideo:    boolean;
      StartTry:     boolean;
      PlayTime:     real;

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
      procedure TransposeNote(Transpose: integer);
      procedure ChangeWholeTone(Tone: integer);
      procedure MoveAllToEnd(Move: integer);
      procedure MoveTextToRight;
      procedure MarkSrc;
      procedure PasteText;
      procedure CopySentence(Src, Dst: integer);
      procedure CopySentences(Src, Dst, Num: integer);
      //Note Name Mod
      function GetNoteName(Note: Integer): String;
      function GetMedleyLength: real; //returns if availible the length of the medley in seconds, else 0
      procedure DrawInfoBar(x, y, w, h: integer);
      procedure DrawStatics;
    public
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

implementation
uses UGraphic, UDraw, UMain, USkins, ULanguage;

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
    case PressedKey of
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_ESCAPE:
        begin
          Music.Close;
          acClose;
          
          FadeTo(@ScreenSong);
        end;

      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_BACKQUOTE:
        begin
          // Increase Note Length (same as Alt + Right)
          Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
          if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
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
          // Increase VideoGAP
          if SDL_ModState = 0 then
            temp := 1;         //10ms
          if SDL_ModState = KMOD_LSHIFT then
            temp := 10;        //100ms
          if SDL_ModState = KMOD_LCTRL then
            temp := 100;       //1000ms

          AktSong.VideoGap := (round(AktSong.VideoGAP*100) + temp)/100;

          if PlayVideo then
            StartVideo;
        end;

      SDLK_7:
        begin
          // Decrease VideoGAP
          if SDL_ModState = 0 then
            temp := -1;        //10ms
          if SDL_ModState = KMOD_LSHIFT then
            temp := -10;       //100ms
          if SDL_ModState = KMOD_LCTRL then
            temp := -100;      //1000ms

          AktSong.VideoGap := (round(AktSong.VideoGAP*100) + temp)/100;

          if PlayVideo then
            StartVideo;
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
          if SDL_ModState = 0 then begin
            // Insert start of sentece
            if AktNuta > 0 then
              DivideSentence;
          end;

          if SDL_ModState = KMOD_LSHIFT then begin
            // Join next sentence with current
            if Czesci[0].Akt < Czesci[0].High  then
              JoinSentence;
          end;

          if SDL_ModState = KMOD_LCTRL then begin
            // divide note
            DivideNote;
          end;

        end;


      SDLK_S:
        begin
          //Medley MOD:
          if (MedleyNotes.isStart and MedleyNotes.isEnd) and
            (MedleyNotes.start.line < MedleyNotes.end_.line) then
          begin
            AktSong.Medley.Source := msTag;
            AktSong.Medley.StartBeat:=Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].Start;
            AktSong.Medley.EndBeat:=Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Start +
              Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Dlugosc;
            AktSong.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;
            AktSong.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
          end else begin
            AktSong.Medley.Source := msNone;
            AktSong.Medley.StartBeat:=0;
            AktSong.Medley.EndBeat:=0;
          end;

          // Save Song
          if SDL_ModState = KMOD_LSHIFT then
          begin
            if (AktSong.Medley.Source = msTag) then
            begin
              ScreenPopupError.ShowPopup(Language.Translate('Medley and Relative is not supported!'));
              Exit;
            end;

            SResult := SaveSong(AktSong, Czesci[0], Path + FileName, true); //save with relative
          end else
            SResult := SaveSong(AktSong, Czesci[0], Path + FileName, false);

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
            ScreenPopupError.ShowPopup(Language.Translate('Medley and Relative is not supported!'));
            Exit;
          end;

          if SDL_ModState = KMOD_LSHIFT then //Medley End Note
          begin
            if MedleyNotes.isEnd then
            begin
              if (Czesci[0].Akt=MedleyNotes.end_.line) and (AktNuta=MedleyNotes.end_.note) then
              begin
                MedleyNotes.isEnd := false;
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsMedley := false;
              end else
              begin
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsMedley := true;
                Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].IsMedley := false;
                MedleyNotes.end_.line := Czesci[0].Akt;
                MedleyNotes.end_.note := AktNuta;
              end;
            end else
            begin
              MedleyNotes.isEnd := true;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsMedley := true;
              MedleyNotes.end_.line := Czesci[0].Akt;
              MedleyNotes.end_.note := AktNuta;
            end;
          end else
          begin        //Medley Start Note
            if MedleyNotes.isStart then
            begin
              if (Czesci[0].Akt=MedleyNotes.start.line) and (AktNuta=MedleyNotes.start.note) then
              begin
                MedleyNotes.isStart := false;
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsMedley := false;
              end else
              begin
                Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsMedley := true;
                Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].IsMedley := false;
                MedleyNotes.start.line := Czesci[0].Akt;
                MedleyNotes.start.note := AktNuta;
              end;
            end else
            begin
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsMedley := true;
              MedleyNotes.isStart := true;
              MedleyNotes.start.line := Czesci[0].Akt;
              MedleyNotes.start.note := AktNuta;
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
            ScreenPopupError.ShowPopup(Language.Translate('Medley and Relative is not supported!'));
            Exit;
          end;

          if (SDL_ModState = KMOD_LSHIFT) and MedleyNotes.IsEnd then //Medley End Note
          begin
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Czesci[0].Akt := MedleyNotes.end_.line;
            AktNuta := MedleyNotes.end_.note;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := AktNuta;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
          end else if MedleyNotes.IsStart then
          begin
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Czesci[0].Akt := MedleyNotes.start.line;
            AktNuta := MedleyNotes.start.note;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := AktNuta;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
          end;

          if (SDL_ModState = KMOD_LALT) then
          begin
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Music.Stop;
            LineChanged:=false;
            R := GetTimeFromBeat(Czesci[0].Czesc[MedleyNotes.start.line].Nuta[MedleyNotes.start.note].Start);
            if R <= Music.Length then begin
              Music.MoveTo(R);

              noteStart := AktNuta;
              lineStart := Czesci[0].Akt;

              PlayStopTime := GetTimeFromBeat(
                Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Start +
                Czesci[0].Czesc[MedleyNotes.end_.line].Nuta[MedleyNotes.end_.note].Dlugosc);
              PlaySentence := true;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
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
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Czesci[0].Akt := MedleyNotes.Preview.line;
            AktNuta := MedleyNotes.Preview.note;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := AktNuta;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
          end else
          begin
            if (Czesci[0].Akt = MedleyNotes.Preview.line) and (AktNuta = MedleyNotes.Preview.note) then //reset ?
            begin

            end else //set
            begin
              Czesci[0].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].IsStartPreview := false;
              MedleyNotes.Preview.line := Czesci[0].Akt;
              MedleyNotes.Preview.note := AktNuta;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsStartPreview := true;
              AktSong.PreviewStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].start);
            end;
          end;
        end;


      SDLK_D:
        begin
          // Divide lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
            CzesciDivide;
        end;

      SDLK_M:
        begin
          // Multiply lengths by 2
          if (SDL_ModState = KMOD_LSHIFT) then
            CzesciMultiply;
        end;

      SDLK_C:
        begin
          // Capitalize letter at the beginning of line
          if SDL_ModState = 0 then
            LyricsCapitalize;

          // Correct spaces
          if SDL_ModState = KMOD_LSHIFT then
            LyricsCorrectSpaces;

          // Copy sentence
          if SDL_ModState = KMOD_LCTRL then
            MarkSrc;
        end;

      SDLK_R:   //reload
        begin
          MidiOut.Close;
          MidiOut.Free;
          Music.Close;
          acClose;

          onShow;
        end;

      SDLK_V:
        begin
          // Paste text
          if SDL_ModState = KMOD_LCTRL then begin
            if Czesci[0].Czesc[Czesci[0].Akt].IlNut >= Czesci[0].Czesc[CopySrc].IlNut then
              PasteText
            else
              beep;
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Czesci[0].Akt);
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
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Czesci[0].Akt);
            CopySentence(CopySrc+1, Czesci[0].Akt+1);
            CopySentence(CopySrc+2, Czesci[0].Akt+2);
            CopySentence(CopySrc+3, Czesci[0].Akt+3);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then begin
            CopySentences(CopySrc, Czesci[0].Akt, 4);
          end;
        end;
      SDLK_5:
        begin
          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then begin
            CopySentence(CopySrc, Czesci[0].Akt);
            CopySentence(CopySrc+1, Czesci[0].Akt+1);
            CopySentence(CopySrc+2, Czesci[0].Akt+2);
            CopySentence(CopySrc+3, Czesci[0].Akt+3);
            CopySentence(CopySrc+4, Czesci[0].Akt+4);
          end;

          if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then begin
            CopySentences(CopySrc, Czesci[0].Akt, 5);
          end;
        end;

      SDLK_T:
        begin
          // Fixes timings between sentences
          FixTimings;
        end;

      SDLK_F4:
        begin
          // Enter Text Edit Mode
          editText := Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst;
          TextEditMode := true;
        end;

      SDLK_F5:
        begin
          // Enter BPM Edit Mode
          Text[TextBPM].Text := Text[TextBPM].Text + '|';
          BPMEditMode := true;
        end;

      SDLK_P:
        begin
          if SDL_ModState = 0 then begin
            // Play Sentence
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Click := true;
            Music.Stop;
            R := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            if R <= Music.Length then begin
              Music.MoveTo(R);
              PlayStopTime := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
              PlaySentence := true;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
            end;
          end;

          if SDL_ModState = KMOD_LSHIFT then begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            MidiTime := USTime.GetTime;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            MidiStop := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec);

            LastClick := -100;
          end;

          if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            MidiStop := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            LastClick := -100;

            PlaySentence := true;
            PlayOneNote := false;
            Click := true;
            Music.Stop;
            Music.MoveTo(GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote)+0{-0.10});
            PlayStopTime := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Koniec)+0;
            Music.Play;
            LastClick := -100;
          end;

          //new: play hole file + LALT
          if SDL_ModState = KMOD_LALT then begin
            // Play Sentence
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false;
            PlayOneNoteMidi := false;
            Click := true;
            Music.Stop;
            R := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            if R <= Music.Length then begin
              Music.MoveTo(R);
              PlayStopTime := Music.Length;
              PlaySentence := true;
              PlayOneNote := false;
              Music.Play;
              LastClick := -100;
            end;
          end;

          if SDL_ModState = KMOD_LSHIFT or KMOD_LALT then begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            MidiStop := Music.Length;

            LastClick := -100;
          end;

          if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT then begin
            PlaySentenceMidi := true;
            PlayOneNoteMidi := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            MidiStop := Music.Length;
            LastClick := -100;

            PlaySentence := true;
            PlayOneNote := false;
            Click := true;
            Music.Stop;
            Music.MoveTo(GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote)+0{-0.10});
            PlayStopTime := Music.Length;
            Music.Play;
            LastClick := -100;
          end;

          if PlaySentenceMidi or PlaySentence then
          begin
            noteStart := AktNuta;
            lineStart := Czesci[0].Akt;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            AktNuta := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
            LineChanged:=false;
            PlayVideo := false;
          end;

        end;

      SDLK_SPACE:
        begin
          //Thx to f1fth_freed0m for his One Note Midi Playback
          if SDL_ModState = KMOD_LSHIFT then begin //Play One Notes Midi [Shift + Space]
            PlaySentenceMidi := false;
            PlayOneNoteMidi := true;
            Music.Stop;
            PlaySentence := false;
            PlayOneNote := false;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
            MidiStop := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
            LastClick := -100;
          end

          else if SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL then begin
            //Play One Notes Midi + MP3 [CTRL + Shift + Space]
            PlaySentenceMidi := false;
            PlayOneNoteMidi := true;
            MidiTime := USTime.GetTime;
            MidiStart := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
            MidiStop := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
            LastClick := -100;

            PlaySentence := false;
            PlayOneNote := true;
            Click := true;
            Music.Stop;
            Music.MoveTo(GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start));
            PlayStopTime := (GetTimeFromBeat(
                             Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start +
                             Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc));
            Music.Play;
            LastClick := -100;
          end

          Else
          begin
            // Play One Notes MP3 [Space]
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
            PlaySentenceMidi := false; // stop midi
            PlayOneNoteMidi := false;
            PlaySentence := false;
            PlayOneNote := true;
            Click := false;
            Music.Stop;

            Music.MoveTo(GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start));
            PlayStopTime := (GetTimeFromBeat(
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start +
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc));
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
          if SDL_ModState = KMOD_LCTRL then begin
            // moves text to right in current sentence
            DeleteNote;
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
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            if (lineStart = Czesci[0].Akt) then
              AktNuta := noteStart;

            Dec(AktNuta);
            if AktNuta = -1 then AktNuta := Czesci[0].Czesc[Czesci[0].Akt].HighNut;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
          end;
          MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // right
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Inc(AktNuta);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].IlNut then AktNuta := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
          end;

          // ctrl + right
          if SDL_ModState = KMOD_LCTRL then begin
            if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc > 1 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
              if AktNuta = 0 then begin
                Inc(Czesci[0].Czesc[Czesci[0].Akt].Start);
                Inc(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
              end;
              FixTimings;
            end;
          end;

          // shift + right
          if SDL_ModState = KMOD_LSHIFT then begin
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
            if AktNuta = 0 then begin
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Start);
              Inc(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            end;
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            FixTimings;
          end;

          // alt + right
          if SDL_ModState = KMOD_LALT then begin
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
              Inc(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            FixTimings;
          end;

          // alt + ctrl + shift + right = move all from cursor to right
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then begin
            MoveAllToEnd(1);
            FixTimings;
          end;
        end;

      SDLK_LEFT:
        begin
          if PlaySentenceMidi or PlaySentence then
          begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            if (lineStart = Czesci[0].Akt) then
              AktNuta := noteStart;

            Inc(AktNuta);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].IlNut then AktNuta := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
          end;
          MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // left
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Dec(AktNuta);
            if AktNuta = -1 then AktNuta := Czesci[0].Czesc[Czesci[0].Akt].HighNut;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
          end;

          // ctrl + left
          if SDL_ModState = KMOD_LCTRL then begin
            Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
            Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
            if AktNuta = 0 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Start);
              Dec(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            end;
            FixTimings;
          end;

          // shift + left
          if SDL_ModState = KMOD_LSHIFT then begin
            Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);

            // resizing sentences
            if AktNuta = 0 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Start);
              Dec(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
            end;

            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            FixTimings;
          end;

          // alt + left
          if SDL_ModState = KMOD_LALT then begin
            if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc > 1 then begin
              Dec(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
              if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].HighNut then
                Dec(Czesci[0].Czesc[Czesci[0].Akt].Koniec);
            end;
            FixTimings;
          end;

          // alt + ctrl + shift + right = move all from cursor to left
          if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then begin
            MoveAllToEnd(-1);
            FixTimings;
          end;
        end;

      SDLK_DOWN:
        begin
          MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // skip to next sentence
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Inc(Czesci[0].Akt);
            AktNuta := 0;
            if Czesci[0].Akt > Czesci[0].High then
              Czesci[0].Akt := 0;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := 0;
          end;

          // decrease tone
          if SDL_ModState = KMOD_LCTRL then begin
            TransposeNote(-1);
          end;

        end;

      SDLK_UP:
        begin
          MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
          PlaySentenceMidi := false;
          PlayOneNoteMidi := false;
          Music.Stop;
          LineChanged:=false;
          PlaySentence := false;
          PlayOneNote := false;

          // skip to previous sentence
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Dec(Czesci[0].Akt);
            AktNuta := 0;
            if Czesci[0].Akt = -1 then
              Czesci[0].Akt := Czesci[0].High;

            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;

            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := 0;
          end;

          // increase tone
          if SDL_ModState = KMOD_LCTRL then begin
            TransposeNote(1);
          end;
        end;

      // Golden Note Patch
      SDLK_G:
        begin
          case Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc of
            0, 1: Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 2;
            2:    Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 1;
          end; // case
          Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Freestyle := False;
        end;

      // Freestyle Note Patch
      SDLK_F:
        begin
           case Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc of
            0:
            begin;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 1;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Freestyle := False;
            end;
            1,2:
            begin;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc := 0;
              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Freestyle := True;
            end;
          end; // case

        end;

      //MP3-Volume Up
      SDLK_PAGEUP:
        begin
          if (MP3Volume<100) then
            MP3Volume := MP3Volume+5;
            Music.SetMusicVolume(MP3Volume);
            Text[TextDebug].Text := 'MP3 Volume: ' + IntToStr(MP3Volume) + '%';
        end;

      //MP3-Volume Down
      SDLK_PAGEDOWN:
        begin
          if (MP3Volume>0) then
            MP3Volume := MP3Volume-5;
            Music.SetMusicVolume(MP3Volume);
            Text[TextDebug].Text := 'MP3 Volume: ' + IntToStr(MP3Volume) + '%';
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
      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst :=
      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst + chr(ScanCode);

      Lyric.ChangeCurText(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst);
      Lyric.AddCzesc(Czesci[0].Akt);
      Exit;
    end;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE:
        begin
          Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst := editText;
          Lyric.AddCzesc(Czesci[0].Akt);
          TextEditMode := false;
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          TextEditMode := false;
        end;
      
      SDLK_BACKSPACE:
        begin
          Delete(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst,
            Length(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst), 1);

          Lyric.ChangeCurText(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst);
          Lyric.AddCzesc(Czesci[0].Akt);
        end;
      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Inc(AktNuta);
            if AktNuta = Czesci[0].Czesc[Czesci[0].Akt].IlNut then AktNuta := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
            editText := Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            Dec(AktNuta);
            if AktNuta = -1 then AktNuta := Czesci[0].Czesc[Czesci[0].Akt].HighNut;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
            Lyric.Selected := AktNuta;
            editText := Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst;
          end;
      end;
    end;
  end;
end;

function TScreenEditSub.ParseInputEditBPM(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  SDL_ModState: Word;
  strBPM:       string;
  temp:         real;

begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

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
          Delete(strBPM, Length(strBPM)-1, 2);
          Text[TextBPM].Text := strBPM + '|';
        end;
    end;
  end;
end;

procedure TScreenEditSub.StartVideo;
var
  R:  real;

begin
  // Play Sentences with Video
  MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
  PlaySentenceMidi := false;
  PlayOneNoteMidi := false;
  Click := false;
  Music.Stop;

  if PlayVideo then
  begin
    Czesci[0].Akt := lineStart;
    AktNuta := noteStart;
  end;
  R := GetTimeFromBeat(Czesci[0].Czesc[Czesci[0].Akt].StartNote);
  if R <= Music.Length then
  begin
    Music.MoveTo(R);
    PlayStopTime := Music.Length;
    PlaySentence := true;
    PlayOneNote := false;
    Music.Play;
    LastClick := -100;
  end;

  noteStart := AktNuta;
  lineStart := Czesci[0].Akt;
  Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
  AktNuta := 0;
  Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
  Lyric.Selected := AktNuta;
  LineChanged:=false;
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
  C:    integer;
  N:    integer;
  f:    real;

begin                    
  f := newBPM/AktSong.BPM[0].BPM;    //z.B. neu/alt => 1/2 = 0.5 => *0.5
  AktSong.BPM[0].BPM := newBPM;

  for C := 0 to Czesci[0].High do
  begin
    Czesci[0].Czesc[C].Start :=    ceil(Czesci[0].Czesc[C].Start *f);
    Czesci[0].Czesc[C].StartNote := ceil(Czesci[0].Czesc[C].StartNote *f);
    for N := 0 to Czesci[0].Czesc[C].HighNut do
    begin
      Czesci[0].Czesc[C].Nuta[N].Start :=   ceil(Czesci[0].Czesc[C].Nuta[N].Start *f);
      Czesci[0].Czesc[C].Nuta[N].Dlugosc := floor(Czesci[0].Czesc[C].Nuta[N].Dlugosc *f);
      if (Czesci[0].Czesc[C].Nuta[N].Dlugosc=0) then
        Czesci[0].Czesc[C].Nuta[N].Dlugosc := 1;
    end; // N (notes)
    Czesci[0].Czesc[C].Koniec :=    Czesci[0].Czesc[C].Nuta[Czesci[0].Czesc[C].HighNut].Start +
      Czesci[0].Czesc[C].Nuta[Czesci[0].Czesc[C].HighNut].Dlugosc;
  end; // C (lines)
end;


procedure TScreenEditSub.CzesciDivide;
var
  C:    integer;
  N:    integer;
begin                    
  AktSong.BPM[0].BPM := AktSong.BPM[0].BPM / 2;
  for C := 0 to Czesci[0].High do begin
    Czesci[0].Czesc[C].Start :=     Czesci[0].Czesc[C].Start div 2;
    Czesci[0].Czesc[C].StartNote := Czesci[0].Czesc[C].StartNote div 2;
    Czesci[0].Czesc[C].Koniec :=    Czesci[0].Czesc[C].Koniec div 2;
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      Czesci[0].Czesc[C].Nuta[N].Start :=   Czesci[0].Czesc[C].Nuta[N].Start div 2;
      Czesci[0].Czesc[C].Nuta[N].Dlugosc := Round(Czesci[0].Czesc[C].Nuta[N].Dlugosc / 2);
    end; // N
  end; // C
end;

procedure TScreenEditSub.CzesciMultiply;
var
  C:    integer;
  N:    integer;
begin
  AktSong.BPM[0].BPM := AktSong.BPM[0].BPM * 2;
  for C := 0 to Czesci[0].High do begin
    Czesci[0].Czesc[C].Start :=     Czesci[0].Czesc[C].Start * 2;
    Czesci[0].Czesc[C].StartNote := Czesci[0].Czesc[C].StartNote * 2;
    Czesci[0].Czesc[C].Koniec :=    Czesci[0].Czesc[C].Koniec * 2;
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      Czesci[0].Czesc[C].Nuta[N].Start :=   Czesci[0].Czesc[C].Nuta[N].Start * 2;
      Czesci[0].Czesc[C].Nuta[N].Dlugosc := Czesci[0].Czesc[C].Nuta[N].Dlugosc * 2;
    end; // N
  end; // C
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  C:    integer;
  N:    integer; // temporary
  S:    string;
begin
  // temporary
{  for C := 0 to Czesci[0].High do
    for N := 0 to Czesci[0].Czesc[C].HighNut do
      Czesci[0].Czesc[C].Nuta[N].Tekst := AnsiLowerCase(Czesci[0].Czesc[C].Nuta[N].Tekst);}

  for C := 0 to Czesci[0].High do begin
    S := AnsiUpperCase(Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 1, 1));
    S := S + Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 2, Length(Czesci[0].Czesc[C].Nuta[0].Tekst)-1);
    Czesci[0].Czesc[C].Nuta[0].Tekst := S;
  end; // C
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  C:    integer;
  N:    integer;
begin
  for C := 0 to Czesci[0].High do begin
    // correct starting spaces in the first word
    while Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 1, 1) = ' ' do
      Czesci[0].Czesc[C].Nuta[0].Tekst := Copy(Czesci[0].Czesc[C].Nuta[0].Tekst, 2, 100);

    // move spaces on the start to the end of the previous note
    for N := 1 to Czesci[0].Czesc[C].HighNut do begin
      while (Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, 1, 1) = ' ') do begin
        Czesci[0].Czesc[C].Nuta[N].Tekst := Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, 2, 100);
        Czesci[0].Czesc[C].Nuta[N-1].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst + ' ';
      end;
    end; // N

    // correct '-'  to '- '
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      if Czesci[0].Czesc[C].Nuta[N].Tekst = '-' then
        Czesci[0].Czesc[C].Nuta[N].Tekst := '- ';
    end; // N

    // add space to the previous note when the current word is '- '
    for N := 1 to Czesci[0].Czesc[C].HighNut do begin
      if Czesci[0].Czesc[C].Nuta[N].Tekst  = '- ' then
        Czesci[0].Czesc[C].Nuta[N-1].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst + ' ';
    end; // N

    // correct too many spaces at the end of note
    for N := 0 to Czesci[0].Czesc[C].HighNut do begin
      while Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, Length(Czesci[0].Czesc[C].Nuta[N].Tekst)-1, 2) = '  ' do
        Czesci[0].Czesc[C].Nuta[N].Tekst := Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, 1, Length(Czesci[0].Czesc[C].Nuta[N].Tekst)-1);
    end; // N

    // and correct if there is no space at the end of sentence
    N := Czesci[0].Czesc[C].HighNut;
    if Copy(Czesci[0].Czesc[C].Nuta[N].Tekst, Length(Czesci[0].Czesc[C].Nuta[N].Tekst), 1) <> ' ' then
      Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[C].Nuta[N].Tekst + ' ';

  end; // C
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.FixTimings;
var
  C:    integer;
  S:    integer;
  Min:  integer;
  Max:  integer;
begin
  for C := 1 to Czesci[0].High do begin
    with Czesci[0].Czesc[C-1] do begin
      Min := Nuta[HighNut].Start + Nuta[HighNut].Dlugosc;
      Max := Czesci[0].Czesc[C].StartNote;
      case (Max - Min) of
        0:    S := Max;
        1:    S := Max;
        2:    S := Max - 1;
        3:    S := Max - 2;
        else
          S := Min + 2;

      end; // case

      Czesci[0].Czesc[C].Start := S;
    end; // with
  end; // for
end;

procedure TScreenEditSub.DivideSentence;
var
  C:      integer;
  CStart: integer;
  CNew:   integer;
  CLen:   integer;
  N:      integer;
  NStart: integer;
  NHigh:  integer;
  NNewL:  integer;
begin
  // increase sentence length by 1
  CLen := Length(Czesci[0].Czesc);
  SetLength(Czesci[0].Czesc, CLen + 1);
  Inc(Czesci[0].Ilosc);
  Inc(Czesci[0].High);

  // move needed sentences to one forward. newly has the copy of divided sentence
  CStart := Czesci[0].Akt;
  for C := CLen-1 downto CStart do
    Czesci[0].Czesc[C+1] := Czesci[0].Czesc[C];

  // clear and set new sentence
  CNew := CStart + 1;
  NStart := AktNuta;
  Czesci[0].Czesc[CNew].Start := Czesci[0].Czesc[CStart].Nuta[NStart].Start;
  Czesci[0].Czesc[CNew].StartNote := Czesci[0].Czesc[CStart].Nuta[NStart].Start;
  Czesci[0].Czesc[CNew].Lyric := '';
  Czesci[0].Czesc[CNew].LyricWidth := 0;
  Czesci[0].Czesc[CNew].Koniec := 0;
  Czesci[0].Czesc[CNew].BaseNote := 0; // 0.5.0: we modify it later in this procedure
  Czesci[0].Czesc[CNew].IlNut := 0;
  Czesci[0].Czesc[CNew].HighNut := -1;
  SetLength(Czesci[0].Czesc[CNew].Nuta, 0);

  // move right notes to new sentences
  NHigh := Czesci[0].Czesc[CStart].HighNut;
  for N := NStart to NHigh do begin
    NNewL := Czesci[0].Czesc[CNew].IlNut;
    SetLength(Czesci[0].Czesc[CNew].Nuta, NNewL + 1);
    Czesci[0].Czesc[CNew].Nuta[NNewL] := Czesci[0].Czesc[CStart].Nuta[N];

    // increase sentence counters
    Inc(Czesci[0].Czesc[CNew].IlNut);
    Inc(Czesci[0].Czesc[CNew].HighNut);
    Czesci[0].Czesc[CNew].Koniec := Czesci[0].Czesc[CNew].Nuta[NNewL].Start +
      Czesci[0].Czesc[CNew].Nuta[NNewL].Dlugosc;
  end;

  // clear old notes and set sentence counters
  Czesci[0].Czesc[CStart].HighNut := NStart - 1;
  Czesci[0].Czesc[CStart].IlNut := Czesci[0].Czesc[CStart].HighNut + 1;
  Czesci[0].Czesc[CStart].Koniec := Czesci[0].Czesc[CStart].Nuta[NStart-1].Start +
    Czesci[0].Czesc[CStart].Nuta[NStart-1].Dlugosc;
  SetLength(Czesci[0].Czesc[CStart].Nuta, Czesci[0].Czesc[CStart].IlNut);

  // 0.5.0: modify BaseNote
  Czesci[0].Czesc[CNew].BaseNote := 120;
  for N := 0 to Czesci[0].Czesc[CNew].IlNut do
    if Czesci[0].Czesc[CNew].Nuta[N].Ton < Czesci[0].Czesc[CNew].BaseNote then
      Czesci[0].Czesc[CNew].BaseNote := Czesci[0].Czesc[CNew].Nuta[N].Ton;

  Czesci[0].Akt := Czesci[0].Akt + 1;
  AktNuta := 0;
  Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
  Lyric.AddCzesc(Czesci[0].Akt);

end;

procedure TScreenEditSub.JoinSentence;
var
  C:      integer;
  N:      integer;
  NStart: integer;
  NDst:   integer;
begin
  C := Czesci[0].Akt;

  // set new sentence
  NStart := Czesci[0].Czesc[C].IlNut;
  Czesci[0].Czesc[C].IlNut := Czesci[0].Czesc[C].IlNut + Czesci[0].Czesc[C+1].IlNut;
  Czesci[0].Czesc[C].HighNut := Czesci[0].Czesc[C].HighNut + Czesci[0].Czesc[C+1].IlNut;
  SetLength(Czesci[0].Czesc[C].Nuta, Czesci[0].Czesc[C].IlNut);

  // move right notes to new sentences
  for N := 0 to Czesci[0].Czesc[C+1].HighNut do begin
    NDst := NStart + N;
    Czesci[0].Czesc[C].Nuta[NDst] := Czesci[0].Czesc[C+1].Nuta[N];
  end;

  // increase sentence counters
  NDst := Czesci[0].Czesc[C].HighNut;
  Czesci[0].Czesc[C].Koniec := Czesci[0].Czesc[C].Nuta[NDst].Start +
    Czesci[0].Czesc[C].Nuta[NDst].Dlugosc;

  // move needed sentences to one backward.
  for C := Czesci[0].Akt + 1 to Czesci[0].High - 1 do
    Czesci[0].Czesc[C] := Czesci[0].Czesc[C+1];

  // increase sentence length by 1
  SetLength(Czesci[0].Czesc, Length(Czesci[0].Czesc) - 1);
  Dec(Czesci[0].Ilosc);
  Dec(Czesci[0].High);
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.DivideNote;
var
  C:    integer;
  N:    integer;
  NLen: integer;
begin
  C := Czesci[0].Akt;

  NLen := Czesci[0].Czesc[C].IlNut + 1;
  SetLength(Czesci[0].Czesc[C].Nuta, NLen);
  Inc(Czesci[0].Czesc[C].HighNut);
  Inc(Czesci[0].Czesc[C].IlNut);

  // we copy all notes including selected one
  for N := Czesci[0].Czesc[C].HighNut downto AktNuta+1 do begin
    Czesci[0].Czesc[C].Nuta[N] := Czesci[0].Czesc[C].Nuta[N-1];
  end;

  // me slightly modify new note
  Czesci[0].Czesc[C].Nuta[AktNuta].Dlugosc := ceil(Czesci[0].Czesc[C].Nuta[AktNuta+1].Dlugosc/2);

  Czesci[0].Czesc[C].Nuta[AktNuta+1].Start := Czesci[0].Czesc[C].Nuta[AktNuta+1].Start +
    Czesci[0].Czesc[C].Nuta[AktNuta].Dlugosc;

  Czesci[0].Czesc[C].Nuta[AktNuta+1].Dlugosc := Czesci[0].Czesc[C].Nuta[AktNuta+1].Dlugosc -
    Czesci[0].Czesc[C].Nuta[AktNuta].Dlugosc;

  if (Czesci[0].Czesc[C].Nuta[AktNuta+1].Dlugosc>0) then
    Czesci[0].Czesc[C].Nuta[AktNuta+1].Tekst := '~ '
  else
    Czesci[0].Czesc[C].Nuta[AktNuta+1].Tekst := ' ';

  Czesci[0].Czesc[C].Nuta[AktNuta].Color := 0;
  Czesci[0].Czesc[C].Nuta[AktNuta+1].Color := 2;

  Inc(AktNuta);
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.DeleteNote;
var
  C:    integer;
  N:    integer;
  NLen: integer;
begin
  C := Czesci[0].Akt;

  //Do Not delete Last Note
  if (Czesci[0].High > 0) OR (Czesci[0].Czesc[C].HighNut > 0) then
  begin

    // we copy all notes from the next to the selected one
    for N := AktNuta+1 to Czesci[0].Czesc[C].HighNut do begin
      Czesci[0].Czesc[C].Nuta[N-1] := Czesci[0].Czesc[C].Nuta[N];
    end;

    NLen := Czesci[0].Czesc[C].IlNut - 1;

    if (NLen > 0) then
    begin
      SetLength(Czesci[0].Czesc[C].Nuta, NLen);
      Dec(Czesci[0].Czesc[C].HighNut);
      Dec(Czesci[0].Czesc[C].IlNut);


      // me slightly modify new note
      if AktNuta > Czesci[0].Czesc[C].HighNut then Dec(AktNuta);
        Czesci[0].Czesc[C].Nuta[AktNuta].Color := 2;
    end
    //Last Note of current Sentence Deleted - > Delete Sentence
    else
    begin
      //Move all Sentences after the current to the Left
      for N := C+1 to Czesci[0].High do
        Czesci[0].Czesc[N-1] := Czesci[0].Czesc[N];

      //Delete Last Sentence
      SetLength(Czesci[0].Czesc, Czesci[0].High);
      Czesci[0].High := High(Czesci[0].Czesc);
      Czesci[0].Ilosc := Length(Czesci[0].Czesc);

      AktNuta := 0;
      if (C > 0) then
        Czesci[0].Akt := C - 1
      else
        Czesci[0].Akt := 0;

      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
    end;
  end;
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.TransposeNote(Transpose: integer);
begin
  Inc(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton, Transpose);
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: integer);
var
  C:  integer;
  N:  integer;
begin
  for C := 0 to Czesci[0].High do begin
    Czesci[0].Czesc[C].BaseNote := Czesci[0].Czesc[C].BaseNote + Tone;
    for N := 0 to Czesci[0].Czesc[C].HighNut do
      Czesci[0].Czesc[C].Nuta[N].Ton := Czesci[0].Czesc[C].Nuta[N].Ton + Tone;
  end;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: integer);
var
  C:    integer;
  N:    integer;
  NStart: integer;
begin
  for C := Czesci[0].Akt to Czesci[0].High do begin
    NStart := 0;
    if C = Czesci[0].Akt then NStart := AktNuta;
    for N := NStart to Czesci[0].Czesc[C].HighNut do begin
      Inc(Czesci[0].Czesc[C].Nuta[N].Start, Move); // move note start

      if N = 0 then begin // fix beginning
        Inc(Czesci[0].Czesc[C].Start, Move);
        Inc(Czesci[0].Czesc[C].StartNote, Move);
      end;

      if N = Czesci[0].Czesc[C].HighNut then // fix ending
        Inc(Czesci[0].Czesc[C].Koniec, Move);

    end; // for
  end; // for
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.MoveTextToRight;
var
  C:      integer;
  N:      integer;
  NHigh:  integer;
begin
{  C := Czesci[0].Akt;

  for N := Czesci[0].Czesc[C].HighNut downto 1 do begin
    Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst;
  end; // for

  Czesci[0].Czesc[C].Nuta[0].Tekst := '- ';}

  C := Czesci[0].Akt;
  NHigh := Czesci[0].Czesc[C].HighNut;

  // last word
  Czesci[0].Czesc[C].Nuta[NHigh].Tekst := Czesci[0].Czesc[C].Nuta[NHigh-1].Tekst + Czesci[0].Czesc[C].Nuta[NHigh].Tekst;

  // other words
  for N := NHigh - 1 downto AktNuta + 1 do begin
    Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[C].Nuta[N-1].Tekst;
  end; // for
  Czesci[0].Czesc[C].Nuta[AktNuta].Tekst := '- ';
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.MarkSrc;
begin
  CopySrc := Czesci[0].Akt;
end;

procedure TScreenEditSub.PasteText;
var
  C:    integer;
  N:    integer;
begin
  C := Czesci[0].Akt;

  for N := 0 to Czesci[0].Czesc[CopySrc].HighNut do
    Czesci[0].Czesc[C].Nuta[N].Tekst := Czesci[0].Czesc[CopySrc].Nuta[N].Tekst;
  Lyric.AddCzesc(Czesci[0].Akt);
end;

procedure TScreenEditSub.CopySentence(Src, Dst: integer);
var
  N:      integer;
  Time1:  integer;
  Time2:  integer;
  TD:  integer;
begin
  Time1 := Czesci[0].Czesc[Src].Nuta[0].Start;
  Time2 := Czesci[0].Czesc[Dst].Nuta[0].Start;
  TD := Time2-Time1;

  SetLength(Czesci[0].Czesc[Dst].Nuta, Czesci[0].Czesc[Src].IlNut);
  Czesci[0].Czesc[Dst].IlNut := Czesci[0].Czesc[Src].IlNut;
  Czesci[0].Czesc[Dst].HighNut := Czesci[0].Czesc[Src].HighNut;
  for N := 0 to Czesci[0].Czesc[Src].HighNut do begin
    Czesci[0].Czesc[Dst].Nuta[N].Tekst := Czesci[0].Czesc[Src].Nuta[N].Tekst;
    Czesci[0].Czesc[Dst].Nuta[N].Dlugosc := Czesci[0].Czesc[Src].Nuta[N].Dlugosc;
    Czesci[0].Czesc[Dst].Nuta[N].Ton := Czesci[0].Czesc[Src].Nuta[N].Ton;
    Czesci[0].Czesc[Dst].Nuta[N].Start := Czesci[0].Czesc[Src].Nuta[N].Start + TD;
  end;
  N := Czesci[0].Czesc[Src].HighNut;
  Czesci[0].Czesc[Dst].Koniec := Czesci[0].Czesc[Dst].Nuta[N].Start + Czesci[0].Czesc[Dst].Nuta[N].Dlugosc;
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

  //Theme:
  //bg

  //AddStatic(0, 0, 800, 600, 0.3, 0.5, 0.6, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');

  // Line
  //AddStatic(20, 5, 200, 40, 0.95, 0.95, 0.95, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');
  AddText(40, 14, 1, 8, 0, 0, 0, 'Line:');
  TextSentence := AddText(110, 14, 1, 8, 0, 0, 0, '0 / 0');

  // Note
  //AddStatic(260, 5, 200, 40, 0.95, 0.95, 0.95, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');
  AddText(282, 14, 1, 8, 0, 0, 0, 'Note:');
  TextNote := AddText(360, 14, 1, 8, 0, 0, 0, '0 / 0');

  // some borders
  {
  AddStatic(18, 53, 764, 240, 0, 0, 0, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');
  AddStatic(20, 55, 760, 236, 0.95, 0.95, 0.95, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');

  AddStatic(18, 303, 764, 139, 0, 0, 0, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');
  AddStatic(20, 305, 760, 135, 0.95, 0.95, 0.95, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');

  AddStatic(18, 498, 764, 44, 0, 0, 0, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');
  AddStatic(20, 500, 760, 40, 0.95, 0.95, 0.95, Skin.GetTextureFileName('ButtonFade'), 'JPG', 'Font Black');
   }

  AddText(30, 65,  0, 8, 0, 0, 0, 'Title:');
  AddText(30, 90,  0, 8, 0, 0, 0, 'Artist:');
  AddText(30, 115, 0, 8, 0, 0, 0, 'Mp3:');
  AddText(30, 140, 0, 8, 0, 0, 0, 'BPM:');
  AddText(30, 165, 0, 8, 0, 0, 0, 'GAP:');

  TextTitle :=  AddText(180, 65,  0, 8, 0, 0, 0, 'a');
  TextArtist := AddText(180, 90,  0, 8, 0, 0, 0, 'b');
  TextMp3 :=    AddText(180, 115, 0, 8, 0, 0, 0, 'c');
  TextBPM :=    AddText(180, 140, 0, 8, 0, 0, 0, 'd');
  TextGAP :=    AddText(180, 165, 0, 8, 0, 0, 0, 'e');

{  AddInteraction(2, TextTitle);
  AddInteraction(2, TextArtist);
  AddInteraction(2, TextMp3);
  AddInteraction(2, TextBPM);
  AddInteraction(2, TextGAP);}

  // note info
  AddText(30, 190,  0, 8, 0, 0, 0, 'Start:');
  AddText(30, 215,  0, 8, 0, 0, 0, 'Duration:');
  AddText(30, 240,  0, 8, 0, 0, 0, 'Tone:');
  AddText(30, 265,  0, 8, 0, 0, 0, 'Text:');      AddText(500, 265,  0, 8, 0, 0, 0, 'VideoGap:');

  TextNStart :=   AddText(180, 190,  0, 8, 0, 0, 0, 'a');
  TextNDlugosc := AddText(180, 215,  0, 8, 0, 0, 0, 'b');
  TextNTon :=     AddText(180, 240,  0, 8, 0, 0, 0, 'c');
  TextNText :=    AddText(180, 265,  0, 8, 0, 0, 0, 'd');

  TextVideoGap :=  AddText(600, 265,  0, 8, 0, 0, 0, 'e');

  // debug
  TextDebug :=  AddText(30, 550, 0, 9, 0, 0, 0, '');
end;

procedure TScreenEditSub.onShow;
begin
  Log.LogStatus('Initializing', 'TEditScreen.onShow');

  try
    ResetSingTemp;
    AktSong := CatSongs.Song[SongIndex];
    Error := not LoadSong(Path + FileName, SONG_LOAD_COMPLETE);
    if not Error then
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
  else begin
    MidiOut := TMidiOutput.Create(nil);
    MidiOut.Open;

    //Set Volume
    MP3Volume := 50;
    Music.SetVolume(MP3Volume);

    if not Help.SetHelpID(ID) then
      Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenEditSub)');

    Text[TextTitle].Text :=   AktSong.Title;
    Text[TextArtist].Text :=  AktSong.Artist;
    Text[TextMp3].Text :=     AktSong.Mp3;

    Czesci[0].Akt := 0;
    AktNuta := 0;
    noteStart := 0; //when playing sentence
    lineStart := 0;
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
    Czesci[0].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].IsStartPreview := true;
    AktSong.PreviewStart := GetTimeFromBeat(Czesci[0].Czesc[MedleyNotes.Preview.line].Nuta[MedleyNotes.Preview.note].start);

    Music.Open(Path + AktSong.Mp3);
    //Set Down Music Volume for Better hearability of Midi Sounds
    //Music.SetVolume(40);
    
    Lyric.Clear;
    Lyric.X := 400;
    Lyric.Y := 500;
    Lyric.Align := 1;
    Lyric.Size := 14;
    Lyric.ColR := 0;
    Lyric.ColG := 0;
    Lyric.ColB := 0;
    Lyric.ColSR := Skin_FontHighlightR;
    Lyric.ColSG := Skin_FontHighlightG;
    Lyric.ColSB := Skin_FontHighlightB;
    Lyric.Style := 0;
    Lyric.AddCzesc(0);
    Lyric.Selected := 0;

    NotesH := 7;
    NotesW := 4;

  end;

//  Interaction := 0;
  TextEditMode := false;
  BPMEditMode := false;

  MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
  PlaySentenceMidi := false;
  PlayOneNoteMidi := false;
  Music.Stop;
  LineChanged:=false;
  PlaySentence := false;
  PlayOneNote := false;

  StartTry := false;
  PlayTime := 0;
  PlayVideo := false;
end;

function TScreenEditSub.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  Pet:    integer;
  PlayClick:  boolean;
  line, note: integer;
  end_:   boolean;

  Window: TRectCoords;
  Blend: real;
begin
  DrawStatics;

  glClearColor(1,1,1,1);

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

    if AktBeat <> LastClick then
    begin
      for line := 0 to Length(Czesci[0].Czesc) - 1 do
      begin
        for note := 0 to Length(Czesci[0].Czesc[line].Nuta) - 1 do
        begin
          //line change
          if (Czesci[0].Czesc[line].Start = AktBeat) and (line <> Czesci[0].Akt) and not end_ then
          begin
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
            AktNuta := 0;
            Czesci[0].Akt := line;
            //Inc(Czesci[0].Akt);
            //if Czesci[0].Akt > Length(Czesci[0].Czesc)-1 then //useful?
            //  Czesci[0].Akt := 0;
            Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 1;
            Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := AktNuta;
            LineChanged := true;
          end;

          if (Czesci[0].Czesc[line].Nuta[note].Start = AktBeat) then
          begin
            LastClick := AktBeat;
            PlayClick := true;
          end;
        end;
      end;
    end;
  end else
  begin
    LineChanged := false;
    PlayVideo := false;
  end;

  // midi music
  if PlaySentenceMidi then begin

    // stop the music
    if end_ then
    begin
      MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
      PlaySentenceMidi := false;
      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
      if (Czesci[0].Akt = lineStart) then
        AktNuta := noteStart;
        
      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
      Lyric.Selected := AktNuta;
    end;

    // click
    Text[TextDebug].Text := IntToStr(AktBeat);

    if PlayClick then
    begin
      for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
        if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = AktBeat) then
        begin
          if Pet > 0 then
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet-1].Ton + 60, 127);
          MidiOut.PutShort($91, Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Ton + 60, 127);
          MidiLastNote := Pet;
        end;
    end;
  end; // if PlaySentenceMidi

  // mp3 music
  if PlaySentence then
  begin
    // stop the music
    if end_ then begin
      Music.Stop;
      PlaySentence := false;
      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
      if (Czesci[0].Akt = lineStart) then
        AktNuta := noteStart;

      Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
      Lyric.Selected := AktNuta;
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
    for line := 0 to Length(Czesci[0].Czesc) - 1 do
    begin
      for note := 0 to Length(Czesci[0].Czesc[line].Nuta) - 1 do
      begin
        //note change
        if (Czesci[0].Czesc[line].Nuta[note].Start = AktBeat) and
          ((note <> AktNuta) or LineChanged) then
        begin
          Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 0;
          if not LineChanged then
          begin
            AktNuta := note;
            Czesci[0].Akt := line;
            //Inc(AktNuta);
            //if AktNuta > Length(Czesci[0].Czesc[Czesci[0].Akt].Nuta)-1 then
            //  Dec(AktNuta);
          end else
            LineChanged := false;

          Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Color := 2;
          Lyric.AddCzesc(Czesci[0].Akt);
            Lyric.Selected := AktNuta;
        end;
      end;
    end;
  end;

  // midi music
  if PlayOneNoteMidi then
  begin
    MidiPos := USTime.GetTime - MidiTime + MidiStart;
    // stop the music
    if (MidiPos > MidiStop) then
    begin
      MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[MidiLastNote].Ton + 60, 127);
      PlayOneNoteMidi := false;
    end;

    // click
    AktBeat := Floor(GetMidBeat(MidiPos - AktSong.GAP / 1000));
    Text[TextDebug].Text := IntToStr(AktBeat);

    if AktBeat <> LastClick then
    begin
      for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
      begin
        if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = AktBeat) then
        begin
          LastClick := AktBeat;
          if Pet > 0 then
            MidiOut.PutShort($81, Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet-1].Ton + 60, 127);
          MidiOut.PutShort($91, Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Ton + 60, 127);
          MidiLastNote := Pet;
        end;
      end;
    end;
  end; // if PlayOneNoteMidi

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
        for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
        begin
          if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = AktBeat) then
          begin
            Music.PlayClick;
            LastClick := AktBeat;
          end;
        end;
      end;
    end; // click
  end; // if PlayOneNote

  Text[TextSentence].Text := IntToStr(Czesci[0].Akt + 1) + ' / ' + IntToStr(Czesci[0].Ilosc);
  Text[TextNote].Text := IntToStr(AktNuta + 1) + ' / ' + IntToStr(Czesci[0].Czesc[Czesci[0].Akt].IlNut);

  // Song info
  if not BPMEditMode then
    Text[TextBPM].Text := FloatToStr(AktSong.BPM[0].BPM / 4);

  Text[TextGAP].Text := FloatToStr(AktSong.GAP);
  Text[TextVideoGap].Text := FloatToStr(AktSong.VideoGap);

  //Error reading Variables when no Song is loaded
  if not Error then
  begin
    // Note info
    Text[TextNStart].Text :=    IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
    Text[TextNDlugosc].Text :=  IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
    Text[TextNTon].Text :=      IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton) + ' ( ' + GetNoteName(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton) + ' )';
    Text[TextNText].Text :=              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst;

    //F and G and Medley Mod:
    if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].FreeStyle then
      Text[TextNTon].Text := Text[TextNTon].Text + ' *F*'
    else if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Wartosc = 2 then
      Text[TextNTon].Text := Text[TextNTon].Text + ' *G*';

    if MedleyNotes.isStart and (Czesci[0].Akt = MedleyNotes.start.line)
      and (AktNuta = MedleyNotes.start.note) then
      Text[TextNTon].Text := Text[TextNTon].Text + ' MedleyStart';
    if MedleyNotes.isEnd and (Czesci[0].Akt = MedleyNotes.end_.line) and
      (AktNuta = MedleyNotes.end_.note) then
      Text[TextNTon].Text := Text[TextNTon].Text + ' MedleyEnd';

    //preview mod
    if Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].IsStartPreview then
      Text[TextNTon].Text := Text[TextNTon].Text + ' [PreviewStart]';
  end;

  // Text Edit Mode
  if TextEditMode then
    Text[TextNText].Text := Text[TextNText].Text + '|';

  // draw static menu
  inherited Draw;

  // draw notes
  SingDrawNoteLines(20, 305, 780, 15);
  //Error Drawing when no Song is loaded
  if not Error then
  begin
    SingDrawBeatDelimeters(40, 305, 760, 0);
    EditDrawCzesc(40, 410, 760, 0, 15);
  end;

  // draw text
  Lyric.Draw;

  DrawInfoBar(20, 460, 760, 20);
  glLineWidth(1); //bad fix...

  if UVideo.VideoOpened and PlayVideo then
  begin
    Czas.Teraz := Czas.Teraz + TimeSkip;
    try
      acGetFrame(Czas.Teraz);

      if VidVis=windowed then
      begin
        Window.Left := 500;
        Window.Right := 770;
        Window.Upper := 65;
        Window.Lower := 250;
        Window.windowed := true;

        {if CoverTime>=Ini.PreviewFading then
        begin
          glColor4f(0, 0, 0, 1);

          glbegin(gl_quads);
            glVertex2f(Window.Left, Window.Upper);
            glVertex2f(Window.Left, Window.Lower);
            glVertex2f(Window.Right, Window.Lower);
            glVertex2f(Window.Right, Window.Upper);
          glEnd;
        end; }
        SetAspectCorrection(acoCrop);
        Blend := (PlayTime-0.2);
        if Blend<0 then
          Blend := 0
        else if Blend>1 then
          Blend := 1;

        acDrawGLi(ScreenAct, Window, Blend);
      end else if VidVis=full then
      begin
        acDrawGL(ScreenAct);
      end;

      //ResetAspectCorrection;

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
    StartVideoPreview;
end;

procedure TScreenEditSub.DrawStatics;
var
  x, y, w, h: Integer;
begin
  //Theme:
  //bg
  glDisable(GL_BLEND);

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

  // Line
  glColor4f(0.95, 0.95, 0.95, 1);
  x := 20;
  y := 5;
  w := 200;
  h := 40;
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  // Note
  x := 260;
  y := 5;
  w := 200;
  h := 40;
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;


  // some borders
  x := 20;
  y := 55;
  w := 760;
  h := 236;
  glColor4f(0.95, 0.95, 0.95, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  x := 20;
  y := 305;
  w := 760;
  h := 135;
  glColor4f(0.95, 0.95, 0.95, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  x := 20;
  y := 500;
  w := 760;
  h := 40;
  glColor4f(0.95, 0.95, 0.95, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;

  glColor4f(0, 0, 0, 1);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  glLineWidth(1);
end;

procedure TScreenEditSub.DrawInfoBar(x, y, w, h: integer);
var
  start, end_:        integer;
  ww:                 integer;

  pos:                real;
  br:                 real;

  line, note:         integer;
  numLines, numNotes: integer;

begin
  numLines := Length(Czesci[0].Czesc);

  if(numLines=0) then
    Exit;

  start := Czesci[0].Czesc[0].Start;
  end_ := Czesci[0].Czesc[numLines-1].Koniec;
  ww := end_ - start;

  glColor4f(0, 0, 0, 1);
  glDisable(GL_BLEND);
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x-1, y-1);
    glVertex2f(x+w+1, y-1);
    glVertex2f(x+w+1, y+h+1);
    glVertex2f(x-1, y+h+1);
  glEnd;

  glColor4f(0.9, 0.9, 0.9, 1);
  glbegin(gl_quads);
   glVertex2f(x, y);
   glVertex2f(x, y+h);
   glVertex2f(x+w, y+h);
   glVertex2f(x+w, y);
  glEnd;


  for line := 0 to numLines - 1 do
  begin
    if (line = Czesci[0].Akt) and not (PlaySentence or PlaySentenceMidi) then
      glColor4f(0.4, 0.4, 0, 1)
    else
      glColor4f(1, 0.6, 0, 1);


    start := Czesci[0].Czesc[line].Nuta[0].Start;
    end_ := Czesci[0].Czesc[line].Nuta[Czesci[0].Czesc[line].HighNut].Start+
      Czesci[0].Czesc[line].Nuta[Czesci[0].Czesc[line].HighNut].Dlugosc;

    pos := start/ww*w;
    br := (end_-start)/ww*w;

    glbegin(gl_quads);
      glVertex2f(x+pos, y);
      glVertex2f(x+pos, y+h);
      glVertex2f(x+pos+br, y+h);
      glVertex2f(x+pos+br, y);
    glEnd;
    {
    numNotes := Length(Czesci[0].Czesc[line].Nuta);

    for note := 0 to numNotes - 1 do
    begin

    end;  }
  end;

  if(PlaySentence or PlaySentenceMidi) then
  begin
    glColor4f(0, 0, 0, 0.5);
    pos := 0;
    br := AktBeat/ww*w;
    if (br>w) then
      br := w;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    pos := Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start/ww*w;
    br := Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc/ww*w;
    if (br<1) then
      br := 1;
  end;

  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glVertex2f(x+pos, y);
    glVertex2f(x+pos, y+h);
    glVertex2f(x+pos+br, y+h);
    glVertex2f(x+pos+br, y);
  glEnd;
  glDisable(GL_BLEND);
end;

procedure TScreenEditSub.onHide;
begin
  MidiOut.Close;
  MidiOut.Free;
  //Music.SetVolume(100);
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
