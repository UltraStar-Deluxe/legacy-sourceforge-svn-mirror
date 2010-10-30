unit TextGL;

interface

uses gl, SDL, UTexture, Classes, ULog;

procedure BuildFont;			                // Build Our Bitmap Font
procedure KillFont;     		                // Delete The Font
function  glTextWidth(text: pchar): real;     // Returns Text Width
procedure glPrintDone(text: pchar; Done: real; ColR, ColG, ColB, Alpha: real);
procedure glPrintLetter(letter: char);
procedure glPrintLetterCut(letter: char; Start, Finish: real);
procedure glPrint(text: pchar);	                  // Custom GL "Print" Routine
procedure glPrintCut(text: pchar; Alpha: real);
procedure SetFontPos(X, Y: real);                     // Sets X And Y
procedure SetFontSize(Size: real);
procedure SetFontStyle(Style: integer); // sets active font style (normal, bold, etc)
procedure SetFontItalic(Enable: boolean); // sets italic type letter (works for all fonts)
procedure SetFontAspectW(Aspect: real);

type
  TChar = record
    id:       integer;
    x:        integer;
    y:        integer;
    width:    integer;
    height:   integer;
    xOffset:  integer;
    yOffset:  integer;
    xAdvance: integer;
    page:     integer;
  end;

  TTextGL = record
    X:        real;
    Y:        real;
    Text:     string;
    Size:     real;
    ColR:     real;
    ColG:     real;
    ColB:     real;
  end;

  TFont = record
    Tex:      array of TTexture;
    Chars:    array of TChar;
    IDs:      array of integer;
    TexSize:  integer;
    Size:     integer;
    Done:     real;
    AspectW:  real;
    AspectH:  real;
    Bold:     boolean;
    Italic:   boolean;
    lineH:    integer;
    base:     integer;
    X, Y:     real;
    W, H:     real;
  end;

const
  SCALE = 28;

var
  base:       GLuint;			                // Base Display List For The Font Set
  Fonts:      array of TFont;
  ActFont:    integer;
  PColR:      real;  // temps for glPrintDone
  PColG:      real;
  PColB:      real;

implementation

uses UMain, Windows, SysUtils, UGraphic, UFiles;

procedure BuildFont;			                // Build Our Bitmap Font
var
  I:          integer;
  FontFiles:  array of string;

  procedure ReadFontFile(num: integer);
  var
    Line:       string;
    Fractal:    string;
    FontFile:   TextFile;
    Position:   word;
    id:         integer;
    idstr:      string;
    len:        integer;
    I:          integer;
    Ident:      string;
    maxID:      integer;

  begin
    maxID := 0;
    AssignFile(FontFile, FontPath + FontFiles[num]);
    Reset(FontFile);

    ReadLn(FontFile, Line);
    While (Length(Line) <> 0) do
    begin
      Position := Pos(' ', Line);
      Ident := Trim(Copy(Line, 1, Position - 1));
      Fractal := Trim(Copy(Line, Position + 1, Length(Line) - Position));

      if (Ident='info') then
      begin
        //face

        //size
        Position := Pos('size=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 5, Length(Fractal) - Position - 4));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].size);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //bold

        //italic

        //charset

        //unicode

        //stretchH

        //smooth

        //aa

        //padding

        //spacing

        //outline
      end

      else if (Ident='common') then
      begin
        //lineHeight
        Position := Pos('lineHeight=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 11, Length(Fractal) - Position - 10));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].lineH);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //base
        Position := Pos('base=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 5, Length(Fractal) - Position - 4));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].base);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //scaleW (TexSize)
        Position := Pos('scaleW=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 7, Length(Fractal) - Position - 6));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].TexSize);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //scaleH
      end

      else if (Ident='page') then
      begin
        len := Length(Fonts[num].Tex);
        SetLength(Fonts[num].Tex, len+1);

        //id
        Position := Pos('id=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 3, Length(Fractal) - Position - 2));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), id);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //file
        Position := Pos('file="', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 6, Length(Fractal) - Position - 5));
        Position := Pos('"', Fractal);
        idstr := Trim(Copy(Fractal, 1, Position - 1));
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        if (num<2) then
          Fonts[num].Tex[len] := Texture.LoadTexture(false, PChar(FontPath + idstr), 'PNG', 'Font', 0)
        else
          Fonts[num].Tex[len] := Texture.LoadTexture(false, PChar(FontPath + idstr), 'PNG', 'Font Outline', 0);
      end

      else if (Ident='chars') then
      begin
      end

      else if (Ident='char') then
      begin
        len := Length(Fonts[num].Chars);
        SetLength(Fonts[num].Chars, len+1);

        //id
        Position := Pos('id=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 3, Length(Fractal) - Position - 2));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].id);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        if (maxID < Fonts[num].Chars[len].id) then
          maxID := Fonts[num].Chars[len].id;

        //x
        Position := Pos('x=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 2, Length(Fractal) - Position - 1));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].x);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //y
        Position := Pos('y=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 2, Length(Fractal) - Position - 1));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].y);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //width
        Position := Pos('width=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 6, Length(Fractal) - Position - 5));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].width);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //height
        Position := Pos('height=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 7, Length(Fractal) - Position - 6));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].height);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //xoffset
        Position := Pos('xoffset=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 8, Length(Fractal) - Position - 7));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].xOffset);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //yoffset
        Position := Pos('yoffset=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 8, Length(Fractal) - Position - 7));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].yOffset);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //xadvance
        Position := Pos('xadvance=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 9, Length(Fractal) - Position - 8));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].xAdvance);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //page
        Position := Pos('page=', Fractal);
        Fractal := Trim(Copy(Fractal, Position + 5, Length(Fractal) - Position - 4));
        Position := Pos(' ', Fractal);
        TryStrtoInt(Trim(Copy(Fractal, 1, Position - 1)), Fonts[num].Chars[len].page);
        Fractal := Trim(Copy(Fractal, Position + 1, Length(Fractal) - Position));

        //chnl
      end

      else if (Ident='kernings') then
      begin
      end

      else if (Ident='kerning') then
      begin
      end;

      if not EOf(FontFile) then
        ReadLn (FontFile, Line)
      else
        break;
    end;

    CloseFile(FontFile);

    SetLength(Fonts[num].IDs, maxID+1);
    for I := 0 to Length(Fonts[num].Chars) - 1 do
    begin
      Fonts[num].IDs[Fonts[num].Chars[I].id] := I;
    end;
  end;

begin
  ActFont := 0;
  SetLength(FontFiles, 5);
  FontFiles[0] := 'Normal.fnt';
  FontFiles[1] := 'Bold.fnt';
  FontFiles[2] := 'FontO.fnt';
  FontFiles[3] := 'FontO2.fnt';
  FontFiles[4] := 'HighResNumbersO.fnt';

  SetLength(Fonts, 5);
  for I := 0 to Length(FontFiles) - 1 do
  begin
    Fonts[I].Done := -1;
    Fonts[I].AspectW := 1.0;
    Fonts[I].AspectH := 1.0;
    Fonts[I].H := 30;
    ReadFontFile(I);
  end;
end;

procedure KillFont;     		                // Delete The Font
begin
//  glDeleteLists(base, 256); 		                // Delete All 96 Characters
end;

function glTextWidth(text: pchar): real;
var
  Letter:       char;
begin
  Result := 0;
  while (length(text) > 0) do
  begin
    Letter := Text[0];
    text := pchar(Copy(text, 2, Length(text)-1));
    Result := Result + Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].xAdvance *
      (Fonts[ActFont].H/30) * Fonts[ActFont].AspectW * SCALE/Fonts[ActFont].lineH;
  end;
end;

procedure glPrintDone(text: pchar; Done: real; ColR, ColG, ColB, Alpha: real);
begin
  Fonts[ActFont].Done := Done;
  PColR := ColR;
  PColG := ColG;
  PColB := ColB;
  glPrintCut(text, Alpha);
  Fonts[ActFont].Done := -1;
end;

procedure glPrintLetter(Letter: char);
var
  XItal:        real; // X shift for italic type letter
  gXO, gYO:     real;
  gX, gY:       real;
  gW, gH:       real;
  tW, tH:       real;
  fW, fH:       real;
begin
  fW := Fonts[ActFont].H/30 * Fonts[ActFont].AspectW * SCALE/Fonts[ActFont].lineH;
  fH := Fonts[ActFont].H/30 * Fonts[ActFont].AspectH * SCALE/Fonts[ActFont].lineH;

  tW := Fonts[ActFont].TexSize;
  tH := Fonts[ActFont].TexSize;

  gX := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].x;
  gY := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].y;
  gW := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].width;
  gH := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].height;
  gXO := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].xOffset * fW;
  gYO := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].yOffset * fH +
    Fonts[ActFont].H/30 * 2.5;

  if Fonts[ActFont].Italic = false then
    XItal := 0
  else
    XItal := fH*gH*0.3;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Fonts[ActFont].Tex[Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].page].TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(gX/tW, gY/tH);
    glVertex2f(Fonts[ActFont].X + gXO + XItal,  Fonts[ActFont].Y + gYO);

    glTexCoord2f((gX+gW)/tW, gY/tH);
    glVertex2f(Fonts[ActFont].X + gW*fW + gXO + XItal,  Fonts[ActFont].Y + gYO);

    glTexCoord2f((gX+gW)/tW, (gY+gH)/tH);
    glVertex2f(Fonts[ActFont].X + gW*fW + gXO,  Fonts[ActFont].Y + gH*fH + gYO);

    glTexCoord2f(gX/tW, (gY+gH)/tH);
    glVertex2f(Fonts[ActFont].X + gXO,  Fonts[ActFont].Y + gH*fH + gYO);
  glEnd;
  Fonts[ActFont].X := Fonts[ActFont].X + Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].xAdvance * fW;
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure glPrintLetterCut(letter: char; Start, Finish: real);
var
  gXO, gYO:     real;
  gX, gY:       real;
  gW, gH:       real;
  tW, tH:       real;
  fW, fH:       real;
  TexR:         real;
  XItal:        real;

begin
  fW := Fonts[ActFont].H/30 * Fonts[ActFont].AspectW * SCALE/Fonts[ActFont].lineH;
  fH := Fonts[ActFont].H/30 * Fonts[ActFont].AspectH * SCALE/Fonts[ActFont].lineH;

  tW := Fonts[ActFont].TexSize;
  tH := Fonts[ActFont].TexSize;

  gX := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].x;
  gY := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].y;
  gW := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].width;
  gH := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].height;
  gXO := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].xOffset * fW;
  gYO := Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].yOffset * fH +
    Fonts[ActFont].H/30 * 2.5;

  TexR := gX + Finish * gW;
  gX := gX + Start * gW;

  if Fonts[ActFont].Italic = false then
    XItal := 0
  else
    XItal := fH*gH*0.3;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, Fonts[ActFont].Tex[Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].page].TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(gX/tW, gY/tH);
    glVertex2f(Fonts[ActFont].X + gXO + XItal,  Fonts[ActFont].Y + gYO);

    glTexCoord2f(TexR/tW, gY/tH);
    glVertex2f(Fonts[ActFont].X + gXO + gW*fW*(Finish-Start) + XItal,  Fonts[ActFont].Y + gYO);

    glTexCoord2f(TexR/tW, (gY+gH)/tH);
    glVertex2f(Fonts[ActFont].X + gXO + gW*fW*(Finish-Start),  Fonts[ActFont].Y + gH*fH + gYO);

    glTexCoord2f(gX/tW, (gY+gH)/tH);
    glVertex2f(Fonts[ActFont].X + gXO,  Fonts[ActFont].Y + gH*fH + gYO);
  glEnd;
	Fonts[ActFont].X := Fonts[ActFont].X + gW*fW*(Finish-Start);
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure glPrint(text: pchar);	                // Custom GL "Print" Routine
var
  Letter:       char;
begin
  if (Text = '') then   			        // If There's No Text
                Exit;					        // Do Nothing

  while (length(text) > 0) do begin
    // cut
    Letter := Text[0];
    Text := pchar(Copy(Text, 2, Length(Text)-1));

    // print
    glPrintLetter(Letter);
  end; // while
end;

procedure glPrintCut(text: pchar; Alpha: real);
var
  Letter:       char;
  PToDo:        real;
  PTotWidth:    real;
  PDoingNow:    real;
  S:            string;
  lastX:		real;
begin
  if (Text = '') then   			        // If There's No Text
                Exit;					        // Do Nothing

  PTotWidth := glTextWidth(Text);
  PToDo := Fonts[ActFont].Done;

  while (length(text) > 0) do begin
    // cut
    Letter := Text[0];
    Text := pchar(Copy(Text, 2, Length(Text)-1));

    // analyze
    S := Letter;
    PDoingNow := glTextWidth(pchar(S)) / PTotWidth;

    // drawing
    if (PToDo > 0) and (PDoingNow <= PToDo) then
      glPrintLetter(Letter);

    if (PToDo > 0) and (PDoingNow > PToDo) then
    begin
	  lastX := Fonts[ActFont].X;
      glPrintLetterCut(Letter, 0, PToDo / PDoingNow);
      glColor4f(PColR, PColG,  PColB, Alpha);
      glPrintLetterCut(Letter, PToDo / PDoingNow, 1);
	  Fonts[ActFont].X := lastX + Fonts[ActFont].Chars[Fonts[ActFont].IDs[Ord(Letter)]].xAdvance *
        (Fonts[ActFont].H/30) * Fonts[ActFont].AspectW * SCALE/Fonts[ActFont].lineH;
    end;

    if (PToDo <= 0) then
      glPrintLetter(Letter);

    PToDo := PToDo - PDoingNow;

  end; // while
end;


procedure SetFontPos(X, Y: real);
begin
  Fonts[ActFont].X := X;
  Fonts[ActFont].Y := Y;
end;

procedure SetFontSize(Size: real);
begin
  Fonts[ActFont].H := 30 * (Size/10);
end;

procedure SetFontStyle(Style: integer);
begin
  ActFont := Style;
end;

procedure SetFontItalic(Enable: boolean);
begin
  Fonts[ActFont].Italic := Enable;
end;

procedure SetFontAspectW(Aspect: real);
begin
  Fonts[ActFont].AspectW := Aspect;
end;

end.