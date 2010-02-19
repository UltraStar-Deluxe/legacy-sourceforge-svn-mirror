unit UScreenPopup;

interface

uses
  UMenu, SDL, UMusic, math, UFiles, SysUtils, UThemes, UHelp, gl, glu;

type
  TScreenPopupCheck = class(TMenu)
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure ShowPopup(msg: String);
      function Draw: boolean; override;
  end;

  TScreenPopupError = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu
    public
      Visible: Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure ShowPopup(msg: String);
      function Draw: boolean; override;
  end;

  TRect = record
    left, right, top, bottom: integer;
  end;

  TLine = record
    fX, fY, tX, tY: integer;
  end;

  TText = record
    X, Y:   integer;
    Style:  integer;
    Size:   real;
    Italic: boolean;
    text:   string;
  end;

  TResLine = record
    Y:      integer;
    H:      integer;
    lines:  array of TLine;
    texts:  array of TText;
  end;

  TScreenPopupHelp = class(TMenu)
    private
      CurMenu: Byte; //Num of the cur. Shown Menu

      TextsGFX:   array of TResLine;
      msg:        TTextResult;
      Rect:       TRect;

      max_high:   real;
      step:       double;
      barH:       double;

      procedure   DrawTable;
      procedure   DrawLine(line, index, Y: integer);
      procedure   DrawText(line, index, Y: integer);
      procedure   DrawScroll(X, Y, W, H: integer; pos, len: double);
    public
      Visible:    Boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onHide; override;
      procedure ShowPopup();
      function Draw: boolean; override;
  end;
var
//  ISelections: Array of String;
  SelectValue: Integer;


implementation

uses Classes, TextGL, UGraphic, UMain, UIni, UTexture, ULanguage, UParty, UPlaylist, UDisplay;

function TScreenPopupCheck.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Display.CheckOK:=False;
          Display.NextScreenWithCheck:=NIL;
          Visible:=False;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          case Interaction of
          0: begin
               //Hack to Finish Singscreen correct on Exit with Q Shortcut
               if (Display.NextScreenWithCheck = NIL) then
               begin
                 if (Display.ActualScreen = @ScreenSing) then
                   ScreenSing.Finish
                 else if (Display.ActualScreen = @ScreenSingModi) then
                   ScreenSingModi.Finish;
               end;

               Display.CheckOK:=True;
             end;
          1: begin
               Display.CheckOK:=False;
               Display.NextScreenWithCheck:=NIL;
             end;
          end;
          Visible:=False;
          Result := false;
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end;
end;

constructor TScreenPopupCheck.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.CheckPopup.Background.Tex);

  AddButton(Theme.CheckPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.CheckPopup.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  AddText(Theme.CheckPopup.TextCheck);

  for I := 0 to High(Theme.CheckPopup.Static) do
    AddStatic(Theme.CheckPopup.Static[I]);

  for I := 0 to High(Theme.CheckPopup.Text) do
    AddText(Theme.CheckPopup.Text[I]);

  Interaction := 0;
end;

function TScreenPopupCheck.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenPopupCheck.onShow;
begin

end;

procedure TScreenPopupCheck.ShowPopup(msg: String);
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

  Text[0].Text := Language.Translate(msg);

  Button[0].Visible := True;
  Button[1].Visible := True;

  Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
  Button[1].Text[0].Text := Language.Translate('SONG_MENU_NO');
end;

// error popup

function TScreenPopupError.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
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
          Visible:=False;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end;
end;

constructor TScreenPopupError.Create;
var
  I:    integer;
begin
  inherited Create;

  AddBackground(Theme.CheckPopup.Background.Tex);

  AddButton(Theme.ErrorPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddText(Theme.ErrorPopup.TextError);

  for I := 0 to High(Theme.ErrorPopup.Static) do
    AddStatic(Theme.ErrorPopup.Static[I]);

  for I := 0 to High(Theme.ErrorPopup.Text) do
    AddText(Theme.ErrorPopup.Text[I]);

  Interaction := 0;
end;

function TScreenPopupError.Draw: boolean;
begin
  inherited Draw;
end;

procedure TScreenPopupError.onShow;
begin

end;

procedure TScreenPopupError.onHide;
var i: integer;
begin
end;

procedure TScreenPopupError.ShowPopup(msg: String);
var i: integer;
begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

{  //dirty hack... Text[0] is invisible for some strange reason
  for i:=1 to high(Text) do
    if i-1 <= high(msg) then
    begin
      Text[i].Visible:=True;
      Text[i].Text := msg[i-1];
    end
    else
    begin
      Text[i].Visible:=False;
    end;}
  Text[0].Text:=msg;

  Button[0].Visible := True;

  Button[0].Text[0].Text := 'OK';
end;

// Help popup

function TScreenPopupHelp.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  pos:  double;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    pos := Help.GetScrollPos();
    case PressedKey of
      SDLK_TAB,
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_DOWN:
        begin
          InteractNext;
          if pos<(1-step) then
            Help.SetScrollPos(pos+step)
          else if pos>0 then
            Help.SetScrollPos(1);
        end;
      SDLK_UP:
        begin
          InteractPrev;
          if pos>step then
            Help.SetScrollPos(pos-step)
          else if pos>0 then
            Help.SetScrollPos(0);
        end;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end;
end;

constructor TScreenPopupHelp.Create;
var
  I:    integer;
begin
  inherited Create;

  AddButton(Theme.HelpPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');
  Button[0].Visible := false;
  Interaction := 0;
end;

function TScreenPopupHelp.Draw: boolean;
var
  msg:  TTextResult;
  I:    integer;
  abs:  real;
begin
  inherited Draw;
  if step<1 then
    abs := 20
  else
    abs := 5;

  //Background:
  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.left-5, Rect.top-5);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.right+abs, Rect.top-5);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.right+abs, Rect.bottom+5);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.left-5, Rect.bottom+5);
  glEnd;
  glDisable(GL_BLEND);
  //glScissor(Rect.left-1, ScreenH-Rect.bottom-1, Rect.right-Rect.left+2, Rect.bottom-Rect.top+2);
  glScissor(round((Rect.left-1)*(ScreenW/Screens)/RenderW+(ScreenW/Screens)*(ScreenAct-1)),
    round((RenderH-Rect.bottom-1)*ScreenH/RenderH),
    round((Rect.right-Rect.left+2)*(ScreenW/Screens)/RenderW),
    round((Rect.bottom-Rect.top+2)*ScreenH/RenderH));
  glEnable(GL_SCISSOR_TEST);
  DrawTable();
  glDisable(GL_SCISSOR_TEST);
  if step<1 then
    DrawScroll(Rect.right+5, Rect.top, 10, Rect.bottom-Rect.top, Help.GetScrollPos(), barH);
end;

procedure TScreenPopupHelp.onShow;
begin

end;

procedure TScreenPopupHelp.onHide;
var i: integer;
begin
end;

procedure TScreenPopupHelp.ShowPopup();
var
  I, J, K:  integer;
  line:     integer;
  SL:       TStringList;
  tempStr:  String;
  KeyEnd:   integer;
  Style:    integer;
  Size:     real;
  Italic:   boolean;
  fieldh:   integer;
  tline:    integer;
  countline:integer;

  procedure AddLine(l, i, fX, fY, tX, tY: integer);
  begin
    TextsGFX[l].lines[i].fX := fX;
    TextsGFX[l].lines[i].fY := fY;
    TextsGFX[l].lines[i].tX := tX;
    TextsGFX[l].lines[i].tY := tY;
  end;

  procedure NewLine(h, lines: integer);
  begin
    inc(line);
    tline := -1;
    SetLength(TextsGFX, line+1);
    TextsGFX[line].H := h;
    TextsGFX[line].Y := TextsGFX[line-1].Y + TextsGFX[line-1].H;
    SetLength(TextsGFX[line].lines, lines);
  end;

  procedure NewText(X, Y: integer);
  begin
    inc(tline);
    SetLength(TextsGFX[line].texts, tline+1);
    TextsGFX[line].texts[tline].X := X;
    TextsGFX[line].texts[tline].Y := Y;
    TextsGFX[line].texts[tline].Style := Style;
    TextsGFX[line].texts[tline].Size := Size;
    TextsGFX[line].texts[tline].Italic := Italic;
  end;

begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

  SetLength(TextsGFX, 0);
  line := 0;
  tline := -1;

  Style := 1;
  Size := 7;
  Italic := false;

  SetFontStyle(Style);
  SetFontSize(Size);
  SetFontItalic(Italic);


  Rect.left := 25;
  Rect.right := 770;
  Rect.top := 25;
  Rect.bottom := 575;

  KeyEnd := round((Rect.right - Rect.left)*0.4);
  fieldh := 22;

  msg := Help.GetHelpStr();

  //Title
  SetLength(TextsGFX, 1);
  TextsGFX[line].H := round(fieldh/4);
  TextsGFX[line].Y := Rect.top;
  SetLength(TextsGFX[line].lines, 3);

  AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y);
  AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
  AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

  NewLine(fieldh*2, 2);
  AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
  AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

  NewText(Rect.left + 5, TextsGFX[line].Y + 2);
  TextsGFX[line].texts[tline].Size:=12;
  TextsGFX[line].texts[tline].text := Language.Translate('MSG_HELP_TITLE') + ': ' + msg.Title;

  NewLine(round(fieldh/4), 3);
  AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y);
  AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
  AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

  //Description
  Style := 1;
  SetFontStyle(Style);
  SL:=TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(msg.Description), SL);
    if SL.Count>0 then
    begin
      NewLine(round(fieldh/4), 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y);
      AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      NewLine(fieldh, 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      NewText(Rect.left + 5, TextsGFX[line].Y + 2);
    end;
    tempStr := '';
    for I := 0 to SL.Count-1 do
    begin
      if glTextWidth(PChar(tempStr + SL[I] + ' ')) <= (Rect.right - Rect.left - 10) then
      begin
        if I<SL.Count-1 then
          tempStr := tempStr + SL[I] + ' '
        else
          tempStr := tempStr + SL[I];
        TextsGFX[line].texts[tline].text := tempStr;
      end else
      begin
        TextsGFX[line].texts[tline].text := tempStr;
        NewLine(fieldh, 2);
        AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

        NewText(Rect.left + 5, TextsGFX[line].Y + 2);

        if I<SL.Count-1 then
          tempStr := SL[I] + ' '
        else
          tempStr := SL[I];
        TextsGFX[line].texts[tline].text := tempStr;
      end;
    end;
  Finally
    SL.Free;
    if Length(msg.Subs)<1 then
    begin
      NewLine(round(fieldh/4), 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y + TextsGFX[line].H , Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end else
    begin
      NewLine(round(fieldh/4), 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end;
  end;

  //Subs
  for K := 0 to Length(msg.Subs) - 1 do
  begin
    //Sub title
    Style := 1;
    SetFontStyle(Style);
    Size := 7;
    SetFontSize(Size);

    tempStr := '';
    NewLine(round(fieldh/2), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    NewLine(round(fieldh), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    NewText(Rect.left+5, TextsGFX[line].Y + 2);
    TextsGFX[line].texts[tline].text := msg.Subs[K].title+':';

    //text
    Style := 1;
    SetFontStyle(Style);
    Size := 7;
    SetFontSize(Size);


    for J := 0 to length(msg.Subs[K].text) - 1 do
    begin
      NewLine(fieldh, 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      SL:=TStringList.Create;
      try
        ExtractStrings([' '], [], PChar(msg.Subs[K].text[J]), SL);

        NewText(Rect.left + 5, TextsGFX[line].Y + 2);
        tempStr := '';
        for I := 0 to SL.Count-1 do
        begin
          if glTextWidth(PChar(tempStr + SL[I] + ' ')) <= (Rect.right - Rect.left - 10) then
          begin
            if I<SL.Count-1 then
              tempStr := tempStr + SL[I] + ' '
            else
              tempStr := tempStr + SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end else
          begin
            TextsGFX[line].texts[tline].text := tempStr;

            NewLine(fieldh, 2);
            AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
            AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

            NewText(Rect.left + 5, TextsGFX[line].Y + 2);

            if I<SL.Count-1 then
              tempStr := SL[I] + ' '
            else
              tempStr := SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end;
        end;
      Finally
        SL.Free;
      end;
    end;

    if K<Length(msg.Subs) - 1 then
    begin
      NewLine(round(fieldh/4), 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end else
    begin
      NewLine(round(fieldh/2), 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y + TextsGFX[line].H , Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end;
  end;

  //Sections
  for K := 0 to Length(msg.Sections) - 1 do
  begin
    //Section title
    Style := 1;
    SetFontStyle(Style);
    Size := 9;
    SetFontSize(Size);

    tempStr := '';
    NewLine(round(fieldh/2), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    NewLine(round(fieldh*1.4), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    tempStr := msg.Sections[K].name;
    NewText(Rect.left + round((Rect.right - Rect.left - 10)/2 - glTextWidth((PChar(tempStr)))/2), TextsGFX[line].Y + 2);
    TextsGFX[line].texts[tline].text := tempStr;

    NewLine(round(fieldh/2), 3);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y + TextsGFX[line].H , Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    Style := 1;
    SetFontStyle(Style);
    Size := 7;
    SetFontSize(Size);
    //keys
    for J := 0 to Length(msg.Sections[K].Keys) - 1 do
    begin
      NewLine(fieldh, 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      countline := 1;
      NewText(Rect.left + 5, TextsGFX[line].Y + 2);
      tempStr := '';
      for I := 0 to Length(msg.Sections[K].Keys[J].Key) - 1 do
      begin
        if glTextWidth(PChar(tempStr + msg.Sections[K].Keys[J].Key[I] + '+')) <= (KeyEnd - Rect.left - 10) then
        begin
          if I<Length(msg.Sections[K].Keys[J].Key)-1 then
            tempStr := tempStr + msg.Sections[K].Keys[J].Key[I] + '+'
          else
            tempStr := tempStr + msg.Sections[K].Keys[J].Key[I];
          TextsGFX[line].texts[tline].text := tempStr;
        end else
        begin
          TextsGFX[line].texts[tline].text := tempStr;
          NewLine(fieldh, 3);
          AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
          AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
          AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

          NewText(Rect.left + 5, TextsGFX[line].Y + 2);

          if I<Length(msg.Sections[K].Keys[J].Key)-1 then
            tempStr := msg.Sections[K].Keys[J].Key[I] + '+'
          else
            tempStr := msg.Sections[K].Keys[J].Key[I];
          TextsGFX[line].texts[tline].text := tempStr;
          inc(countline);
        end;
      end;

      //key-description
      SL:=TStringList.Create;
      try
        ExtractStrings([' '], [], PChar(msg.Sections[K].KeyDescription[J]), SL);
        line := line - countline + 1;
        tline := Length(TextsGFX[line].texts) -1;

        NewText(KeyEnd + 5, TextsGFX[line].Y + 2);
        tempStr := '';
        for I := 0 to SL.Count-1 do
        begin
          if glTextWidth(PChar(tempStr + SL[I] + ' ')) <= (Rect.right - KeyEnd - 10) then
          begin
            if I<SL.Count-1 then
              tempStr := tempStr + SL[I] + ' '
            else
              tempStr := tempStr + SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end else
          begin
            TextsGFX[line].texts[tline].text := tempStr;
            if countline<2 then
            begin
              NewLine(fieldh, 3);
              AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
              AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
              AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
            end else
            begin
              dec(countline);
              inc(line);
              tline := Length(TextsGFX[line].texts) -1;
            end;

            NewText(KeyEnd + 5, TextsGFX[line].Y + 2);

            if I<SL.Count-1 then
              tempStr := SL[I] + ' '
            else
              tempStr := SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end;
        end;
      Finally
        SL.Free;
        line := line + countline -1;
        NewLine(round(fieldh/4), 4);
        AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 3, Rect.left, TextsGFX[line].Y+TextsGFX[line].H, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
      end;
    end;
  end;

  max_high := (TextsGFX[Length(TextsGFX)-1].Y + TextsGFX[Length(TextsGFX)-1].H);

  if max_high=0 then
    max_high:=1.0; //TODO error.log!
  if max_high<=Rect.bottom-Rect.top then
  begin
    barH := 1;
    step := 1;
  end
  else
  begin
    barH := (Rect.bottom-Rect.top)/max_high;
    step := barH/(1/barH);
  end;
end;

procedure TScreenPopupHelp.DrawTable();
var
  I, J:integer;
  maxh:   integer;
  h, offset:      integer;
begin

  maxh := ScreenH+ScreenH-Rect.Bottom;
  h := 0;

  offset := round(Help.GetScrollPos()*(max_high-ScreenH+ScreenH-Rect.Bottom));

  I := 0;
  while (I<Length(TextsGFX)) and (h<maxh) do
  begin
    if (TextsGFX[I].Y >= offset-20) then
    begin
      for J := 0 to Length(TextsGFX[I].lines) - 1 do
        DrawLine(I, J, offset);
      for J := 0 to Length(TextsGFX[I].texts) - 1 do
        DrawText(I, J, offset);

      h := h + TextsGFX[I].H;
    end;
    inc(I);
  end;
end;

procedure TScreenPopupHelp.DrawLine(line, index, Y: integer);
begin
  //glEnable(GL_BLEND);
  glColor4f(1, 1, 1, 1);
  glLineWidth(2);
  glBegin(GL_LINES);
    glVertex2f(TextsGFX[line].lines[index].fX, TextsGFX[line].lines[index].fY - Y);
    glVertex2f(TextsGFX[line].lines[index].tX, TextsGFX[line].lines[index].tY - Y);
  glEnd;
  //glDisable(GL_BLEND);
end;

procedure TScreenPopupHelp.DrawText(line, index, Y: integer);
var
  text: PChar;

begin
  glColor4f(1, 1, 1, 1);
  SetFontStyle(TextsGFX[line].texts[index].Style);
  SetFontItalic(TextsGFX[line].texts[index].Italic);
  SetFontSize(TextsGFX[line].texts[index].Size);
  SetFontPos (TextsGFX[line].texts[index].X, TextsGFX[line].texts[index].Y - Y);
  text := Addr(TextsGFX[line].texts[index].text[1]);
  glPrint(text);
end;

procedure TScreenPopupHelp.DrawScroll(X, Y, W, H: integer; pos, len: double);
var
  fY, tY: double;
begin
  //glEnable(GL_BLEND);
  glColor4f(1, 1, 1, 1);

  glLineWidth(1);
  glBegin(GL_LINE_LOOP);
    glVertex2f(X, Y);
    glVertex2f(X+W, Y);
    glVertex2f(X+W, Y+H);
    glVertex2f(X, Y+H);
  glEnd;

  fY := Y+(H-H*len)*Pos;
  tY := fY+H*len;
  if tY+0.001>=Y+H then
    tY := Y+H;

  glBegin(GL_QUADS);
    glVertex2f(X, fY);
    glVertex2f(X+W, fY);
    glVertex2f(X+W, tY);
    glVertex2f(X, tY);
  glEnd;
  //glDisable(GL_BLEND);
end;

end.
