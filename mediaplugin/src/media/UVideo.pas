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

unit UVideo;

// uncomment if you want to see the debug stuff
{.$define DebugFrames}
{.$define VideoBenchmark}
{.$define Info}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

implementation

uses
  SDL,
  SysUtils,
  Math,
  gl,
  glu,
  glext,
  textgl,
  UCommon,
  UConfig,
  ULog,
  UMusic,
  UGraphicClasses,
  UGraphic,
  UPath;

{$DEFINE PIXEL_FMT_BGR}

const
{$IFDEF PIXEL_FMT_BGR}
  PIXEL_FMT_OPENGL = GL_BGR;
  PIXEL_FMT_SIZE   = 3;

  // looks strange on linux:
  //PIXEL_FMT_OPENGL = GL_RGBA;
  //PIXEL_FMT_SIZE   = 4;
{$ELSE}
  // looks strange on linux:
  PIXEL_FMT_OPENGL = GL_RGB;
  PIXEL_FMT_SIZE   = 3;
{$ENDIF}

  ReflectionH = 0.5; //reflection height (50%)

type
  IVideo_FFmpeg = interface (IVideo)
  ['{E640E130-C8C0-4399-AF02-67A3569313AB}']
    function Open(const Decoder: TVideoDecodeStream): boolean;
  end;

  TVideo_FFmpeg = class( TInterfacedObject, IVideo_FFmpeg )
  private
    fDecoder: TVideoDecodeStream;
    fPaused: boolean;     //**< stream paused

    fFrameData: PByteArray;
    fFrameTex: GLuint; //**< OpenGL texture for FrameBuffer
    fTexWidth, fTexHeight: cardinal;

    fScreen:          integer; //actual screen to draw on

    fPosX:    double;
    fPosY:    double;
    fPosZ:    double;
    fWidth:   double;
    fHeight:  double;

    fFrameRange:        TRectCoords;

    fAlpha:             double;
    fReflectionSpacing: double;

    fAspectCorrection: TAspectCorrection;

    fPboEnabled: boolean;
    fPboId:      GLuint;
    
    procedure Reset();

    procedure GetVideoRect(var ScreenRect, TexRect: TRectCoords);
    procedure DrawBorders(ScreenRect: TRectCoords);
    procedure DrawBordersReflected(ScreenRect: TRectCoords; AlphaUpper, AlphaLower: double);

    procedure ShowDebugInfo();

  public
    constructor Create;
    destructor Destroy; override;

    function Open(const Decoder: TVideoDecodeStream): boolean;
    procedure Close;

    procedure Play;
    procedure Pause;
    procedure Stop;

    procedure SetLoop(Enable: boolean);
    function GetLoop(): boolean;

    procedure SetPosition(Time: real);
    function GetPosition: real;

    procedure SetScreen(Screen: integer);
    function GetScreen(): integer;

    procedure SetScreenPosition(X, Y, Z: double);
    procedure GetScreenPosition(var X, Y, Z: double);

    procedure SetWidth(Width: double);
    function GetWidth(): double;

    procedure SetHeight(Height: double);
    function GetHeight(): double;

    {**
     * Sub-image of the video frame to draw.
     * This can be used for zooming or similar purposes.
     *}
    procedure SetFrameRange(Range: TRectCoords);
    function GetFrameRange(): TRectCoords;

    function GetFrameAspect(): real;

    procedure SetAspectCorrection(AspectCorrection: TAspectCorrection);
    function GetAspectCorrection(): TAspectCorrection;

    procedure SetAlpha(Alpha: double);
    function GetAlpha(): double;

    procedure SetReflectionSpacing(Spacing: double);
    function GetReflectionSpacing(): double;

    procedure GetFrame(Time: Extended);
    procedure Draw();
    procedure DrawReflection();
  end;

  TVideoPlayback_FFmpeg = class(TInterfacedObject, IMediaInterface, IVideoPlayback)
  public
    function GetName: String;
    function GetPriority: integer;

    function Init(): boolean;
    function Finalize: boolean;

    function Open(const FileName : IPath): IVideo;
  end;

{*------------------------------------------------------------------------------
 * TVideoPlayback_ffmpeg
 *------------------------------------------------------------------------------}

function  TVideoPlayback_FFmpeg.GetName: String;
begin
  Result := 'OpenGL_VideoPlayback';
end;

function TVideoPlayback_FFmpeg.GetPriority: integer;
begin
  Result := 80;
end;

function TVideoPlayback_FFmpeg.Init(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_FFmpeg.Finalize(): boolean;
begin
  Result := true;
end;

function TVideoPlayback_FFmpeg.Open(const FileName : IPath): IVideo;
var
  Video: IVideo_FFmpeg;
  Decoder: TVideoDecodeStream;
begin
  Result := nil;

  Decoder := VideoDecoder.Open(FileName);
  if (Decoder = nil) then
    Exit;

  Video := TVideo_FFmpeg.Create();
  if (not Video.Open(Decoder)) then
  begin
    Decoder.Free;
    Exit;
  end;

  Result := Video
end;


{* TVideo_FFmpeg *}

constructor TVideo_FFmpeg.Create;
begin
  glGenTextures(1, PGLuint(@fFrameTex));
  Reset();
end;

destructor TVideo_FFmpeg.Destroy;
begin
  Close();
  glDeleteTextures(1, PGLuint(@fFrameTex));
end;

function TVideo_FFmpeg.Open(const Decoder: TVideoDecodeStream): boolean;
var
  glErr: GLenum;
begin
  Reset();

  fDecoder := Decoder;
  fTexWidth   := Round(Power(2, Ceil(Log2(Decoder.GetFrameWidth()))));
  fTexHeight  := Round(Power(2, Ceil(Log2(Decoder.GetFrameHeight()))));

  fPboEnabled := PboSupported;
  if (fPboEnabled) then
  begin
    glGetError();

    glGenBuffersARB(1, @fPboId);
    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, fPboId);
    glBufferDataARB(
        GL_PIXEL_UNPACK_BUFFER_ARB,
        Decoder.GetFrameWidth() * Decoder.GetFrameHeight() * PIXEL_FMT_SIZE,
        nil,
        GL_STREAM_DRAW_ARB);
    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);

    glErr := glGetError();
    if (glErr <> GL_NO_ERROR) then
    begin
      fPboEnabled := false;
      Log.LogError('PBO initialization failed: ' + gluErrorString(glErr), 'TVideo_FFmpeg.Open');
    end;
  end;

  // we retrieve a texture just once with glTexImage2D and update it with glTexSubImage2D later.
  // Benefits: glTexSubImage2D is faster and supports non-power-of-two widths/height.
  glBindTexture(GL_TEXTURE_2D, fFrameTex);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, fTexWidth, fTexHeight, 0,
      PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  Result := true;
end;

procedure TVideo_FFmpeg.Reset();
begin
  // close previously opened video
  Close();

  fPaused := False;

  fPboId := 0;

  fAspectCorrection := acoCrop;

  fScreen := 1;

  fPosX := 0;
  fPosY := 0;
  fPosZ := 0;
  fWidth := RenderW;
  fHeight := RenderH;

  fFrameRange.Left := 0;
  fFrameRange.Right := 1;
  fFrameRange.Upper := 0;
  fFrameRange.Lower := 1;

  fAlpha := 1;
  fReflectionSpacing := 0;
end;


procedure TVideo_FFmpeg.Close;
begin
  if (fDecoder <> nil) then
  begin
    fDecoder.close();
    FreeAndNil(fDecoder);
  end;

  if (fPboId <> 0) then
    glDeleteBuffersARB(1, @fPboId);
end;

procedure TVideo_FFmpeg.GetFrame(Time: Extended);
var
  glErr: GLenum;
  BufferPtr: PGLvoid;
begin
  if (fDecoder = nil) then
    Exit;

  if fPaused then
    Exit;

  fFrameData := fDecoder.GetFrame(Time);
  if (fFrameData = nil) then
    Exit;

  // TODO: data is not padded, so we will need to tell OpenGL.
  //   Or should we add padding with avpicture_fill? (check which one is faster)
  //glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  // glTexEnvi with GL_REPLACE might give a small speed improvement
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

  if (not fPboEnabled) then
  begin
    glBindTexture(GL_TEXTURE_2D, fFrameTex);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
        fDecoder.GetFrameWidth(), fDecoder.GetFrameHeight(),
        PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, fFrameData);
  end
  else // fPboEnabled
  begin
    glGetError();

    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, fPboId);
    glBufferDataARB(GL_PIXEL_UNPACK_BUFFER_ARB,
        fDecoder.GetFrameHeight() * fDecoder.GetFrameWidth() * PIXEL_FMT_SIZE,
        nil,
        GL_STREAM_DRAW_ARB);

    bufferPtr := glMapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, GL_WRITE_ONLY_ARB);
    if(bufferPtr <> nil) then
    begin
      Move(fFrameData[0], bufferPtr^,
           fDecoder.GetFrameHeight() * fDecoder.GetFrameWidth() * PIXEL_FMT_SIZE);

      // release pointer to mapping buffer
      glUnmapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB);
    end;

    glBindTexture(GL_TEXTURE_2D, fFrameTex);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
        fDecoder.GetFrameWidth(), fDecoder.GetFrameHeight(),
        PIXEL_FMT_OPENGL, GL_UNSIGNED_BYTE, nil);

    glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glErr := glGetError();
    if (glErr <> GL_NO_ERROR) then
      Log.LogError('PBO texture stream error: ' + gluErrorString(glErr), 'TVideo_FFmpeg.GetFrame');
  end;

  // reset to default
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  {$ifdef DebugFrames}
  //frame decode debug display
  GoldenRec.Spawn(200, 35, 1, 16, 0, -1, ColoredStar, $ffff00);
  {$endif}

  {$IFDEF VideoBenchmark}
  Log.BenchmarkEnd(16);
  Log.LogBenchmark('FFmpeg', 15);
  Log.LogBenchmark('Texture', 16);
  {$ENDIF}
end;

procedure TVideo_FFmpeg.GetVideoRect(var ScreenRect, TexRect: TRectCoords);
var
  ScreenAspect: double;  // aspect of screen resolution
  ScaledVideoWidth, ScaledVideoHeight: double;
  FrameAspect: real;
begin
  // Three aspects to take into account:
  //  1. Screen/display resolution (e.g. 1920x1080 -> 16:9)
  //  2. Render aspect (fWidth x fHeight -> variable)
  //  3. Movie aspect (video frame aspect stored in fAspect)
  ScreenAspect := fWidth*((ScreenW/Screens)/RenderW)/(fHeight*(ScreenH/RenderH));
  FrameAspect := fDecoder.GetFrameAspect();

  case fAspectCorrection of
    acoStretch: begin
      ScaledVideoWidth  := fWidth;
      ScaledVideoHeight := fHeight;
    end;

    acoCrop: begin
      if (ScreenAspect >= FrameAspect) then
      begin
        ScaledVideoWidth  := fWidth;
        ScaledVideoHeight := fHeight * ScreenAspect/FrameAspect;
      end else
      begin
        ScaledVideoHeight := fHeight;
        ScaledVideoWidth  := fWidth * FrameAspect/ScreenAspect;
      end;
    end;

    acoLetterBox: begin
      if (ScreenAspect <= FrameAspect) then
      begin
        ScaledVideoWidth  := fWidth;
        ScaledVideoHeight := fHeight * ScreenAspect/FrameAspect;
      end else
      begin
        ScaledVideoHeight := fHeight;
        ScaledVideoWidth  := fWidth * FrameAspect/ScreenAspect;
      end;
    end else
      raise Exception.Create('Unhandled aspect correction!');
  end;

  //center video
  ScreenRect.Left  := (fWidth - ScaledVideoWidth) / 2 + fPosX;
  ScreenRect.Right := ScreenRect.Left + ScaledVideoWidth;
  ScreenRect.Upper := (fHeight - ScaledVideoHeight) / 2 + fPosY;
  ScreenRect.Lower := ScreenRect.Upper + ScaledVideoHeight;

  // texture contains right/lower (power-of-2) padding.
  // Determine the texture coords of the video frame.
  TexRect.Left  := (fDecoder.GetFrameWidth() / fTexWidth) * fFrameRange.Left;
  TexRect.Right := (fDecoder.GetFrameWidth() / fTexWidth) * fFrameRange.Right;
  TexRect.Upper := (fDecoder.GetFrameHeight() / fTexHeight) * fFrameRange.Upper;
  TexRect.Lower := (fDecoder.GetFrameHeight() / fTexHeight) * fFrameRange.Lower;
end;

procedure TVideo_FFmpeg.DrawBorders(ScreenRect: TRectCoords);
  procedure DrawRect(left, right, upper, lower: double);
  begin
    glColor4f(0, 0, 0, fAlpha);
    glBegin(GL_QUADS);
      glVertex3f(left, upper, fPosZ);
      glVertex3f(right, upper, fPosZ);
      glVertex3f(right, lower, fPosZ);
      glVertex3f(left, lower, fPosZ);
    glEnd;
  end;
begin
  //upper border
  if(ScreenRect.Upper > fPosY) then
    DrawRect(fPosX, fPosX+fWidth, fPosY, ScreenRect.Upper);

  //lower border
  if(ScreenRect.Lower < fPosY+fHeight) then
    DrawRect(fPosX, fPosX+fWidth, ScreenRect.Lower, fPosY+fHeight);

  //left border
  if(ScreenRect.Left > fPosX) then
    DrawRect(fPosX, ScreenRect.Left, fPosY, fPosY+fHeight);

  //right border
  if(ScreenRect.Right < fPosX+fWidth) then
    DrawRect(ScreenRect.Right, fPosX+fWidth, fPosY, fPosY+fHeight);
end;

procedure TVideo_FFmpeg.DrawBordersReflected(ScreenRect: TRectCoords; AlphaUpper, AlphaLower: double);
var
  rPosUpper, rPosLower: double;

  procedure DrawRect(left, right, upper, lower: double);
  var
    AlphaTop: double;
    AlphaBottom: double;

  begin
    AlphaTop := AlphaUpper+(AlphaLower-AlphaUpper)*(upper-rPosUpper)/(fHeight*ReflectionH);
    AlphaBottom := AlphaLower+(AlphaUpper-AlphaLower)*(rPosLower-lower)/(fHeight*ReflectionH);

    glBegin(GL_QUADS);
      glColor4f(0, 0, 0, AlphaTop);
      glVertex3f(left, upper, fPosZ);
      glVertex3f(right, upper, fPosZ);

      glColor4f(0, 0, 0, AlphaBottom);
      glVertex3f(right, lower, fPosZ);
      glVertex3f(left, lower, fPosZ);
    glEnd;
  end;
begin
  rPosUpper := fPosY+fHeight+fReflectionSpacing;
  rPosLower := rPosUpper+fHeight*ReflectionH;

  //upper border
  if(ScreenRect.Upper > rPosUpper) then
    DrawRect(fPosX, fPosX+fWidth, rPosUpper, ScreenRect.Upper);

  //lower border
  if(ScreenRect.Lower < rPosLower) then
    DrawRect(fPosX, fPosX+fWidth, ScreenRect.Lower, rPosLower);

  //left border
  if(ScreenRect.Left > fPosX) then
    DrawRect(fPosX, ScreenRect.Left, rPosUpper, rPosLower);

  //right border
  if(ScreenRect.Right < fPosX+fWidth) then
    DrawRect(ScreenRect.Right, fPosX+fWidth, rPosUpper, rPosLower);
end;


procedure TVideo_FFmpeg.Draw();
var
  ScreenRect:   TRectCoords;
  TexRect:      TRectCoords;
  HeightFactor: double;
  WidthFactor:  double;

begin
  // exit if there's nothing to draw
  if (fDecoder = nil) then
    Exit;

  {$IFDEF VideoBenchmark}
  Log.BenchmarkStart(15);
  {$ENDIF}

  // get texture and screen positions
  GetVideoRect(ScreenRect, TexRect);

  WidthFactor := (ScreenW/Screens) / RenderW;
  HeightFactor := ScreenH / RenderH;

  glScissor(
    round(fPosX*WidthFactor + (ScreenW/Screens)*(fScreen-1)),
    round((RenderH-fPosY-fHeight)*HeightFactor),
    round(fWidth*WidthFactor),
    round(fHeight*HeightFactor)
    );

  glEnable(GL_SCISSOR_TEST);
  glEnable(GL_BLEND);
  glDepthRange(0, 10);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_DEPTH_TEST);

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fFrameTex);
  glColor4f(1, 1, 1, fAlpha);
  glBegin(GL_QUADS);
    // upper-left coord
    glTexCoord2f(TexRect.Left, TexRect.Upper);
    glVertex3f(ScreenRect.Left, ScreenRect.Upper, fPosZ);
    // lower-left coord
    glTexCoord2f(TexRect.Left, TexRect.Lower);
    glVertex3f(ScreenRect.Left, ScreenRect.Lower, fPosZ);
    // lower-right coord
    glTexCoord2f(TexRect.Right, TexRect.Lower);
    glVertex3f(ScreenRect.Right, ScreenRect.Lower, fPosZ);
    // upper-right coord
    glTexCoord2f(TexRect.Right, TexRect.Upper);
    glVertex3f(ScreenRect.Right, ScreenRect.Upper, fPosZ);
  glEnd;

  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  //draw black borders
  DrawBorders(ScreenRect);

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);

  {$IFDEF VideoBenchmark}
  Log.BenchmarkEnd(15);
  Log.LogBenchmark('Draw', 15);
  {$ENDIF}

  {$IF Defined(Info) or Defined(DebugFrames)}
  ShowDebugInfo();
  {$IFEND}
end;

procedure TVideo_FFmpeg.DrawReflection();
var
  ScreenRect:   TRectCoords;
  TexRect:      TRectCoords;
  HeightFactor: double;
  WidthFactor:  double;

  AlphaTop:     double;
  AlphaBottom:  double;

  AlphaUpper:   double;
  AlphaLower:   double;

begin
  // exit if there's nothing to draw
  if (fDecoder = nil) then
    Exit;

  // get texture and screen positions
  GetVideoRect(ScreenRect, TexRect);

  WidthFactor := (ScreenW/Screens) / RenderW;
  HeightFactor := ScreenH / RenderH;

  glScissor(
    round(fPosX*WidthFactor + (ScreenW/Screens)*(fScreen-1)),
    round((RenderH-fPosY-fHeight-fReflectionSpacing-fHeight*ReflectionH)*HeightFactor),
    round(fWidth*WidthFactor),
    round(fHeight*HeightFactor*ReflectionH)
    );

  glEnable(GL_SCISSOR_TEST);
  glEnable(GL_BLEND);
  glDepthRange(0, 10);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_DEPTH_TEST);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fFrameTex);

  //calculate new ScreenRect coordinates for Reflection
  ScreenRect.Lower := fPosY + fHeight + fReflectionSpacing
    + (ScreenRect.Upper-fPosY) + (ScreenRect.Lower-ScreenRect.Upper)*ReflectionH;
  ScreenRect.Upper := fPosY + fHeight + fReflectionSpacing
    + (ScreenRect.Upper-fPosY);

  AlphaUpper := fAlpha-0.3;
  AlphaLower := 0;

  AlphaTop := AlphaUpper-(AlphaLower-AlphaUpper)*
    (ScreenRect.Upper-fPosY-fHeight-fReflectionSpacing)/fHeight;
  AlphaBottom := AlphaLower+(AlphaUpper-AlphaLower)*
    (fPosY+fHeight+fReflectionSpacing+fHeight*ReflectionH-ScreenRect.Lower)/fHeight;

  glBegin(GL_QUADS);
    //Top Left
    glColor4f(1, 1, 1, AlphaTop);
    glTexCoord2f(TexRect.Left, TexRect.Lower);
    glVertex3f(ScreenRect.Left, ScreenRect.Upper, fPosZ);

    //Bottom Left
    glColor4f(1, 1, 1, AlphaBottom);
    glTexCoord2f(TexRect.Left, (TexRect.Lower-TexRect.Upper)*(1-ReflectionH));
    glVertex3f(ScreenRect.Left, ScreenRect.Lower, fPosZ);

    //Bottom Right
    glColor4f(1, 1, 1, AlphaBottom);
    glTexCoord2f(TexRect.Right, (TexRect.Lower-TexRect.Upper)*(1-ReflectionH));
    glVertex3f(ScreenRect.Right, ScreenRect.Lower, fPosZ);

    //Top Right
    glColor4f(1, 1, 1, AlphaTop);
    glTexCoord2f(TexRect.Right, TexRect.Lower);
    glVertex3f(ScreenRect.Right, ScreenRect.Upper, fPosZ);
  glEnd;

  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  //draw black borders
  DrawBordersReflected(ScreenRect, AlphaUpper, AlphaLower);

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
end;

procedure TVideo_FFmpeg.ShowDebugInfo();
begin
  {$IFDEF Info}
  if (fFrameTime+fFrameDuration < 0) then
  begin
    glColor4f(0.7, 1, 0.3, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(27);
    SetFontPos (300, 0);
    glPrint('Delay due to negative VideoGap');
    glColor4f(1, 1, 1, 1);
  end;
  {$ENDIF}

  {$IFDEF DebugFrames}
    glColor4f(0, 0, 0, 0.2);
    glbegin(GL_QUADS);
      glVertex2f(0, 0);
      glVertex2f(0, 70);
      glVertex2f(250, 70);
      glVertex2f(250, 0);
    glEnd;

    glColor4f(1, 1, 1, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(27);
    SetFontPos (5, 0);
    glPrint('delaying frame');
    SetFontPos (5, 20);
    glPrint('fetching frame');
    SetFontPos (5, 40);
    glPrint('dropping frame');
  {$ENDIF}
end;

procedure TVideo_FFmpeg.Play;
begin
end;

procedure TVideo_FFmpeg.Pause;
begin
  fPaused := not fPaused;
end;

procedure TVideo_FFmpeg.Stop;
begin
end;

procedure TVideo_FFmpeg.SetLoop(Enable: boolean);
begin
  fDecoder.SetLoop(Enable);
end;

function TVideo_FFmpeg.GetLoop(): boolean;
begin
  Result := fDecoder.GetLoop();
end;

procedure TVideo_FFmpeg.SetPosition(Time: real);
begin
  fDecoder.SetPosition(Time);
end;

function  TVideo_FFmpeg.GetPosition: real;
begin
  Result := fDecoder.GetPosition();
end;

procedure TVideo_FFmpeg.SetScreen(Screen: integer);
begin
  fScreen := Screen;
end;

function TVideo_FFmpeg.GetScreen(): integer;
begin
  Result := fScreen;
end;


procedure TVideo_FFmpeg.SetScreenPosition(X, Y, Z: double);
begin
  fPosX := X;
  fPosY := Y;
  fPosZ := Z;
end;

procedure TVideo_FFmpeg.GetScreenPosition(var X, Y, Z: double);
begin
  X := fPosX;
  Y := fPosY;
  Z := fPosZ;
end;


procedure TVideo_FFmpeg.SetWidth(Width: double);
begin
  fWidth := Width;
end;

function TVideo_FFmpeg.GetWidth(): double;
begin
  Result := fWidth;
end;


procedure TVideo_FFmpeg.SetHeight(Height: double);
begin
  fHeight := Height;
end;

function TVideo_FFmpeg.GetHeight(): double;
begin
  Result := fHeight;
end;


procedure TVideo_FFmpeg.SetFrameRange(Range: TRectCoords);
begin
  fFrameRange := Range;
end;

function TVideo_FFmpeg.GetFrameRange(): TRectCoords;
begin
  Result := fFrameRange;
end;


function TVideo_FFmpeg.GetFrameAspect(): real;
begin
  Result := fDecoder.GetFrameAspect();
end;


procedure TVideo_FFmpeg.SetAspectCorrection(AspectCorrection: TAspectCorrection);
begin
  fAspectCorrection := AspectCorrection;
end;

function TVideo_FFmpeg.GetAspectCorrection(): TAspectCorrection;
begin
  Result := fAspectCorrection;
end;



procedure TVideo_FFmpeg.SetAlpha(Alpha: double);
begin
  fAlpha := Alpha;

  if (fAlpha>1) then
    fAlpha := 1;

  if (fAlpha<0) then
    fAlpha := 0;
end;

function TVideo_FFmpeg.GetAlpha(): double;
begin
  Result := fAlpha;
end;


procedure TVideo_FFmpeg.SetReflectionSpacing(Spacing: double);
begin
  fReflectionSpacing := Spacing;
end;

function TVideo_FFmpeg.GetReflectionSpacing(): double;
begin
  Result := fReflectionSpacing;
end;


initialization
  MediaManager.Add(TVideoPlayback_FFmpeg.Create);

end.
