{############################################################################
#                   FFmpeg support for UltraStar deluxe                     #
#   now uses acinerella                                                     #
#                                                                           #
#   Created by b1indy, modified by brunzel                                  #
#   based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)  #
#   and acinerella demo  (http://sourceforge.net/projects/acinerella/)      #
#############################################################################}

//{$define Info}


unit UVideo;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


uses SDL,
     Classes, //for TFilestream
     UGraphicClasses,
     textgl,
     acinerella,
     math,
     gl,
     glu,
     glext,
     SysUtils,
     {$ifdef DebugDisplay}
     {$ifdef win32}
     dialogs,
     {$endif}
     {$ENDIF}
     UIni;

const
  //PIXEL_FORMAT = GL_RGB;
  numBytes = 3;

type
  TRectCoords = record         //from 1.1
    Left, Right:  double;
    Upper, Lower: double;
    windowed: boolean;
  end;
  
  TAspectCorrection = (acoStretch, acoCrop, acoLetterBox); //from 1.1



procedure Init;
procedure acOpenFile(FileName: pAnsiChar);
procedure acClose;
procedure acGetFrame(Time: Extended);
function  acSearch(Time: Extended): integer;
procedure acDrawGL(Screen: integer);
procedure acDrawGLi(Screen: integer; Window: TRectCoords; Blend: real);
procedure acTogglePause;
procedure acSkip(Gap: Single; Start: Single);
procedure acSkip2(Gap: Single; Start: Single);
procedure ToggleAspectCorrection;
procedure GetVideoRect(var ScreenRect, TexRect: TRectCoords; Window: TRectCoords);
procedure SetAspectCorrection(aspect: TAspectCorrection);
procedure ResetAspectCorrection;



var
  VideoOpened, VideoPaused: Boolean;
  VideoTex: glUint;
  //TexData: array of Byte;
  VideoStreamIndex: Integer;
  SkipLines: Integer;
  LastSkipLines: Integer;
  mmfps: Real;
  Counter: Integer;
  //myBuffer: pByte;
  TexX, TexY, dataX, dataY: Cardinal;
  VideoTimeBase, VideoTime, LastFrameTime, TimeDifference, NegativeSkipTime: Extended;
  ScaledVideoWidth, ScaledVideoHeight: Real;
  VideoAspect: Real;
  VideoSkipTime: Single;
  ActualH, ActualW: Integer;

  inst: PAc_instance;
  pack: PAc_package;
  info: TAc_stream_info;
  videodecoder: PAc_decoder;

  fAspect: real;        //**< width/height ratio   (from 1.1)
  fAspectCorrection: TAspectCorrection;            //from 1.1

  fs: TFileStream;
  fName: string;


  timediff: PChar;        //for debug
  timediff_str: string;   //for debug
  mtime: PChar;           //for debug
  mtime_str: string;      //for debug

  pbo:  glUint;
  pbo_supported: boolean;
implementation

uses
  UGraphic, ULog, UDisplay;

function read_proc(sender: Pointer; buf: PByte; size: integer): integer; cdecl;
begin
  result := fs.Read(buf^, size);
end;

function seek_proc(sender: Pointer; pos: int64; whence: integer): int64; cdecl;
begin
  if whence in [0, 1, 2] then
    result := fs.Seek(pos, TSeekOrigin(whence))
  else
    result := -1;
end;

procedure Init;
begin
  videodecoder := nil;

  VideoOpened:=False;
  VideoPaused:=False;
  fName := '';

  glGenTextures(1, @VideoTex);

  //pbo_supported := glext_LoadExtension('GL_ARB_pixel_buffer_object') and
  //  glext_LoadExtension('GL_version_1_5');
  pbo_supported := false;
  if (pbo_supported) then
    glGenBuffers(1, @pbo);

  //SetLength(TexData,0);

  fAspectCorrection := TAspectCorrection(Ini.AspectCorrect);
  SkipLines := 0;
  LastSkipLines := 0;
  Counter := 0;
end;

procedure acOpenFile(FileName: pAnsiChar);
var
  I: integer;
  rTimeBase: Extended;
begin

  VideoPaused    := False;
  VideoTimeBase  := 0;
  rTimeBase      := 0;
  VideoTime      := 0;
  LastFrameTime  := 0;
  TimeDifference := 0;

  acClose;

  if not FileExists(FileName) then
    Exit; //TODO: error.log

  fs := TFileStream.Create(FileName, fmOpenRead);
  fs.Position := 0;

  inst := ac_init();
  videodecoder := nil;
  ac_open(inst, nil, nil, @read_proc, @seek_proc, nil);

  if not inst^.opened then
  begin
    fs.Free;
    Exit; //TODO: error.log
  end;

  //find VideoStreamIndex
  VideoStreamIndex:=-1;
  for I := 0 to inst^.stream_count - 1 do
  begin
    ac_get_stream_info(inst, I, @info);
    case info.stream_type of
      AC_STREAM_TYPE_VIDEO:
      begin

        if videodecoder = nil then
        begin
          VideoStreamIndex:=I;
          inst^.output_format := AC_OUTPUT_RGB24;
          videodecoder := ac_create_decoder(inst, I);
        end;
      end;
    end;
  end;

  if(VideoStreamIndex < 0) then
  begin
    if videodecoder <> nil then
      ac_free_decoder(videodecoder);
    ac_close(inst);
    ac_free(inst);
    fs.Free;
    Exit;
  end;

  VideoOpened:=True;
  fName := FileName;

  TexX := videodecoder^.stream_info.additional_info.video_info.frame_width;
  TexY := videodecoder^.stream_info.additional_info.video_info.frame_height;
  dataX := Round(Power(2, Ceil(Log2(TexX))));
  dataY := Round(Power(2, Ceil(Log2(TexY))));
  //SetLength(TexData, TexX*numBytes);
  //for I := 0 to TexX*numBytes - 1 do
  //  TexData[I]:=0;

  // calculate some information for video display
  VideoAspect:=videodecoder^.stream_info.additional_info.video_info.pixel_aspect;
  if (VideoAspect = 0) then
    VideoAspect:=TexX/TexY
  else
    VideoAspect:=VideoAspect*TexX/TexY;

  fAspect := VideoAspect;

  VideoTimeBase:=info.additional_info.video_info.frames_per_second;

  glBindTexture(GL_TEXTURE_2D, VideoTex);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_PRIORITY, 1.0);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, dataX, dataY, 0,
    GL_RGB, GL_UNSIGNED_BYTE, nil);

  glBindTexture(GL_TEXTURE_2D, 0);
  if(pbo_supported) then
  begin
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, pbo);
    glBufferData(GL_PIXEL_UNPACK_BUFFER_ARB, numBytes*TexX*TexY, nil, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
  end;

  //SkipLines := 0;
  //LastSkipLines := 0;
  //Counter := 0;
  mmfps := 50;
end;

procedure acClose;
begin
  if VideoOpened then begin

    if videodecoder <> nil then
      ac_free_decoder(videodecoder);
    videodecoder:=nil;
    ac_close(inst);
    ac_free(inst);
    inst := nil;
    fs.Free;
    fs:=nil;
    //SetLength(TexData,0);
    VideoOpened:=False;
    fName := '';
  end;
end;

procedure acTogglePause;
begin
  if VideoPaused then VideoPaused:=False
  else VideoPaused:=True;
end;

procedure acSkip2(Gap: Single; Start: Single);
var
  seek_target: uint64;
begin
  VideoSkiptime:=Gap;
  NegativeSkipTime:=Start+Gap;
  if Start+Gap > 0 then
  begin
    VideoTime:=Start+Gap;
    //ac_seek(videodecoder, 0, Floor((Start+Gap)*1000));
    ac_seek(videodecoder, -1, Floor((Start+Gap)*1000));
    //acSearch(Gap+Start);
    {if (ac_seek(videodecoder, -1, Floor((Start+Gap)*1000))=0) then
      if (ac_seek(videodecoder, 0, Floor((Start+Gap)*1000))=0) then
      begin
        ac_seek(videodecoder, 0, 0);
        VideoTime:=0;
        //acSearch(Gap+Start);

      end; }
  end else
  begin
    ac_seek(videodecoder, 0, 0);
    VideoTime:=0;
  end;
end;

procedure acSkip(Gap: Single; Start: Single);
var
  seek_target: uint64;
begin
  VideoSkiptime:=Gap;
  NegativeSkipTime:=Start+Gap;
  if Start+Gap > 0 then
  begin
    VideoTime:=0;
    ac_seek(videodecoder, -1, Floor((Start+Gap)*1000));
    if (acSearch(Gap+Start)=0) then
    begin
      ac_seek(videodecoder, 0, 0);
      VideoTime:=0;
      acSearch(Gap+Start);
    end;
  end else
  begin
    ac_seek(videodecoder, 0, 0);
    VideoTime:=0;
  end;
end;

function acSearch(Time: Extended): integer;
var
  FrameFinished: Integer;
  errnum: Integer;
  FrameDataPtr: PByteArray;
  myTime: Extended;
  DropFrame: Boolean;

begin
  Result := 0;
  if not VideoOpened then Exit;
  if (NegativeSkipTime < 0)and(Time+NegativeSkipTime>=0) then
    NegativeSkipTime:=0;

  myTime:=Time+VideoSkipTime;
  TimeDifference:=myTime-VideoTime;
  if Ini.Debug = 1 then
  begin
    timediff_str:= 't-diff: ' + FormatFloat('#0.00', TimeDifference);
    timediff := Addr(timediff_str[1]);
    mtime_str:= 'mytime: ' + FormatFloat('#0.00', myTime);
    mtime := Addr(mtime_str[1]);
  end;
  DropFrame:=False;

  if (VideoTime <> 0) and (TimeDifference <= VideoTimeBase) then begin
    if Ini.Debug = 1 then
    begin
      // frame delay debug display
      GoldenRec.Spawn(200,65,1,16,0,-1,ColoredStar,$00ff00);
    end;
    Exit;// we don't need a new frame now
  end;

  if TimeDifference >= 2*VideoTimeBase then
  begin // skip frames
    if Ini.Debug = 1 then
    begin
      //frame drop debug display
      GoldenRec.Spawn(200,105,1,16,0,-1,ColoredStar,$ff0000);
    end;

    DropFrame:=True;
  end;

  pack := ac_read_package(inst);
  FrameFinished:=0;
  // read packets until we have a finished frame (or there are no more packets)
  while ({(FrameFinished=0) or }(VideoTime < Time-VideoTimeBase)) and (pack <> nil) do
  begin
    // if we got a packet from the video stream, then decode it
    if (videodecoder^.stream_index = pack^.stream_index) then
    begin
      FrameFinished := ac_decode_package(pack, videodecoder);
      VideoTime := videodecoder^.timecode;
      ac_free_package(pack);
      if ((VideoTime < Time-VideoTimeBase) {or (FrameFinished=0)}) then
        pack := ac_read_package(inst);
    end else
    begin
      ac_free_package(pack);
      pack := ac_read_package(inst);
      //TimeDifference:=myTime-VideoTime;
    end;
  end;
  if (pack<>nil) then
    ac_free_package(pack);

  // if we did not get an new frame, there's nothing more to do
  if Framefinished=0 then
  begin
//    GoldenRec.Spawn(220,15,1,16,0,-1,ColoredStar,$0000ff);
    Exit;
  end;

  errnum:=1;       //TODO!!
  if errnum >=0 then begin
    FrameDataPtr:=Pointer(videodecoder^.buffer);
    Result := 1;
    glBindTexture(GL_TEXTURE_2D, VideoTex);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TexX, TexY, GL_RGB, GL_UNSIGNED_BYTE, @FrameDataPtr[0]);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    if Ini.Debug = 1 then
    begin
      //frame decode debug display
      GoldenRec.Spawn(200,85,1,16,0,-1,ColoredStar,$ffff00);
    end;

  end;
end;

procedure acGetFrame(Time: Extended);
var
  FrameFinished: Integer;
  errnum, x, y: Integer;
  FrameDataPtr: PByteArray;
  FrameDataPtr2: PByteArray;
  linesize: integer;
  myTime: Extended;
  DropFrame: Boolean;
  droppedFrames: Integer;
  I, J: Integer;

  glError: glEnum;
  glErrorStr: String;
const
  FRAMEDROPCOUNT=3;
begin
  if not VideoOpened then Exit;
  if VideoPaused then Exit;
  if (NegativeSkipTime < 0)and(Time+NegativeSkipTime>=0) then NegativeSkipTime:=0;

  myTime:=Time+VideoSkipTime;
  TimeDifference:=myTime-VideoTime;
  if Ini.Debug = 1 then
  begin
    timediff_str:= 't-diff: ' + FormatFloat('#0.00', TimeDifference);
    timediff := Addr(timediff_str[1]);
    mtime_str:= 'mytime: ' + FormatFloat('#0.00', myTime);
    mtime := Addr(mtime_str[1]);
  end;
  DropFrame:=False;

  if (VideoTime <> 0) and (TimeDifference <= VideoTimeBase) then begin
    if Ini.Debug = 1 then
    begin
      // frame delay debug display
      GoldenRec.Spawn(200,65,1,16,0,-1,ColoredStar,$00ff00);
    end;
    Exit;// we don't need a new frame now
  end;

  if TimeDifference >= (FRAMEDROPCOUNT-1)*VideoTimeBase then begin // skip frames
    if Ini.Debug = 1 then
    begin
      //frame drop debug display
      GoldenRec.Spawn(200,105,1,16,0,-1,ColoredStar,$ff0000);
    end;

    DropFrame:=True;
  end;

  pack := ac_read_package(inst);
  FrameFinished:=0;
  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished=0) and (pack <> nil) do
  begin
    // if we got a packet from the video stream, then decode it
    if (videodecoder^.stream_index = pack^.stream_index) then
    begin
      FrameFinished := ac_decode_package(pack, videodecoder);
      ac_free_package(pack);
      if (FrameFinished=0) then
        pack := ac_read_package(inst);
    end else
    begin
      ac_free_package(pack);
      pack := ac_read_package(inst);
    end;
  end;


  if DropFrame then
  begin
    //acSearch(Time);
    for droppedFrames:=1 to FRAMEDROPCOUNT do
    begin
      pack := ac_read_package(inst);
      FrameFinished:=0;
      // read packets until we have a finished frame (or there are no more packets)
      while (FrameFinished=0) and (pack <> nil) do
      begin
        // if we got a packet from the video stream, then decode it
        if (videodecoder^.stream_index = pack^.stream_index) then
        begin
          FrameFinished := ac_decode_package(pack, videodecoder);
          ac_free_package(pack);
          if (FrameFinished=0) then
            pack := ac_read_package(inst);
        end else
        begin
          ac_free_package(pack);
          pack := ac_read_package(inst);
        end;
      end;
    end;
  end;

  // if we did not get an new frame, there's nothing more to do
  if Framefinished=0 then
  begin
//    GoldenRec.Spawn(220,15,1,16,0,-1,ColoredStar,$0000ff);
    acClose;
    Exit;
  end;

  errnum:=1;       //TODO!!
  if errnum >=0 then
  begin
    mmfps := (Display.mFPS+mmfps)/2;
    if(Ini.PerformanceMode=1) then
    begin
      if (mmfps<45) then
      begin
        if(SkipLines<3) and (Counter<100) then
          Counter := round(Counter+70/mmfps)
        else if (SkipLines<3) and (Counter>=100) then
        begin
          Inc(SkipLines);
          mmfps:=50;
          Counter := 0;
        end;
      end else if (mmfps>75) then
      begin
        if(SkipLines>0) and (Counter<=100) then
          Counter := round(Counter+50/mmfps)
        else if (SkipLines>0) and (Counter>=100) then
        begin
          Dec(SkipLines);
          LastSkipLines := SkipLines;
          Counter := 0;
        end;
      end else Counter := 0;
    end;

    if(not pbo_supported) then
    begin
      FrameDataPtr:=Pointer(videodecoder^.buffer);

      glBindTexture(GL_TEXTURE_2D, VideoTex);

      if(SkipLines>0)then
      begin
        for I := 0 to TexY - 1 do
        begin
          if(I mod (SkipLines+1) = 0) then
            glTexSubImage2D(GL_TEXTURE_2D, 0, 0, (I div (SkipLines+1)), TexX, 1,
              GL_RGB, GL_UNSIGNED_BYTE, @FrameDataPtr[I*numBytes*TexX]);
        end;
      end else
          glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TexX, TexY,
            GL_RGB, GL_UNSIGNED_BYTE, @FrameDataPtr[0]);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end else
    begin
      glGetError();
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, pbo);

      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video "glBindBuffer"');
        Exit;
      end;

      FrameDataPtr := glMapBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, GL_WRITE_ONLY);
      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video pbo "glMapBuffer"');
        Exit;
      end;

      FrameDataPtr2:=Pointer(videodecoder^.buffer);
      move(FrameDataPtr2[0], FrameDataPtr[0], numBytes*TexX*TexY);

      glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER_ARB);
      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video pbo "glUnmapBuffer"');
        Exit;
      end;    

      glBindTexture(GL_TEXTURE_2D, VideoTex);
      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        Log.LogError('Error drawing Video pbo "glBindTexture"');
        Exit;
      end;

      glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, TexX, TexY,
        GL_RGB, GL_UNSIGNED_BYTE, 0);

      glError := glGetError;
      if glError <> GL_NO_ERROR then
      begin
        acClose;
        case glError of
              GL_INVALID_ENUM: glErrorStr:='INVALID_ENUM';
              GL_INVALID_VALUE: glErrorStr:='INVALID_VALUE';
              GL_INVALID_OPERATION: glErrorStr:='INVALID_OPERATION';
              GL_STACK_OVERFLOW: glErrorStr:='STACK_OVERFLOW';
              GL_STACK_UNDERFLOW: glErrorStr:='STACK_UNDERFLOW';
              GL_OUT_OF_MEMORY: glErrorStr:='OUT_OF_MEMORY';
              else glErrorStr:='unknown error';
            end;
        Log.LogError('Error drawing Video pbo "glTexSubImage2D" ('+glErrorStr+')');
        Exit;
      end;
      glBindTexture(GL_TEXTURE_2D, 0);
      glBindBuffer(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    end;
    VideoTime := videodecoder^.timecode;

    if Ini.Debug = 1 then
    begin
      //frame decode debug display
      GoldenRec.Spawn(200,85,1,16,0,-1,ColoredStar,$ffff00);
    end;

  end;
end;

procedure ToggleAspectCorrection();
begin
  case fAspectCorrection of
    acoCrop      : fAspectCorrection := acoStretch;
    acoStretch   : fAspectCorrection := acoLetterBox;
    acoLetterBox : fAspectCorrection := acoCrop;
  end;
  //Ini.AspectCorrect := integer(fAspectCorrection);
  //Ini.Save;
end;

procedure SetAspectCorrection(aspect: TAspectCorrection);
begin
  fAspectCorrection := aspect;
end;

procedure ResetAspectCorrection;
begin
  case Ini.AspectCorrect of
    integer(acoCrop)      : fAspectCorrection := acoCrop;
    integer(acoStretch)   : fAspectCorrection := acoStretch;
    integer(acoLetterBox) : fAspectCorrection := acoLetterBox;
  end;
end;

procedure GetVideoRect(var ScreenRect, TexRect: TRectCoords; Window: TRectCoords);
var
  ScreenAspect: double;  // aspect of screen resolution
  ScaledVideoWidth, ScaledVideoHeight: double;
  rW, rH: double;
begin
  // Three aspects to take into account:
  //  1. Screen/display resolution (e.g. 1920x1080 -> 16:9)
  //  2. Render aspect (fixed to 800x600 -> 4:3)
  //  3. Movie aspect (video frame aspect stored in fAspect)
  if (Window.windowed) then
  begin
    rW := Window.Right-Window.Left;
    rH := Window.Lower-Window.Upper;
    ScreenAspect := rW/rH;
  end else
  begin
    rW := RenderW;
    rH := RenderH;
    ScreenAspect := (ScreenW/Screens) / ScreenH;
  end;

  case fAspectCorrection of
    acoStretch: begin
      ScaledVideoWidth  := rW;
      ScaledVideoHeight := rH;
    end;
    acoCrop: begin
      if (ScreenAspect >= fAspect) then
      begin
        ScaledVideoWidth  := rW;
        ScaledVideoHeight := rH * ScreenAspect/fAspect;
      end
      else
      begin
        ScaledVideoHeight := rH;
        ScaledVideoWidth  := rW * fAspect/ScreenAspect;
      end;
    end;
    acoLetterBox: begin
      if (ScreenAspect <= fAspect) then
      begin
        ScaledVideoWidth  := rW;
        ScaledVideoHeight := rH * ScreenAspect/fAspect;
      end
      else
      begin
        ScaledVideoHeight := rH;
        ScaledVideoWidth  := rW * fAspect/ScreenAspect;
      end;
    end
    else
      raise Exception.Create('Unhandled aspect correction!');
  end;

  ScreenRect.Left := (rW - ScaledVideoWidth) / 2 + Window.Left;
  ScreenRect.Right := ScreenRect.Left + ScaledVideoWidth;
  ScreenRect.Upper := (rH - ScaledVideoHeight) / 2 + Window.Upper;
  ScreenRect.Lower := ScreenRect.Upper + ScaledVideoHeight;

  // texture contains right/lower (power-of-2) padding.
  // Determine the texture coords of the video frame.
  TexRect.Left  := 0;
  TexRect.Right := TexX  / dataX;
  TexRect.Upper := 0;
  TexRect.Lower := (TexY/(SkipLines+1)) / dataY;
end;

procedure acDrawGL(Screen: integer);
var
  Window: TRectCoords;
begin
  Window.Left := 0;
  Window.Right := RenderW;
  Window.Upper := 0;
  Window.Lower := RenderH;
  Window.windowed := false;
  acDrawGLi(Screen, Window, 1);
end;

procedure acDrawGLi(Screen: integer; Window: TRectCoords; Blend: real);
var
  text: string;
  test: PChar;
  ScreenRect, TexRect: TRectCoords;
  FrameDataPtr: PByteArray;
  FrameDataPtr2: PByteArray;
  Offset: Integer;

begin
  // have a nice black background to draw on (even if there were errors opening the vid)
  if (Screen=1) and not Window.windowed then begin
    glDisable(GL_BLEND);
    //glDisable(GL_DEPTH_TEST);
    //glDepthMask(GL_FALSE);
    //glDisable(GL_CULL_FACE);

    glClearColor(0,0,0,1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end else
    glEnable(GL_BLEND);

  // exit if there's nothing to draw
  if not VideoOpened then Exit;
  // if we're still inside negative skip, then exit
  //if (NegativeSkipTime < 0) then Exit;

  GetVideoRect(ScreenRect, TexRect, Window);



  glScissor(round((Window.Left)*(ScreenW/Screens)/RenderW+(ScreenW/Screens)*(Screen-1)),
    round((RenderH-Window.Lower)*ScreenH/RenderH),
    round((Window.Right-Window.Left)*(ScreenW/Screens)/RenderW),
    round((Window.Lower-Window.Upper)*ScreenH/RenderH));
  glEnable(GL_SCISSOR_TEST);


  glEnable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, Blend);
  glBindTexture(GL_TEXTURE_2D, VideoTex);

  glbegin(gl_quads);
    // upper-left coord
    glTexCoord2f(TexRect.Left, TexRect.Upper);
    glVertex2f(ScreenRect.Left, ScreenRect.Upper);
    // lower-left coord
    glTexCoord2f(TexRect.Left, TexRect.Lower);
    glVertex2f(ScreenRect.Left, ScreenRect.Lower);
    // lower-right coord
    glTexCoord2f(TexRect.Right, TexRect.Lower);
    glVertex2f(ScreenRect.Right, ScreenRect.Lower);
    // upper-right coord
    glTexCoord2f(TexRect.Right, TexRect.Upper);
    glVertex2f(ScreenRect.Right, ScreenRect.Upper);

  glEnd;
  glDisable(GL_TEXTURE_2D);
  //glBindBuffer(GL_PIXEL_UNPACK_BUFFER_EXT, 0);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_BLEND);

  if (Ini.MovieSize < 1) then  //HalfSize BG
  begin
    // draw fading bars over top and bottom of background/video
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      (* black top *)
      glColor4f(0,0,0,1);
      glVertex2f(0, 0);
      glVertex2f(800,0);
      glVertex2f(800,110);
      glVertex2f(0,110);
      (* top gradient *)
      glVertex2f(0, 110);
      glVertex2f(800,110);
      glColor4f(0,0,0,0);
      glVertex2f(800,130);
      glVertex2f(0,130);

      (* bottom gradient *)
      glColor4f(0,0,0,0);
      glVertex2f(0, 600-130);
      glVertex2f(800,600-130);
      glColor4f(0,0,0,1);
      glVertex2f(800,600-110);
      glVertex2f(0,600-110);
      (* black bottom *)
      glVertex2f(0, 600-110);
      glVertex2f(800,600-110);
      glVertex2f(800,600);
      glVertex2f(0,600);

    glEnd;
    glDisable(GL_BLEND);
  end;

// info-stuff

{$ifdef Info}
  if VideoSkipTime+videodecoder^.timecode+VideoTimeBase < 0 then begin
    glColor4f(0.7, 1, 0.3, 1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(9);
    SetFontPos (300, 0);
    glPrint('Delay due to negative VideoGap');
    glColor4f(1, 1, 1, 1);
  end;
{$endif}

  if Ini.Debug = 1 then
  begin
    glColor4f(0, 0, 0, 0.2);
    glbegin(gl_quads);
      glVertex2f(0, 50);
      glVertex2f(0, 160);
      glVertex2f(250, 160);
      glVertex2f(250, 50);
    glEnd;

    glColor4f(1,1,1,1);
    SetFontStyle (1);
    SetFontItalic(False);
    SetFontSize(7);
    SetFontPos (5, 50);
    text:= 'vtime: ' + FormatFloat('#0.00', VideoTime);
    test := Addr(text[1]);
    glPrint(test);
    SetFontPos (5, 65);
    //text:= 'vtime: ' + FormatFloat('#0.00', videodecoder^.timecode);
    //test := Addr(text[1]);
    glPrint(timediff);
    SetFontPos (5, 80);
    glPrint(mtime);
    SetFontPos (5, 95);
    case fAspectCorrection of
      acoCrop      : glPrint('Crop');
      acoStretch   : glPrint('Stretch');
      acoLetterBox : glPrint('LetterBox');
    end;

    SetFontPos (5, 110);
    glPrint(PChar('mmfps: '+FormatFloat('#0.00', mmfps)));

    SetFontPos (5, 125);
    glPrint(PChar('skipL: '+inttostr(SkipLines)));
  end;
end;

end.
