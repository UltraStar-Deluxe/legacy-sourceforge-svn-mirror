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

unit UVideoDecoder_FFmpeg;

{*
 * based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)
 *}

// uncomment if you want to see the debug stuff
{.$define DebugDisplay}
{.$define DebugFrames}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// use BGR-format for accelerated colorspace conversion with swscale
{$IFDEF UseSWScale}
  {$DEFINE PIXEL_FMT_BGR}
{$ENDIF}

implementation

uses
  SysUtils,
  Math,
  avcodec,
  avformat,
  avutil,
  avio,
  rational,
  {$IFDEF UseSWScale}
  swscale,
  {$ENDIF}
  UMediaCore_FFmpeg,
  UCommon,
  UConfig,
  ULog,
  UMusic,
  UPath;

{$DEFINE PIXEL_FMT_BGR}

const
{$IFDEF PIXEL_FMT_BGR}
  PIXEL_FMT_FFMPEG = PIX_FMT_BGR24;
  PIXEL_FMT_SIZE   = 3;

  // looks strange on linux:
  //PIXEL_FMT_FFMPEG = PIX_FMT_BGR32;
  //PIXEL_FMT_SIZE   = 4;
{$ELSE}
  // looks strange on linux:
  PIXEL_FMT_FFMPEG = PIX_FMT_RGB24;
  PIXEL_FMT_SIZE   = 3;
{$ENDIF}

type
  TVideoDecodeStream_FFmpeg = class (TVideoDecodeStream)
  private
    fOpened: boolean;     //**< stream successfully opened
    fEOF: boolean;        //**< end-of-file state

    fLoop: boolean;       //**< looping enabled

    fStream:        PAVStream;
    fStreamIndex :  integer;
    fFormatContext: PAVFormatContext;
    fCodecContext:  PAVCodecContext;
    fCodec:         PAVCodec;

    fAVFrame:     PAVFrame;
    fAVFrameRGB:  PAVFrame;

    fFrameBuffer: PByte;  //**< stores a FFmpeg video frame
    fFrameTexValid: boolean; //**< if true, fFrameTex contains the current frame

    {$IFDEF UseSWScale}
    fSwScaleContext: PSwsContext;
    {$ENDIF}

    fAspect: real;        //**< width/height ratio

    fFrameDuration: extended; //**< duration of a video frame in seconds (= 1/fps)
    fFrameTime: extended; //**< video time position (absolute)
    fLoopTime: extended;  //**< start time of the current loop

    procedure Reset();
    function DecodeFrame(): boolean;
    procedure SynchronizeTime(Frame: PAVFrame; var pts: double);

  public
    constructor Create;
    destructor Destroy; override;

    function Open(const FileName: IPath): boolean; override;
    procedure Close; override;

    procedure SetLoop(Enable: boolean); override;
    function GetLoop(): boolean; override;

    procedure SetPosition(Time: real); override;
    function GetPosition: real; override;

    function GetFrameWidth(): integer; override;
    function GetFrameHeight(): integer; override;

    function GetFrameAspect(): real; override;
    function GetFrame(Time: Extended): PByteArray; override;
  end;

  TVideoDecoder_FFmpeg = class( TInterfacedObject, IVideoDecoder )
  private
    fInitialized: boolean;

  public
    function GetName: String;

    function InitializeDecoder(): boolean;
    function FinalizeDecoder: boolean;

    function Open(const FileName: IPath): TVideoDecodeStream;
  end;

var
  FFmpegCore: TMediaCore_FFmpeg;


// These are called whenever we allocate a frame buffer.
// We use this to store the global_pts in a frame at the time it is allocated.
function PtsGetBuffer(CodecCtx: PAVCodecContext; Frame: PAVFrame): integer; cdecl;
var
  pts: Pint64;
  VideoPktPts: Pint64;
begin
  Result := avcodec_default_get_buffer(CodecCtx, Frame);
  VideoPktPts := CodecCtx^.opaque;
  if (VideoPktPts <> nil) then
  begin
    // Note: we must copy the pts instead of passing a pointer, because the packet
    // (and with it the pts) might change before a frame is returned by av_decode_video.
    pts := av_malloc(sizeof(int64));
    pts^ := VideoPktPts^;
    Frame^.opaque := pts;
  end;
end;

procedure PtsReleaseBuffer(CodecCtx: PAVCodecContext; Frame: PAVFrame); cdecl;
begin
  if (Frame <> nil) then
    av_freep(@Frame^.opaque);
  avcodec_default_release_buffer(CodecCtx, Frame);
end;


{*------------------------------------------------------------------------------
 * TVideoPlayback_ffmpeg
 *------------------------------------------------------------------------------}

function  TVideoDecoder_FFmpeg.GetName: String;
begin
  result := 'FFmpeg_VideoDecoder';
end;

function TVideoDecoder_FFmpeg.InitializeDecoder(): boolean;
begin
  Result := true;

  if (fInitialized) then
    Exit;
  fInitialized := true;

  FFmpegCore := TMediaCore_FFmpeg.GetInstance();

  av_register_all();
end;

function TVideoDecoder_FFmpeg.FinalizeDecoder(): boolean;
begin
  Result := true;
end;

function TVideoDecoder_FFmpeg.Open(const FileName : IPath): TVideoDecodeStream;
var
  Stream: TVideoDecodeStream_FFmpeg;
begin
  Result := nil;

  Stream := TVideoDecodeStream_FFmpeg.Create;
  if (not Stream.Open(FileName)) then
  begin
    Stream.Free;
    Exit;
  end;

  Result := Stream
end;


{* TVideoDecoder_FFmpeg *}

constructor TVideoDecodeStream_FFmpeg.Create;
begin
  Reset();
end;

destructor TVideoDecodeStream_FFmpeg.Destroy;
begin
  Close();
end;

function TVideoDecodeStream_FFmpeg.Open(const FileName: IPath): boolean;
var
  errnum: Integer;
  AudioStreamIndex: integer;
begin
  Result := false;
  Reset();

  // use custom 'ufile' protocol for UTF-8 support
  errnum := av_open_input_file(fFormatContext, PAnsiChar('ufile:'+FileName.ToUTF8), nil, 0, nil);
  if (errnum <> 0) then
  begin
    Log.LogError('Failed to open file "'+ FileName.ToNative +'" ('+FFmpegCore.GetErrorString(errnum)+')');
    Exit;
  end;

  // update video info
  if (av_find_stream_info(fFormatContext) < 0) then
  begin
    Log.LogError('No stream info found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  Log.LogInfo('VideoStreamIndex : ' + inttostr(fStreamIndex), 'TVideoPlayback_ffmpeg.Open');

  // find video stream
  FFmpegCore.FindStreamIDs(fFormatContext, fStreamIndex, AudioStreamIndex);
  if (fStreamIndex < 0) then
  begin
    Log.LogError('No video stream found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  fStream := fFormatContext^.streams[fStreamIndex];
  fCodecContext := fStream^.codec;

  fCodec := avcodec_find_decoder(fCodecContext^.codec_id);
  if (fCodec = nil) then
  begin
    Log.LogError('No matching codec found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // set debug options
  fCodecContext^.debug_mv := 0;
  fCodecContext^.debug := 0;

  // detect bug-workarounds automatically
  fCodecContext^.workaround_bugs := FF_BUG_AUTODETECT;
  // error resilience strategy (careful/compliant/agressive/very_aggressive)
  //fCodecContext^.error_resilience := FF_ER_CAREFUL; //FF_ER_COMPLIANT;
  // allow non spec compliant speedup tricks.
  //fCodecContext^.flags2 := fCodecContext^.flags2 or CODEC_FLAG2_FAST;

  // Note: avcodec_open() and avcodec_close() are not thread-safe and will
  // fail if called concurrently by different threads.
  FFmpegCore.LockAVCodec();
  try
    errnum := avcodec_open(fCodecContext, fCodec);
  finally
    FFmpegCore.UnlockAVCodec();
  end;
  if (errnum < 0) then
  begin
    Log.LogError('No matching codec found', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // register custom callbacks for pts-determination
  fCodecContext^.get_buffer := PtsGetBuffer;
  fCodecContext^.release_buffer := PtsReleaseBuffer;

  {$ifdef DebugDisplay}
  DebugWriteln('Found a matching Codec: '+ fCodecContext^.Codec.Name + sLineBreak +
    sLineBreak +
    '  Width = '+inttostr(fCodecContext^.width) +
    ', Height='+inttostr(fCodecContext^.height) + sLineBreak +
    '  Aspect    : '+inttostr(fCodecContext^.sample_aspect_ratio.num) + '/' +
                     inttostr(fCodecContext^.sample_aspect_ratio.den) + sLineBreak +
    '  Framerate : '+inttostr(fCodecContext^.time_base.num) + '/' +
                     inttostr(fCodecContext^.time_base.den));
  {$endif}

  // allocate space for decoded frame and rgb frame
  fAVFrame := avcodec_alloc_frame();
  fAVFrameRGB := avcodec_alloc_frame();
  fFrameBuffer := av_malloc(avpicture_get_size(PIXEL_FMT_FFMPEG,
      fCodecContext^.width, fCodecContext^.height));

  if ((fAVFrame = nil) or (fAVFrameRGB = nil) or (fFrameBuffer = nil)) then
  begin
    Log.LogError('Failed to allocate buffers', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // TODO: pad data for OpenGL to GL_UNPACK_ALIGNMENT
  // (otherwise video will be distorted if width/height is not a multiple of the alignment)
  errnum := avpicture_fill(PAVPicture(fAVFrameRGB), fFrameBuffer, PIXEL_FMT_FFMPEG,
      fCodecContext^.width, fCodecContext^.height);
  if (errnum < 0) then
  begin
    Log.LogError('avpicture_fill failed: ' + FFmpegCore.GetErrorString(errnum), 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;

  // calculate some information for video display
  fAspect := av_q2d(fCodecContext^.sample_aspect_ratio);
  if (fAspect = 0) then
    fAspect := fCodecContext^.width /
               fCodecContext^.height
  else
    fAspect := fAspect * fCodecContext^.width /
                         fCodecContext^.height;

  fFrameDuration := 1/av_q2d(fStream^.r_frame_rate);

  // hack to get reasonable framerate (for divx and others)
  if (fFrameDuration < 0.02) then // 0.02 <-> 50 fps
  begin
    fFrameDuration := av_q2d(fStream^.r_frame_rate);
    while (fFrameDuration > 50) do
      fFrameDuration := fFrameDuration/10;
    fFrameDuration := 1/fFrameDuration;
  end;

  Log.LogInfo('Framerate: '+inttostr(floor(1/fFrameDuration))+'fps', 'TVideoPlayback_ffmpeg.Open');

  {$IFDEF UseSWScale}
  // if available get a SWScale-context -> faster than the deprecated img_convert().
  // SWScale has accelerated support for PIX_FMT_RGB32/PIX_FMT_BGR24/PIX_FMT_BGR565/PIX_FMT_BGR555.
  // Note: PIX_FMT_RGB32 is a BGR- and not an RGB-format (maybe a bug)!!!
  // The BGR565-formats (GL_UNSIGNED_SHORT_5_6_5) is way too slow because of its
  // bad OpenGL support. The BGR formats have MMX(2) implementations but no speed-up
  // could be observed in comparison to the RGB versions.
  fSwScaleContext := sws_getContext(
      fCodecContext^.width, fCodecContext^.height,
      fCodecContext^.pix_fmt,
      fCodecContext^.width, fCodecContext^.height,
      PIXEL_FMT_FFMPEG,
      SWS_FAST_BILINEAR, nil, nil, nil);
  if (fSwScaleContext = nil) then
  begin
    Log.LogError('Failed to get swscale context', 'TVideoPlayback_ffmpeg.Open');
    Close();
    Exit;
  end;
  {$ENDIF}

  fOpened := true;
  Result := true;
end;

procedure TVideoDecodeStream_FFmpeg.Reset();
begin
  // close previously opened video
  Close();

  fOpened := False;
  fFrameDuration := 0;
  fFrameTime := 0;
  fStream := nil;
  fStreamIndex := -1;
  fFrameTexValid := false;

  fEOF := false;

  fLoop := false;
  fLoopTime := 0;
end;

procedure TVideoDecodeStream_FFmpeg.Close;
begin
  if (fFrameBuffer <> nil) then
    av_free(fFrameBuffer);
  if (fAVFrameRGB <> nil) then
    av_free(fAVFrameRGB);
  if (fAVFrame <> nil) then
    av_free(fAVFrame);

  fAVFrame     := nil;
  fAVFrameRGB  := nil;
  fFrameBuffer := nil;

  if (fCodecContext <> nil) then
  begin
    // avcodec_close() is not thread-safe
    FFmpegCore.LockAVCodec();
    try
      avcodec_close(fCodecContext);
    finally
      FFmpegCore.UnlockAVCodec();
    end;
  end;

  if (fFormatContext <> nil) then
    av_close_input_file(fFormatContext);

  fCodecContext  := nil;
  fFormatContext := nil;

  fOpened := False;
end;

procedure TVideoDecodeStream_FFmpeg.SynchronizeTime(Frame: PAVFrame; var pts: double);
var
  FrameDelay: double;
begin
  if (pts <> 0) then
  begin
    // if we have pts, set video clock to it
    fFrameTime := pts;
  end else
  begin
    // if we aren't given a pts, set it to the clock
    pts := fFrameTime;
  end;
  // update the video clock
  FrameDelay := av_q2d(fCodecContext^.time_base);
  // if we are repeating a frame, adjust clock accordingly
  FrameDelay := FrameDelay + Frame^.repeat_pict * (FrameDelay * 0.5);
  fFrameTime := fFrameTime + FrameDelay;
end;

{**
 * Decode a new frame from the video stream.
 * The decoded frame is stored in fAVFrame. fFrameTime is updated to the new frame's
 * time.
 * @param pts will be updated to the presentation time of the decoded frame.
 * returns true if a frame could be decoded. False if an error or EOF occured.
 *}
function TVideoDecodeStream_FFmpeg.DecodeFrame(): boolean;
var
  FrameFinished: Integer;
  VideoPktPts: int64;
  pbIOCtx: PByteIOContext;
  errnum: integer;
  AVPacket: TAVPacket;
  pts: double;
begin
  Result := false;
  FrameFinished := 0;

  if fEOF then
    Exit;

  // read packets until we have a finished frame (or there are no more packets)
  while (FrameFinished = 0) do
  begin
    errnum := av_read_frame(fFormatContext, AVPacket);
    if (errnum < 0) then
    begin
      // failed to read a frame, check reason

      {$IF (LIBAVFORMAT_VERSION_MAJOR >= 52)}
      pbIOCtx := fFormatContext^.pb;
      {$ELSE}
      pbIOCtx := @fFormatContext^.pb;
      {$IFEND}

      // check for end-of-file (EOF is not an error)
      if (url_feof(pbIOCtx) <> 0) then
      begin
        fEOF := true;
        Exit;
      end;

      // check for errors
      if (url_ferror(pbIOCtx) <> 0) then
      begin
        Log.LogError('Video decoding file error', 'TVideoPlayback_FFmpeg.DecodeFrame');
        Exit;
      end;

      // url_feof() does not detect an EOF for some mov-files (e.g. deluxe.mov)
      // so we have to do it this way.
      if ((fFormatContext^.file_size <> 0) and
          (pbIOCtx^.pos >= fFormatContext^.file_size)) then
      begin
        fEOF := true;
        Exit;
      end;

      // error occured, log and exit
      Log.LogError('Video decoding error', 'TVideoPlayback_FFmpeg.DecodeFrame');
      Exit;
    end;

    // if we got a packet from the video stream, then decode it
    if (AVPacket.stream_index = fStreamIndex) then
    begin
      // save pts to be stored in pFrame in first call of PtsGetBuffer()
      VideoPktPts := AVPacket.pts;
      fCodecContext^.opaque := @VideoPktPts;

      // decode packet
      avcodec_decode_video(fCodecContext, fAVFrame,
          frameFinished, AVPacket.data, AVPacket.size);

      // reset opaque data
      fCodecContext^.opaque := nil;

      // update pts
      if (AVPacket.dts <> AV_NOPTS_VALUE) then
      begin
        pts := AVPacket.dts;
      end
      else if ((fAVFrame^.opaque <> nil) and
               (Pint64(fAVFrame^.opaque)^ <> AV_NOPTS_VALUE)) then
      begin
        pts := Pint64(fAVFrame^.opaque)^;
      end
      else
      begin
        pts := 0;
      end;

      if fStream^.start_time <> AV_NOPTS_VALUE then
        pts := pts - fStream^.start_time;

      pts := pts * av_q2d(fStream^.time_base);

      // synchronize time on each complete frame
      if (frameFinished <> 0) then
        SynchronizeTime(fAVFrame, pts);
    end;

    // free the packet from av_read_frame
    av_free_packet( @AVPacket );
  end;

  Result := true;
end;

function TVideoDecodeStream_FFmpeg.GetFrame(Time: Extended): PByteArray;
var
  errnum: Integer;
  CurrentTime: Extended;
  TimeDiff: Extended;
  DropFrameCount: Integer;
  i: Integer;
  Success: boolean;
const
  SKIP_FRAME_DIFF = 0.010; // start skipping if we are >= 10ms too late
begin
  Result := nil;

  if not fOpened then
    Exit;

  {*
   * Synchronization - begin
   *}

  // requested stream position (relative to the last loop's start)
  if (fLoop) then
    CurrentTime := Time - fLoopTime
  else
    CurrentTime := Time;

  // check if current texture still contains the active frame
  if (fFrameTexValid) then
  begin
    // time since the last frame was returned
    TimeDiff := CurrentTime - fFrameTime;

    {$IFDEF DebugDisplay}
    DebugWriteln('Time:      '+inttostr(floor(Time*1000)) + sLineBreak +
                 'VideoTime: '+inttostr(floor(fFrameTime*1000)) + sLineBreak +
                 'TimeBase:  '+inttostr(floor(fFrameDuration*1000)) + sLineBreak +
                 'TimeDiff:  '+inttostr(floor(TimeDiff*1000)));
    {$endif}

    // check if time has reached the next frame
    if (TimeDiff < fFrameDuration) then
    begin
      {$ifdef DebugFrames}
      // frame delay debug display
      GoldenRec.Spawn(200,15,1,16,0,-1,ColoredStar,$00ff00);
      {$endif}

      {$IFDEF DebugDisplay}
      DebugWriteln('not getting new frame' + sLineBreak +
          'Time:      '+inttostr(floor(Time*1000)) + sLineBreak +
          'VideoTime: '+inttostr(floor(fFrameTime*1000)) + sLineBreak +
          'TimeBase:  '+inttostr(floor(fFrameDuration*1000)) + sLineBreak +
          'TimeDiff:  '+inttostr(floor(TimeDiff*1000)));
      {$endif}

      // we do not need a new frame now
      Exit;
    end;
  end;

  // fetch new frame (updates fFrameTime)
  Success := DecodeFrame();
  TimeDiff := CurrentTime - fFrameTime;

  // check if we have to skip frames
  // Either if we are one frame behind or if the skip threshold has been reached.
  // Do not skip if the difference is less than fFrameDuration as there is no next frame.
  // Note: We assume that fFrameDuration is the length of one frame.
  if (TimeDiff >= Max(fFrameDuration, SKIP_FRAME_DIFF)) then
  begin
    {$IFDEF DebugFrames}
    //frame drop debug display
    GoldenRec.Spawn(200,55,1,16,0,-1,ColoredStar,$ff0000);
    {$ENDIF}
    {$IFDEF DebugDisplay}
    DebugWriteln('skipping frames' + sLineBreak +
        'TimeBase:  '+inttostr(floor(fFrameDuration*1000)) + sLineBreak +
        'TimeDiff:  '+inttostr(floor(TimeDiff*1000)));
    {$endif}

    // update video-time
    DropFrameCount := Trunc(TimeDiff / fFrameDuration);
    fFrameTime := fFrameTime + DropFrameCount*fFrameDuration;

    // skip frames
    for i := 1 to DropFrameCount do
      Success := DecodeFrame();
  end;

  // check if we got an EOF or error
  if (not Success) then
  begin
    if fLoop then
    begin
      // we have to loop, so rewind
      SetPosition(0);
      // record the start-time of the current loop, so we can
      // determine the position in the stream (fFrameTime-fLoopTime) later.
      fLoopTime := Time;
    end;
    Exit;
  end;

  {*
   * Synchronization - end
   *}

  // TODO: support for pan&scan
  //if (fAVFrame.pan_scan <> nil) then
  //begin
  //  Writeln(Format('PanScan: %d/%d', [fAVFrame.pan_scan.width, fAVFrame.pan_scan.height]));
  //end;

  // otherwise we convert the pixeldata from YUV to RGB
  {$IFDEF UseSWScale}
  errnum := sws_scale(fSwScaleContext, @fAVFrame.data, @fAVFrame.linesize,
          0, fCodecContext^.Height,
          @fAVFrameRGB.data, @fAVFrameRGB.linesize);
  {$ELSE}
  // img_convert from lib/ffmpeg/avcodec.pas is actually deprecated.
  // If ./configure does not find SWScale then this gives the error
  // that the identifier img_convert is not known or similar.
  // I think this should be removed, but am not sure whether there should
  // be some other replacement or a warning, Therefore, I leave it for now.
  // April 2009, mischi
  errnum := img_convert(PAVPicture(fAVFrameRGB), PIXEL_FMT_FFMPEG,
            PAVPicture(fAVFrame), fCodecContext^.pix_fmt,
            fCodecContext^.width, fCodecContext^.height);
  {$ENDIF}

  if (errnum < 0) then
  begin
    Log.LogError('Image conversion failed', 'TVideoPlayback_ffmpeg.GetFrame');
    Exit;
  end;

  if (not fFrameTexValid) then
    fFrameTexValid := true;

  Result := PByteArray(fAVFrameRGB^.data[0]);
end;

procedure TVideoDecodeStream_FFmpeg.SetLoop(Enable: boolean);
begin
  fLoop := Enable;
  fLoopTime := 0;
end;

function TVideoDecodeStream_FFmpeg.GetLoop(): boolean;
begin
  Result := fLoop;
end;

{**
 * Sets the stream's position.
 * The stream is set to the first keyframe with timestamp <= Time.
 * Note that fFrameTime is set to Time no matter if the actual position seeked to is
 * at Time or the time of a preceding keyframe. fFrameTime will be updated to the
 * actual frame time when GetFrame() is called the next time.
 * @param Time new position in seconds
 *}
procedure TVideoDecodeStream_FFmpeg.SetPosition(Time: real);
var
  SeekFlags: integer;
begin
  if not fOpened then
    Exit;

  if (Time < 0) then
    Time := 0;

  // TODO: handle fLoop-times
  //Time := Time mod VideoDuration;

  // Do not use the AVSEEK_FLAG_ANY here. It will seek to any frame, even
  // non keyframes (P-/B-frames). It will produce corrupted video frames as
  // FFmpeg does not use the information of the preceding I-frame.
  // The picture might be gray or green until the next keyframe occurs.
  // Instead seek the first keyframe smaller than the requested time
  // (AVSEEK_FLAG_BACKWARD). As this can be some seconds earlier than the
  // requested time, let the sync in GetFrame() do its job.
  SeekFlags := AVSEEK_FLAG_BACKWARD;

  fFrameTime := Time;
  fEOF := false;
  fFrameTexValid := false;

  if (av_seek_frame(fFormatContext,
     fStreamIndex,
     Round(Time / av_q2d(fStream^.time_base)),
     SeekFlags) < 0) then
  begin
    Log.LogError('av_seek_frame() failed', 'TVideoPlayback_ffmpeg.SetPosition');
    Exit;
  end;

  avcodec_flush_buffers(fCodecContext);
end;

function  TVideoDecodeStream_FFmpeg.GetPosition: real;
begin
  Result := fFrameTime;
end;

function TVideoDecodeStream_FFmpeg.GetFrameWidth(): integer;
begin
  Result := fCodecContext^.width;
end;

function TVideoDecodeStream_FFmpeg.GetFrameHeight(): integer;
begin
  Result := fCodecContext^.height;
end;

function TVideoDecodeStream_FFmpeg.GetFrameAspect(): real;
begin
  Result := fAspect;
end;

initialization
  MediaManager.Add(TVideoDecoder_FFmpeg.Create);

end.
