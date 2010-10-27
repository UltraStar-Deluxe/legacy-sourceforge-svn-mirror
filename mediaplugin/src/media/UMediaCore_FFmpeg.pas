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

unit UMediaCore_FFmpeg;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  ctypes,
  sdl,
  avcodec,
  avformat,
  avutil,
  avio,
  swscale,
  UMusic,
  ULog,
  UPath;

const
  STATUS_PACKET: PChar = 'STATUS_PACKET';
const
  PKT_STATUS_FLAG_EOF     = 1; // signal end-of-file
  PKT_STATUS_FLAG_FLUSH   = 2; // request the decoder to flush its avcodec decode buffers
  PKT_STATUS_FLAG_ERROR   = 3; // signal an error state
  PKT_STATUS_FLAG_EMPTY   = 4; // request the decoder to output empty data (silence or black frames)

type
  TMediaCore_FFmpeg = class
    private
      AVCodecLock: PSDL_Mutex;
    public
      constructor Create();
      destructor Destroy(); override;
      class function GetInstance(): TMediaCore_FFmpeg;

      function GetErrorString(ErrorNum: integer): string;
      function FindStreamIDs(FormatCtx: PAVFormatContext; out FirstVideoStream, FirstAudioStream: integer ): boolean;
      function FindAudioStreamIndex(FormatCtx: PAVFormatContext): integer;
      function ConvertFFmpegToAudioFormat(FFmpegFormat: TSampleFormat; out Format: TAudioSampleFormat): boolean;
      procedure LockAVCodec();
      procedure UnlockAVCodec();
  end;

implementation

uses
  SysUtils,
  UConfig;

function FFmpegStreamOpen(h: PURLContext; filename: PChar; flags: cint): cint; cdecl; forward;
function FFmpegStreamRead(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl; forward;
function FFmpegStreamWrite(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl; forward;
function FFmpegStreamSeek(h: PURLContext; pos: int64; whence: cint): int64; cdecl; forward;
function FFmpegStreamClose(h: PURLContext): cint; cdecl; forward;

const
  UTF8FileProtocol: TURLProtocol = (
      name:      'ufile';
      url_open:  FFmpegStreamOpen;
      url_read:  FFmpegStreamRead;
      url_write: FFmpegStreamWrite;
      url_seek:  FFmpegStreamSeek;
      url_close: FFmpegStreamClose;
  );

var
  Instance: TMediaCore_FFmpeg;

procedure CheckVersions();
begin
end;

constructor TMediaCore_FFmpeg.Create();
begin
  inherited;

  CheckVersions();
  av_register_protocol(@UTF8FileProtocol);
  AVCodecLock := SDL_CreateMutex();
end;

destructor TMediaCore_FFmpeg.Destroy();
begin
  SDL_DestroyMutex(AVCodecLock);
  inherited;
end;

class function TMediaCore_FFmpeg.GetInstance(): TMediaCore_FFmpeg;
begin
  if (not Assigned(Instance)) then
    Instance := TMediaCore_FFmpeg.Create();
  Result := Instance;
end;

procedure TMediaCore_FFmpeg.LockAVCodec();
begin
  SDL_mutexP(AVCodecLock);
end;

procedure TMediaCore_FFmpeg.UnlockAVCodec();
begin
  SDL_mutexV(AVCodecLock);
end;

function TMediaCore_FFmpeg.GetErrorString(ErrorNum: integer): string;
begin
  case ErrorNum of
    AVERROR_IO:           Result := 'AVERROR_IO';
    AVERROR_NUMEXPECTED:  Result := 'AVERROR_NUMEXPECTED';
    AVERROR_INVALIDDATA:  Result := 'AVERROR_INVALIDDATA';
    AVERROR_NOMEM:        Result := 'AVERROR_NOMEM';
    AVERROR_NOFMT:        Result := 'AVERROR_NOFMT';
    AVERROR_NOTSUPP:      Result := 'AVERROR_NOTSUPP';
    AVERROR_NOENT:        Result := 'AVERROR_NOENT';
    AVERROR_PATCHWELCOME: Result := 'AVERROR_PATCHWELCOME';
    else                  Result := 'AVERROR_#'+inttostr(ErrorNum);
  end;
end;

{
  @param(FormatCtx is a PAVFormatContext returned from av_open_input_file )
  @param(FirstVideoStream is an OUT value of type integer, this is the index of the video stream)
  @param(FirstAudioStream is an OUT value of type integer, this is the index of the audio stream)
  @returns(@true on success, @false otherwise)
}
function TMediaCore_FFmpeg.FindStreamIDs(FormatCtx: PAVFormatContext; out FirstVideoStream, FirstAudioStream: integer): boolean;
var
  i: integer;
  Stream: PAVStream;
begin
  // find the first video stream
  FirstAudioStream := -1;
  FirstVideoStream := -1;

  for i := 0 to FormatCtx.nb_streams-1 do
  begin
    Stream := FormatCtx.streams[i];

{$IF LIBAVCODEC_VERSION < 52064000} // < 52.64.0
    if (Stream.codec.codec_type = CODEC_TYPE_VIDEO) and
       (FirstVideoStream < 0) then
    begin
      FirstVideoStream := i;
    end;

    if (Stream.codec.codec_type = CODEC_TYPE_AUDIO) and
       (FirstAudioStream < 0) then
    begin
      FirstAudioStream := i;
    end;
  end;
{$ELSE}
    if (Stream.codec.codec_type = AVMEDIA_TYPE_VIDEO) and
       (FirstVideoStream < 0) then
    begin
      FirstVideoStream := i;
    end;

    if (Stream.codec.codec_type = AVMEDIA_TYPE_AUDIO) and
       (FirstAudioStream < 0) then
    begin
      FirstAudioStream := i;
    end;
  end;
{$IFEND}

  // return true if either an audio- or video-stream was found
  Result := (FirstAudioStream > -1) or
            (FirstVideoStream > -1) ;
end;

function TMediaCore_FFmpeg.FindAudioStreamIndex(FormatCtx: PAVFormatContext): integer;
var
  i: integer;
  StreamIndex: integer;
  Stream: PAVStream;
begin
  // find the first audio stream
  StreamIndex := -1;

  for i := 0 to FormatCtx^.nb_streams-1 do
  begin
    Stream := FormatCtx^.streams[i];

{$IF LIBAVCODEC_VERSION < 52064000} // < 52.64.0
    if (Stream.codec^.codec_type = CODEC_TYPE_AUDIO) then
{$ELSE}
    if (Stream.codec^.codec_type = AVMEDIA_TYPE_AUDIO) then
{$IFEND}
    begin
      StreamIndex := i;
      Break;
    end;
  end;

  Result := StreamIndex;
end;

function TMediaCore_FFmpeg.ConvertFFmpegToAudioFormat(FFmpegFormat: TSampleFormat; out Format: TAudioSampleFormat): boolean;
begin
  case FFmpegFormat of
    SAMPLE_FMT_U8:  Format := asfU8;
    SAMPLE_FMT_S16: Format := asfS16;
    SAMPLE_FMT_S32: Format := asfS32;
    SAMPLE_FMT_FLT: Format := asfFloat;
    SAMPLE_FMT_DBL: Format := asfDouble;
    else begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;


{**
 * UTF-8 Filename wrapper based on:
 * http://www.mail-archive.com/libav-user@mplayerhq.hu/msg02460.html
 *}

function FFmpegStreamOpen(h: PURLContext; filename: PChar; flags: cint): cint; cdecl;
var
  Stream: TStream;
  Mode: word;
  ProtPrefix: string;
  FilePath: IPath;
begin
  // check for protocol prefix ('ufile:') and strip it
  ProtPrefix := Format('%s:', [UTF8FileProtocol.name]);
  if (StrLComp(filename, PChar(ProtPrefix), Length(ProtPrefix)) = 0) then
  begin
    Inc(filename, Length(ProtPrefix));
  end;

  FilePath := Path(filename);

  if ((flags and URL_RDWR) <> 0) then
    Mode := fmCreate
  else if ((flags and URL_WRONLY) <> 0) then
    Mode := fmCreate // TODO: fmCreate is Read+Write -> reopen with fmOpenWrite
  else
    Mode := fmOpenRead or fmShareDenyWrite;

  Result := 0;

  try
    Stream := TBinaryFileStream.Create(FilePath, Mode);
    h.priv_data := Stream;
  except
    Result := AVERROR_NOENT;
  end;
end;

function FFmpegStreamRead(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;
var
  Stream: TStream;
begin
  Stream := TStream(h.priv_data);
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamRead on nil');
  try
    Result := Stream.Read(buf[0], size);
  except
    Result := -1;
  end;
end;

function FFmpegStreamWrite(h: PURLContext; buf: PByteArray; size: cint): cint; cdecl;
var
  Stream: TStream;
begin
  Stream := TStream(h.priv_data);
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamWrite on nil');
  try
    Result := Stream.Write(buf[0], size);
  except
    Result := -1;
  end;
end;

function FFmpegStreamSeek(h: PURLContext; pos: int64; whence: cint): int64; cdecl;
var
  Stream : TStream;
  Origin : TSeekOrigin;
begin
  Stream := TStream(h.priv_data);
  if (Stream = nil) then
    raise EInvalidContainer.Create('FFmpegStreamSeek on nil');
  case whence of
    0 {SEEK_SET}: Origin := soBeginning;
    1 {SEEK_CUR}: Origin := soCurrent;
    2 {SEEK_END}: Origin := soEnd;
    AVSEEK_SIZE: begin
      Result := Stream.Size;
      Exit;
    end
  else
    Origin := soBeginning;
  end;
  Result := Stream.Seek(pos, Origin);
end;

function FFmpegStreamClose(h: PURLContext): cint; cdecl;
var
  Stream : TStream;
begin
  Stream := TStream(h.priv_data);
  Stream.Free;
  Result := 0;
end;

end.
