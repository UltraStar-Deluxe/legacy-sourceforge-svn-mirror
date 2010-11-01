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

unit UAudioConverter;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  ULog,
  ctypes,
  {$IFDEF UseSRCResample}
  samplerate,
  {$ENDIF}
  UMediaPlugin,
  UMediaCore_SDL,
  sdl,
  SysUtils,
  Math;

type
  TAudioConverter_SDL = class(TInterfacedObject, IAudioConverter)
    public
      function GetName(): string;
      function GetPriority(): integer;
      function Init(): boolean;
      function Finalize(): boolean;

      function Open(SrcFormatInfo: TAudioFormatInfo;
                    DstFormatInfo: TAudioFormatInfo): TAudioConvertStream;
  end;

  TAudioConvertStream_SDL = class(TAudioConvertStream)
    private
      cvt: TSDL_AudioCVT;
      function Init(): boolean;
    protected
      constructor Create(SrcFormatInfo: TAudioFormatInfo;
                    DstFormatInfo: TAudioFormatInfo);
    public
      destructor Destroy(); override;

      class function Open(SrcFormatInfo: TAudioFormatInfo;
                    DstFormatInfo: TAudioFormatInfo): TAudioConvertStream_SDL;

      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; override;
      function GetOutputBufferSize(InputSize: integer): integer; override;
      function GetRatio(): double; override;
  end;

  {$IFDEF UseFFmpegResample}
  TAudioConverterPlugin = class(TAudioConverter)
    private
      fPluginInfo: PMediaPluginInfo;
      fAudioConverter: PAudioConverterInfo;
      fStream: PAudioConvertStream;
    public
      function Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean; override;
      destructor Destroy(); override;

      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; override;
      function GetOutputBufferSize(InputSize: integer): integer; override;
      function GetRatio(): double; override;
  end;
  {$ENDIF}

  {$IFDEF UseSRCResample}
  TAudioConverter_SRC = class(TAudioConverter)
    private
      ConverterState: PSRC_STATE;
      ConversionData: SRC_DATA;
      FormatConverter: TAudioConverter;
    public
      function Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean; override;
      destructor Destroy(); override;

      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; override;
      function GetOutputBufferSize(InputSize: integer): integer; override;
      function GetRatio(): double; override;
  end;

  // Note: SRC (=libsamplerate) provides several converters with different quality
  // speed trade-offs. The SINC-types are slow but offer best quality.
  // The SRC_SINC_* converters are too slow for realtime conversion,
  // (SRC_SINC_FASTEST is approx. ten times slower than SRC_LINEAR) resulting
  // in audible clicks and pops.
  // SRC_LINEAR is very fast and should have a better quality than SRC_ZERO_ORDER_HOLD
  // because it interpolates the samples. Normal "non-audiophile" users should not
  // be able to hear a difference between the SINC_* ones and LINEAR. Especially
  // if people sing along with the song.
  // But FFmpeg might offer a better quality/speed ratio than SRC_LINEAR.
  const
    SRC_CONVERTER_TYPE = SRC_LINEAR;
  {$ENDIF}

implementation

{ TAudioConverter_SDL }

function TAudioConverter_SDL.GetName(): string;
begin
  Result := 'AudioConverter_SDL';
end;

function TAudioConverter_SDL.GetPriority(): integer;
begin
  {* Notes:
   *  - 44.1kHz to 48kHz conversion or vice versa is not supported
   *    by SDL 1.2 (will be introduced in 1.3).
   *    No conversion takes place in this cases.
   *    This is because SDL just converts differences in powers of 2.
   *    So the result might not be that accurate.
   *    This IS audible (voice to high/low) and it needs good synchronization
   *    with the video or the lyrics timer.
   *  - float<->int16 conversion is not supported (will be part of 1.3) and
   *    SDL (<1.3) is not capable of handling floats at all.
   *  -> Using FFmpeg or libsamplerate for resampling is preferred.
   *     Use SDL for channel and format conversion only.
   *}
  Result := 0;
end;

function TAudioConverter_SDL.Init(): boolean;
begin
  Result := true;
end;

function TAudioConverter_SDL.Finalize(): boolean;
begin
  Result := true;
end;

function TAudioConverter_SDL.Open(SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo): TAudioConvertStream;
begin
  Result := TAudioConvertStream_SDL.Open(SrcFormatInfo, DstFormatInfo);
end;

{ TAudioConvertStream_SDL }

constructor TAudioConvertStream_SDL.Create(SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo);
begin
  inherited Create(SrcFormatInfo, DstFormatInfo);
end;

destructor TAudioConvertStream_SDL.Destroy();
begin
  inherited;
end;

class function TAudioConvertStream_SDL.Open(SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo): TAudioConvertStream_SDL;
var
  Stream: TAudioConvertStream_SDL;
begin
  Result := nil;
  Stream := TAudioConvertStream_SDL.Create(SrcFormatInfo, DstFormatInfo);
  if (not Stream.Init()) then
  begin
    Stream.Free;
    Exit;
  end;
  Result := Stream;
end;

function TAudioConvertStream_SDL.Init(): boolean;
var
  srcFormat: UInt16;
  dstFormat: UInt16;
begin
  Result := false;

  if (not ConvertAudioFormatToSDL(srcFormatInfo.Format, srcFormat) or
      not ConvertAudioFormatToSDL(dstFormatInfo.Format, dstFormat)) then
  begin
    Log.LogError('Audio-format not supported by SDL', 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  if (SDL_BuildAudioCVT(@cvt,
    srcFormat, srcFormatInfo.Channels, Round(srcFormatInfo.SampleRate),
    dstFormat, dstFormatInfo.Channels, Round(dstFormatInfo.SampleRate)) = -1) then
  begin
    Log.LogError(SDL_GetError(), 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  Result := true;
end;

(*
 * Returns the size of the output buffer. This might be bigger than the actual
 * size of resampled audio data.
 *)
function TAudioConvertStream_SDL.GetOutputBufferSize(InputSize: integer): integer;
begin
  // Note: len_ratio must not be used here. Even if the len_ratio is 1.0, len_mult might be 2.
  // Example: 44.1kHz/mono to 22.05kHz/stereo -> len_ratio=1, len_mult=2
  Result := InputSize * cvt.len_mult;
end;

function TAudioConvertStream_SDL.GetRatio(): double;
begin
  Result := cvt.len_ratio;
end;

function TAudioConvertStream_SDL.Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer;
begin
  Result := -1;

  if (InputSize <= 0) then
  begin
    // avoid div-by-zero problems
    if (InputSize = 0) then
      Result := 0;
    Exit;
  end;

  // OutputBuffer is always bigger than or equal to InputBuffer
  Move(InputBuffer[0], OutputBuffer[0], InputSize);
  cvt.buf := PUint8(OutputBuffer);
  cvt.len := InputSize;
  if (SDL_ConvertAudio(@cvt) = -1) then
    Exit;

  Result := cvt.len_cvt;
end;


{$IFDEF UseFFmpegResample}

const
{$IFDEF MSWINDOWS}
  ffmpegPlugin = 'ffmpeg_playback.dll';
{$ENDIF}
{$IFDEF LINUX}
  ffmpegPlugin = 'ffmpeg_playback';
{$ENDIF}
{$IFDEF DARWIN}
  ffmpegPlugin = 'ffmpeg_playback.dylib';
  {$linklib ffmpegPlugin}
{$ENDIF}

function Plugin_register(core: PMediaPluginCore): PMediaPluginInfo;
  cdecl; external ffmpegPlugin;

function TAudioConverterPlugin.Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean;
var
  CSrcFormatInfo: TCAudioFormatInfo;
  CDstFormatInfo: TCAudioFormatInfo;
begin
  inherited Init(SrcFormatInfo, DstFormatInfo);

  Result := false;
  fPluginInfo := Plugin_register(MediaPluginCore);
  fPluginInfo.initialize();
  fAudioConverter := fPluginInfo.audioConverter;

  AudioFormatInfoToCStruct(SrcFormatInfo, CSrcFormatInfo);
  AudioFormatInfoToCStruct(DstFormatInfo, CDstFormatInfo);
  fStream := fAudioConverter.open(@CSrcFormatInfo, @CDstFormatInfo);
  if (fStream = nil) then
  begin
    Log.LogError('fAudioConverter.open() failed', 'TAudioConverterPlugin.Init');
    Exit;
  end;

  Result := true;
end;

destructor TAudioConverterPlugin.Destroy();
begin
  if (fStream <> nil) then
    fAudioConverter.close(fStream);
  inherited;
end;

function TAudioConverterPlugin.Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer;
begin
  Result := fAudioConverter.convert(fStream,
      PCuint8(InputBuffer), PCuint8(OutputBuffer), @InputSize);
end;

function TAudioConverterPlugin.GetOutputBufferSize(InputSize: integer): integer;
begin
  Result := fAudioConverter.getOutputBufferSize(fStream, InputSize);
end;

function TAudioConverterPlugin.GetRatio(): double;
begin
  Result := fAudioConverter.getRatio(fStream);
end;

{$ENDIF}


{$IFDEF UseSRCResample}

function TAudioConverter_SRC.Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean;
var
  error: integer;
  TempSrcFormatInfo: TAudioFormatInfo;
  TempDstFormatInfo: TAudioFormatInfo;
begin
  inherited Init(SrcFormatInfo, DstFormatInfo);

  Result := false;

  FormatConverter := nil;

  // SRC does not handle channel or format conversion
  if ((SrcFormatInfo.Channels <> DstFormatInfo.Channels) or
      not (SrcFormatInfo.Format in [asfS16, asfFloat])) then
  begin
    // SDL can not convert to float, so we have to convert to SInt16 first
    TempSrcFormatInfo := TAudioFormatInfo.Create(
        SrcFormatInfo.Channels, SrcFormatInfo.SampleRate, SrcFormatInfo.Format);
    TempDstFormatInfo := TAudioFormatInfo.Create(
        DstFormatInfo.Channels, SrcFormatInfo.SampleRate, asfS16);

    // init format/channel conversion
    FormatConverter := TAudioConverter_SDL.Create();
    if (not FormatConverter.Init(TempSrcFormatInfo, TempDstFormatInfo)) then
    begin
      Log.LogError('Unsupported input format', 'TAudioConverter_SRC.Init');
      FormatConverter.Free;
      // exit after the format-info is freed
    end;

    // this info was copied so we do not need it anymore 
    TempSrcFormatInfo.Free;
    TempDstFormatInfo.Free;

    // leave if the format is not supported
    if (not assigned(FormatConverter)) then
      Exit;

    // adjust our copy of the input audio-format for SRC conversion
    Self.SrcFormatInfo.Channels := DstFormatInfo.Channels;
    Self.SrcFormatInfo.Format := asfS16;
  end;

  if ((DstFormatInfo.Format <> asfS16) and
      (DstFormatInfo.Format <> asfFloat)) then
  begin
    Log.LogError('Unsupported output format', 'TAudioConverter_SRC.Init');
    Exit;
  end;

  ConversionData.src_ratio := DstFormatInfo.SampleRate / SrcFormatInfo.SampleRate;
  if (src_is_valid_ratio(ConversionData.src_ratio) = 0) then
  begin
    Log.LogError('Invalid samplerate ratio', 'TAudioConverter_SRC.Init');
    Exit;
  end;

  ConverterState := src_new(SRC_CONVERTER_TYPE, DstFormatInfo.Channels, @error);
  if (ConverterState = nil) then
  begin
    Log.LogError('src_new() failed: ' + src_strerror(error), 'TAudioConverter_SRC.Init');
    Exit;
  end;

  Result := true;
end;

destructor TAudioConverter_SRC.Destroy();
begin
  if (ConverterState <> nil) then
    src_delete(ConverterState);
  FormatConverter.Free;
  inherited;
end;

function TAudioConverter_SRC.Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer;
var
  FloatInputBuffer: PSingle;
  FloatOutputBuffer: PSingle;
  TempBuffer: PByteArray;
  TempSize: integer;
  NumSamples: integer;
  OutputSize: integer;
  error: integer;
begin
  Result := -1;

  TempBuffer := nil;

  // format conversion with external converter (to correct number of channels and format)
  if (assigned(FormatConverter)) then
  begin
    TempSize := FormatConverter.GetOutputBufferSize(InputSize);
    GetMem(TempBuffer, TempSize);
    InputSize := FormatConverter.Convert(InputBuffer, TempBuffer, InputSize);
    InputBuffer := TempBuffer;
  end;

  if (InputSize <= 0) then
  begin
    // avoid div-by-zero problems
    if (InputSize = 0) then
      Result := 0;
    if (TempBuffer <> nil) then
      FreeMem(TempBuffer);
    Exit;
  end;

  if (SrcFormatInfo.Format = asfFloat) then
  begin
    FloatInputBuffer := PSingle(InputBuffer);
  end else begin
    NumSamples := InputSize div AudioSampleSize[SrcFormatInfo.Format];
    GetMem(FloatInputBuffer, NumSamples * SizeOf(Single));
    src_short_to_float_array(PCshort(InputBuffer), PCfloat(FloatInputBuffer), NumSamples);
  end;

  // calculate approx. output size
  OutputSize := Ceil(InputSize * ConversionData.src_ratio);

  if (DstFormatInfo.Format = asfFloat) then
  begin
    FloatOutputBuffer := PSingle(OutputBuffer);
  end else begin
    NumSamples := OutputSize div AudioSampleSize[DstFormatInfo.Format];
    GetMem(FloatOutputBuffer, NumSamples * SizeOf(Single));
  end;

  with ConversionData do
  begin
    data_in := PCFloat(FloatInputBuffer);
    input_frames := InputSize div SrcFormatInfo.FrameSize;
    data_out := PCFloat(FloatOutputBuffer);
    output_frames := OutputSize div DstFormatInfo.FrameSize;
    // TODO: set this to 1 at end of file-playback
    end_of_input := 0;
  end;

  error := src_process(ConverterState, @ConversionData);
  if (error <> 0) then
  begin
    Log.LogError(src_strerror(error), 'TAudioConverter_SRC.Convert');
    if (SrcFormatInfo.Format <> asfFloat) then
      FreeMem(FloatInputBuffer);
    if (DstFormatInfo.Format <> asfFloat) then
      FreeMem(FloatOutputBuffer);
    if (TempBuffer <> nil) then
      FreeMem(TempBuffer);
    Exit;
  end;

  if (SrcFormatInfo.Format <> asfFloat) then
    FreeMem(FloatInputBuffer);

  if (DstFormatInfo.Format <> asfFloat) then
  begin
    NumSamples := ConversionData.output_frames_gen * DstFormatInfo.Channels;
    src_float_to_short_array(PCfloat(FloatOutputBuffer), PCshort(OutputBuffer), NumSamples);
    FreeMem(FloatOutputBuffer);
  end;

  // free format conversion buffer if used
  if (TempBuffer <> nil) then
    FreeMem(TempBuffer);

  if (assigned(FormatConverter)) then
    InputSize := ConversionData.input_frames_used * FormatConverter.SrcFormatInfo.FrameSize
  else
    InputSize := ConversionData.input_frames_used * SrcFormatInfo.FrameSize;

  // set result to output size according to SRC
  Result := ConversionData.output_frames_gen * DstFormatInfo.FrameSize;
end;

function TAudioConverter_SRC.GetOutputBufferSize(InputSize: integer): integer;
begin
  Result := Ceil(InputSize * GetRatio());
end;

function TAudioConverter_SRC.GetRatio(): double;
begin
  // if we need additional channel/format conversion, use this ratio
  if (assigned(FormatConverter)) then
    Result := FormatConverter.GetRatio()
  else
    Result := 1.0;

  // now the SRC ratio (Note: the format might change from SInt16 to float)
  Result := Result *
            ConversionData.src_ratio *
            (DstFormatInfo.FrameSize / SrcFormatInfo.FrameSize);
end;

{$ENDIF}

initialization
  MediaManager.add(TAudioConverter_SDL.Create);

end.