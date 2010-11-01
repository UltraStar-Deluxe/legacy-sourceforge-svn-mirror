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

initialization
  MediaManager.add(TAudioConverter_SDL.Create);

end.