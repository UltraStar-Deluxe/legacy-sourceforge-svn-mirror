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

unit UAudioDecoder_FFmpeg;

(*******************************************************************************
 *
 * This unit is primarily based upon -
 *   http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
 *
 *   and tutorial03.c
 *
 *   http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html
 *
 *******************************************************************************)

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

implementation

uses
  ctypes,
  Classes,
  SysUtils,
  UMediaPlugin,
  UMusic,
  UIni,
  UMain,
  ULog,
  UPath;

type
  TFFmpegDecodeStream = class(TAudioDecodeStream)
    private
      fFilename: IPath;
      fStream: PDecodeStream;
      fFormatInfo: TAudioFormatInfo;

    public
      constructor Create();
      destructor Destroy(); override;

      function Open(const Filename: IPath): boolean;
      procedure Close();                     override;

      function GetLength(): real;            override;
      function GetAudioFormatInfo(): TAudioFormatInfo; override;
      function GetPosition: real;            override;
      procedure SetPosition(Time: real);     override;
      function GetLoop(): boolean;           override;
      procedure SetLoop(Enabled: boolean);   override;
      function IsEOF(): boolean;             override;
      function IsError(): boolean;           override;

      function ReadData(Buffer: PByteArray; BufferSize: integer): integer; override;
  end;

type
  TAudioDecoder_FFmpeg = class(TInterfacedObject, IAudioDecoder)
    public
      function GetName: string;

      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      function Open(const Filename: IPath): TAudioDecodeStream;
  end;

{ TFFmpegDecodeStream }

constructor TFFmpegDecodeStream.Create();
begin
  inherited Create();
  fFilename := PATH_NONE;
end;

{*
 * Frees the decode-stream data.
 *}
destructor TFFmpegDecodeStream.Destroy();
begin
  Close();
  inherited;
end;

function TFFmpegDecodeStream.Open(const Filename: IPath): boolean;
var
  Info: TCAudioFormatInfo;
begin
  Result := false;

  Close();

  fStream := DecodeStream_open(PChar(Filename.ToUTF8()));
  if (fStream = nil) then
    Exit;

  fFilename := Filename;

  DecodeStream_getAudioFormatInfo(fStream, Info);
  fFormatInfo := TAudioFormatInfo.Create(
    Info.channels,
    Info.sampleRate,
    TAudioSampleFormat(Info.format)
  );

  Result := true;
end;

procedure TFFmpegDecodeStream.Close();
begin
  Self.fFilename := PATH_NONE;
  if (fStream <> nil) then
  begin
    DecodeStream_close(fStream);
    fStream := nil;
  end;
end;

function TFFmpegDecodeStream.GetLength(): real;
begin
  Result := DecodeStream_getLength(fStream);
end;

function TFFmpegDecodeStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := fFormatInfo;
end;

function TFFmpegDecodeStream.IsEOF(): boolean;
begin
  Result := DecodeStream_isEOF(fStream);
end;

function TFFmpegDecodeStream.IsError(): boolean;
begin
  Result := DecodeStream_isError(fStream);
end;

function TFFmpegDecodeStream.GetPosition(): real;
begin
  Result := DecodeStream_getPosition(fStream);
end;

procedure TFFmpegDecodeStream.SetPosition(Time: real);
begin
  DecodeStream_setPosition(fStream, Time);
end;

function TFFmpegDecodeStream.GetLoop(): boolean;
begin
  Result := DecodeStream_getLoop(fStream);
end;

procedure TFFmpegDecodeStream.SetLoop(Enabled: boolean);
begin
  DecodeStream_setLoop(fStream, Enabled);
end;

function TFFmpegDecodeStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
begin
  Result := DecodeStream_readData(fStream, PCUint8(Buffer), BufferSize);
end;


{ TAudioDecoder_FFmpeg }

function TAudioDecoder_FFmpeg.GetName: String;
begin
  Result := 'FFmpeg_Decoder';
end;

function TAudioDecoder_FFmpeg.InitializeDecoder: boolean;
begin
  Result := Plugin_initialize(MediaPluginCore);
end;

function TAudioDecoder_FFmpeg.FinalizeDecoder(): boolean;
begin
  Result := true;
end;

function TAudioDecoder_FFmpeg.Open(const Filename: IPath): TAudioDecodeStream;
var
  Stream: TFFmpegDecodeStream;
begin
  Result := nil;

  Stream := TFFmpegDecodeStream.Create();
  if (not Stream.Open(Filename)) then
  begin
    Stream.Free;
    Exit;
  end;

  Result := Stream;
end;

initialization
  MediaManager.Add(TAudioDecoder_FFmpeg.Create);

end.
