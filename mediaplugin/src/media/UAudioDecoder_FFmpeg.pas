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

uses
  UMediaPlugin,
  UMusic,
  UPath;

type
  TAudioDecoder_FFmpeg = class(TInterfacedObject, IAudioDecoder)
    private
      fPluginInfo: PMediaPluginInfo;
    public
      constructor Create(Info: PMediaPluginInfo);

      function GetName: string;

      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      function Open(const Filename: IPath): TAudioDecodeStream;
  end;

implementation

uses
  ctypes,
  Classes,
  SysUtils,
  UIni,
  UMain,
  ULog;

type
  TFFmpegDecodeStream = class(TAudioDecodeStream)
    private
      fAudioDecoderInfo: PAudioDecoderInfo;
      fFilename: IPath;
      fStream: PAudioDecodeStream;
      fFormatInfo: TAudioFormatInfo;

    public
      constructor Create(Info: PAudioDecoderInfo);
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

{ TFFmpegDecodeStream }

constructor TFFmpegDecodeStream.Create(Info: PAudioDecoderInfo);
begin
  inherited Create();
  fAudioDecoderInfo := Info;
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

  fStream := fAudioDecoderInfo.open(PChar(Filename.ToUTF8()));
  if (fStream = nil) then
    Exit;

  fFilename := Filename;

  fAudioDecoderInfo.getAudioFormatInfo(fStream, Info);
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
    fAudioDecoderInfo.close(fStream);
    fStream := nil;
  end;
end;

function TFFmpegDecodeStream.GetLength(): real;
begin
  Result := fAudioDecoderInfo.getLength(fStream);
end;

function TFFmpegDecodeStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := fFormatInfo;
end;

function TFFmpegDecodeStream.IsEOF(): boolean;
begin
  Result := fAudioDecoderInfo.isEOF(fStream);
end;

function TFFmpegDecodeStream.IsError(): boolean;
begin
  Result := fAudioDecoderInfo.isError(fStream);
end;

function TFFmpegDecodeStream.GetPosition(): real;
begin
  Result := fAudioDecoderInfo.getPosition(fStream);
end;

procedure TFFmpegDecodeStream.SetPosition(Time: real);
begin
  fAudioDecoderInfo.setPosition(fStream, Time);
end;

function TFFmpegDecodeStream.GetLoop(): boolean;
begin
  Result := fAudioDecoderInfo.getLoop(fStream);
end;

procedure TFFmpegDecodeStream.SetLoop(Enabled: boolean);
begin
  fAudioDecoderInfo.setLoop(fStream, Enabled);
end;

function TFFmpegDecodeStream.ReadData(Buffer: PByteArray; BufferSize: integer): integer;
begin
  Result := fAudioDecoderInfo.readData(fStream, PCUint8(Buffer), BufferSize);
end;


{ TAudioDecoder_FFmpeg }

constructor TAudioDecoder_FFmpeg.Create(Info: PMediaPluginInfo);
begin
  inherited Create();
  fPluginInfo := Info;
end;

function TAudioDecoder_FFmpeg.GetName: String;
begin
  Result := 'Plugin:AudioDecoder:' + fPluginInfo.name;
end;

function TAudioDecoder_FFmpeg.InitializeDecoder: boolean;
begin
  //fPluginInfo.initialize();
  Result := true;
end;

function TAudioDecoder_FFmpeg.FinalizeDecoder(): boolean;
begin
  //fPluginInfo.finalize();
  Result := true;
end;

function TAudioDecoder_FFmpeg.Open(const Filename: IPath): TAudioDecodeStream;
var
  Stream: TFFmpegDecodeStream;
begin
  Result := nil;

  Stream := TFFmpegDecodeStream.Create(fPluginInfo.audioDecoder);
  if (not Stream.Open(Filename)) then
  begin
    Stream.Free;
    Exit;
  end;

  Result := Stream;
end;

end.
