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
  TVideoDecoder_FFmpeg = class( TInterfacedObject, IVideoDecoder )
  private
    fPluginInfo: PMediaPluginInfo;
  public
    constructor Create(Info: PMediaPluginInfo);

    function GetName: String;

    function InitializeDecoder(): boolean;
    function FinalizeDecoder: boolean;

    function Open(const FileName: IPath): TVideoDecodeStream;
  end;

implementation

uses
  SysUtils,
  Math,
  ctypes,
  UCommon,
  UConfig,
  ULog;

type
  TVideoDecodeStream_FFmpeg = class (TVideoDecodeStream)
  private
    fVideoDecoderInfo: PVideoDecoderInfo;
    fFilename: IPath;
    fStream: PVideoDecodeStream;

  public
    constructor Create(Info: PVideoDecoderInfo);
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

{*------------------------------------------------------------------------------
 * TVideoPlayback_ffmpeg
 *------------------------------------------------------------------------------}

constructor TVideoDecoder_FFmpeg.Create(Info: PMediaPluginInfo);
begin
  inherited Create();
  fPluginInfo := Info;
end;

function  TVideoDecoder_FFmpeg.GetName: String;
begin
  Result := 'Plugin:VideoDecoder:' + fPluginInfo.name;
end;

function TVideoDecoder_FFmpeg.InitializeDecoder(): boolean;
begin
  //fPluginInfo.initialize();
  Result := true;
end;

function TVideoDecoder_FFmpeg.FinalizeDecoder(): boolean;
begin
  //fPluginInfo.finalize();
  Result := true;
end;

function TVideoDecoder_FFmpeg.Open(const FileName : IPath): TVideoDecodeStream;
var
  Stream: TVideoDecodeStream_FFmpeg;
begin
  Result := nil;

  Stream := TVideoDecodeStream_FFmpeg.Create(fPluginInfo.videoDecoder);
  if (not Stream.Open(FileName)) then
  begin
    Stream.Free;
    Exit;
  end;

  Result := Stream
end;


{* TVideoDecoder_FFmpeg *}

constructor TVideoDecodeStream_FFmpeg.Create(Info: PVideoDecoderInfo);
begin
  inherited Create();
  fVideoDecoderInfo := Info;
  fFilename := PATH_NONE;
end;

destructor TVideoDecodeStream_FFmpeg.Destroy;
begin
  Close();
  inherited;
end;

function TVideoDecodeStream_FFmpeg.Open(const FileName: IPath): boolean;
begin
  Result := false;

  Close();

  fStream := fVideoDecoderInfo.open(PChar(Filename.ToUTF8()));
  if (fStream = nil) then
    Exit;

  fFilename := Filename;

  Result := true;
end;

procedure TVideoDecodeStream_FFmpeg.Close;
begin
  Self.fFilename := PATH_NONE;
  if (fStream <> nil) then
  begin
    fVideoDecoderInfo.close(fStream);
    fStream := nil;
  end;
end;

function TVideoDecodeStream_FFmpeg.GetFrame(Time: Extended): PByteArray;
begin
  Result := PByteArray(fVideoDecoderInfo.getFrame(fStream, Time));
end;

procedure TVideoDecodeStream_FFmpeg.SetLoop(Enable: boolean);
begin
  fVideoDecoderInfo.setLoop(fStream, Enable);
end;

function TVideoDecodeStream_FFmpeg.GetLoop(): boolean;
begin
  Result := fVideoDecoderInfo.getLoop(fStream);
end;

procedure TVideoDecodeStream_FFmpeg.SetPosition(Time: real);
begin
  fVideoDecoderInfo.setPosition(fStream, Time);
end;

function  TVideoDecodeStream_FFmpeg.GetPosition: real;
begin
  Result := fVideoDecoderInfo.getPosition(fStream);
end;

function TVideoDecodeStream_FFmpeg.GetFrameWidth(): integer;
begin
  Result := fVideoDecoderInfo.getFrameWidth(fStream);
end;

function TVideoDecodeStream_FFmpeg.GetFrameHeight(): integer;
begin
  Result := fVideoDecoderInfo.getFrameHeight(fStream);
end;

function TVideoDecodeStream_FFmpeg.GetFrameAspect(): real;
begin
  Result := fVideoDecoderInfo.getFrameAspect(fStream);
end;

end.
