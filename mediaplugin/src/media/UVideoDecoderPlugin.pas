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

unit UVideoDecoderPlugin;

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
  TVideoDecoderPlugin = class(TInterfacedObject, IMediaInterface, IVideoDecoder)
  private
    fPluginInfo: PMediaPluginInfo;
  public
    constructor Create(Info: PMediaPluginInfo);

    function GetName(): String;
    function GetPriority(): integer;

    function InitializeDecoder(): boolean;
    function FinalizeDecoder: boolean;

    function Open(const FileName: IPath; Format: TVideoFrameFormat): TVideoDecodeStream;
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
  TPluginVideoDecodeStream = class (TVideoDecodeStream)
  private
    fVideoDecoderInfo: PVideoDecoderInfo;
    fFilename: IPath;
    fStream: PVideoDecodeStream;

  public
    constructor Create(Info: PVideoDecoderInfo);
    destructor Destroy; override;

    function Open(const FileName: IPath; Format: TVideoFrameFormat): boolean; override;
    procedure Close; override;

    procedure SetLoop(Enable: boolean); override;
    function GetLoop(): boolean; override;

    procedure SetPosition(Time: real); override;
    function GetPosition: real; override;

    function GetFrameWidth(): integer; override;
    function GetFrameHeight(): integer; override;
    function GetFrameAspect(): real; override;
    function GetFrameFormat(): TVideoFrameFormat; override;

    function GetFrame(Time: Extended): PByteArray; override;
  end;

{ TVideoDecoderPlugin }

constructor TVideoDecoderPlugin.Create(Info: PMediaPluginInfo);
begin
  inherited Create();
  fPluginInfo := Info;
end;

function TVideoDecoderPlugin.GetName(): String;
begin
  Result := 'Plugin:VideoDecoder:' + fPluginInfo.name;
end;

function TVideoDecoderPlugin.GetPriority(): integer;
begin
  Result := fPluginInfo.videoDecoder.priority;
end;

function TVideoDecoderPlugin.InitializeDecoder(): boolean;
begin
  fPluginInfo.videoDecoder.init();
  Result := true;
end;

function TVideoDecoderPlugin.FinalizeDecoder(): boolean;
begin
  fPluginInfo.videoDecoder.finalize();
  Result := true;
end;

function TVideoDecoderPlugin.Open(const FileName: IPath; Format: TVideoFrameFormat): TVideoDecodeStream;
var
  Stream: TPluginVideoDecodeStream;
begin
  Result := nil;

  Stream := TPluginVideoDecodeStream.Create(fPluginInfo.videoDecoder);
  if (not Stream.Open(FileName, Format)) then
  begin
    Stream.Free;
    Exit;
  end;

  Result := Stream
end;


{ TPluginVideoDecodeStream }

constructor TPluginVideoDecodeStream.Create(Info: PVideoDecoderInfo);
begin
  inherited Create();
  fVideoDecoderInfo := Info;
  fFilename := PATH_NONE;
end;

destructor TPluginVideoDecodeStream.Destroy;
begin
  Close();
  inherited;
end;

function TPluginVideoDecodeStream.Open(const FileName: IPath; Format: TVideoFrameFormat): boolean;
var
  CFormat: TCVideoFrameFormat;
begin
  Result := false;

  Close();

  case Format of
  vffRGB:  CFormat := FRAME_FORMAT_RGB;
  vffRGBA: CFormat := FRAME_FORMAT_RGBA;
  vffBGR:  CFormat := FRAME_FORMAT_BGR;
  vffBGRA: CFormat := FRAME_FORMAT_BGRA;
  else     CFormat := FRAME_FORMAT_UNKNOWN;
  end;
  
  fStream := fVideoDecoderInfo.open(PChar(Filename.ToUTF8()), CFormat);
  if (fStream = nil) then
    Exit;

  fFilename := Filename;

  Result := true;
end;

procedure TPluginVideoDecodeStream.Close;
begin
  Self.fFilename := PATH_NONE;
  if (fStream <> nil) then
  begin
    fVideoDecoderInfo.close(fStream);
    fStream := nil;
  end;
end;

function TPluginVideoDecodeStream.GetFrame(Time: Extended): PByteArray;
begin
  Result := PByteArray(fVideoDecoderInfo.getFrame(fStream, Time));
end;

procedure TPluginVideoDecodeStream.SetLoop(Enable: boolean);
begin
  fVideoDecoderInfo.setLoop(fStream, Enable);
end;

function TPluginVideoDecodeStream.GetLoop(): boolean;
begin
  Result := fVideoDecoderInfo.getLoop(fStream);
end;

procedure TPluginVideoDecodeStream.SetPosition(Time: real);
begin
  fVideoDecoderInfo.setPosition(fStream, Time);
end;

function  TPluginVideoDecodeStream.GetPosition: real;
begin
  Result := fVideoDecoderInfo.getPosition(fStream);
end;

function TPluginVideoDecodeStream.GetFrameWidth(): integer;
var
  FrameInfo: TVideoFrameInfo;
begin
  fVideoDecoderInfo.getFrameInfo(fStream, @FrameInfo);
  Result := FrameInfo.width;
end;

function TPluginVideoDecodeStream.GetFrameHeight(): integer;
var
  FrameInfo: TVideoFrameInfo;
begin
  fVideoDecoderInfo.getFrameInfo(fStream, @FrameInfo);
  Result := FrameInfo.height;
end;

function TPluginVideoDecodeStream.GetFrameAspect(): real;
var
  FrameInfo: TVideoFrameInfo;
begin
  fVideoDecoderInfo.getFrameInfo(fStream, @FrameInfo);
  Result := FrameInfo.aspect;
end;

function TPluginVideoDecodeStream.GetFrameFormat(): TVideoFrameFormat;
var
  FrameInfo: TVideoFrameInfo;
begin
  fVideoDecoderInfo.getFrameInfo(fStream, @FrameInfo);
  case FrameInfo.format of
  FRAME_FORMAT_RGB:  Result := vffRGB;
  FRAME_FORMAT_RGBA: Result := vffRGBA;
  FRAME_FORMAT_BGR:  Result := vffBGR;
  FRAME_FORMAT_BGRA: Result := vffBGRA;
  else               Result := vffUnknown;
  end;
end;

end.
