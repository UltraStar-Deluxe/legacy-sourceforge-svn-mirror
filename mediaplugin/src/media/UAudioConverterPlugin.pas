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

unit UAudioConverterPlugin;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  ULog,
  ctypes,
  UMediaPlugin,
  SysUtils;

type
  TAudioConverterPlugin = class(TInterfacedObject, IAudioConverter)
    private
      fPluginInfo: PMediaPluginInfo;
    public
      constructor Create(Info: PMediaPluginInfo);

      function GetName(): string;
      function GetPriority(): integer;
      function Init(): boolean;
      function Finalize(): boolean;

      function Open(SrcFormatInfo: TAudioFormatInfo;
                    DstFormatInfo: TAudioFormatInfo): TAudioConvertStream;
  end;

  TPluginAudioConvertStream = class(TAudioConvertStream)
    private
      fPluginInfo: PMediaPluginInfo;
      fAudioConverter: PAudioConverterInfo;
      fStream: PAudioConvertStream;
      function Init(): boolean;
    protected
      constructor Create(SrcFormatInfo: TAudioFormatInfo;
                    DstFormatInfo: TAudioFormatInfo);
    public
      destructor Destroy(); override;

      class function Open(SrcFormatInfo: TAudioFormatInfo;
                    DstFormatInfo: TAudioFormatInfo): TPluginAudioConvertStream;

      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; override;
      function GetOutputBufferSize(InputSize: integer): integer; override;
      function GetRatio(): double; override;
  end;

implementation

{ TAudioConverterPlugin }

constructor TAudioConverterPlugin.Create(Info: PMediaPluginInfo);
begin
  inherited Create();
  fPluginInfo := Info;
end;

function TAudioConverterPlugin.GetName(): string;
begin
  Result := 'Plugin:AudioConverter:' + fPluginInfo.name;
end;

function TAudioConverterPlugin.GetPriority(): integer;
begin
  Result := fPluginInfo.audioDecoder.priority;
end;

function TAudioConverterPlugin.Init(): boolean;
begin
  //fPluginInfo.initialize();
  Result := true;
end;

function TAudioConverterPlugin.Finalize(): boolean;
begin
  //fPluginInfo.finalize();
  Result := true;
end;

function TAudioConverterPlugin.Open(SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo): TAudioConvertStream;
begin
  Result := TPluginAudioConvertStream.Open(SrcFormatInfo, DstFormatInfo);
end;

{ TAudioConverterPlugin }

constructor TPluginAudioConvertStream.Create(SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo);
begin
  inherited Create(SrcFormatInfo, DstFormatInfo);
end;

destructor TPluginAudioConvertStream.Destroy();
begin
  if (fStream <> nil) then
    fAudioConverter.close(fStream);
  inherited;
end;

class function TPluginAudioConvertStream.Open(SrcFormatInfo: TAudioFormatInfo;
  DstFormatInfo: TAudioFormatInfo): TPluginAudioConvertStream;
var
  Stream: TPluginAudioConvertStream;
begin
  Result := nil;
  Stream := TPluginAudioConvertStream.Create(SrcFormatInfo, DstFormatInfo);
  if (not Stream.Init()) then
  begin
    Stream.Free;
    Exit;
  end;
  Result := Stream;
end;

function TPluginAudioConvertStream.Init(): boolean;
var
  CSrcFormatInfo: TCAudioFormatInfo;
  CDstFormatInfo: TCAudioFormatInfo;
begin
  Result := false;
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

function TPluginAudioConvertStream.Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer;
begin
  Result := fAudioConverter.convert(fStream,
      PCuint8(InputBuffer), PCuint8(OutputBuffer), @InputSize);
end;

function TPluginAudioConvertStream.GetOutputBufferSize(InputSize: integer): integer;
begin
  Result := fAudioConverter.getOutputBufferSize(fStream, InputSize);
end;

function TPluginAudioConvertStream.GetRatio(): double;
begin
  Result := fAudioConverter.getRatio(fStream);
end;

end.