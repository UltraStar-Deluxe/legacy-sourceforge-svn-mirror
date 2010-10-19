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

unit UMediaPlugin;

interface

{$IFDEF FPC}
  {$MODE DELPHI }
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

uses
  ctypes;

type
  PMediaPluginCore = ^TMediaPluginCore;
  TMediaPluginCore = record
	  log: procedure(level: cint; msg: PChar; context: PChar); cdecl;
  end;

  PDecodeStream = Pointer;

  PCAudioFormatInfo = ^TCAudioFormatInfo;
  TCAudioFormatInfo = record
    sampleRate: double;
    channels: cuint8;
    format: cint;
  end;

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

function Plugin_initialize(core: PMediaPluginCore): cbool;
  cdecl; external ffmpegPlugin;

function DecodeStream_open(filename: PAnsiChar): PDecodeStream;
  cdecl; external ffmpegPlugin;
procedure DecodeStream_close(stream: PDecodeStream);
  cdecl; external ffmpegPlugin;
function DecodeStream_getLength(stream: PDecodeStream): double;
  cdecl; external ffmpegPlugin;
procedure DecodeStream_getAudioFormatInfo(stream: PDecodeStream; var info: TCAudioFormatInfo);
  cdecl; external ffmpegPlugin;
function DecodeStream_getPosition(stream: PDecodeStream): double;
  cdecl; external ffmpegPlugin;
procedure DecodeStream_setPosition(stream: PDecodeStream; time: double);
  cdecl; external ffmpegPlugin;
function DecodeStream_getLoop(stream: PDecodeStream): cbool;
  cdecl; external ffmpegPlugin;
procedure DecodeStream_setLoop(stream: PDecodeStream; enabled: cbool);
  cdecl; external ffmpegPlugin;
function DecodeStream_isEOF(stream: PDecodeStream): cbool;
  cdecl; external ffmpegPlugin;
function DecodeStream_isError(stream: PDecodeStream): cbool;
  cdecl; external ffmpegPlugin;
function DecodeStream_readData(stream: PDecodeStream; buffer: PCUint8; bufferSize: cint): cint;
  cdecl; external ffmpegPlugin;

function MediaPluginCore: PMediaPluginCore;

implementation

uses
  ULog;

var
  MediaPluginCore_Instance: TMediaPluginCore;

const
  DebugLogLevels: array[0 .. 5] of integer = (
    LOG_LEVEL_DEBUG,
    LOG_LEVEL_INFO,
    LOG_LEVEL_STATUS,
    LOG_LEVEL_WARN,
    LOG_LEVEL_ERROR,
    LOG_LEVEL_CRITICAL
  );

procedure LogFunc(level: cint; msg: PChar; context: PChar); cdecl;
begin
  Log.LogMsg(msg, context, DebugLogLevels[level]);
end;

function MediaPluginCore: PMediaPluginCore;
begin
  Result := @MediaPluginCore_Instance;
end;

procedure InitializeMediaPluginCore;
begin
  MediaPluginCore.log := LogFunc;
end;

initialization
  InitializeMediaPluginCore;

end.
