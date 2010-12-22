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
  UMusic,
  ctypes;

type
  PFileStream = Pointer;
  PThread = Pointer;
  PMutex = Pointer;
  PCond = Pointer;

  PMediaPluginCore = ^TMediaPluginCore;
  TMediaPluginCore = record
    version: cint;

    log: procedure(level: cint; msg: PAnsiChar; context: PAnsiChar); cdecl;
    ticksMillis: function(): cuint32; cdecl;

    fileOpen: function(utf8Filename: PAnsiChar; mode: cint): PFileStream; cdecl;
    fileClose: procedure(stream: PFileStream); cdecl;
    fileRead: function(stream: PFileStream; buf: PCuint8; size: cint): cint64; cdecl;
    fileWrite: function(stream: PFileStream; buf: PCuint8; size: cint): cint64; cdecl;
    fileSeek: function(stream: PFileStream; pos: cint64; whence: cint): cint64; cdecl;
    fileSize: function(stream: PFileStream): cint64; cdecl;

    threadCreate: function(func: Pointer; data: Pointer): PThread; cdecl;
    threadCurrentID: function(): cuint32; cdecl;
    threadGetID: function(thread: PThread): cuint32; cdecl;
    threadWait: procedure(thread: PThread; status: PCint); cdecl;
    threadSleep: procedure(millisecs: cuint32); cdecl;

    mutexCreate: function(): PMutex; cdecl;
    mutexDestroy: procedure(mutex: PMutex); cdecl;
    mutexLock: function(mutex: PMutex): cint; cdecl;
    mutexUnlock: function(mutex: PMutex): cint; cdecl;

    condCreate: function(): PCond; cdecl;
    condDestroy: procedure(cond: PCond); cdecl;
    condSignal: function(cond: PCond): cint; cdecl;
    condBroadcast: function(cond: PCond): cint; cdecl;
    condWait: function(cond: PCond; mutex: PMutex): cint; cdecl;
    condWaitTimeout: function(cond: PCond; mutex: PMutex; ms: cuint32): cint; cdecl;
  end;

  PAudioDecodeStream = Pointer;
  PAudioConvertStream = Pointer;
  PVideoDecodeStream = Pointer;

  PCAudioFormatInfo = ^TCAudioFormatInfo;
  TCAudioFormatInfo = record
    sampleRate: double;
    channels: cuint8;
    format: cint;
  end;

  PAudioDecoderInfo = ^TAudioDecoderInfo;
  TAudioDecoderInfo = record
    priority: cint;
    init: function(): cbool; cdecl;
    finalize: function(): cbool; cdecl;
    open: function(filename: PAnsiChar): PAudioDecodeStream; cdecl;
    close: procedure(stream: PAudioDecodeStream); cdecl;
    getLength: function(stream: PAudioDecodeStream): double; cdecl;
    getAudioFormatInfo: procedure(stream: PAudioDecodeStream; var info: TCAudioFormatInfo); cdecl;
    getPosition: function(stream: PAudioDecodeStream): double; cdecl;
    setPosition: procedure(stream: PAudioDecodeStream; time: double); cdecl;
    getLoop: function(stream: PAudioDecodeStream): cbool; cdecl;
    setLoop: procedure(stream: PAudioDecodeStream; enabled: cbool); cdecl;
    isEOF: function(stream: PAudioDecodeStream): cbool; cdecl;
    isError: function(stream: PAudioDecodeStream): cbool; cdecl;
    readData: function(stream: PAudioDecodeStream; buffer: PCUint8; bufferSize: cint): cint; cdecl;
  end;

  PAudioConverterInfo = ^TAudioConverterInfo;
  TAudioConverterInfo = record
    priority: cint;
    init: function(): cbool; cdecl;
    finalize: function(): cbool; cdecl;
    open: function(inputFormat: PCAudioFormatInfo; outputFormat: PCAudioFormatInfo): PAudioConvertStream; cdecl;
    close: procedure(stream: PAudioConvertStream); cdecl;
    convert: function(stream: PAudioConvertStream; input, output: PCuint8; numSamples: PCint): cint; cdecl;
	  getOutputBufferSize: function(stream: PAudioConvertStream; inputSize: cint): cint; cdecl;
    getRatio: function(stream: PAudioConvertStream): double; cdecl;
  end;

type
  TCVideoFrameFormat = cint;
const
  FRAME_FORMAT_UNKNOWN = 0;
  FRAME_FORMAT_RGB  = 1; // packed RGB  24bpp (R:8,G:8,B:8)
  FRAME_FORMAT_RGBA = 2; // packed RGBA 32bpp (R:8,G:8,B:8,A:8)
  FRAME_FORMAT_BGR  = 3; // packed RGB  24bpp (B:8,G:8,R:8)
  FRAME_FORMAT_BGRA = 4; // packed BGRA 32bpp (B:8,G:8,R:8,A:8)

type
  PVideoFrameInfo = ^TVideoFrameInfo;
  TVideoFrameInfo = record
    width: cint;
    height: cint;
    aspect: double;
    format: TCVideoFrameFormat;
  end;

  PVideoDecoderInfo = ^TVideoDecoderInfo;
  TVideoDecoderInfo = record
    priority: cint;
    init: function(): cbool; cdecl;
    finalize: function(): cbool; cdecl;
    open: function(filename: PAnsiChar; format: TCVideoFrameFormat): PVideoDecodeStream; cdecl;
    close: procedure(stream: PVideoDecodeStream); cdecl;
    setLoop: procedure(stream: PVideoDecodeStream; enable: cbool); cdecl;
    getLoop: function(stream: PVideoDecodeStream): cbool; cdecl;
    setPosition: procedure(stream: PVideoDecodeStream; time: double); cdecl;
    getPosition: function(stream: PVideoDecodeStream): double; cdecl;
    getFrameInfo: procedure(stream: PVideoDecodeStream; info: PVideoFrameInfo); cdecl;
    getFrame: function (stream: PVideoDecodeStream; time: clongdouble): PCuint8; cdecl;
  end;

  PMediaPluginInfo = ^TMediaPluginInfo;
  TMediaPluginInfo = record
    version: cint;
    name: PAnsiChar;
    initialize: function(): cbool; cdecl;
    finalize: function(): cbool; cdecl;
    audioDecoder: PAudioDecoderInfo;
    audioConverter: PAudioConverterInfo;
    videoDecoder: PVideoDecoderInfo;
  end;

  TPluginRegisterFunc = function(core: PMediaPluginCore): PMediaPluginInfo; cdecl;

procedure LoadMediaPlugins();
procedure UnloadMediaPlugins();

function MediaPluginCore: PMediaPluginCore;

procedure AudioFormatInfoToCStruct(
    const Info: TAudioFormatInfo; var CInfo: TCAudioFormatInfo);

implementation

uses
  SysUtils,
  Classes,
  SDL,
  moduleloader,
  UFilesystem,
  UPath,
  UPathUtils,
  ULog,
  UPlatform,
  UAudioDecoderPlugin,
  UAudioConverterPlugin,
  UVideoDecoderPlugin;

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

function MediaPluginCore: PMediaPluginCore;
begin
  Result := @MediaPluginCore_Instance;
end;

procedure AudioFormatInfoToCStruct(
    const Info: TAudioFormatInfo; var CInfo: TCAudioFormatInfo);
begin
  CInfo.sampleRate := Info.SampleRate;
  CInfo.channels := Info.Channels;
  CInfo.format := Ord(Info.Format);
end;

{* Misc *}

procedure Core_log(level: cint; msg: PAnsiChar; context: PAnsiChar); cdecl;
begin
  Log.LogMsg(msg, context, DebugLogLevels[level]);
end;

function Core_ticksMillis(): cuint32; cdecl;
begin
  Result := SDL_GetTicks();
end;

{* File *}

const
  FILE_OPEN_MODE_READ  = $01;
  FILE_OPEN_MODE_WRITE = $02;
  FILE_OPEN_MODE_READ_WRITE = FILE_OPEN_MODE_READ or FILE_OPEN_MODE_WRITE;

function Core_fileOpen(utf8Filename: PAnsiChar; mode: cint): PFileStream; cdecl;
var
  OpenMode: word;
begin
  if (mode = FILE_OPEN_MODE_READ_WRITE) then
    OpenMode := fmCreate
  else if (mode = FILE_OPEN_MODE_WRITE) then
    OpenMode := fmCreate // TODO: fmCreate is Read+Write -> reopen with fmOpenWrite
  else // (mode = FILE_OPEN_MODE_READ)
    OpenMode := fmOpenRead or fmShareDenyWrite;

  try
    Result := TBinaryFileStream.Create(Path(utf8Filename), OpenMode);
  except
    Result := nil;
  end;
end;

procedure Core_fileClose(stream: PFileStream); cdecl;
var
  FileStream : TStream;
begin
  FileStream := TStream(stream);
  FileStream.Free;
end;

function Core_fileRead(stream: PFileStream; buf: PCuint8; size: cint): cint64; cdecl;
var
  FileStream: TStream;
begin
  FileStream := TStream(stream);
  try
    Result := FileStream.Read(buf^, size);
  except
    Result := -1;
  end;
end;

function Core_fileWrite(stream: PFileStream; buf: PCuint8; size: cint): cint64; cdecl;
var
  FileStream: TStream;
begin
  FileStream := TStream(stream);
  try
    Result := FileStream.Write(buf^, size);
  except
    Result := -1;
  end;
end;

function Core_fileSeek(stream: PFileStream; pos: cint64; whence: cint): cint64; cdecl;
var
  FileStream : TStream;
  Origin : TSeekOrigin;
begin
  FileStream := TStream(stream);
  case whence of
    0 {SEEK_SET}: Origin := soBeginning;
    1 {SEEK_CUR}: Origin := soCurrent;
    2 {SEEK_END}: Origin := soEnd;
  else
    Origin := soBeginning;
  end;
  Result := FileStream.Seek(pos, Origin);
end;

function Core_fileSize(stream: PFileStream): cint64; cdecl;
var
  FileStream : TStream;
begin
  FileStream := TStream(stream);
  Result := FileStream.Size;
end;

{* Thread *}

function Core_threadCreate(func: Pointer; data: Pointer): PThread; cdecl;
begin
  Result := SDL_CreateThread(func, data);
end;

function Core_threadCurrentID(): cuint32; cdecl;
begin
  Result := SDL_ThreadID();
end;

function Core_threadGetID(thread: PThread): cuint32; cdecl;
begin
  Result := SDL_GetThreadID(PSDL_Thread(thread));
end;

procedure Core_threadWait(thread: PThread; status: PCint); cdecl;
begin
  SDL_WaitThread(PSDL_Thread(thread), status^);
end;

procedure Core_threadSleep(millisecs: cuint32); cdecl;
begin
  SDL_Delay(millisecs);
end;

{* Mutex *}

function Core_mutexCreate(): PMutex; cdecl;
begin
  Result := PMutex(SDL_CreateMutex());
end;

procedure Core_mutexDestroy(mutex: PMutex); cdecl;
begin
  SDL_DestroyMutex(PSDL_Mutex(mutex));
end;

function Core_mutexLock(mutex: PMutex): cint; cdecl;
begin
  Result := SDL_mutexP(PSDL_Mutex(mutex));
end;

function Core_mutexUnlock(mutex: PMutex): cint; cdecl;
begin
  Result := SDL_mutexV(PSDL_Mutex(mutex));
end;

{* Condition *}

function Core_condCreate(): PCond; cdecl;
begin
  Result := PCond(SDL_CreateCond());
end;

procedure Core_condDestroy(cond: PCond); cdecl;
begin
  SDL_DestroyCond(PSDL_Cond(cond));
end;

function Core_condSignal(cond: PCond): cint; cdecl;
begin
  Result := SDL_CondSignal(PSDL_Cond(cond));
end;

function Core_condBroadcast(cond: PCond): cint; cdecl;
begin
  Result := SDL_CondBroadcast(PSDL_Cond(cond));
end;

function Core_condWait(cond: PCond; mutex: PMutex): cint; cdecl;
begin
  Result := SDL_CondWait(PSDL_Cond(cond), PSDL_Mutex(mutex));
end;

function Core_condWaitTimeout(cond: PCond; mutex: PMutex; ms: cuint32): cint; cdecl;
begin
  Result := SDL_CondWaitTimeout(PSDL_Cond(cond), PSDL_Mutex(mutex), ms);
end;

procedure InitializeMediaPluginCore;
begin
  with MediaPluginCore_Instance do
  begin
    version := 0;
    log := Core_log;
    ticksMillis := Core_ticksMillis;
    fileOpen := Core_fileOpen;
    fileClose := Core_fileClose;
    fileRead := Core_fileRead;
    fileWrite := Core_fileWrite;
    fileSeek := Core_fileSeek;
    fileSize := Core_fileSize;
    threadCreate := Core_threadCreate;
    threadCurrentID := Core_threadCurrentID;
    threadGetID := Core_threadGetID;
    threadWait := Core_threadWait;
    threadSleep := Core_threadSleep;
    mutexCreate := Core_mutexCreate;
    mutexDestroy := Core_mutexDestroy;
    mutexLock := Core_mutexLock;
    mutexUnlock := Core_mutexUnlock;
    condCreate := Core_condCreate;
    condDestroy := Core_condDestroy;
    condSignal := Core_condSignal;
    condBroadcast := Core_condBroadcast;
    condWait := Core_condWait;
    condWaitTimeout := Core_condWaitTimeout;
  end;
end;

var
  MediaPlugins: TList;

type
  PMediaPluginEntry = ^TMediaPluginEntry;
  TMediaPluginEntry = record
    Module: TModuleHandle;
    Info: PMediaPluginInfo;
  end;

procedure LoadMediaPlugins();
var
  LibPath: IPath;
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  ModuleFile: IPath;
  Module: TModuleHandle;
  RegisterFunc: TPluginRegisterFunc;
  PluginInfo: PMediaPluginInfo;
  PluginEntry: PMediaPluginEntry;
const
  {$IF Defined(MSWINDOWS)}
  ModuleExt = '.dll';
  {$ELSEIF Defined(DARWIN)}
  ModuleExt = '.dylib';
  {$ELSE} //Defined(UNIX)
  ModuleExt = '.so';
  {$IFEND}
begin
  MediaPlugins := TList.Create;

  {$IFDEF MSWINDOWS}
  // add an additional dll search path for plugin dependencies
  Platform.AddLibrarySearchPath(MediaPluginPath.Append('deps'));
  {$ENDIF}

  LibPath := MediaPluginPath.Append('*' + ModuleExt);
  Iter := FileSystem.FileFind(LibPath, faAnyFile);
  while (Iter.HasNext) do
  begin
    FileInfo := Iter.Next();
    ModuleFile := MediaPluginPath.Append(FileInfo.Name);
    Module := INVALID_MODULEHANDLE_VALUE;
    if (not LoadModule(Module, PChar(ModuleFile.ToNative))) then
    begin
      Log.LogInfo('Failed to load media plugin: "' + FileInfo.Name.ToNative + '"',
          'LoadMediaPlugins');
      Continue;
    end;
    RegisterFunc := GetModuleSymbol(Module, 'Plugin_register');
    if (@RegisterFunc = nil) then
    begin
      Log.LogError('Invalid media plugin: "' + FileInfo.Name.ToNative + '"',
          'LoadMediaPlugins');
      UnloadModule(Module);
      Continue;
    end;
    PluginInfo := RegisterFunc(MediaPluginCore);
    if (PluginInfo = nil) then
    begin
      Log.LogError('Invalid media plugin info: "' + FileInfo.Name.ToNative + '"',
          'LoadMediaPlugins');
      UnloadModule(Module);
      Continue;
    end;
    if ((@PluginInfo.initialize <> nil) and
        (not PluginInfo.initialize())) then
    begin
      Log.LogError('Failed to initialize media plugin: "' + PluginInfo.name + '"',
          'LoadMediaPlugins');
      UnloadModule(Module);
      Continue;
    end;

    Log.LogStatus('Loaded media plugin: "' + PluginInfo.name + '"',
        'LoadMediaPlugins');

    New(PluginEntry);
    PluginEntry.Module := Module;
    PluginEntry.Info := PluginInfo;
    MediaPlugins.Add(PluginEntry);

    // register modules
    if (PluginInfo.audioDecoder <> nil) then
      MediaManager.Add(TAudioDecoderPlugin.Create(PluginInfo));
    if (PluginInfo.audioConverter <> nil) then
      MediaManager.Add(TAudioConverterPlugin.Create(PluginInfo));
    if (PluginInfo.videoDecoder <> nil) then
      MediaManager.Add(TVideoDecoderPlugin.Create(PluginInfo));
  end;
end;

procedure UnloadMediaPlugins();
var
  I: integer;
  PluginEntry: PMediaPluginEntry;
begin
  for I := 0 to MediaPlugins.Count - 1 do
  begin
    PluginEntry := MediaPlugins[I];
    if ((@PluginEntry.Info.finalize <> nil) and
        (not PluginEntry.Info.finalize())) then
    begin
      Log.LogError('Failed to finalize media plugin: "' + PluginEntry.Info.name + '"',
          'UnloadMediaPlugins');
    end;
    UnloadModule(PluginEntry.Module);
  end;
end;

initialization
  InitializeMediaPluginCore;

end.
