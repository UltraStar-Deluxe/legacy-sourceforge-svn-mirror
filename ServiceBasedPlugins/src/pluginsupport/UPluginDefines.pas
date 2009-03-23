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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UMain.pas $
 * $Id: UMain.pas 1485 2008-10-28 20:16:05Z tobigun $
 *}
{*
  some defines that are used by usdx and plugins
*}
unit UPluginDefines;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

const
  SDK_Version = 0001000000000000; //< identifies the used SDK version (1.0.0.0)

type
  { this type is used for all version ints in this SDK }
  TUS_Version = LongInt;

  { this type is used for all plugin handles in this SDK }
  TUS_Handle = LongInt;

const
  US_HANDLE_CORE = 137; //handle of core, also first defined handle
  US_HANDLE_UNDEFINED = 0; //undefined handle, used to indicate errors

  { this type is used for all id strings in this SDK }
const
  MaxLength_TUS_Identifier = 32;  //max length of identifier strings, has to be multiply of 4

type
  TUS_Identifier = String[MaxLength_TUS_Identifier];

  { this record is used to offer some information about this plugin to the core }
  TUS_PluginInfo = packed record
    Name       : Array [1..32] of Char;   //name of the plugin
    Author     : Array [1..32] of Char;   //plugins author
    PluginDesc : Array [1..64] of Char;   //some further explentation
    Version    : TUS_Version;
  end;
  PUS_PluginInfo = ^TUS_PluginInfo;

  { this function reads the plugins information from a TUS_PluginInfo record,
    and saves it. Can be called onced per plugin.
    Version is the SDK_Version used by the plugin
    BufferLength is the size of buffer in bytes }
  TUS_Func_Identify     = function (Handle: TUS_Handle; Version: TUS_Version; Buffer: PUS_PluginInfo; BufferLength: LongInt): LongInt; stdcall;

  { this plugin is used for selfshutdown by the plugin on an error
    case.
    Reason should be used to describe the reason for the shutdown
    OnChangeStatus(psDeInited) is called when old status was psInited }
  TUS_Func_Error = function (Handle: TUS_Handle; Reason: PWideChar): LongInt; stdcall;

  { this function writes the plugins filename including its path ,
    to buffer. A call with Buffer=nil or Len<=0 returns the actual
    length of the path.
    BufferLength is the size of buffer in bytes
    Filename is used to identify duplicate load of one plugin
    so it should even set when plugin is not loaded from file}
  TUS_Func_GetFilename  = function (Handle: TUS_Handle; Version: TUS_Version; Buffer: PWideChar; BufferLength: LongInt): LongInt; stdcall;

  { this function writes an interface, identified by 'ID', to the
    buffer.
    Version is the version of the interface that is used
    BufferLength is the size of buffer in bytes}
  TUS_Func_GetInterface = function (ID: TUS_Identifier; Version: TUS_Version; Buffer: Pointer; BufferLength: LongInt): LongInt; stdcall;

  { this function reads an interface, identified by 'ID', from the
    buffer, to offer it to calls of GetInterface.
    version is the version of the interface
    BufferLength is the size of buffer in bytes }
  TUS_Func_SetInterface = function (ID: TUS_Identifier; Version: TUS_Version; Buffer: Pointer; BufferLength: LongInt): LongInt; stdcall;

  { this function writes a record or some other data,
    identified by 'ID', to the buffer.
    Version is the version of the data that is used
    BufferLength is the size of buffer in bytes}
  TUS_Func_GetData = function (ID: TUS_Identifier; Version: TUS_Version; Buffer: Pointer; BufferLength: LongInt): LongInt; stdcall;

  { this function reads a record or some other data,
    identified by 'ID', from the buffer, to offer it
    to calls of GetData.
    version is the version of the record
    BufferLength is the size of buffer in bytes }
  TUS_Func_SetData = function (ID: TUS_Identifier; Version: TUS_Version; Buffer: Pointer; BufferLength: LongInt): LongInt; stdcall;

  { a record w/ some useful methods for plugins }
  TUS_PluginInterface = packed record
    Version     : TUS_Version;     //SDK_Version
    Handle      : LongInt;         //the plugins handle

    Identify    : TUS_Func_Identify;
    Error       : TUS_Func_Error;
    GetFilename : TUS_Func_GetFilename;

    GetInterface: TUS_Func_GetInterface;
    SetInterface: TUS_Func_SetInterface;

    GetData     : TUS_Func_GetData;
    SetData     : TUS_Func_SetData;
  end;
  PUS_PluginInterface = ^TUS_PluginInterface;

  { this function writes the PluginInterface to the buffer }
  TUS_Func_InitInterface = function (Handle: TUS_Handle; Version: TUS_Version; Buffer: PUS_PluginInterface; BufferSize: LongInt): LongInt;
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}


  { this function is called by core, in the WaitingInit status
    it offers the plugin the possibility to load its plugininterface
    and to check it's sdk version against the cores.
    It has to be implemented by plugins loaded as library }
  TUS_Plugin_Proc_Init = procedure (Handle: TUS_Handle; CoreVersion: TUS_Version; InitInterface: TUS_Func_InitInterface);
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}


  TUS_PluginStatus = (psNone, psWaitingInit, psWaitingIdentify, psInited, psDeInited, psError);

  { this function is called everytime tbe plugins status changes
    status is the new status.
    it has to be implemented by plugins loaded as library }
  TUS_Plugin_OnChangeStatus = procedure (Handle: TUS_Handle; Status: TUS_PluginStatus);
  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}  


  { this interface represents a loaded plugin, it eitber has to
    be implemented by the pluginloader, e.g. for libs or by the
    plugin itself in the case of integrated plugins }
  IUS_Plugin = Interface
    ['{23F7B722-D979-4402-9953-C6B229A6E888}']
    function GetStatus: TUS_PluginStatus;
    procedure SetStatus(status: TUS_PluginStatus);

    function GetHandle: TUS_Handle;
    function GetUniqueID: TUS_Handle; //< This ID is unique for the given plugin it may be a hash of the plugins code or s/t like that. used to identify plugins laoded twice
    function GetFilename: WideString;

    Procedure Init;
    Procedure DeInit;

    Procedure SetError(Reason: WideString);
    Function GetErrorReason: WideString;
  end;

  { this function will be called when a hookable event is spawn
    data is a pointer to a struct that can contain any data
    defined by the event creator. if there is no data this should
    be nil.
    breakable is true if the hook chain can be interupted by
    returning US_HOOK_BREAK or US_HOOK_UNHOOKME_BREAK
    by returning US_HOOK_UNHOOKME the callback will immediately
    be removed from the chain }
  TUS_Func_OnEvent = function(data: pointer; breakable: boolean): integer;
  TUS_cFunc_OnEvent = function(data: pointer; breakable: boolean): integer of object;

  { this interface represents an event that can be hooked by
    e.g. the core or other}
  IUS_HookableEvent = Interface
    // this GID has to be changed if we change this interface
    ['{FFC6F09D-EC0B-41E2-87F6-70EC6F055C0E}']
    function GetIdentifier: TUS_Identifier;
    function GetHash: LongInt;

    //adds the callback to the top of the events hook chain
    //proc is the callback function
    //parent is the plugins handle, or US_HANDLE_CORE if called by core
    //returns hook-handle on succes or US_HANDLE_UNDEFINED on failure
    function Hook(proc: TUS_Func_OnEvent; Parent: TUS_Handle): TUS_Handle;
    function cHook(proc: TUS_cFunc_OnEvent; Parent: TUS_Handle): TUS_Handle;

    //removes a callback from the chain using the handle returned
    //by the hook function
    //returns an US_ERROR code
    function UnHook(Handle: TUS_Handle): byte;

    //removes all callbacks of a specified parent (plugin)
    procedure UnHookbyParent(Parent: TUS_Handle);
  end;


const
  { some errorcodes that are returned by functions using this SDK }
  US_ERROR_None = 0;            //< indicates success ;)
  US_ERROR_VersionMismatch = 1; //< returned when the version the plugin wants to use is not supported
  US_ERROR_SizeMismatch = 2;    //< returned when the wanted data does not fit the buffersize
  US_ERROR_WrongHandle = 3;     //< returned when the given handle is not
  US_ERROR_WrongID = 4;         //< returned when the given id does not exist
  US_ERROR_NameExists = 5;      //< returned when the given name already exists

  US_ERROR_Unknown = 255;       //< returned when an unknown error occured

  { return values for hook function }
  US_HOOK_BREAK = High(integer);              //this will break the hook chain
  US_HOOK_UNHOOKME = High(integer) - 2;       //this will remove the callback from the hooks chain
  US_HOOK_UNHOOKME_BREAK = High(integer) - 1; //this will do both explained above

{ this function converts an errorcode to its textual expression }
function USErrortoString(Code: LongInt): String;

{ this funtion creates a hash for an TUS_Identifier
  the hash is not guaranteed to be unique, it is just
  for a fast search of an identifier }
function CalculateUSHash(Identifier: TUS_Identifier): LongInt;

implementation

{ this function converts an errorcode to its textual expression }
Function USErrortoString(Code: LongInt): String;
begin
  Case Code of
    US_Error_None:            Result := 'success, no error occured';
    US_Error_VersionMismatch: Result := 'version mismatch';
    US_Error_SizeMismatch:    Result := 'size mismatch';
    US_Error_WrongHandle:     Result := 'handle does not exist';
    US_Error_WrongID:         Result := 'id does not exist';
    US_Error_NameExists:      Result := 'another item with this name does already exist';

    Else Result := 'unknown error';
  end;
end;

{ this funtion creates a hash for an TUS_Identifier
  the hash is not guaranteed to be unique, it is just
  for a fast search of an identifier }
function CalculateUSHash(Identifier: TUS_Identifier): LongInt;
  var
    L: Integer;
    I: Integer;

  const
    StartHash = -1337456101;
begin
  // fill the unused chars of the identifier w/ zeros
  L := Length(Identifier);
  if (L < MaxLength_TUS_Identifier) then
  FillChar(Identifier[L+1], MaxLength_TUS_Identifier - L, 0);

  // calculate the hash
  // this is done by just adding respectively 4 bytes of the string
  // to the hash (longint)
  Result := StartHash;
  for I := 1 to (MaxLength_TUS_Identifier div 4) do
    Result := Result + LongInt(Identifier[I*4]); 
end;

end.

