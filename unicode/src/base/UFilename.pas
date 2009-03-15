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

unit UFilename;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  TntClasses,
  {$ENDIF}
  Classes;

type
  TPathEncoding = (
    // pencLossless,
    pencSystemUTF8,  // use system encoding. Mac: UTF8, Linux: None, Win: UTF8
                     // Note:
                     // - On windows, this format is lossless but is not
                     //   supported direclty (UTF-16 only).
                     // - This is the best format to store filenames as there is
                     //   no dataloss.
                     // - Do not use with glPrint() or other display functions
                     //   as the resulting string is neither guaranteed to be
                     //   UTF-8 nor ANSI.

    pencSystemANSI,  // use system encoding. Mac: UTF8, Linux: None, Win: ANSI
                     // Note:
                     // - On windows, the ANSI format may break filenames
                     //   that are not encoded in the active codepage.
                     // - Use only if there is no equivalent WideString
                     //   filesystem function.
                     // - Do not use with glPrint() or other display functions
                     //   as the resulting string is neither guaranteed to be
                     //   UTF-8 nor ANSI.

    // pencPrintable,
    pencUTF8         // tries to present the filename as UTF-8 encoded string
                     // that can be used for printing etc.
                     // Note:
                     // - Some characters whose encodings are unknown are
                     //   represented either by '?' or as the representative of
                     //   a different encoding. 
                     // - On linux not all filenames can be represented
                     //   correclty in UTF-8 as it does not have filename
                     //   encodings (though most filenames are stored in UTF-8).
                     // - Never use the result to store filenames or open files
                     //   as the filenames may break due to conversion.
  );

  IPath = interface;

  {$IFDEF MSWINDOWS}
  TUniFileStream = class(TTntFileStream)
  {$ELSE}
  TUniFileStream = class(TFileStream)
  {$ENDIF}
  public
    constructor Create(const FileName: IPath; Mode: Word);
  end;

  TUniMemoryStream = class(TMemoryStream)
  public
    procedure LoadFromFile(const FileName: IPath);
    procedure SaveToFile(const FileName: IPath);
  end;
  
  IPath = interface
    function ToString(Encoding: TPathEncoding = pencSystemUTF8): AnsiString;
    function ToWideString(): WideString;

    function Adjust(): boolean;

    function Expand(): IPath;
    {** File must be closed with FileClose(Handle) after usage }
    function CreateFile(): integer;                    
    function CreateDir(): boolean;
    {** File must be closed with FileClose(Handle) after usage }
    function Open(Mode: LongWord): integer;
    {** Stream must be freed with TUniFileStream.Free after usage }
    function OpenStream(Mode: Word): TUniFileStream;
    function GetFileAge(): integer; overload;
    function GetFileAge(out FileDateTime: TDateTime): boolean; overload;
    function Exists(): boolean;
    function IsFile(): boolean;
    function IsDirectory(): boolean;
    function GetAttr(): cardinal;
    function SetAttr(Attr: Integer): boolean;
    function IsReadOnly(): boolean;
    function SetReadOnly(ReadOnly: boolean): boolean;
    function ForceDirectories(): boolean;              //-
    function FileSearch(const DirList: IPath): IPath;  //-
    function Rename(const NewName: IPath): boolean;
    function DeleteFile(): boolean;
    function DeleteEmptyDir(): boolean;
    function CopyFile(const Target: IPath; FailIfExists: boolean): boolean;
  end;

function Path(const Name: AnsiString; Encoding: TPathEncoding = pencSystemANSI): IPath; overload;
function Path(const Name: WideString): IPath; overload;
function Path(PathComponents: array of const): IPath; overload;

implementation

uses
  UFilesystem,
  SysUtils;

type
  TPathImpl = class(TInterfacedObject, IPath)
    private
      fName: AnsiString;
    public
      constructor Create(const Name: string);
      destructor Destroy(); override;

      function ToString(Encoding: TPathEncoding = pencSystemANSI): AnsiString;
      function ToWideString(): WideString;

      //function Append(const IPath): IPath;
      function Adjust(): boolean;
      {**
       * Removes trailing path-delimiter and replaces path-delimiters with '/'.
       *}
      procedure Unify();
      function Expand(): IPath;

      (*
      function IncludeTrailingPathDelimiter(): IPath;
      function ExcludeTrailingPathDelimiter(): IPath;
      function ChangeFileExt(const Extension: IPath): IPath;
      function ExtractFilePath(): IPath;
      function ExtractFileDir(): IPath;
      function ExtractFileDrive(): IPath;
      function ExtractFileName(): IPath;
      function ExtractFileExt(): IPath;
      function ExtractRelativePath(const BaseName: IPath): IPath;
      *)

      function CreateFile(): integer;
      function CreateDir(): boolean;
      function Open(Mode: LongWord): integer;
      function OpenStream(Mode: Word): TUniFileStream;
      function GetFileAge(): integer; overload;
      function GetFileAge(out FileDateTime: TDateTime): boolean; overload;
      function Exists(): boolean;
      function IsFile(): boolean;
      function IsDirectory(): boolean;
      function GetAttr(): cardinal;
      function SetAttr(Attr: Integer): boolean;
      function IsReadOnly(): boolean;
      function SetReadOnly(ReadOnly: boolean): boolean;
      function ForceDirectories(): boolean;
      function FileSearch(const DirList: IPath): IPath;
      function Rename(const NewName: IPath): boolean;
      function DeleteFile(): boolean;
      function DeleteEmptyDir(): boolean;
      function CopyFile(const Target: IPath; FailIfExists: boolean): boolean;
  end;

  IPathIterator = interface
    function Next(): IPath;
  end;

  TPathIteratorImpl = class(TInterfacedObject, IPathIterator)
    private
      fPath: IPath;
      fPos: integer;
    public
      constructor Create(Path: IPath);
      
      function Next(): IPath;
  end;

function Path(const Name: AnsiString; Encoding: TPathEncoding): IPath;
begin
  {$IFDEF MSWINDOWS}
  case Encoding of
    pencUTF8:
      // TODO: Check if UTF8
      Result := TPathImpl.Create(Name);
    pencSystemANSI:
      Result := TPathImpl.Create(AnsiToUtf8(Name));
    else
      raise Exception.Create('Unhandled encoding');
  end;
  {$ELSE}
  Result := TPathImpl.Create(Name);
  {$ENDIF}
end;

function Path(const Name: WideString): IPath;
begin
  Result := TPathImpl.Create(UTF8Encode(Name));
end;

function Path(PathComponents: array of const): IPath;
var
  CompIndex: integer;
  Name: string;
begin
  Name := '';
  for CompIndex := 0 to High(PathComponents) do
  begin
    with PathComponents[CompIndex] do
    begin
      case (VType) of
        vtString:
          Name := Name + VString^;
        vtAnsiString:
          Name := Name + AnsiString(VAnsiString);
        vtWideString:
          Name := Name + UTF8Encode(WideString(VWideString));
        vtObject:
          raise Exception.Create('Unhandled Object type');
        else
          raise Exception.Create('Unhandled Path type');
      end;
    end;
    Name := Name + PathDelim;
  end;
  Result := Path(Name);
end;

constructor TPathImpl.Create(const Name: string);
begin
  inherited Create();
  fName := Name;
  Unify();
end;

destructor TPathImpl.Destroy();
begin
  inherited;
end;

procedure TPathImpl.Unify();
var
  I: integer;
begin
  for I := 1 to Length(fName) do
  begin
    if (fName[I] in ['\', '/']) then
      fName[I] := PathDelim;
  end;
  fName := ExcludeTrailingPathDelimiter(fName);
end;

function TPathImpl.ToString(Encoding: TPathEncoding): AnsiString;
begin
  {$IFDEF MSWINDOWS}
  case Encoding of
    pencUTF8:
      Result := fName;
    pencSystemANSI:
      Result := Utf8ToAnsi(fName);
    else
      raise Exception.Create('Unhandled encoding');
  end;
  {$ELSE}
  Result := fName;
  {$ENDIF}
end;

function TPathImpl.ToWideString(): WideString;
begin
  Result := UTF8Decode(fName);
end;

function TPathImpl.Adjust(): boolean;
begin
  Result := false; //TODO
end;



function TPathImpl.Expand(): IPath;
begin
  Result := FileSystem.ExpandFileName(Self);
end;

function TPathImpl.CreateFile(): integer;
begin
  Result := FileSystem.FileCreate(Self);
end;

function TPathImpl.CreateDir(): boolean;
begin
  Result := FileSystem.DirectoryCreate(Self);
end;

function TPathImpl.Open(Mode: LongWord): integer;
begin
  Result := FileSystem.FileOpen(Self, Mode);
end;

function TPathImpl.OpenStream(Mode: Word): TUniFileStream;
begin
  Result := TUniFileStream.Create(Self, Mode);
end;

function TPathImpl.GetFileAge(): integer;
begin
  Result := FileSystem.FileAge(Self);
end;

function TPathImpl.GetFileAge(out FileDateTime: TDateTime): boolean;
begin
  Result := FileSystem.FileAge(Self, FileDateTime);
end;

function TPathImpl.Exists(): boolean;
begin
  Result := IsFile() or IsDirectory();
end;

function TPathImpl.IsFile(): boolean;
begin
  Result := FileSystem.FileExists(Self);
end;

function TPathImpl.IsDirectory(): boolean;
begin
  Result := FileSystem.DirectoryExists(Self);
end;

function TPathImpl.GetAttr(): cardinal;
begin
  Result := FileSystem.FileGetAttr(Self);
end;

function TPathImpl.SetAttr(Attr: Integer): boolean;
begin
  Result := FileSystem.FileSetAttr(Self, Attr);
end;

function TPathImpl.IsReadOnly(): boolean;
begin
  Result := FileSystem.FileIsReadOnly(Self);
end;

function TPathImpl.SetReadOnly(ReadOnly: boolean): boolean;
begin
  Result := FileSystem.FileSetReadOnly(Self, ReadOnly);
end;

function TPathImpl.ForceDirectories(): boolean;
begin
  Result := FileSystem.ForceDirectories(Self);
end;

function TPathImpl.FileSearch(const DirList: IPath): IPath;
begin
  Result := FileSystem.FileSearch(Self, DirList);
end;

function TPathImpl.Rename(const NewName: IPath): boolean;
begin
  Result := FileSystem.RenameFile(Self, NewName);
end;

function TPathImpl.DeleteFile(): boolean;
begin
  Result := FileSystem.DeleteFile(Self);
end;

function TPathImpl.DeleteEmptyDir(): boolean;
begin
  Result := FileSystem.RemoveDir(Self);
end;

function TPathImpl.CopyFile(const Target: IPath; FailIfExists: boolean): boolean;
begin
  Result := FileSystem.CopyFile(Self, Target, FailIfExists);
end;



{ TPathIteratorImpl }

constructor TPathIteratorImpl.Create(Path: IPath);
begin
  fPath := Path;
  fPos := -1;
end;

function TPathIteratorImpl.Next(): IPath;
begin
end;


{ TUniFileStream }

{$IFDEF MSWINDOWS}

constructor TUniFileStream.Create(const FileName: IPath; Mode: Word);
begin
  inherited Create(FileName.ToWideString(), Mode);
end;

{$ELSE}

constructor TUniFileStream.Create(const FileName: IPath; Mode: Word);
begin
  inherited Create(FileName.ToSystemString(), Mode);
end;

{$ENDIF}

{ TUniMemoryStream }

procedure TUniMemoryStream.LoadFromFile(const FileName: IPath);
var
  Stream: TStream;
begin
  Stream := TUniFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUniMemoryStream.SaveToFile(const FileName: IPath);
var
  Stream: TStream;
begin
  Stream := TUniFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
