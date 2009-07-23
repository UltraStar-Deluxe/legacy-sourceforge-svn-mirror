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

unit UPath;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

interface

uses
  SysUtils,
  Classes,
  {$IFDEF MSWINDOWS}
  TntClasses,
  {$ENDIF}
  UUnicodeUtils;

type
  IPath = interface;

  {**
   * TUnicodeFileStream
   *}
  {$IFDEF MSWINDOWS}
  TUnicodeFileStream = class(TTntFileStream)
  {$ELSE}
  TUnicodeFileStream = class(TFileStream)
  {$ENDIF}
  public
    constructor Create(const FileName: IPath; Mode: Word);
  end;

  {**
   * TUnicodeMemoryStream
   *}
  TUnicodeMemoryStream = class(TMemoryStream)
  public
    procedure LoadFromFile(const FileName: IPath);
    procedure SaveToFile(const FileName: IPath);
  end;

  IPathDynArray = array of IPath;
  
  {**
   * IPath
   * The Path's pathname is immutable and cannot be changed after creation.
   *}
  IPath = interface
    {**
     * Returns the path as an UTF8 encoded string.
     * If UseNativeDelim is set to true, the native path delimiter ('\' on win32)
     * is used. If it is set to false the (more) portable '/' delimiter will used.
     *}
    function ToUTF8(UseNativeDelim: boolean = true): UTF8String;

    {**
     * Returns the path as an UTF-16 encoded string.
     * If UseNativeDelim is set to true, the native path delimiter ('\' on win32)
     * is used. If it is set to false the delimiter will be '/'.
     *}
    function ToWide(UseNativeDelim: boolean = true): WideString;

    {**
     * Returns the path with the system's native encoding and path delimiter.
     * Win32: ANSI (use the UTF-16 version IPath.ToWide() whenever possible)
     * Mac:   UTF8
     * Unix:  UTF8 or ANSI according to LC_CTYPE
     *}
    function ToNative(): RawByteString;

    {**
     * Note: File must be closed with FileClose(Handle) after usage
     * @seealso SysUtils.FileOpen()
     *}
    function Open(Mode: LongWord): THandle;

    {**
     * Note: Stream must be freed with TUniFileStream.Free after usage
     *}
    function OpenStream(Mode: Word): TUnicodeFileStream;

    {** @seealso SysUtils.ExtractFileDrive() *}
    function GetDrive(): IPath;

    {** @seealso SysUtils.ExtractFilePath() *}
    function GetPath(): IPath;

    {** @seealso SysUtils.ExtractFileDir() *}
    function GetDir(): IPath;

    {** @seealso SysUtils.ExtractFileName() *}
    function GetName(): IPath;

    {** @seealso SysUtils.ExtractFileExtension() *}
    function GetExtension(): IPath;

    {**
     * Returns a copy of the path with the extension changed to Extension.
     * The file itself is not changed, use Rename() for this task.
     * @seealso SysUtils.ChangeFileExt()
     *}
    function SetExtension(const Extension: IPath): IPath;

    {**
     * Returns the representation of the path relative to Basename.
     * Note that the basename must be terminated with a path delimiter
     * otherwise the last path component will be ignored.
     * @seealso SysUtils.ExtractRelativePath()
     *}
    function GetRelativePath(const BaseName: IPath): IPath;

    {** @seealso SysUtils.ExpandFileName() *}
    function GetAbsolutePath(): IPath;

    {**
     * Returns the concatenation of this path with Child. If this path does not
     * end with a path delimiter one is inserted in front of the Child path.
     * Example: Path('parent').Append(Path('child')) -> Path('parent/child')
     *}
    function Append(const Child: IPath): IPath;

    {**
     * Splits the path into its components.
     * Example: C:\test\dir -> ['C:\', 'test\', 'dir']
     *}
    function SplitDirs(): IPathDynArray;

    {**
     * Returns the parent directory or PathNone if none exists.
     *}
    function GetParent(): IPath;

    {**
     * Checks if this path is a subdir of or file inside Parent.
     * If Direct is true this path must be a direct child.
     * Example: C:\test\file is a direct child of C:\test and a child of C:\
     *}
    function IsChildOf(const Parent: IPath; Direct: boolean): boolean;

    {**
     * Adjusts the case of the path on case senstitive filesystems.
     * If the path does not exist or the filesystem is case insensitive
     * the original path will be returned. Otherwise a corrected copy.
     *}
    function AdjustCase(AdjustAllLevels: boolean): IPath;

    {** @seealso SysUtils.IncludeTrailingPathDelimiter() *}
    function IncludeTrailingPathDelimiter(): IPath;

    {** @seealso SysUtils.ExcludeTrailingPathDelimiter() *}
    function ExcludeTrailingPathDelimiter(): IPath;

    function Exists(): boolean;
    function IsFile(): boolean;
    function IsDirectory(): boolean;
    function IsAbsolute(): boolean;
    function GetFileAge(): integer; overload;
    function GetFileAge(out FileDateTime: TDateTime): boolean; overload;
    function GetAttr(): cardinal;
    function SetAttr(Attr: Integer): boolean;
    function IsReadOnly(): boolean;
    function SetReadOnly(ReadOnly: boolean): boolean;

    {**
     * Compares this path with Other and returns true if both paths are
     * equal. Both paths are expanded and trailing slashes excluded before
     * comparison. If IgnoreCase is true, the case will be ignored on
     * case-sensitive filesystems.
     *}
    function Equals(const Other: IPath; IgnoreCase: boolean = false): boolean;

    {**
     * Searches for a file in DirList. The Result is nil if the file was
     * not found. Use IFileSystem.FileFind() instead if you want to use
     * wildcards.
     * @seealso SysUtils.FileSearch()
     *}
    function FileSearch(const DirList: IPath): IPath;

    {** File must be closed with FileClose(Handle) after usage }
    function CreateFile(): THandle;
    function DeleteFile(): boolean;
    function CreateDirectory(Force: boolean = false): boolean;
    function DeleteEmptyDir(): boolean;
    function Rename(const NewName: IPath): boolean;
    function CopyFile(const Target: IPath; FailIfExists: boolean): boolean;

    // TODO: Dirwatch stuff
    // AddFileChangeListener(Listener: TFileChangeListener);
  end;

{**
 * Creates a new path with the given pathname. PathName can be either in UTF8
 * or the local encoding.
 * Notes:
 * - On Apple only UTF8 is supported
 * - Same applies to Unix with LC_CTYPE set to UTF8 encoding (default on newer systems)
 *}
function Path(const PathName: RawByteString): IPath; overload;

{**
 * Creates a new path with the given UTF-16 pathname.
 *}
function Path(const PathName: WideString): IPath; overload;

{**
 * Returns a reference to a singleton with path simply set to ''.
 *}
function PathNone(): IPath;

implementation

uses
  UFilesystem;

type
  TPathImpl = class(TInterfacedObject, IPath)
    private
      fName: UTF8String; //<** internal filename string, always UTF8 with PathDelim

      {**
       * Unifies the filename. Path-delimiters are replaced by '/'.
       *}
      procedure Unify();

      {**
       * Returns a copy of fName with path delimiters changed to '/'.
       *}
      function GetPortableString(): UTF8String;

      procedure AssertRefCount; inline;

    public
      constructor Create(const Name: UTF8String);
      destructor Destroy(); override;

      function ToUTF8(UseNativeDelim: boolean): UTF8String;
      function ToWide(UseNativeDelim: boolean): WideString;
      function ToNative(): RawByteString;

      function Open(Mode: LongWord): THandle;
      function OpenStream(Mode: Word): TUnicodeFileStream;

      function GetDrive(): IPath;
      function GetPath(): IPath;
      function GetDir(): IPath;
      function GetName(): IPath;
      function GetExtension(): IPath;
      function SetExtension(const Extension: IPath): IPath;
      function GetRelativePath(const BaseName: IPath): IPath;
      function GetAbsolutePath(): IPath;
      function GetParent(): IPath;
      function SplitDirs(): IPathDynArray;
      function Append(const Child: IPath): IPath;

      function Equals(const Other: IPath; IgnoreCase: boolean): boolean;
      function IsChildOf(const Parent: IPath; Direct: boolean): boolean;

      function AdjustCase(AdjustAllLevels: boolean): IPath;

      function IncludeTrailingPathDelimiter(): IPath;
      function ExcludeTrailingPathDelimiter(): IPath;

      function GetFileAge(): integer; overload;
      function GetFileAge(out FileDateTime: TDateTime): boolean; overload;
      function Exists(): boolean;
      function IsFile(): boolean;
      function IsDirectory(): boolean;
      function IsAbsolute(): boolean;
      function GetAttr(): cardinal;
      function SetAttr(Attr: Integer): boolean;
      function IsReadOnly(): boolean;
      function SetReadOnly(ReadOnly: boolean): boolean;
      
      function FileSearch(const DirList: IPath): IPath;

      function CreateFile(): THandle;
      function DeleteFile(): boolean;
      function CreateDirectory(Force: boolean): boolean;
      function DeleteEmptyDir(): boolean;
      function Rename(const NewName: IPath): boolean;
      function CopyFile(const Target: IPath; FailIfExists: boolean): boolean;
  end;

function Path(const PathName: RawByteString): IPath;
begin
  if (IsUTF8String(PathName)) then
    Result := TPathImpl.Create(PathName)
  else if (IsNativeUTF8()) then
    Result := PathNone
  else
    Result := TPathImpl.Create(AnsiToUtf8(PathName));
end;

function Path(const PathName: WideString): IPath;
begin
  Result := TPathImpl.Create(UTF8Encode(PathName));
end;



procedure TPathImpl.AssertRefCount;
begin
  if (FRefCount <= 0) then
    raise Exception.Create('RefCount error: ' + inttostr(FRefCount));
end;

constructor TPathImpl.Create(const Name: UTF8String);
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
  // convert all path delimiters to native ones
  for I := 1 to Length(fName) do
  begin
    if (fName[I] in ['\', '/']) and (fName[I] <> PathDelim) then
      fName[I] := PathDelim;
  end;
end;

function TPathImpl.GetPortableString(): UTF8String;
var
  I: integer;
begin
  Result := fName;
  if (PathDelim = '/') then
    Exit;

  for I := 1 to Length(Result) do
  begin
    if (Result[I] = PathDelim) then
      Result[I] := '/';
  end;
end;

function TPathImpl.ToUTF8(UseNativeDelim: boolean): UTF8String;
begin
  if (UseNativeDelim) then
    Result := fName
  else
    Result := GetPortableString();
end;

function TPathImpl.ToWide(UseNativeDelim: boolean): WideString;
begin
  if (UseNativeDelim) then
    Result := UTF8Decode(fName)
  else
    Result := UTF8Decode(GetPortableString());
end;

function TPathImpl.ToNative(): RawByteString;
begin
  if (IsNativeUTF8()) then
    Result := fName
  else
    Result := Utf8ToAnsi(fName);
end;

function TPathImpl.GetDrive(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileDrive(Self);
end;

function TPathImpl.GetPath(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFilePath(Self);
end;

function TPathImpl.GetDir(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileDir(Self);
end;

function TPathImpl.GetName(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileName(Self);
end;

function TPathImpl.GetExtension(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractFileExt(Self);
end;

function TPathImpl.SetExtension(const Extension: IPath): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ChangeFileExt(Self, Extension);
end;

function TPathImpl.GetRelativePath(const BaseName: IPath): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExtractRelativePath(BaseName, Self);
end;

function TPathImpl.GetAbsolutePath(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExpandFileName(Self);
end;

function TPathImpl.GetParent(): IPath;
var
  CurPath, ParentPath: IPath;
begin
  AssertRefCount;

  Result := PathNone;

  CurPath := Self.ExcludeTrailingPathDelimiter();
  // check if current path has a parent (no further '/')
  if (Pos(PathDelim, CurPath.ToUTF8()) = 0) then
    Exit;

  // set new path and check if it has changed to avoid endless loops
  // e.g. with invalid paths like '/C:' (GetPath() uses ':' as delimiter too)
  // on delphi/win32
  ParentPath := CurPath.GetPath();
  if (ParentPath.ToUTF8 = CurPath.ToUTF8)  then
    Exit;

  Result := ParentPath;
end;

function TPathImpl.SplitDirs(): IPathDynArray;
var
  CurPath: IPath;
  TmpPath: IPath;
  Components: array of IPath;
  CurPathStr: UTF8String;
  DelimPos: integer;
  I: integer;
begin
  SetLength(Result, 0);

  if (Length(Self.ToUTF8(true)) = 0) then
    Exit;

  CurPath := Self;
  SetLength(Components, 0);
  repeat
    SetLength(Components, Length(Components)+1);

    CurPathStr := CurPath.ToUTF8();
    DelimPos := LastDelimiter(PathDelim, SysUtils.ExcludeTrailingPathDelimiter(CurPathStr));
    Components[High(Components)] := Path(Copy(CurPathStr, DelimPos+1, Length(CurPathStr)));

    // TODO: remove this workaround for FPC bug
    TmpPath := CurPath;
    CurPath := TmpPath.GetParent();
  until (CurPath = PathNone);

  // reverse list
  SetLength(Result, Length(Components));
  for I := 0 to High(Components) do
    Result[I] := Components[High(Components)-I];
end;

function TPathImpl.Append(const Child: IPath): IPath;
begin
  AssertRefCount;

  if (fName = '') then
    Result := Child
  else
    Result := Path(Self.IncludeTrailingPathDelimiter().ToUTF8() + Child.ToUTF8());
end;

function TPathImpl.Equals(const Other: IPath; IgnoreCase: boolean): boolean;
var
  SelfPath, OtherPath: UTF8String;
begin
  SelfPath := Self.GetAbsolutePath().ExcludeTrailingPathDelimiter().ToUTF8();
  OtherPath := Other.GetAbsolutePath().ExcludeTrailingPathDelimiter().ToUTF8();
  if (FileSystem.IsCaseSensitive() and not IgnoreCase) then
    Result := (CompareStr(SelfPath, OtherPath) = 0)
  else
    Result := (CompareText(SelfPath, OtherPath) = 0);
end;

function TPathImpl.IsChildOf(const Parent: IPath; Direct: boolean): boolean;
var
  SelfPath, ParentPath: UTF8String;
  TmpPath, TmpPath2: IPath;
begin
  Result := false;

  if (Direct) then
  begin
    // TODO: remove workaround for fpc refcount bug
    TmpPath := Self.GetParent();
    TmpPath2 := TmpPath.GetAbsolutePath();
    SelfPath := TmpPath2.IncludeTrailingPathDelimiter().ToUTF8();

    // TODO: remove workaround for fpc refcount bug
    TmpPath := Parent.GetAbsolutePath();
    ParentPath := TmpPath.IncludeTrailingPathDelimiter().ToUTF8();

    // simply check if this paths parent path (SelfPath) equals ParentPath
    Result := (SelfPath = ParentPath);
  end
  else
  begin
    // TODO: remove workaround for fpc refcount bug
    TmpPath := Self.GetAbsolutePath();
    SelfPath := TmpPath.IncludeTrailingPathDelimiter().ToUTF8();

    // TODO: remove workaround for fpc refcount bug
    TmpPath := Parent.GetAbsolutePath();
    ParentPath := TmpPath.IncludeTrailingPathDelimiter().ToUTF8();

    if (Length(SelfPath) <= Length(ParentPath)) then
      Exit;

    // check if ParentPath is a substring of SelfPath
    if (FileSystem.IsCaseSensitive()) then
      Result := (StrLComp(PAnsiChar(SelfPath), PAnsiChar(ParentPath), Length(ParentPath)) = 0)
    else
      Result := (StrLIComp(PAnsiChar(SelfPath), PAnsiChar(ParentPath), Length(ParentPath)) = 0)
  end;
end;

function AdjustCaseRecursive(CurPath: IPath; AdjustAllLevels: boolean): IPath;
var
  OldParent, AdjustedParent: IPath;
  TmpPath: IPath;
  LocalName: IPath;
  PathFound: IPath;
  PathWithAdjParent: IPath;
  SearchInfo: TFileInfo;
  FileIter: IFileIterator;
  Pattern: IPath;
begin
  // if case-sensitive path exists there is no need to adjust case
  if (CurPath.Exists()) then
  begin
    Result := CurPath;
    Exit;
  end;

  // extract name component of current path
  // TODO: remove workaround for fpc refcount bug
  TmpPath := CurPath.ExcludeTrailingPathDelimiter();
  LocalName := TmpPath.GetName();

  // try to adjust parent
  OldParent := CurPath.GetParent();
  if (OldParent <> PathNone) then
  begin
    if (not AdjustAllLevels) then
    begin
      AdjustedParent := OldParent;
    end
    else
    begin
      AdjustedParent := AdjustCaseRecursive(OldParent, AdjustAllLevels);
      if (AdjustedParent = nil) then
      begin
        // parent path was not found case-insensitive
        Result := nil;
        Exit;
      end;

      // check if the path with adjusted parent can be found now
      PathWithAdjParent := AdjustedParent.Append(LocalName);
      if (PathWithAdjParent.Exists()) then
      begin
        Result := PathWithAdjParent;
        Exit;
      end;
    end;
    Pattern := AdjustedParent.Append(Path('*'));
  end
  else // path has no parent
  begin
    // the top path can either be absolute or relative
    if (CurPath.IsAbsolute) then
    begin
      // the only absolute directory at Unix without a parent is root ('/')
      // and hence does not need to be adjusted
      Result := CurPath;
      Exit;
    end;
    // this is a relative path, search in the current working dir
    AdjustedParent := nil;
    Pattern := Path('*');
  end;

  // compare name with all files in the current directory case-insensitive
  FileIter := FileSystem.FileFind(Pattern, faAnyFile);
  while (FileIter.HasNext()) do
  begin
    SearchInfo := FileIter.Next();
    PathFound := SearchInfo.Name;
    if (CompareText(LocalName.ToUTF8, PathFound.ToUTF8) = 0) then
    begin
      if (AdjustedParent <> nil) then
        Result := AdjustedParent.Append(PathFound)
      else
        Result := PathFound;
      Exit;
    end;
  end;

  // no matching file found
  Result := nil;
end;

function TPathImpl.AdjustCase(AdjustAllLevels: boolean): IPath;
begin
  AssertRefCount;

  Result := Self;

  if (FileSystem.IsCaseSensitive) then
  begin
    Result := AdjustCaseRecursive(Self, AdjustAllLevels);
    if (Result = nil) then
      Result := Self;
  end;
end;

function TPathImpl.IncludeTrailingPathDelimiter(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.IncludeTrailingPathDelimiter(Self);
end;

function TPathImpl.ExcludeTrailingPathDelimiter(): IPath;
begin
  AssertRefCount;
  Result := FileSystem.ExcludeTrailingPathDelimiter(Self);
end;

function TPathImpl.CreateFile(): THandle;
begin
  Result := FileSystem.FileCreate(Self);
end;

function TPathImpl.CreateDirectory(Force: boolean): boolean;
begin
  if (Force) then
    Result := FileSystem.ForceDirectories(Self)
  else
    Result := FileSystem.DirectoryCreate(Self);
end;

function TPathImpl.Open(Mode: LongWord): THandle;
begin
  Result := FileSystem.FileOpen(Self, Mode);
end;

function TPathImpl.OpenStream(Mode: Word): TUnicodeFileStream;
begin
  Result := TUnicodeFileStream.Create(Self, Mode);
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
  // note the different specifications of FileExists() on Win32 <> Unix
  {$IFDEF MSWINDOWS}
  Result := IsFile() or IsDirectory();
  {$ELSE}
  Result := FileSystem.FileExists(Self);
  {$ENDIF}
end;

function TPathImpl.IsFile(): boolean;
begin
  // note the different specifications of FileExists() on Win32 <> Unix
  {$IFDEF MSWINDOWS}
  Result := FileSystem.FileExists(Self);
  {$ELSE}
  Result := Exists() and not IsDirectory();
  {$ENDIF}
end;

function TPathImpl.IsDirectory(): boolean;
begin
  Result := FileSystem.DirectoryExists(Self);
end;

function TPathImpl.IsAbsolute(): boolean;
begin
  AssertRefCount;
  Result := FileSystem.FileIsReadOnly(Self);
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

function TPathImpl.FileSearch(const DirList: IPath): IPath;
begin
  AssertRefCount;
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


{ TUnicodeFileStream }

{$IFDEF MSWINDOWS}

constructor TUnicodeFileStream.Create(const FileName: IPath; Mode: Word);
begin
  inherited Create(FileName.ToWide(), Mode);
end;

{$ELSE}

constructor TUnicodeFileStream.Create(const FileName: IPath; Mode: Word);
begin
  inherited Create(FileName.ToNative(), Mode);
end;

{$ENDIF}

{ TUnicodeMemoryStream }

procedure TUnicodeMemoryStream.LoadFromFile(const FileName: IPath);
var
  Stream: TStream;
begin
  Stream := TUnicodeFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUnicodeMemoryStream.SaveToFile(const FileName: IPath);
var
  Stream: TStream;
begin
  Stream := TUnicodeFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

var
  PathNone_Singelton: IPath;

function PathNone(): IPath;
begin
  Result := PathNone_Singelton;
end;

initialization
  PathNone_Singelton := Path('');

finalization
  PathNone_Singelton := nil;

end.
