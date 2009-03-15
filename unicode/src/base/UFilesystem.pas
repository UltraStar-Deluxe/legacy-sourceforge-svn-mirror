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

unit UFilesystem;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  TntSysUtils,
  {$ENDIF}
  UFilename,
  SysUtils,
  Classes;

type
  {$IFDEF MSWINDOWSe}
  TSytemSearchRec = TSearchRecW;
  {$ELSE}
  TSytemSearchRec = TSearchRec;
  {$ENDIF}

  TUniSearchRec = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    Name: IPath;
    ExcludeAttr: Integer;
    //FindHandle(): THandle;
  end;

  IFileFindIterator = interface
    function HasNext(): Boolean;
    function Next(): TUniSearchRec;
  end;

  IFileSystem = interface
    function ExpandFileName(const FileName: IPath): IPath;
    function FileCreate(const FileName: IPath): Integer;
    function DirectoryCreate(const Dir: IPath): Boolean;
    function FileOpen(const FileName: IPath; Mode: LongWord): Integer;
    function FileAge(const FileName: IPath): Integer; overload;
    function FileAge(const FileName: IPath; out FileDateTime: TDateTime): Boolean; overload;
    function DirectoryExists(const Name: IPath): Boolean;
    function FileExists(const Name: IPath): Boolean;
    function FileGetAttr(const FileName: IPath): Cardinal;
    function FileSetAttr(const FileName: IPath; Attr: Integer): Boolean;
    function FileIsReadOnly(const FileName: IPath): Boolean;
    function FileSetReadOnly(const FileName: IPath; ReadOnly: Boolean): Boolean;
    function ForceDirectories(const Dir: IPath): Boolean;
    function FileSearch(const Name: IPath; DirList: array of IPath): IPath;
    function RenameFile(const OldName, NewName: IPath): Boolean;
    function DeleteFile(const FileName: IPath): Boolean;
    function RemoveDir(const Dir: IPath): Boolean;
    function CopyFile(const Source, Target: IPath; FailIfExists: Boolean): Boolean;

    function FileFind(const FilePattern: IPath; Attr: Integer): IFileFindIterator;

    function FindFirst(const FilePattern: IPath; Attr: Integer; var F: TSytemSearchRec): Integer;
    function FindNext(var F: TSytemSearchRec): Integer;
    procedure FindClose(var F: TSytemSearchRec);

    function GetCurrentDir: IPath;
    function SetCurrentDir(const Dir: IPath): Boolean;

    function CreateFileStream(const FileName: IPath; Mode: Word): THandleStream;
  end;

var
  FileSystem: IFileSystem;

implementation

type
  TFileSystemImpl = class(TInterfacedObject, IFileSystem)
  public
    function ExpandFileName(const FileName: IPath): IPath;
    function FileCreate(const FileName: IPath): Integer;
    function DirectoryCreate(const Dir: IPath): Boolean;
    function FileOpen(const FileName: IPath; Mode: LongWord): Integer;
    function FileAge(const FileName: IPath): Integer; overload;
    function FileAge(const FileName: IPath; out FileDateTime: TDateTime): Boolean; overload;
    function DirectoryExists(const Name: IPath): Boolean;
    function FileExists(const Name: IPath): Boolean;
    function FileGetAttr(const FileName: IPath): Cardinal;
    function FileSetAttr(const FileName: IPath; Attr: Integer): Boolean;
    function FileIsReadOnly(const FileName: IPath): Boolean;
    function FileSetReadOnly(const FileName: IPath; ReadOnly: Boolean): Boolean;
    function ForceDirectories(const Dir: IPath): Boolean;
    function FileSearch(const Name: IPath; DirList: array of IPath): IPath;
    function RenameFile(const OldName, NewName: IPath): Boolean;
    function DeleteFile(const FileName: IPath): Boolean;
    function RemoveDir(const Dir: IPath): Boolean;
    function CopyFile(const Source, Target: IPath; FailIfExists: Boolean): Boolean;

    function FileFind(const FilePattern: IPath; Attr: Integer): IFileFindIterator;

    function FindFirst(const FilePattern: IPath; Attr: Integer; var F: TSytemSearchRec): Integer;
    function FindNext(var F: TSytemSearchRec): Integer;
    procedure FindClose(var F: TSytemSearchRec);

    function GetCurrentDir: IPath;
    function SetCurrentDir(const Dir: IPath): Boolean;

    function CreateFileStream(const FileName: IPath; Mode: Word): THandleStream;
  end;

  TFileFindIterator = class(TInterfacedObject, IFileFindIterator)
  private
    fHasNext: Boolean;
    fSearchRec: TSytemSearchRec;
  public
    constructor Create(const FilePattern: IPath; Attr: Integer);
    destructor Destroy(); override;

    function HasNext(): Boolean;
    function Next(): TUniSearchRec;
  end;


function TFileSystemImpl.CreateFileStream(const FileName: IPath; Mode: Word): THandleStream;
begin
  Result := TUniFileStream.Create(FileName, Mode);
end;

function TFileSystemImpl.FileFind(const FilePattern: IPath; Attr: Integer): IFileFindIterator;
begin
  Result := TFileFindIterator.Create(FilePattern, Attr);
end;

{$IFDEF MSWINDOWSs}

function TFileSystemImpl.ExpandFileName(const FileName: IPath): IPath;
begin
  Result := Path(WideExpandFileName(FileName.ToWideString()));
end;

function TFileSystemImpl.FileCreate(const FileName: IPath): Integer;
begin
  Result := WideFileCreate(FileName.ToWideString());
end;

function TFileSystemImpl.DirectoryCreate(const Dir: IPath): Boolean;
begin
  Result := WideCreateDir(Dir.ToWideString());
end;

function TFileSystemImpl.FileOpen(const FileName: IPath; Mode: LongWord): Integer;
begin
  Result := WideFileOpen(FileName.ToWideString(), Mode);
end;

function TFileSystemImpl.FileAge(const FileName: IPath): Integer;
begin
  Result := WideFileAge(FileName.ToWideString());
end;

function TFileSystemImpl.FileAge(const FileName: IPath; out FileDateTime: TDateTime): Boolean;
begin
  Result := WideFileAge(FileName.ToWideString(), FileDateTime);
end;

function TFileSystemImpl.DirectoryExists(const Name: IPath): Boolean;
begin
  Result := WideDirectoryExists(Name.ToWideString());
end;

function TFileSystemImpl.FileExists(const Name: IPath): Boolean;
begin
  Result := WideFileExists(Name.ToWideString());
end;

function TFileSystemImpl.FileGetAttr(const FileName: IPath): Cardinal;
begin
  Result := WideFileGetAttr(FileName.ToWideString());
end;

function TFileSystemImpl.FileSetAttr(const FileName: IPath; Attr: Integer): Boolean;
begin
  Result := WideFileSetAttr(FileName.ToWideString(), Attr);
end;

function TFileSystemImpl.FileIsReadOnly(const FileName: IPath): Boolean;
begin
  Result := WideFileIsReadOnly(FileName.ToWideString());
end;

function TFileSystemImpl.FileSetReadOnly(const FileName: IPath; ReadOnly: Boolean): Boolean;
begin
  Result := WideFileSetReadOnly(FileName.ToWideString(), ReadOnly);
end;

function TFileSystemImpl.ForceDirectories(const Dir: IPath): Boolean;
begin
  Result := WideForceDirectories(Dir.ToWideString());
end;

function TFileSystemImpl.FileSearch(const Name: IPath; DirList: array of IPath): IPath;
var
  I: integer;
  DirListStr: WideString;
begin
  DirListStr := '';
  for I := 0 to High(DirList) do
  begin
    if (I > 0) then
      DirListStr := DirListStr + PathSep;
    DirListStr := DirListStr + DirList[I].ToWideString();
  end;
  Result := Path(WideFileSearch(Name.ToWideString(), DirListStr));
end;

function TFileSystemImpl.RenameFile(const OldName, NewName: IPath): Boolean;
begin
  Result := WideRenameFile(OldName.ToWideString(), NewName.ToWideString());
end;

function TFileSystemImpl.DeleteFile(const FileName: IPath): Boolean;
begin
  Result := WideDeleteFile(FileName.ToWideString());
end;

function TFileSystemImpl.RemoveDir(const Dir: IPath): Boolean;
begin
  Result := WideRemoveDir(Dir.ToWideString());
end;

function TFileSystemImpl.CopyFile(const Source, Target: IPath; FailIfExists: Boolean): Boolean;
begin
  Result := WideCopyFile(Source.ToWideString(), Target.ToWideString(), FailIfExists);
end;

function TFileSystemImpl.FindFirst(const FilePattern: IPath; Attr: Integer; var F: TSytemSearchRec): Integer;
begin
  Result := WideFindFirst(FilePattern.ToWideString(), Attr, F);
end;

function TFileSystemImpl.FindNext(var F: TSytemSearchRec): Integer;
begin
  Result := WideFindNext(F);
end;

procedure TFileSystemImpl.FindClose(var F: TSytemSearchRec);
begin
  WideFindClose(F);
end;

function TFileSystemImpl.GetCurrentDir: IPath;
begin
  Result := Path(WideGetCurrentDir());
end;

function TFileSystemImpl.SetCurrentDir(const Dir: IPath): Boolean;
begin
  Result := WideSetCurrentDir(Dir.ToWideString());
end;

{$ELSE} // UNIX

function TFileSystemImpl.ExpandFileName(const FileName: IPath): IPath;
begin
  Result := Path(SysUtils.ExpandFileName(FileName.ToString(pencSystemANSI)), pencSystemANSI);
end;

function TFileSystemImpl.FileCreate(const FileName: IPath): Integer;
begin
  Result := SysUtils.FileCreate(FileName.ToString(pencSystemANSI));
end;

function TFileSystemImpl.DirectoryCreate(const Dir: IPath): Boolean;
begin
  Result := SysUtils.CreateDir(Dir.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileOpen(const FileName: IPath; Mode: LongWord): Integer;
begin
  Result := SysUtils.FileOpen(FileName.ToString(pencSystemANSI), Mode);
end;

function TFileSystemImpl.FileAge(const FileName: IPath): Integer;
begin
  Result := SysUtils.FileAge(FileName.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileAge(const FileName: IPath; out FileDateTime: TDateTime): Boolean;
var
  FileDate: longint;
begin
  FileDate := SysUtils.FileAge(FileName.ToString(pencSystemANSI));
  if (FileDate > -1) then
    FileDateTime := FileDateToDateTime(FileDate);
  Result := (FileDate > -1);
end;

function TFileSystemImpl.DirectoryExists(const Name: IPath): Boolean;
begin
  Result := SysUtils.DirectoryExists(Name.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileExists(const Name: IPath): Boolean;
begin
  Result := SysUtils.FileExists(Name.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileGetAttr(const FileName: IPath): Cardinal;
begin
  Result := SysUtils.FileGetAttr(FileName.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileSetAttr(const FileName: IPath; Attr: Integer): Boolean;
begin
  Result := (SysUtils.FileSetAttr(FileName.ToString(pencSystemANSI), Attr) = 0);
end;

function TFileSystemImpl.FileIsReadOnly(const FileName: IPath): Boolean;
begin
  Result := SysUtils.FileIsReadOnly(FileName.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileSetReadOnly(const FileName: IPath; ReadOnly: Boolean): Boolean;
begin
  Result := (SysUtils.FileSetAttr(FileName.ToString(pencSystemANSI), faReadOnly) = 0);
end;

function TFileSystemImpl.ForceDirectories(const Dir: IPath): Boolean;
begin
  Result := SysUtils.ForceDirectories(Dir.ToString(pencSystemANSI));
end;

function TFileSystemImpl.FileSearch(const Name: IPath; DirList: array of IPath): IPath;
var
  I: integer;
  DirListStr: AnsiString;
begin
  DirListStr := '';
  for I := 0 to High(DirList) do
  begin
    if (I > 0) then
      DirListStr := DirListStr + PathSep;
    DirListStr := DirListStr + DirList[I].ToString(pencSystemANSI);
  end;
  Result := Path(SysUtils.FileSearch(Name.ToString(pencSystemANSI), DirListStr), pencSystemANSI);
end;

function TFileSystemImpl.RenameFile(const OldName, NewName: IPath): Boolean;
begin
  Result := SysUtils.RenameFile(OldName.ToString(pencSystemANSI), NewName.ToString(pencSystemANSI));
end;

function TFileSystemImpl.DeleteFile(const FileName: IPath): Boolean;
begin
  Result := SysUtils.DeleteFile(FileName.ToString(pencSystemANSI));
end;

function TFileSystemImpl.RemoveDir(const Dir: IPath): Boolean;
begin
  Result := SysUtils.RemoveDir(Dir.ToString(pencSystemANSI));
end;

function TFileSystemImpl.CopyFile(const Source, Target: IPath; FailIfExists: Boolean): Boolean;
const
  COPY_BUFFER_SIZE = 4096; // a good tradeoff between speed and memory consumption
var
  SourceFile, TargetFile: TFileStream;
  FileCopyBuffer: array [0..COPY_BUFFER_SIZE-1] of byte; // temporary copy-buffer.
  NumberOfBytes: integer; // number of bytes read from SourceFile
begin
  Result := false;
  SourceFile := nil;
  TargetFile := nil;

  // if overwrite is disabled return if the target file already exists
  if (FailIfExists and FileExists(Target)) then
    Exit;

  try
    try
      // open source and target file (might throw an exception on error)
      SourceFile := TFileStream.Create(Source.ToString(pencSystemANSI), fmOpenRead);
      TargetFile := TFileStream.Create(Target.ToString(pencSystemANSI), fmCreate or fmOpenWrite);

      while true do
      begin
        // read a block from the source file and check for errors or EOF
        NumberOfBytes := SourceFile.Read(FileCopyBuffer, SizeOf(FileCopyBuffer));
        if (NumberOfBytes <= 0) then
          Break;
        // write block to target file and check if everything was written
        if (TargetFile.Write(FileCopyBuffer, NumberOfBytes) <> NumberOfBytes) then
          Exit;
      end;
    except
      Exit;
    end;
  finally
    SourceFile.Free;
    TargetFile.Free;
  end;

  Result := true;
end;

function TFileSystemImpl.FindFirst(const FilePattern: IPath; Attr: Integer; var F: TSytemSearchRec): Integer;
begin
  Result := SysUtils.FindFirst(FilePattern.ToString(pencSystemANSI), Attr, F);
end;

function TFileSystemImpl.FindNext(var F: TSytemSearchRec): Integer;
begin
  Result := SysUtils.FindNext(F);
end;

procedure TFileSystemImpl.FindClose(var F: TSytemSearchRec);
begin
  SysUtils.FindClose(F);
end;

function TFileSystemImpl.GetCurrentDir: IPath;
begin
  Result := Path(SysUtils.GetCurrentDir(), pencSystemANSI);
end;

function TFileSystemImpl.SetCurrentDir(const Dir: IPath): Boolean;
begin
  Result := SysUtils.SetCurrentDir(Dir.ToString(pencSystemANSI));
end;

{$ENDIF}


{ TFileFindIterator }

constructor TFileFindIterator.Create(const FilePattern: IPath; Attr: Integer);
begin
  inherited Create();
  fHasNext := (FileSystem.FindFirst(FilePattern, Attr, fSearchRec) = 0);
end;

destructor TFileFindIterator.Destroy();
begin
  FileSystem.FindClose(fSearchRec);
  inherited;
end;

function TFileFindIterator.HasNext(): Boolean;
begin
  Result := fHasNext;
end;

function TFileFindIterator.Next(): TUniSearchRec;
begin
  if (not fHasNext) then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  Result.Time := fSearchRec.Time;
  Result.Size := fSearchRec.Size;
  Result.Attr := fSearchRec.Attr;
  {$IFDEF MSWINDOWS}
  Result.Name := Path(fSearchRec.Name);
  {$ELSE}
  Result.Name := Path(fSearchRec.Name, pencSystemANSI);
  {$ENDIF}
  Result.ExcludeAttr := fSearchRec.ExcludeAttr;

  // fetch next entry
  fHasNext := (FileSystem.FindNext(fSearchRec) = 0);
end;


initialization
  FileSystem := TFileSystemImpl.Create;

finalization
  FileSystem := nil;

end.
