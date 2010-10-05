unit UCovers;

interface
uses gl, Windows, Math, Classes, SysUtils, Graphics, UThemes, UTexture;

type
  TCover = record
    Name:       string;
    W:          word;
    H:          word;
    Size:       integer;
    Position:   integer; // position of picture in the cache file
//    Data:       array of byte;
  end;

  TCovers = class
  private
    SearchPos:  integer;
  public
    Cover:      array of TCover;
    W:          word;
    H:          word;
    Size:       integer;
    Data:       array of byte;
    WritetoFile: Boolean;

    constructor Create;
    procedure Load;
    procedure Save;
    procedure AddCover(const Name: string);
    function CoverExists(const Name: string): boolean;
    function CoverNumber(const Name: string): integer;
    procedure PrepareData(const Name: string);
  end;

var
  Covers:     TCovers;

implementation
uses UFiles, ULog, DateUtils;

constructor TCovers.Create;
begin
  W := 128;
  H := 128;
  Size := W*H*3;
  Load;
  WritetoFile := True;
  SearchPos := 0;
end;

procedure TCovers.Load;
var
  F:      File;
  W:      word;
  H:      word;
  Bits:   byte;
  NLen:   word;
  Name:   string;

begin
  if FileExists(GamePath + 'covers.cache') then
  begin
    AssignFile(F, GamePath + 'covers.cache');
    Reset(F, 1);

    WritetoFile := not FileIsReadOnly(GamePath + 'covers.cache');

    SetLength(Cover, 0);

    while not EOF(F) do
    begin
      SetLength(Cover, Length(Cover)+1);

      BlockRead(F, W, 2);
      Cover[High(Cover)].W := W;

      BlockRead(F, H, 2);
      Cover[High(Cover)].H := H;

      BlockRead(F, Bits, 1);

      Cover[High(Cover)].Size := W * H * (Bits div 8);

      BlockRead(F, NLen, 2);
      SetLength(Name, NLen);

      BlockRead(F, Name[1], NLen);
      Cover[High(Cover)].Name := Name;

      Cover[High(Cover)].Position := FilePos(F);
      Seek(F, FilePos(F) + W*H*(Bits div 8));
  end;

  CloseFile(F);
  end; // fileexists
end;

procedure TCovers.Save;
{
var
  F:      File;
  C:      integer; // cover number
  W:      word;
  H:      word;
  NLen:   word;
  Bits:   byte;  }
begin
{  AssignFile(F, GamePath + 'covers.cache');
  Rewrite(F, 1);

  Bits := 24;
  for C := 0 to High(Cover) do begin
    W := Cover[C].W;
    H := Cover[C].H;

    BlockWrite(F, W, 2);
    BlockWrite(F, H, 2);
    BlockWrite(F, Bits, 1);

    NLen := Length(Cover[C].Name);
    BlockWrite(F, NLen, 2);
    BlockWrite(F, Cover[C].Name[1], NLen);
    BlockWrite(F, Cover[C].Data[0], W*H*(Bits div 8));
  end;

  CloseFile(F);}
end;

procedure TCovers.AddCover(const Name: string);
var
  //B:      integer;
  F:      File;
  //C:      integer; // cover number
  NLen:   word;
  Bits:   byte;
begin
  if not CoverExists(Name) then
  begin
    SetLength(Cover, Length(Cover)+1);
    Cover[High(Cover)].Name := Name;

    Cover[High(Cover)].W := W;
    Cover[High(Cover)].H := H;
    Cover[High(Cover)].Size := Size;

    if WritetoFile then
    begin
      AssignFile(F, GamePath + 'covers.cache');
      
      if FileExists(GamePath + 'covers.cache') then
      begin
        Reset(F, 1);
        Seek(F, FileSize(F));
      end else
        Rewrite(F, 1);

      Bits := 24;

      BlockWrite(F, W, 2);
      BlockWrite(F, H, 2);
      BlockWrite(F, Bits, 1);

      NLen := Length(Name);
      BlockWrite(F, NLen, 2);
      BlockWrite(F, Name[1], NLen);

      Cover[High(Cover)].Position := FilePos(F);
      BlockWrite(F, CacheMipmap[0], W*H*(Bits div 8));

      CloseFile(F);
    end;
  end else
    Cover[High(Cover)].Position := 0;
end;

function TCovers.CoverExists(const Name: string): boolean;
var
  C:    integer; // cover
begin
  Result := false;
  C := SearchPos;
  while (C <= High(Cover)) and not Result do
  begin
    if Cover[C].Name = Name then
    begin
      Result := true;
      SearchPos := C;
    end;
    Inc(C);
  end;

  if Result then
    Exit;

  C := 0;
  while (C <= High(Cover)) and not Result do
  begin
    if Cover[C].Name = Name then
    begin
      Result := true;
      SearchPos := C;
    end;
    Inc(C);
  end;
end;

function TCovers.CoverNumber(const Name: string): integer;
var
  C:    integer;
begin
  Result := -1;
  C := 0;
  while (C <= High(Cover)) and (Result = -1) do
  begin
    if Cover[C].Name = Name then
      Result := C;
    Inc(C);
  end;
end;

procedure TCovers.PrepareData(const Name: string);
var
  F:  File;
  C:  integer;
begin
  if FileExists(GamePath + 'covers.cache') then
  begin
    AssignFile(F, GamePath + 'covers.cache');
    Reset(F, 1);

    C := CoverNumber(Name);
    SetLength(Data, Cover[C].Size);
    if Length(Data) < 6 then beep;
    Seek(F, Cover[C].Position);
    BlockRead(F, Data[0], Cover[C].Size);
    CloseFile(F);
  end;
end;

end.