unit UMergeSort;

interface

type
	TDirection = (dSourceSource, dSourceTemp, dTempTemp, dTempSource);

	TCompareProc = function(TempIndex, SourceIndex: integer): boolean; cdecl;
  PCompareProc = ^TCompareProc;

	TCopyProc = function(iSourceIndex, iDestIndex: integer; bDirection: TDirection): boolean; cdecl;
  PCopyProc = ^TCopyProc;
	
	TMergeSorter = class
	private
		n:					integer;
		
		aCallbackCompare:	TCompareProc;
		aCallbackCopy:		TCopyProc;
		
		procedure MergeSort(lo, hi: integer);
		procedure Merge(lo, m, hi: integer);
	public
		procedure Sort(n: integer; aCompare: TCompareProc; aCopy: TCopyProc);
	end;
		

implementation

procedure TMergeSorter.Sort(n: integer; aCompare: TCompareProc; aCopy: TCopyProc);
begin
  Self.n := n;
	Self.aCallbackCompare := aCompare;
	Self.aCallbackCopy := aCopy;
  MergeSort(0, n-1);
end;

procedure TMergeSorter.MergeSort(lo, hi: integer);
var
	m:	integer;
	
begin
  if (lo<hi) then
	begin
		m := (lo+hi) div 2;
    MergeSort(lo, m);
    MergeSort(m+1, hi);
    Merge(lo, m, hi);
	end;
end;

procedure TMergeSorter.Merge(lo, m, hi: integer);
var
	i, j, k:	integer;
	
begin
	i := 0;
	j := lo;

  // vordere Hälfte von a in Hilfsarray b kopieren
  while (j<=m) do
	begin
		aCallbackCopy(j, i, dSourceTemp);
    //b[i] := a[j];
		inc(i);
		inc(j);
	end;

  i := 0;
	k := lo;
  // jeweils das nächstgrößte Element zurückkopieren
  while ((k<j) and (j<=hi)) do
	begin
		if aCallbackCompare(i, j) then
    //if (b[i]<=a[j]) then
		begin
			aCallbackCopy(i, k, dTempSource);
      //a[k] := b[i];
			inc(k);
			inc(i);
    end else
		begin
			aCallbackCopy(j, k, dSourceSource);
      //a[k] := a[j];
			inc(k);
			inc(j);
		end;
	end;

  // Rest von b falls vorhanden zurückkopieren
  while (k<j) do
	begin
		aCallbackCopy(i, k, dTempSource);
    //a[k] := b[i];
		inc(k);
		inc(i);
	end;

end;

end.
