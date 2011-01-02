// this unit based on the code sniplet from https://forums.embarcadero.com/thread.jspa?threadID=8341&tstart=359
// and was modified by brunzel
// the original author is "harrie pearce"


unit UCaptureWDM;

interface

uses
	Classes, Windows, DSPack, DirectShow9, DSUtil, SDL, ExtCtrls, SyncObjs, ULog;

type
	TCaptureState = (csPlay, csStop, csDisbaled);
  TList = array of string;

	TCapture = class(TObject)
	private
		SysDev:           TSysDevEnum;
		FilterGraph:      TFilterGraph;
		SampleGrabber:    TSampleGrabber;
		Filter:           TFilter;
		NullRenderer:     TFilter;
		DeviceIndex:      Integer;
    MediaType:        Integer;
		CaptureState:     TCaptureState;
    VideoMediaTypes:  TEnumMediaType;
		function PrepareGraph: Boolean;
	public
		Image: TImage;
		constructor Create(DeviceID, MediaTypeID: integer);
		destructor Destroy; override;

		procedure SelectDevice(Index: Integer);
		procedure Play;
		procedure Stop;
	end;

	TSampleClass = class(TThread)
	private
		Capture:    TCapture;
    frame:      Pointer;
    width:      integer;
    height:     integer;
    capturing:  boolean;
    ready:      boolean;
    Error:      boolean;

    procedure GetImage;
	protected
    procedure Execute; override;
  public
    FramePtr:     PByteArray;
    EventDecode:  TEvent;
    
    constructor Create(DeviceID, MediaTypeID: integer);
    destructor Destroy; override;

    function GetWidth: integer;
    function GetHeight: integer;
    property CapStatus: boolean read ready;

    procedure TriggerCapture;
		procedure SelectDevice(Index: Integer);
		procedure Start;
		procedure Stop;
	end;

  procedure ListMediaTypes(DeviceID: integer; var types: TList);
  procedure GetCapDevices(var names: TList);

implementation

uses
	Graphics, SysUtils;

procedure GetCapDevices(var names: TList);
var
  k:        Integer;
  tSysDev:   TSysDevEnum;
begin
  SetLength(names, 0);
  try
    tSysDev := TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);
    SetLength(names, tSysDev.CountFilters);
    for k := 0 to tSysDev.CountFilters - 1 do
      names[k] := tSysDev.Filters[k].FriendlyName;
  except
    SetLength(names, 0);
    //Log.LogError('GetCapDevices #4');
  end;

  try
    if (tSysDev<>nil) then
      tSysDev.Free;
  except
    //Log.LogError('GetCapDevices #6');
  end;
end;

procedure ListMediaTypes(DeviceID: integer; var types: TList);
var
  PinList:          TPinList;
  tSysDev:          TSysDevEnum;
  Filter:           TFilter;
  FilterGraph:      TFilterGraph;
  VideoMediaTypes:  TEnumMediaType;
  k:                Integer;

begin
  SetLength(types, 0);

  try
    tSysDev := TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);

    FilterGraph := TFilterGraph.Create( nil );
	  FilterGraph.Mode := gmCapture;
	  FilterGraph.Active := False;
	  FilterGraph.AutoCreate := False;
	  FilterGraph.GraphEdit := True;

	  Filter := TFilter.Create( nil );
	  Filter.FilterGraph := FilterGraph;

    Filter.BaseFilter.Moniker := tSysDev.GetMoniker(DeviceID);
    Filter.FilterGraph.Active := true;
    PinList := TPinList.Create(Filter as IBaseFilter);
    VideoMediaTypes := TEnumMediaType.Create(PinList.First);

    SetLength(types, VideoMediaTypes.Count);
    for k := 0 to VideoMediaTypes.Count - 1 do
    begin
      types[k] := VideoMediaTypes.MediaFormatDescription[k];
      //writeln(Result[k]);
      //writeln(VideoMediaTypes.MediaFormatDescription[k]);
    end;
  except
    SetLength(types, 0);
  end;

  try
    if (PinList<>nil) then
      PinList.Free;
  except
  end;

  try
    if (VideoMediaTypes<>nil) then
      VideoMediaTypes.Free;
  except
  end;

  try
    if (FilterGraph<>nil) then
    begin
	    FilterGraph.Stop;
	    FilterGraph.ClearGraph;
	    FilterGraph.Active := False;
	    FilterGraph.Free;
    end;
  except
  end;

  try
    if (Filter<>nil) then
	    Filter.Free;
  except
  end;

  try
    if (tSysDev<>nil) then
	    tSysDev.Free;
  except
  end;
end;

constructor TSampleClass.Create(DeviceID, MediaTypeID: integer);
begin
  inherited Create(true);

  //Self.Priority := tpLower;
  Self.FreeOnTerminate := false;

	Capture := TCapture.Create(DeviceID, MediaTypeID);

  width := 320;
  height := 240;

  Capture.Image.Picture.Bitmap.SetSize(width, height);
  Capture.Image.Picture.Bitmap.PixelFormat := pf24bit;

  GetMem(frame, width*height*3);
  FramePtr := frame;

  capturing := false;
  ready := true;
  Error := false;

  EventDecode := TEvent.Create(nil, false, false, '');

  Self.Resume;
end;

destructor TSampleClass.Destroy;
begin
	inherited;

  FreeAndNil(EventDecode);
  
  if(frame<>nil) then
    FreeMem(frame);
  frame := nil;

	Capture.Free;
end;


procedure TSampleClass.Execute;
begin
  while not terminated do
  begin
    if (EventDecode.WaitFor(100) = wrSignaled) and capturing then
    begin
      ready := false;
      GetImage;

      capturing := false;
      ready := true;

      if Error then
        Self.Terminate;
    end;
  end;
end;

function TSampleClass.GetWidth: integer;
begin
  Result := Capture.Image.Picture.Width;
end;

function TSampleClass.GetHeight: integer;
begin
  Result := Capture.Image.Picture.Height;
end;

procedure TSampleClass.TriggerCapture;
begin
  if ready then
  begin
    ready := false;
    capturing := true;
  end else
    capturing := false;

  if capturing then
    EventDecode.SetEvent;
end;

procedure TSampleClass.GetImage;
var
  y:        integer;
  w, h:     integer;
  PLine:    PByteArray;

begin
	if (Capture.FilterGraph.State = gsPlaying) and (Capture.CaptureState = csPlay) then
	begin
		Capture.Image.Canvas.Lock;
		try
      try
			  Capture.SampleGrabber.GetBitmap(Capture.Image.Picture.Bitmap);

        w := Capture.Image.Picture.Width;
        h := Capture.Image.Picture.Height;

        if (w<>width) or (h<>height) then
        begin
          FreeMem(frame);
          frame := nil;
          GetMem(frame, w*h*3);
          width := w;
          height := h;
        end;

        FramePtr := frame;
        for y := 0 to h - 1 do
        begin
          PLine := Capture.Image.Picture.Bitmap.ScanLine[h-y-1];
          move(PLine[0], FramePtr[y*w*3], w*3);
        end;
      except
        Error := true;
      end;
    finally
			Capture.Image.Canvas.Unlock;
		end;
	end;
end;

procedure TSampleClass.SelectDevice(Index: Integer);
begin
	Capture.SelectDevice(Index);
end;

procedure TSampleClass.Start;
begin
	Capture.Play;
end;

procedure TSampleClass.Stop;
begin
	Capture.Stop;
end;



constructor TCapture.Create(DeviceID, MediaTypeID: integer);
var
	i, j:       Integer;

begin
	inherited Create;

	CaptureState := csStop;
	DeviceIndex := DeviceID;
  MediaType := MediaTypeID;

	SysDev := TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);

	Image := TImage.Create( nil );
	FillRect(Image.Canvas.Handle, Image.ClientRect,	GetStockObject(BLACK_BRUSH));

	FilterGraph := TFilterGraph.Create( nil );
	FilterGraph.Mode := gmCapture;
	FilterGraph.Active := False;
	FilterGraph.AutoCreate := False;
	FilterGraph.GraphEdit := True;

	SampleGrabber := TSampleGrabber.Create( nil );
	SampleGrabber.FilterGraph := FilterGraph;

	Filter := TFilter.Create( nil );
	Filter.FilterGraph := FilterGraph;

	NullRenderer := TFilter.Create( nil );
	NullRenderer.FilterGraph := FilterGraph;

	SysDev.SelectGUIDCategory(CLSID_ActiveMovieCategories);

	for i := 0 to SysDev.CountCategories - 1 do
	begin
		if SysDev.Categories[i].FriendlyName = 'DirectShow Filters' then
		begin
			SysDev.SelectIndexCategory(i);
			if SysDev.CountFilters > 0 then
			begin
				for j := 0 to SysDev.CountFilters - 1 do
				begin
					if SysDev.Filters[j].FriendlyName = 'Null Renderer' then
					begin
						NullRenderer.BaseFilter.Moniker := SysDev.GetMoniker(j);
						Break;
					end;
				end;
			end;
		Break;
		end;
	end;
end;

destructor TCapture.Destroy;
begin
	inherited;
  VideoMediaTypes.Free;
	FilterGraph.Stop;
	FilterGraph.ClearGraph;
	FilterGraph.Active := False;
	Image.Free;
	FilterGraph.Free;
	SampleGrabber.Free;
	Filter.Free;
	NullRenderer.Free;
	SysDev.Free;
end;

function TCapture.PrepareGraph: Boolean;
var
  PinList:  TPinList;

begin
	Result := False;
	SysDev.SelectGUIDCategory(CLSID_VideoInputDeviceCategory);
	if SysDev.CountFilters > 0 then
	begin
		if DeviceIndex < SysDev.CountFilters then
		begin
			FilterGraph.ClearGraph;
			FilterGraph.Active := False;
			Filter.BaseFilter.Moniker := SysDev.GetMoniker(DeviceIndex);

      FilterGraph.Active := True;

      if Filter.FilterGraph <> nil then
      begin
        PinList := TPinList.Create(Filter as IBaseFilter);
        if (VideoMediaTypes=nil) then
          VideoMediaTypes := TEnumMediaType.Create(PinList.First)
        else
          VideoMediaTypes.Assign(PinList.First);

          with (PinList.First as IAMStreamConfig) do
            SetFormat(VideoMediaTypes.Items[MediaType].AMMediaType^);
        PinList.Free;
      end;

			with FilterGraph as ICaptureGraphBuilder2 do
				RenderStream(@PIN_CATEGORY_PREVIEW, nil, Filter as IBaseFilter,
					SampleGrabber as IBaseFilter, NullRenderer as IbaseFilter);
			Result := True;
		end;
	end;
end;

procedure TCapture.SelectDevice(Index: Integer);
begin
	DeviceIndex := Index;
end;

procedure TCapture.Play;
begin
	if FilterGraph.State <> gsPlaying then
	begin
		if PrepareGraph then
		begin
			FilterGraph.Play;
			CaptureState := csPlay;
		end;
	end;
end;

procedure TCapture.Stop;
begin
	if (CaptureState = csPlay) or (FilterGraph.State = gsPlaying) or
		(FilterGraph.State = gsPaused) then
	begin
		CaptureState := csStop;
		FilterGraph.Stop;
		FilterGraph.Active := False;
	end;
end;

end.