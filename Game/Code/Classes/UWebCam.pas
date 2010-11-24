unit UWebCam;

interface

uses
     math,
     gl,
     glu,
     glext,
     SysUtils,
     UIni,
     UTime,
     SDL,
     UCaptureWDM;

function wStartWebCam: boolean;
procedure wStopWebCam;

procedure wInit;
procedure wClose;
procedure wDraw(DoDraw: boolean; Screen: integer);


var
  WebCamReady:        boolean;
  FGrabFrameFlag :    boolean;

  FTex:               glUint;
  FTexX, FTexY:       integer;
  FdataX, FdataY:     integer;
  FrameDataPtr:       PByteArray;

  frame:              Pointer;

  WDMSample:          TSampleClass;

implementation

uses
  UGraphic,
  ULog,
  UDisplay;

function wStartWebCam(): boolean;
begin
  if not WebCamReady then
    wInit();

  Result := WebCamReady;
  if WebCamReady and not FGrabFrameFlag then
  begin
    glBindTexture(GL_TEXTURE_2D, FTex);

    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, FTexX, FTexY,
      GL_BGR, GL_UNSIGNED_BYTE, @FrameDataPtr[0]);
    glBindTexture(GL_TEXTURE_2D, 0);
      
    FGrabFrameFlag := true;

    WDMSample.Start;
  end;
end;

procedure wStopWebCam();
begin
  if WebCamReady and FGrabFrameFlag then
  begin
    FGrabFrameFlag := false;
    WDMSample.Stop;
  end;
end;

procedure wInit;
const
  width = 320;
  height = 240;

var
  IWebCamDevice:  TList;

begin
  if WebCamReady then
    exit;

  WebCamReady := false;

  if(Ini.EnableWebCam=0) then
    exit;

  GetCapDevices(IWebCamDevice);
  if (Length(IWebCamDevice)-1 < Ini.WebCamID) then
    Exit;

  try
    WDMSample := TSampleClass.Create(Ini.WebCamID, Ini.WebCamMediaID);
  except
    wClose;
    Log.LogError('Error init WDM (UWebCam.wInitWDM)');
    Exit;
  end;

  FTexX := width;
  FTexY := height;
  FdataX := Round(Power(2, Ceil(Log2(FTexX))));
  FdataY := Round(Power(2, Ceil(Log2(FTexY))));

  FrameDataPtr:=WDMSample.FramePtr;

  glGenTextures(1, @FTex);
  glBindTexture(GL_TEXTURE_2D, FTex);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_PRIORITY, 1.0);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_2D, 0, 3, FdataX, FdataY, 0,
    GL_BGR, GL_UNSIGNED_BYTE, nil);
  glBindTexture(GL_TEXTURE_2D, 0);

  FGrabFrameFlag := false;
  WebCamReady := true;
end;

procedure UploadNewFrame;
begin
  if (not WDMSample.CapStatus) then
    exit;

  FrameDataPtr := WDMSample.FramePtr;

  if (FTexX<>WDMSample.GetWidth) or (FTexY<>WDMSample.GetHeight) then
  begin
    FTexX := WDMSample.GetWidth;
    FTexY := WDMSample.GetHeight;
    FdataX := Round(Power(2, Ceil(Log2(FTexX))));
    FdataY := Round(Power(2, Ceil(Log2(FTexY))));

    glGenTextures(1, @FTex);
    glBindTexture(GL_TEXTURE_2D, FTex);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_PRIORITY, 1.0);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexImage2D(GL_TEXTURE_2D, 0, 3, FdataX, FdataY, 0,
      GL_BGR, GL_UNSIGNED_BYTE, nil);
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  glBindTexture(GL_TEXTURE_2D, FTex);

  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, FTexX, FTexY,
    GL_BGR, GL_UNSIGNED_BYTE, @FrameDataPtr[0]);
  glBindTexture(GL_TEXTURE_2D, 0);

  WDMSample.TriggerCapture;
end;


procedure wDraw(DoDraw: boolean; Screen: integer);
var
  SRect: record
    left, right, upper, lower:  double;
  end;

  ScreenAspect:       double;
  CamAspect:          double;
  ScaledVideoWidth:   double;
  ScaledVideoHeight:  double;

begin
  if not WebCamReady then
    exit;

  if DoDraw then
  begin
    try
      UploadNewFrame;
    except
      wClose;
      Log.LogError('Error Uploading new Frame (UWebCam.wDraw)');
      Exit;
    end;
  end else
    Exit;

  ScreenAspect := (ScreenW/Screens) / ScreenH;
  CamAspect := FTexX/FTexY;
  if (ScreenAspect >= 1) then
  begin
    ScaledVideoWidth  := RenderW;
    ScaledVideoHeight := RenderH * ScreenAspect/CamAspect;
  end else
  begin
    ScaledVideoHeight := RenderH;
    ScaledVideoWidth  := RenderW * CamAspect/ScreenAspect;
  end;

  SRect.left := (RenderW - ScaledVideoWidth) / 2;
  SRect.right := SRect.left + ScaledVideoWidth;
  SRect.lower := (RenderH - ScaledVideoHeight) / 2;
  SRect.upper := SRect.lower + ScaledVideoHeight;

  if (Screen=1) then
  begin
    glClearColor(0,0,0,1);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, FTex);
  glbegin(gl_quads);
    // upper-left coord
    glTexCoord2f(FTexX/FdataX, 0);
    glVertex2f(SRect.left, SRect.upper);
    // lower-left coord
    glTexCoord2f(FTexX/FdataX, FTexY/FdataY);
    glVertex2f(SRect.left, SRect.lower);
    // lower-right coord
    glTexCoord2f(0, FTexY/FdataY);
    glVertex2f(SRect.Right, SRect.lower);
    // upper-right coord
    glTexCoord2f(0, 0);
    glVertex2f(SRect.Right, SRect.upper);
  glEnd;

  glDisable(GL_BLEND);
end;

procedure wClose();
begin
  WebCamReady := false;
  FGrabFrameFlag := false;
  if (WDMSample<>nil) then
  begin
    WDMSample.Terminate;
    WDMSample.WaitFor;
    WDMSample.Free;
    WDMSample := nil;
  end;

  if(frame<>nil) then
    FreeMem(frame);
  frame := nil;

  WebCamReady := false;
  glDeleteTextures(1, @FTex);
end;

end.
