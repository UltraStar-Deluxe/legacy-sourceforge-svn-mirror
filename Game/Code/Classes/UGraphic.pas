unit UGraphic;

interface
uses
  SDL, gl, glext, UVideo, UTexture, ULanguage, TextGL, ULog, SysUtils, ULyrics, UScreenLoading,
  UScreenWelcome, UScreenMain, UScreenName, UScreenLevel, UScreenOptions, UScreenOptionsGame,
  UScreenOptionsGraphics, UScreenOptionsSound, UScreenOptionsLyrics, UScreenOptionsThemes, UScreenOptionsRecord,
  UScreenOptionsAdvanced,
  UScreenSong, UScreenSing, UScreenScore, UScreenTop, UScreenEditSub,
  UScreenEdit, UScreenEditConvert, UScreenEditHeader, UScreenOpen, UThemes, USkins, UScreenSongMenu, UScreenSongJumpto,
  {Party Screens} UScreenSingModi, UScreenPartyNewRound, UScreenPartyScore, UScreenPartyOptions, UScreenPartyWin, UScreenPartyPlayer,
  {Stats Screens} UScreenStatMain, UScreenStatDetail,
  {CreditsScreen} UScreenCredits,
  {Popup for errors, etc.} UScreenPopup,
  UScreenPartyOptionsM2, UScreenPartyPlayerM2, UScreenPartyNewRoundM2;

type
  TRecR = record
    Top:    real;
    Left:   real;
    Right:  real;
    Bottom: real;
  end;

var
  Screen:             PSDL_Surface;

  RenderW:    integer;
  RenderH:    integer;
  ScreenW:    integer;
  ScreenH:    integer;
  Screens:    integer;
  ScreenAct:  integer;
  ScreenX:    integer;

  ScreenLoading:      TScreenLoading;
  ScreenWelcome:      TScreenWelcome;
  ScreenMain:         TScreenMain;
  ScreenName:         TScreenName;
  ScreenLevel:        TScreenLevel;
  ScreenSong:         TScreenSong;
  ScreenSing:         TScreenSing;
  ScreenScore:        TScreenScore;
  ScreenTop:          TScreenTop;
  ScreenOptions:          TScreenOptions;
  ScreenOptionsGame:      TScreenOptionsGame;
  ScreenOptionsGraphics:  TScreenOptionsGraphics;
  ScreenOptionsSound:     TScreenOptionsSound;
  ScreenOptionsLyrics:    TScreenOptionsLyrics;
  ScreenOptionsThemes:    TScreenOptionsThemes;
  ScreenOptionsRecord:    TScreenOptionsRecord;
  ScreenOptionsAdvanced:  TScreenOptionsAdvanced;
  ScreenEditSub:      TScreenEditSub;
  ScreenEdit:         TScreenEdit;
  ScreenEditConvert:  TScreenEditConvert;
  ScreenEditHeader:   TScreenEditHeader;
  ScreenOpen:         TScreenOpen;

  ScreenSongMenu:     TScreenSongMenu;
  ScreenSongJumpto:     TScreenSongJumpto;

  //Party Screens
  ScreenSingModi:         TScreenSingModi;
  ScreenPartyNewRound:    TScreenPartyNewRound;
  ScreenPartyScore:       TScreenPartyScore;
  ScreenPartyWin:         TScreenPartyWin;
  ScreenPartyOptions:     TScreenPartyOptions;
  ScreenPartyPlayer:      TScreenPartyPlayer;

  //PartyM2 Screens
  //ScreenSingModi:         TScreenSingModi;
  ScreenPartyNewRoundM2:  TScreenPartyNewRoundM2;
  //ScreenPartyScore:       TScreenPartyScore;
  //ScreenPartyWin:         TScreenPartyWin;
  ScreenPartyOptionsM2:   TScreenPartyOptionsM2;
  ScreenPartyPlayerM2:    TScreenPartyPlayerM2;

  //StatsScreens
  ScreenStatMain:         TScreenStatMain;
  ScreenStatDetail:       TScreenStatDetail;

  //CreditsScreen
  ScreenCredits: TScreenCredits;

  //popup mod
  ScreenPopupCheck: TScreenPopupCheck;
  ScreenPopupError: TScreenPopupError;

  //popup Help-System
  ScreenPopupHelp:  TScreenPopupHelp;

  //Notes
  Tex_Left:       array[0..6] of TTexture;
  Tex_Mid:        array[0..6] of TTexture;
  Tex_Right:      array[0..6] of TTexture;

  Tex_BG_Left:    array[1..6] of TTexture;
  Tex_BG_Mid:     array[1..6] of TTexture;
  Tex_BG_Right:   array[1..6] of TTexture;

  Tex_Note_Star:  TTexture;
  Tex_Note_Perfect_Star: TTexture;


  Tex_Ball:       TTexture;
  Tex_Lyric_Help_Bar: TTexture;
  FullScreen:     boolean;

  Tex_TimeProgress: TTexture;

  //Sing Bar Mod
  Tex_SingBar_Back:  TTexture;
  Tex_SingBar_Bar:  TTexture;
  Tex_SingBar_Front:  TTexture;
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  Tex_SingLineBonusBack: TTexture;
  //End PhrasenBonus - Line Bonus Mod

const
  Skin_BGColorR = 1;
  Skin_BGColorG = 1;
  Skin_BGColorB = 1;

  Skin_SpectrumR = 0;
  Skin_SpectrumG = 0;
  Skin_SpectrumB = 0;

  Skin_Spectograph1R = 0.6;
  Skin_Spectograph1G = 0.8;
  Skin_Spectograph1B = 1;

  Skin_Spectograph2R = 0;
  Skin_Spectograph2G = 0;
  Skin_Spectograph2B = 0.2;

  Skin_SzczytR = 0.8;
  Skin_SzczytG = 0;
  Skin_SzczytB = 0;

  Skin_SzczytLimitR = 0;
  Skin_SzczytLimitG = 0.8;
  Skin_SzczytLimitB = 0;

  Skin_FontR = 0;
  Skin_FontG = 0;
  Skin_FontB = 0;

  Skin_FontHighlightR = 0.3; // 0.3
  Skin_FontHighlightG = 0.3; // 0.3
  Skin_FontHighlightB = 1;   // 1

  Skin_TimeR = 0.25; //0,0,0
  Skin_TimeG = 0.25;
  Skin_TimeB = 0.25;

  Skin_OscR = 0;
  Skin_OscG = 0;
  Skin_OscB = 0;

  Skin_LyricsT = 494; // 500 / 510 / 400
  Skin_SpectrumT = 470;
  Skin_SpectrumBot = 570;
  Skin_SpectrumH = 100;

  Skin_P1_LinesR = 0.5;  // 0.6 0.6 1
  Skin_P1_LinesG = 0.5;
  Skin_P1_LinesB = 0.5;

  Skin_P2_LinesR = 0.5; // 1 0.6 0.6
  Skin_P2_LinesG = 0.5;
  Skin_P2_LinesB = 0.5;

  Skin_P1_NotesB = 250;
  Skin_P2_NotesB = 430; // 430 / 300

  Skin_P1_ScoreT = 50;
  Skin_P1_ScoreL = 20;

  Skin_P2_ScoreT = 50;
  Skin_P2_ScoreL = 640;

procedure Initialize3D (Title: string);
procedure Reinitialize3D;
procedure SwapBuffers;

procedure LoadTextures;
procedure InitializeScreen;
procedure LoadScreens( aShowLoading : boolean = true );
procedure UnLoadScreens;
procedure UpdateScreenLoading(txt: string);

implementation
uses UMain, UIni, UDisplay, UCommandLine, Graphics, Classes, Windows, UWebCam;

procedure LoadTextures;
var
  P:        integer;
  R, G, B:  real;
  Col:      integer;
begin
   // zaladowanie tekstur
  Log.LogStatus('Loading Textures', 'LoadTextures');
  Tex_Left[0] :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayLeft')),  'BMP', 'Transparent', 0);
  Tex_Mid[0] :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayMid')),   'BMP', 'Plain', 0);
  Tex_Right[0] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayRight')), 'BMP', 'Transparent', 0);

  // P1-6
  for P := 1 to 6 do begin
    LoadColor(R, G, B, 'P' + IntToStr(P) + 'Light');
    Col := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Left[P] :=    Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayLeft')),  'BMP', 'Note Transparent', Col);
    Tex_Mid[P] :=     Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayMid')),   'BMP', 'Note Plain', Col);
    Tex_Right[P] :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('GrayRight')), 'BMP', 'Note Transparent', Col);

    Tex_BG_Left[P] :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGLeft')),  'BMP', 'Alpha Black Colored', Col);
    Tex_BG_Mid[P] :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGMid')),   'BMP', 'Alpha Black Colored', Col);
    Tex_BG_Right[P] := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteBGRight')), 'BMP', 'Alpha Black Colored', Col);
  end;

  Tex_Note_Perfect_Star := Texture.LoadTexture(pchar(Skin.GetTextureFileName('NotePerfectStar')), 'JPG', 'Font Black', 0);
  Tex_Note_Star :=   Texture.LoadTexture(pchar(Skin.GetTextureFileName('NoteStar')) , 'JPG', 'Alpha Black Colored', $FFFFFF);
  Tex_Ball :=        Texture.LoadTexture(pchar(Skin.GetTextureFileName('Ball')), 'BMP', 'Transparent', $FF00FF);
  Tex_Lyric_Help_Bar := Texture.LoadTexture(pchar(Skin.GetTextureFileName('LyricHelpBar')), 'BMP', 'Transparent', $FF00FF);


  //TimeBar mod
  Tex_TimeProgress :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('TimeBar')));
  //eoa TimeBar mod

  //SingBar Mod
  Tex_SingBar_Back :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarBack')),   'JPG', 'Plain', 0);
  Tex_SingBar_Bar :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarBar')),   'JPG', 'Plain', 0);
  Tex_SingBar_Front :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('SingBarFront')),   'JPG', 'Font', 0);
  //end Singbar Mod

  //PhrasenBonus - Line Bonus Mod
  Tex_SingLineBonusBack :=  Texture.LoadTexture(pchar(Skin.GetTextureFileName('LineBonusBack')), 'JPG', 'Font Black', 0);
  {//Set Texture to Font High
  Tex_SingLineBonusL.H := 32; Tex_SingLineBonusL.W := 8;
  Tex_SingLineBonusM.H := 32; //Tex_SingLineBonusM.TexW := Tex_SingLineBonusM.TexW/2;
  Tex_SingLineBonusR.H := 32; Tex_SingLineBonusR.W := 8;  }
  //PhrasenBonus - Line Bonus Mod End

  // tworzenie czcionek
  Log.LogStatus('Building Fonts', 'LoadTextures');
  BuildFont;
end;

procedure Initialize3D (Title: string);
//var
  //Icon: TIcon;
  //Res:  TResourceStream;
  //ISurface: PSDL_Surface;
  //Pixel: PByteArray;
begin
  Log.LogStatus('LoadOpenGL', 'Initialize3D');
  Log.BenchmarkStart(2);

  {
  Log.LogStatus('SDL_Init', 'Initialize3D');
  if ( SDL_Init(SDL_INIT_VIDEO)= -1 ) then begin
    Log.LogError('SDL_Init Failed', 'Initialize3D');
    exit;
  end;}

 { //Load Icon
  Res := TResourceStream.CreateFromID(HInstance, 3, RT_ICON);
  Icon := TIcon.Create;
  Icon.LoadFromStream(Res);
  Res.Free;
  Icon.
  //Create icon Surface
  SDL_CreateRGBSurfaceFrom (
  SDL_SWSURFACE,
  Icon.Width,
  Icon.Height,
  32,
  128 or 64,
  32 or 16,
  8 or 4,
  2 or 1);
  //SDL_BlitSurface(


  SDL_WM_SetIcon(SDL_LoadBMP('DEFAULT_WINDOW_ICON'), 0); //}

  SDL_WM_SetCaption(PChar(Title), nil);

  InitializeScreen;

  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Setting Screen', 2);

  // ladowanie tekstur
  Log.BenchmarkStart(2);
  Texture := TTextureUnit.Create;
  Texture.Limit := 1024*1024;

  LoadTextures;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Textures', 2);

  Log.BenchmarkStart(2);
  //Lyric := TLyric.Create;
  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Fonts', 2);

  Log.BenchmarkStart(2);
  Display := TDisplay.Create;
  SDL_EnableUnicode(1);
  Log.BenchmarkEnd(2); Log.LogBenchmark('====> Creating Display', 2);

  Log.LogStatus('Loading Screens', 'Initialize3D');
  Log.BenchmarkStart(3);

  LoadScreens;

  if (Ini.LoadFaultySongs=1) and (Log.NumErrors>0) then
    ScreenMain.ShowNumErrors := true;

  if (Ini.ShowCredits=1) then
  begin
    Ini.ShowCredits := 0;
    Ini.Save;
    Display.ActualScreen^.FadeTo(@ScreenCredits)
  end else
    Display.ActualScreen^.FadeTo(@ScreenMain);

  Log.BenchmarkEnd(2);
  Log.LogBenchmark('--> Loading Screens', 2);

  Log.LogStatus('Finish', 'Initialize3D');
end;

procedure SwapBuffers;
begin
  SDL_GL_SwapBuffers;
  glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(0, RenderW, RenderH, 0, -1, 100);
  glMatrixMode(GL_MODELVIEW);
end;

procedure Reinitialize3D;
begin
//  InitializeScreen;
//  LoadTextures;
//  LoadScreens;
end;

procedure InitializeScreen;
var
  S:          String;
  I:          Integer;
  W, H:       Integer;
  Depth:      Integer;
  videoFlags: Integer;
  videoInfo:  PSDL_VideoInfo;

begin
  if (Params.Screens <> -1) then
    Screens := Params.Screens + 1
  else
    Screens := Ini.Screens + 1;

  // If there is a resolution in Parameters, use it, else use the Ini value
  I := Params.Resolution;
  if (I <> -1) then
    S := IResolution[I]
  else
    S := IResolution[Ini.Resolution];

  I := Pos('x', S);
  W := StrToInt(Copy(S, 1, I-1)) * Screens;
  H := StrToInt(Copy(S, I+1, 1000));

  If (Params.Depth <> -1) then
    Depth := Params.Depth
  else
    Depth := Ini.Depth;


  Log.LogStatus('SDL_SetVideoMode', 'Initialize3D');

  videoInfo := SDL_GetVideoInfo;
  if ( videoInfo = nil ) then
  begin
    Log.LogError('Could not get video info: ' + SDL_GetError, 'Initialize3D' );
    Exit;
  end;

  videoFlags := SDL_OPENGL or
                SDL_DOUBLEBUF or
                SDL_HWPALETTE;

  if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;

  if ( videoInfo.blit_hw <> 0 ) then videoFlags := videoFlags or SDL_HWACCEL;
  //LoadOpenGL( GLLibName );
  
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE,      5);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,    5);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,     5);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,    16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER,  1);

  SDL_GL_SetAttribute( SDL_GL_STENCIL_SIZE,  8 );

  if (Ini.FullScreen = 0) and (Not Params.FullScreen) then
    screen := SDL_SetVideoMode(W, H, (Depth+1) * 16, videoFlags)
  else begin
    screen := SDL_SetVideoMode(W, H, (Depth+1) * 16, videoFlags or SDL_FULLSCREEN);
    SDL_ShowCursor(0);
  end;

  if (screen = nil) then begin
    Log.LogError('SDL_SetVideoMode Failed', 'Initialize3D');
    exit;
  end;
  
  // Load OpenGL 1.2 extensions for OpenGL 1.2 compatibility
  if (not Load_GL_version_1_2()) then
  begin
    Log.LogError('Failed loading OpenGL 1.2');
  end;
  
  pbo_supported := false;
  if (Ini.EnablePBO=1) then
  begin
    try
      pbo_supported := glext_LoadExtension('GL_ARB_pixel_buffer_object') and
        glext_LoadExtension('GL_version_1_5');
    except
      pbo_supported := false;
      Log.LogError('The device does not support Pixel Buffer Object (UVideo)!');
    end;
  end;

  // zmienne
  RenderW := 800;
  RenderH := 600;
  ScreenW := W;
  ScreenH := H;

  // clear screen once window is being shown
  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  SwapBuffers;
  SDL_Delay(1);
end;

procedure LoadScreens( aShowLoading : boolean = true );
begin

  ScreenLoading := TScreenLoading.Create;
  if aShowLoading then
  begin
    ScreenLoading.onShow;
    Display.ActualScreen := @ScreenLoading;
    ScreenLoading.Draw;
    Display.Draw;
    SwapBuffers;
  end;
  SDL_Delay(1);
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Loading', 3); Log.BenchmarkStart(3);
{ ScreenWelcome :=          TScreenWelcome.Create; //'BG', 4, 3);
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Welcome', 3); Log.BenchmarkStart(3);}
  ScreenMain :=             TScreenMain.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Main', 3); Log.BenchmarkStart(3);
  ScreenName :=             TScreenName.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Name', 3); Log.BenchmarkStart(3);
  ScreenLevel :=            TScreenLevel.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Level', 3); Log.BenchmarkStart(3);
  ScreenSong :=             TScreenSong.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Song', 3); Log.BenchmarkStart(3);

  if(aShowLoading) then
    UpdateScreenLoading(Language.Translate('SING_LOADING'));
  SDL_Delay(1);
  ScreenSongMenu :=             TScreenSongMenu.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Song Menu', 3); Log.BenchmarkStart(3);
  ScreenSing :=             TScreenSing.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Sing', 3); Log.BenchmarkStart(3);
  ScreenScore :=            TScreenScore.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Score', 3); Log.BenchmarkStart(3);
  ScreenTop :=             TScreenTop.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Top5', 3); Log.BenchmarkStart(3);
  ScreenOptions :=          TScreenOptions.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options', 3); Log.BenchmarkStart(3);
  ScreenOptionsGame :=      TScreenOptionsGame.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Game', 3); Log.BenchmarkStart(3);
  ScreenOptionsGraphics  :=  TScreenOptionsGraphics.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Graphics', 3); Log.BenchmarkStart(3);
  ScreenOptionsSound    :=     TScreenOptionsSound.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Sound', 3); Log.BenchmarkStart(3);
  ScreenOptionsLyrics   :=    TScreenOptionsLyrics.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Lyrics', 3); Log.BenchmarkStart(3);
  ScreenOptionsThemes   :=    TScreenOptionsThemes.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Themes', 3); Log.BenchmarkStart(3);
  ScreenOptionsRecord   :=    TScreenOptionsRecord.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Record', 3); Log.BenchmarkStart(3);
  ScreenOptionsAdvanced :=    TScreenOptionsAdvanced.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Options Advanced', 3); Log.BenchmarkStart(3);
  ScreenEditSub :=          TScreenEditSub.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Edit Sub', 3); Log.BenchmarkStart(3);
  ScreenEdit :=             TScreenEdit.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Edit', 3); Log.BenchmarkStart(3);
  ScreenEditConvert :=      TScreenEditConvert.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen EditConvert', 3); Log.BenchmarkStart(3);
//  ScreenEditHeader :=       TScreenEditHeader.Create(Skin.ScoreBG);
//  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Edit Header', 3); Log.BenchmarkStart(3);
  ScreenOpen :=             TScreenOpen.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Open', 3); Log.BenchmarkStart(3);
  ScreenSingModi :=         TScreenSingModi.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Sing with Modi support', 3); Log.BenchmarkStart(3);
  ScreenSongMenu :=         TScreenSongMenu.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen SongMenu', 3); Log.BenchmarkStart(3);
  ScreenSongJumpto :=         TScreenSongJumpto.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen SongJumpto', 3); Log.BenchmarkStart(3);
  ScreenPopupCheck := TScreenPopupCheck.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Popup (Check)', 3); Log.BenchmarkStart(3);
  ScreenPopupError := TScreenPopupError.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Popup (Error)', 3); Log.BenchmarkStart(3);
  ScreenPopupHelp := TScreenPopupHelp.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Popup (Help)', 3); Log.BenchmarkStart(3);
  ScreenPartyNewRound :=    TScreenPartyNewRound.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyNewRound', 3); Log.BenchmarkStart(3);
  ScreenPartyScore :=       TScreenPartyScore.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyScore', 3); Log.BenchmarkStart(3);
  ScreenPartyWin :=         TScreenPartyWin.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyWin', 3); Log.BenchmarkStart(3);
  ScreenPartyOptions :=     TScreenPartyOptions.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyOptions', 3); Log.BenchmarkStart(3);
  ScreenPartyPlayer :=      TScreenPartyPlayer.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyPlayer', 3); Log.BenchmarkStart(3);
  ScreenStatMain :=         TScreenStatMain.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Stat Main', 3); Log.BenchmarkStart(3);
  ScreenStatDetail :=       TScreenStatDetail.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Stat Detail', 3); Log.BenchmarkStart(3);
  if (Ini.ShowCredits=1) then
  begin
    ScreenCredits    :=       TScreenCredits.Create;
    Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen Credits', 3); Log.BenchmarkStart(3);
  end;
  //PartyM2 Screens
  ScreenPartyOptionsM2 :=   TScreenPartyOptionsM2.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyOptionsM2', 3); Log.BenchmarkStart(3);
  ScreenPartyPlayerM2 :=    TScreenPartyPlayerM2.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyPlayerM2', 3); Log.BenchmarkStart(3);
  ScreenPartyNewRoundM2 :=  TScreenPartyNewRoundM2.Create;
  Log.BenchmarkEnd(3); Log.LogBenchmark('====> Screen PartyNewRoundM2', 3); Log.BenchmarkStart(3);
  SDL_Delay(1);
  end;

procedure UnLoadScreens;
begin
(*
  ScreenLoading := TScreenLoading.Create;
  ScreenLoading.onShow;

  Display.ActualScreen := @ScreenLoading;

  ScreenLoading.Draw;
  Display.Draw;
  SwapBuffers;
*)  

  freeandnil( ScreenMain );
  freeandnil( ScreenName );
  freeandnil( ScreenLevel);
  freeandnil( ScreenSong );
  freeandnil( ScreenSongMenu );
  freeandnil( ScreenSing );
  freeandnil( ScreenScore);
  freeandnil( ScreenTop );
  freeandnil( ScreenOptions );
  freeandnil( ScreenOptionsGame );
  freeandnil( ScreenOptionsGraphics );
  freeandnil( ScreenOptionsSound );
  freeandnil( ScreenOptionsLyrics );
//  freeandnil( ScreenOptionsThemes );
  freeandnil( ScreenOptionsRecord );
  freeandnil( ScreenOptionsAdvanced );
  freeandnil( ScreenEditSub );
  freeandnil( ScreenEdit );
  freeandnil( ScreenEditConvert );
  freeandnil( ScreenOpen );
  freeandnil( ScreenSingModi );
  freeandnil( ScreenSongMenu );
  freeandnil( ScreenSongJumpto);
  freeandnil( ScreenPopupCheck );
  freeandnil( ScreenPopupError );
  freeandnil( ScreenPartyNewRound );
  freeandnil( ScreenPartyScore    );
  freeandnil( ScreenPartyWin     );
  freeandnil( ScreenPartyOptions  );
  freeandnil( ScreenPartyPlayer   );
  freeandnil( ScreenStatMain     );
  freeandnil( ScreenStatDetail    );

  //Party M2 Screens
  freeandnil( ScreenPartyNewRoundM2 );
  //freeandnil( ScreenPartyScoreM2    );
  //freeandnil( ScreenPartyWinM2     );
  freeandnil( ScreenPartyOptionsM2  );
  freeandnil( ScreenPartyPlayerM2   );
end;

procedure UpdateScreenLoading(txt: string);
begin
  ScreenLoading.Text[0].Text := txt;
  ScreenLoading.Draw;
  //Display.Draw;
  SwapBuffers;
end;

end.