program UltraStar;

{$DEFINE TRANSLATE}

{$R 'UltraStar.res' 'UltraStar.rc'}

uses
  acinerella in 'lib\acinerella\acinerella.pas',
  UCaptureWDM in 'lib\DSPack\UCaptureWDM.pas',
  UWebCam in 'classes\UWebCam.pas',
  UMergeSort in 'classes\UMergeSort.pas',
  SDL in 'lib\JEDI-SDLv1.0\SDL\Pas\SDL.pas',
  moduleloader in 'lib\JEDI-SDLv1.0\SDL\Pas\moduleloader.pas',
  sdlutils in 'lib\JEDI-SDLv1.0\SDL\Pas\sdlutils.pas',
  sdl_image in 'lib\JEDI-SDLv1.0\SDL_Image\Pas\sdl_image.pas',
  gl in 'lib\JEDI-SDLv1.0\OpenGL\Pas\gl.pas',
  glu in 'lib\JEDI-SDLv1.0\OpenGL\Pas\glu.pas',
  glext in 'lib\JEDI-SDLv1.0\OpenGL\Pas\glext.pas',
  sdl_ttf in 'lib\JEDI-SDLv1.0\SDL_ttf\Pas\sdl_ttf.pas',
  bass in 'lib\bass\delphi\bass.pas',
  PNGImage in 'lib\PNGImage\PNGImage.pas',
  PNGzLib in 'lib\PNGImage\PNGzLib.pas',
  pnglang in 'lib\PNGImage\pnglang.pas',
  midiout in 'lib\midi\midiout.pas',
  midiin in 'lib\midi\midiin.pas',
  CIRCBUF in 'lib\midi\CIRCBUF.PAS',
  MidiType in 'lib\midi\MidiType.PAS',
  MidiDefs in 'lib\midi\MidiDefs.PAS',
  MidiCons in 'lib\midi\MidiCons.PAS',
  MidiFile in 'lib\midi\MidiFile.PAS',
  Delphmcb in 'lib\midi\Delphmcb.PAS',
  zlportio in 'lib\zlportio\zlportio.pas',
  ddkint in 'lib\zlportio\ddkint.pas',
  SQLiteTable3 in 'lib\SQLite\SQLiteTable3.pas',
  SQLite3 in 'lib\SQLite\SQLite3.pas',
  UDisplay in 'Menu\UDisplay.pas',
  UMenu in 'Menu\UMenu.pas',
  UMenuStatic in 'Menu\UMenuStatic.pas',
  UMenuText in 'Menu\UMenuText.pas',
  UMenuButton in 'Menu\UMenuButton.pas',
  UMenuInteract in 'Menu\UMenuInteract.pas',
  UMenuSelect in 'Menu\UMenuSelect.pas',
  UMenuSelectSlide in 'Menu\UMenuSelectSlide.pas',
  UDrawTexture in 'Menu\UDrawTexture.pas',
  UMenuButtonCollection in 'Menu\UMenuButtonCollection.pas',
  UGraphic in 'Classes\UGraphic.pas',
  UTexture in 'Classes\UTexture.pas',
  UMusic in 'Classes\UMusic.pas',
  ULanguage in 'Classes\ULanguage.pas',
  UMain in 'Classes\UMain.pas',
  UDraw in 'Classes\UDraw.pas',
  URecord in 'Classes\URecord.pas',
  UTime in 'Classes\UTime.pas',
  TextGL in 'Classes\TextGL.pas',
  USongs in 'Classes\USongs.pas',
  UIni in 'Classes\UIni.pas',
  UHelp in 'Classes\UHelp.pas',
  ULyrics in 'Classes\ULyrics.pas',
  USkins in 'Classes\USkins.pas',
  UThemes in 'Classes\UThemes.pas',
  ULog in 'Classes\ULog.pas',
  UDataBase in 'Classes\UDataBase.pas',
  UCovers in 'Classes\UCovers.pas',
  UCatCovers in 'Classes\UCatCovers.pas',
  UFiles in 'Classes\UFiles.pas',
  UGraphicClasses in 'Classes\UGraphicClasses.pas',


  UDLLManager in 'Classes\UDLLManager.pas',
  UParty in 'Classes\UParty.pas',
  UPartyM2 in 'Classes\UPartyM2.pas',  //Party M2-MOD

  UPlaylist in 'Classes\UPlaylist.pas',
  UCommandLine in 'Classes\UCommandLine.pas',


  UScreenLoading in 'Screens\UScreenLoading.pas',
  UScreenWelcome in 'Screens\UScreenWelcome.pas',
  UScreenMain in 'Screens\UScreenMain.pas',
  UScreenName in 'Screens\UScreenName.pas',
  UScreenLevel in 'Screens\UScreenLevel.pas',
  UScreenSong in 'Screens\UScreenSong.pas',
  UScreenSing in 'Screens\UScreenSing.pas',
  UScreenScore in 'Screens\UScreenScore.pas',
  UScreenOptions in 'Screens\UScreenOptions.pas',
  UScreenOptionsGame in 'Screens\UScreenOptionsGame.pas',
  UScreenOptionsGraphics in 'Screens\UScreenOptionsGraphics.pas',
  UScreenOptionsSound in 'Screens\UScreenOptionsSound.pas',
  UScreenOptionsLyrics in 'Screens\UScreenOptionsLyrics.pas',
  UScreenOptionsThemes in 'Screens\UScreenOptionsThemes.pas',
  UScreenOptionsRecord in 'Screens\UScreenOptionsRecord.pas',
  UScreenOptionsAdvanced in 'Screens\UScreenOptionsAdvanced.pas',
  UScreenEditSub in 'Screens\UScreenEditSub.pas',
  UScreenEdit in 'Screens\UScreenEdit.pas',
  UScreenEditConvert in 'Screens\UScreenEditConvert.pas',
  UScreenEditHeader in 'Screens\UScreenEditHeader.pas',
  UScreenOpen in 'Screens\UScreenOpen.pas',
  UScreenTop in 'Screens\UScreenTop.pas',
  UScreenSongMenu in 'Screens\UScreenSongMenu.pas',
  UScreenSongJumpto in 'Screens\UScreenSongJumpto.pas',
  UScreenStatMain in 'Screens\UScreenStatMain.pas',
  UScreenStatDetail in 'Screens\UScreenStatDetail.pas',
  UScreenCredits in 'Screens\UScreenCredits.pas',
  UScreenPopup in 'Screens\UScreenPopup.pas',

  UScreenSingModi in 'Screens\UScreenSingModi.pas',
  UScreenPartyNewRound in 'Screens\UScreenPartyNewRound.pas',
  UScreenPartyScore in 'Screens\UScreenPartyScore.pas',
  UScreenPartyPlayer in 'Screens\UScreenPartyPlayer.pas',
  UScreenPartyOptions in 'Screens\UScreenPartyOptions.pas',
  UScreenPartyWin in 'Screens\UScreenPartyWin.pas',

  UScreenPartyNewRoundM2 in 'Screens\UScreenPartyNewRoundM2.pas',
  UScreenPartyPlayerM2 in 'Screens\UScreenPartyPlayerM2.pas',
  UScreenPartyOptionsM2 in 'Screens\UScreenPartyOptionsM2.pas',

  ModiSDK in '..\..\Modis\SDK\ModiSDK.pas',


  Windows,
  SysUtils,
  UVideo in 'Classes\UVideo.pas';

const
  VersionName = 'UltraStar Deluxe Challenge, Medley & Duet Edition';
  VersionNumber = 'r9.22';

var
  WndTitle:       string;
  hWnd:           THandle;
  I:              Integer;
  VersionString:  string;

begin
  VersionString := VersionName + ' ' + VersionNumber;
  WndTitle := VersionString;

  //------------------------------
  //Start more than One Time Prevention
  //------------------------------
  hWnd:= FindWindow(nil, PChar(WndTitle));
  //Programm already started
  if (hWnd <> 0) then
  begin
    I := Messagebox(0, PChar('Another Instance of Ultrastar is already running. Continue ?'), PChar(WndTitle), MB_ICONWARNING or MB_YESNO);
    if (I = IDYes) then
    begin
      I := 1;
      repeat
        Inc(I);
        hWnd := FindWindow(nil, PChar(WndTitle + ' Instance ' + InttoStr(I)));
      until (hWnd = 0);

      WndTitle := WndTitle + ' Instance ' + InttoStr(I);
    end
    else
      Exit;
  end;

  //------------------------------
  //StartUp - Create Classes and Load Files
  //------------------------------
  USTime := TTime.Create;

  // Commandline Parameter Parser
  Params := TCMDParams.Create;

  // Log + Benchmark
  Log := TLog.Create;
  PerfLog := TPerformanceLog.Create;
  Log.Title := WndTitle;
  Log.Enabled := Not Params.NoLog;
  Log.BenchmarkStart(0);

  // Language
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize Paths', 'Initialization');        InitializePaths;
  Log.LogStatus('Load Language', 'Initialization');           Language := TLanguage.Create;
  //Add Const Values:
  Language.AddConst('US_VERSION', VersionString);
  Language.AddConst('US_VERSION_NUMBER', VersionNumber);

  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Language', 1);

  // SDL
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize SDL', 'Initialization');
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Initializing SDL', 1);

   // Skin
  Log.BenchmarkStart(1);
  Log.LogStatus('Loading Skin List', 'Initialization');             Skin := TSkin.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Skin List', 1);

  // Sound Card List
  Log.BenchmarkStart(1);
  Log.LogStatus('Loading Soundcard list', 'Initialization');
  Recording := TRecord.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Soundcard list', 1);

  // Ini + Paths
  Log.BenchmarkStart(1);
  Log.LogStatus('Load Ini', 'Initialization');                Ini := TIni.Create;
                                                              Ini.Load;

  // Help
  Log.BenchmarkStart(1);
  Log.LogStatus('Load Help', 'Initialization');               Help := THelp.Create; 

  //Load Languagefile
  if (Params.Language <> -1) then
  begin
    Language.ChangeLanguage(ILanguage[Params.Language]);
    Help.ChangeLanguage(ILanguage[Params.Language]);
  end else
  begin
    Language.ChangeLanguage(ILanguage[Ini.Language]);
    Help.ChangeLanguage(ILanguage[Ini.Language]);
  end;

  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Ini', 1);

  // Theme
  Log.BenchmarkStart(1);
  Log.LogStatus('Load Themes', 'Initialization');             Theme := TTheme.Create('Themes\' + ITheme[Ini.Theme] + '.ini', Ini.Color);
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Themes', 1);

  // Covers Cache
  Log.BenchmarkStart(1);
  Log.LogStatus('Creating Covers Cache', 'Initialization');   Covers := TCovers.Create;
  Log.LogBenchmark('Loading Covers Cache Array', 1);
  Log.BenchmarkStart(1);

  // Category Covers
  Log.BenchmarkStart(1);
  Log.LogStatus('Creating Category Covers Array', 'Initialization');
  CatCovers:= TCatCovers.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Category Covers Array', 1);

  // PluginManager
  Log.BenchmarkStart(1);
  Log.LogStatus('PluginManager', 'Initialization');
  DLLMan := TDLLMan.Create;   //Load PluginList
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading PluginManager', 1);

  // Party Mode Manager
  Log.BenchmarkStart(1);
  Log.LogStatus('PartySession Manager', 'Initialization');
  PartySession := TParty_Session.Create;   //Load PartySession
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading PartySession Manager', 1);

  // Party M2 Mode Manager
  Log.BenchmarkStart(1);
  Log.LogStatus('PartySessionM2 Manager', 'Initialization');
  PartySessionM2 := TPartySessionM2.Create;   //Load PartySessionM2
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading PartySessionM2 Manager', 1);

  // Graphics
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize 3D', 'Initialization');           Initialize3D(WndTitle);
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Initializing 3D', 1);

  // Score Saving System
  Log.BenchmarkStart(1);
  Log.LogStatus('DataBase System', 'Initialization');
  DataBase := TDataBaseSystem.Create;

  if (Params.ScoreFile = '') then
    DataBase.Init ('Ultrastar.db')
  else
    DataBase.Init (Params.ScoreFile);

  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading DataBase System', 1);

  // Songs
  Log.BenchmarkStart(1);
  Log.LogStatus('Creating Song Array', 'Initialization');     Songs := TSongs.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Song Array', 1);

  Log.BenchmarkStart(1);
  Songs.LoadSongList;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Song List', 1);

  Log.BenchmarkStart(1);
  Log.LogStatus('Creating 2nd Song Array', 'Initialization'); CatSongs := TCatSongs.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Song Cats', 1);

  // Refresh ScreenSong
  Log.BenchmarkStart(1);
  ScreenSong.Refresh(true);
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Song Refresh', 1);

  // Sound
  Log.BenchmarkStart(1);
  Log.LogStatus('Initialize Sound', 'Initialization');
  Log.LogStatus('Creating Music', 'InitializeSound');         Music := TMusic.Create;
  InitializeSound;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Initializing Sound', 1);

  //Playlist Manager
  Log.BenchmarkStart(1);
  Log.LogStatus('Playlist Manager', 'Initialization');
  PlaylistMan := TPlaylistManager.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Playlist Manager', 1);

  //GoldenStarsTwinkleMod
  Log.BenchmarkStart(1);
  Log.LogStatus('Effect Manager', 'Initialization');
  GoldenRec := TEffectManager.Create;
  Log.BenchmarkEnd(1);
  Log.LogBenchmark('Loading Particel System', 1);

  Log.BenchmarkEnd(0);
  Log.LogBenchmark('Loading Time', 0);

  //Create/Save Ini
  Ini.Save;

  //------------------------------
  //Start- Mainloop
  //------------------------------

  Log.LogStatus('Main Loop', 'Initialization');               MainLoop;

  //------------------------------
  //Finish Application
  //------------------------------

  Log.Free;
end.