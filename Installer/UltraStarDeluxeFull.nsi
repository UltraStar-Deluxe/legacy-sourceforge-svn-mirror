; --------------------------------------------------------------
; UltraStar Deluxe - Installation Wizard with NSIS (USDXIWWNSIS)
; --------------------------------------------------------------

!include "MUI.nsh"        ; Include the macros for the Modern User Interface
!include "LogicLib.nsh"
!include ".\settings\GameExplorer.nsh"
!include ".\settings\functions.nsh"
!include "WinVer.nsh"

!define icon_inst "ustar.ico"                ; Icon for Installation
!define icon_uninst "uninstall.ico"        ; Icon for Uninstallation

SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On

; XPStyle on

; ------------------------------------------------------
; Declaration of Variables (See .\settings\variables.nsh)
; ------------------------------------------------------

!include ".\settings\variables.nsh"

!addPluginDir "..\InstallerDependencies\plugins\"

; -------------------------------
; Strings for Installation Wizard
; -------------------------------

!define MUI_WELCOMEPAGE_TITLE "$(wp_title)"

!define MUI_WELCOMEPAGE_TEXT "$(wp_text)"
;!define MUI_FINISHPAGE_SHOWREADME_TEXT "$(fp_showreadme)"
!define MUI_FINISHPAGE_LINK "$(fp_link)"
!define MUI_UNCONFIRMPAGE_TEXT_TOP "UltraStar Deluxe - $(sm_uninstall)"

!define MUI_LICENSEPAGE_RADIOBUTTONS
!define MUI_WELCOMEPAGE_TITLE_3LINES

!define MUI_FINISHPAGE_TITLE_3LINES

!define MUI_FINISHPAGE_RUN "$INSTDIR\${exe}.exe"
!define MUI_FINISHPAGE_RUN_NOTCHECKED

;!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\ReadMe.txt"

!define MUI_FINISHPAGE_LINK_LOCATION "${homepage}"
!define MUI_FINISHPAGE_NOREBOOTSUPPORT
!define MUI_FINISHPAGE_TEXT_LARGE
!define MUI_FINISHPAGE_TEXT "$(fp_text)"

!define MUI_UNFINISHPAGE_LINK_LOCATION "${forum}"

!define MUI_COMPONENTSPAGE_SMALLDESC

; The other (multi) language Strings are at the bottom of this file

; --------------------------------------------------
; Begin of the installation wizard
; --------------------------------------------------

Name "${p_name} V.${version}"
Brandingtext "${p_name} Installation"
OutFile "ultrastardx-${version}-installer-full.exe"
!define ins_name "Install ${p_name} V.${version}.exe"

InstallDir "$PROGRAMFILES\${p_name}"

ShowInstDetails show
ShowUnInstDetails show

!define PRODUCT_NAME "${p_name}"
!define PRODUCT_VERSION "${version}"
!define PRODUCT_PUBLISHER "${publisher}"
!define PRODUCT_WEB_SITE "${homepage}"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"
!define PRODUCT_STARTMENU_REGVAL "NSIS:StartMenuDir"

; Modern User Interface (MUI) Stuff

!define MUI_ICON "${icon_inst}"                ; Icon for Installation
!define MUI_UNICON "${icon_uninst}"        ; Icon for Uninstallation

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${bmp_header}"                  ; Header Bitmap of the installation (Size: 150x57 px)
!define MUI_HEADERIMAGE_UNBITMAP "${bmp_header}"         ; Header Bitmap of the uninstallation (Size: 150x57 px)
!define MUI_WELCOMEFINISHPAGE_BITMAP "${bmp_side}"         ; Left Side Bitmap of Welcome & Finish Page while Installation (Size: 164x314 px)
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${bmp_side}"         ; Left Side Bitmap of Welcome & Finish Page while Uninstallation (Size: 164x314 px)
!define MUI_BGCOLOR "FFFFFF"                                 ; RGB Background color (for header, welcome & finish page)
!define MUI_WELCOMEFINISHPAGE_INI "${mui_ini}"                 ; Installation Options for Welcome & Finish Page (Installation)
!define MUI_UNWELCOMEFINISHPAGE_INI "${mui_ini}"         ; Installation Options for Welcome & Finish Page (Uninstallation)
!define MUI_LICENSEPAGE_BGCOLOR "${license_bgcolor}"         ; Background Color of Licence agreement
!define MUI_COMPONENTSPAGE_CHECKBITMAP "${bmp_check}"          ; Bitmap of Checks at Components Selection Page
!define MUI_DIRECTORYPAGE_BGCOLOR "${directory_bgcolor}" ; RGB Background Color for Directory textbox
!define MUI_STARTMENUPAGE_BGCOLOR "${smp_bgcolor}"         ; RGB Background of Startmenu List and Textbox
;!define MUI_INSTFILESPAGE_COLORS "${dets_bgcolor}"         ; Background Color of Details Screen while files are being extracted


!define MUI_LANGDLL_WINDOWTITLE "USdx In-/Uninstaller: Choose language"
!define MUI_LANGDLL_ALWAYSSHOW

!define MUI_FINISHPAGE_NOAUTOCLOSE                         ; Allows user to check the log file of installation (Comment out if unwanted)
!define MUI_UNFINISHPAGE_NOAUTOCLOSE                         ; Allows user to check the log file of uninstallation (Comment out if unwanted)

!define MUI_ABORTWARNING
!define MUI_ABORTWARNING_TEXT $(str_abort)                 ; Abort Warning message
!define MUI_ABORTWARNING_CANCEL_DEFAULT                         ; Default: Cancel abort (Comment out if unwanted)

!define MUI_FINISHPAGE_SHOWREADME
!define MUI_FINISHPAGE_SHOWREADME_TEXT $(sc_desktop)
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION CreateDesktopShortCuts

; --------------------------------------------------
; Begin of the installation routine
; --------------------------------------------------

; Pages for MUI Installation

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${file_license}"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY


; Start menu page

var ICONS_GROUP
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "UltraStar Deluxe"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${PRODUCT_STARTMENU_REGVAL}"
!insertmacro MUI_PAGE_STARTMENU Application $ICONS_GROUP

!insertmacro MUI_PAGE_INSTFILES
Page custom Settings

; USDX Settings Page

Function Settings

!insertmacro MUI_HEADER_TEXT "$(Settings_TITLE)" "$(Settings_SUBTITLE)"

   !insertmacro MUI_INSTALLOPTIONS_DISPLAY "Settings-$LANGUAGE"

; Get all the variables:

var /GLOBAL fullscreen
var /GLOBAL language2
var /GLOBAL resolution
var /GLOBAL tabs
var /GLOBAL animations
  !insertmacro MUI_INSTALLOPTIONS_READ $fullscreen "Settings-$LANGUAGE" "Field 6" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $language2 "Settings-$LANGUAGE" "Field 7" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $resolution "Settings-$LANGUAGE" "Field 8" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $tabs "Settings-$LANGUAGE" "Field 9" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $animations "Settings-$LANGUAGE" "Field 10" "State"

; Write all variables to config.ini

FileOpen $0 '$INSTDIR\config.ini' w
FileWrite $0 '[Game]$\r$\n'
FileClose $0

${If} $language2 != ""

${WriteToConfig} "Language=$language2$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${If} $tabs != ""

${WriteToConfig} "Tabs=$tabs$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${WriteToConfig} "[Graphics]$\r$\n" "$INSTDIR\config.ini"

${If} $fullscreen != ""

${WriteToConfig} "FullScreen=$fullscreen$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${If} $resolution != ""

${WriteToConfig} "Resolution=$resolution$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${WriteToConfig} "[Advanced]$\r$\n" "$INSTDIR\config.ini"

; Animations On / Off Tasks

${If} $animations == "Off"

${WriteToConfig} "LoadAnimation=Off$\r$\n" "$INSTDIR\config.ini"

${WriteToConfig} "EffectSing=Off$\r$\n" "$INSTDIR\config.ini"

${WriteToConfig} "ScreenFade=Off$\r$\n" "$INSTDIR\config.ini"

${EndIf}


FunctionEnd ;Custom page end

!insertmacro MUI_PAGE_FINISH

; Pages for MUI Uninstallation

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

; Language files

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "German"

; Finish Page

!insertmacro MUI_UNPAGE_FINISH

; ---------------------------------------------------------
; Section1: Main components of UltraStar Deluxe
; ---------------------------------------------------------

LangString DESC_Section1 ${LANG_ENGLISH} "${eng_sec1_desc}"        ; Adds the description to section1
LangString DESC_Section1 ${LANG_GERMAN} "${ger_sec1_desc}"

LangString sec1 ${LANG_ENGLISH} "${eng_sec1}"                        ; Name of section1
LangString sec1 ${LANG_GERMAN} "${ger_sec1}"

Section $(sec1) Section1
  SectionIn RO                        ; readonly
  SetOutPath $INSTDIR
  SetOverwrite try

!include ".\settings\files_in.nsh"

; Create shortcuts

  SetOutPath "$INSTDIR"

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application

  SetShellVarContext all
  SetOutPath "$INSTDIR"

  CreateDirectory "${p_name}"
  CreateDirectory "$SMPROGRAMS\$ICONS_GROUP"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_shortcut).lnk" "$INSTDIR\${exe}.exe"
;  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_documentation).lnk" "$INSTDIR\documentation.pdf"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_website).lnk" "http://www.ultrastardeluxe.org/"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_readme).lnk" "$INSTDIR\ReadMe.txt"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_license).lnk" "$INSTDIR\License.txt"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_uninstall).lnk" "$INSTDIR\Uninstall.exe"
  !insertmacro MUI_STARTMENU_WRITE_END

; WINDOWS VISTA GAME EXPLORER

${If} ${AtLeastWinVista}

${GameExplorer_GenerateGUID}
Pop $0

${GameExplorer_AddGame} all "${gdf_path}" $WINDIR $INSTDIR\${exe}.exe $0

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\1
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\1\Benchmark.lnk" \
  "$INSTDIR\${exe}.exe" "-Benchmark"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\2
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\2\Joypad.lnk" \
  "$INSTDIR\${exe}.exe" "-Joypad"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3\Fullscreen.lnk" \
  "$INSTDIR\${exe}.exe" "-FullScreen"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3\Dual Screen.lnk" \
  "$INSTDIR\${exe}.exe" "-Screen 2"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\SupportTasks\0
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\SupportTasks\0\Support Forum.lnk" \
  "http://forum.ultrastardeluxe.org"

${EndIf}

; Create Uninstaller:

  WriteUninstaller "$INSTDIR\Uninstall.exe"

  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "${p_name}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"

SectionEnd

; ---------------------------------------------------------
; Section2: Example Song "Dead Smiling Pirates"
; ---------------------------------------------------------

 LangString DESC_g2Section1 ${LANG_ENGLISH} "${eng_g2Section1_desc}"
 LangString DESC_g2Section1 ${LANG_GERMAN} "${ger_g2Section1_desc}"

 LangString DESC_g2Section2 ${LANG_ENGLISH} "${eng_g2Section2_desc}"
 LangString DESC_g2Section2 ${LANG_GERMAN} "${ger_g2Section2_desc}"

 LangString DESC_g2Section3 ${LANG_ENGLISH} "${eng_g2Section3_desc}"
 LangString DESC_g2Section3 ${LANG_GERMAN} "${ger_g2Section3_desc}"

 LangString sec2 ${LANG_ENGLISH} "${eng_sec2}"
 LangString sec2 ${LANG_GERMAN} "${ger_sec2}"

SectionGroup $(sec2) Section2

Section /o "Dead Smiling Pirates - I 18" g2Section1
;  AddSize 1400
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]"
   SetOutPath "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\"

; Download song:
; NSISdl::download /TIMEOUT=30000 ${demosong} $TEMP\Song-I-18.zip
; 
; Pop $R0 ;Get the return value
;   StrCmp $R0 "success" dlok
;     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
; dlok:
; nsisunz::Unzip "$TEMP\Song-I-18.zip" "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\"

; Delete "$TEMP\Song-I-18.zip"


  SetOutPath "$INSTDIR"

!include ".\settings\optional\in_song1.nsh"

  SetOutPath "$INSTDIR"

 SectionEnd

Section /o "Steven Dunston - Northern Star" g2Section2
;  AddSize 1500
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]"
   SetOutPath "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\"

; Download song:
; NSISdl::download /TIMEOUT=30000 ${demosong2} $TEMP\Song-Northern-Star.zip

; Pop $R0 ;Get the return value
;   StrCmp $R0 "success" dlok
;     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
; dlok:
; nsisunz::Unzip "$TEMP\Song-Northern-Star.zip" "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\"

; Delete "$TEMP\Song-Northern-Star.zip"

  SetOutPath "$INSTDIR"

!include ".\settings\optional\in_song2.nsh"

  SetOutPath "$INSTDIR"

 SectionEnd

Section /o "Joshua Morin - On the run" g2Section3
;  AddSize 2200
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Joshua Morin - On the run [DEMO]"
   SetOutPath "$INSTDIR\Songs\Joshua Morin - On the run [DEMO]\"

; Download song:
; NSISdl::download /TIMEOUT=30000 ${demosong3} $TEMP\Song-On-the-run.zip

; Pop $R0 ;Get the return value
;   StrCmp $R0 "success" dlok
;     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
; dlok:
; nsisunz::Unzip "$TEMP\Song-On-the-run.zip" "$INSTDIR\Songs\Joshua Morin - On the run [DEMO]\"

; Delete "$TEMP\Song-On-the-run.zip"

  SetOutPath "$INSTDIR"

!include ".\settings\optional\in_song3.nsh"

  SetOutPath "$INSTDIR"

 SectionEnd

 SectionGroupEnd

; ---------------------------------------------------------
; Section3: Optional Themes
; ---------------------------------------------------------

LangString DESC_Section3 ${LANG_ENGLISH} "${eng_sec3_desc}"
LangString DESC_Section3 ${LANG_GERMAN} "${ger_sec3_desc}"

LangString DESC_g1Sec1 ${LANG_ENGLISH} "${eng_g1Sec1_desc}"
LangString DESC_g1Sec1 ${LANG_GERMAN} "${ger_g1Sec1_desc}"

LangString DESC_g1Sec2 ${LANG_ENGLISH} "${eng_g1Sec2_desc}"
LangString DESC_g1Sec2 ${LANG_GERMAN} "${ger_g1Sec2_desc}"

LangString DESC_g1Sec3 ${LANG_ENGLISH} "${eng_g1Sec3_desc}"
LangString DESC_g1Sec3 ${LANG_GERMAN} "${ger_g1Sec3_desc}"

LangString sec_group ${LANG_ENGLISH} "${eng_sec3}"
LangString sec_group ${LANG_GERMAN} "${ger_sec3}"

SectionGroup $(sec_group) Section3

 Section "Orange" g1Sec1
;  AddSize 700

; Download theme orange:
; NSISdl::download /TIMEOUT=30000 ${dl_orange} $TEMP\Theme-Orange.zip

; Pop $R0 ;Get the return value
;   StrCmp $R0 "success" dlok
;     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
; dlok:
; nsisunz::Unzip "$TEMP\Theme-Orange.zip" "$INSTDIR\"

; Delete "$TEMP\Theme-Orange.zip"

  SetOutPath "$INSTDIR"

!include ".\settings\optional\in_orange.nsh"

  SetOutPath "$INSTDIR"

SectionEnd

 Section "Streetlight" g1Sec2
;  AddSize 1000

; Download theme Streetlight:
; NSISdl::download /TIMEOUT=30000 ${dl_streetlight} $TEMP\Theme-Streetlight.zip

; Pop $R0 ;Get the return value
;   StrCmp $R0 "success" dlok
;     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
; dlok:
; nsisunz::Unzip "$TEMP\Theme-Streetlight.zip" "$INSTDIR\"

; Delete "$TEMP\Theme-Streetlight.zip"

  SetOutPath "$INSTDIR"

!include ".\settings\optional\in_streetlight.nsh"

  SetOutPath "$INSTDIR"

SectionEnd

 Section "Vistar" g1Sec3
;   AddSize 1000

; Download theme Vistar:

; NSISdl::download /TIMEOUT=30000 ${dl_vistar} $TEMP\Theme-Vistar.zip

; Pop $R0 ;Get the return value
;  StrCmp $R0 "success" dlok
;    MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
; dlok:
; nsisunz::Unzip "$TEMP\Theme-Vistar.zip" "$INSTDIR\"

; Delete "$TEMP\Theme-Vistar.zip"

  SetOutPath "$INSTDIR"

!include ".\settings\optional\in_vistar.nsh"

  SetOutPath "$INSTDIR"

SectionEnd


SectionGroupEnd

; ---------------------------------------------------------
; Section4: Uninstallation Wizard
; ---------------------------------------------------------

Section Uninstall
 !insertmacro MUI_STARTMENU_GETFOLDER "Application" $ICONS_GROUP

 !include ".\settings\optional\opt_uninstall.nsh"
 !include ".\settings\files_un.nsh"

 DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"

; Unregister from Windows Vista Game Explorer

${If} ${AtLeastWinVista}

${GameExplorer_RemoveGame} $0

${EndIf}

SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${Section1} $(DESC_Section1)

;  !insertmacro MUI_DESCRIPTION_TEXT ${Section2} $(DESC_Section2)
;  !insertmacro MUI_DESCRIPTION_TEXT ${Section3} $(DESC_Section3)

  !insertmacro MUI_DESCRIPTION_TEXT ${g1Sec1} $(DESC_g1Sec1)
  !insertmacro MUI_DESCRIPTION_TEXT ${g1Sec2} $(DESC_g1Sec2)
  !insertmacro MUI_DESCRIPTION_TEXT ${g1Sec3} $(DESC_g1Sec3)

  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section1} $(DESC_g2Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section2} $(DESC_g2Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section3} $(DESC_g2Section3)

!insertmacro MUI_FUNCTION_DESCRIPTION_END

; -----------------------------------------------------------------------
; Language Strings for Installation / Uninstallation Wizard
; can be found at .\settings\langstrings.nsh
; -----------------------------------------------------------------------

!include .\settings\langstrings.nsh

; ------------------------------------------------------------------
; Functions for the beginning of the installation and uninstallation
; ------------------------------------------------------------------

; Function for Installation

Function .onInit

   System::Call 'kernel32::CreateMutexA(i 0, i 0, t "USdx Installer.exe") ?e'

  Pop $R0

  StrCmp $R0 0 +3
    MessageBox MB_OK "The installer is already running."
    Abort

  !insertmacro MUI_LANGDLL_DISPLAY

  ReadRegStr $R0 HKLM \
  "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}" \
  "UninstallString"
  StrCmp $R0 "" done

  MessageBox MB_YESNO|MB_ICONEXCLAMATION \
  "${PRODUCT_NAME} is already installed. $\n$\nAre you sure you want to \
  install it again?" \
  IDYES done
  Abort


done:

     !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1031.ini" "Settings-1031"
     !insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1033.ini" "Settings-1033"

FunctionEnd

; Function for Uninstallation

Function un.onInit

        ${nsProcess::FindProcess} "USdx.exe" $R0
        StrCmp $R0 0 0 +2
        MessageBox MB_YESNO|MB_ICONEXCLAMATION 'UltraStar Deluxe cannot be uninstalled while its running! Do you want to close it?' IDYES closeit IDNO end

        closeit:
        ${nsProcess::KillProcess} "USdx.exe" $R0
        goto continue

        end:
        ${nsProcess::Unload}
        Abort

        continue:
           !insertmacro MUI_LANGDLL_DISPLAY

FunctionEnd