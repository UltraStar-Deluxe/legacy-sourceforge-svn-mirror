; --------------------------------------------------------------
; UltraStar Deluxe - Installation Wizard with NSIS (USDXIWWNSIS) 
; --------------------------------------------------------------

!include "MUI.nsh"	; Include the macros for the Modern User Interface

SetCompressor bzip2

; XPStyle on

; ------------------------------------------------------
; Declaration of Variables (Change to whatever you want)
; ------------------------------------------------------

!define version "1.01"						; Current version of UltraStar Deluxe
!define p_name "UltraStar Deluxe" 				; Just the name of the program
!define publisher "USDX Team"					; Publisher
!define homepage "http://www.ultrastardeluxe.org/"		; Project Homepage


!define icon_inst ".\ustar.ico"		; Icon for Installation
!define icon_uninst ".\ustar.ico"	; Icon for Uninstallation
!define bmp_header ".\header.bmp"	; Bitmap of the Installation Header (Size: 150x57 px)
!define bmp_side ".\left-164x314.bmp"		; Bitmap on the left side of Welcome & Finish Page (Size: 164x314 px)
!define mui_ini ".\ioSpecial.ini"	; Installation Options for Welcome & Finish Page
!define license_bgcolor "FFFFFF"	; RGB Background Color for Licence agreement
!define bmp_check ".\modern.bmp"	; Bitmap of Checks at Components Selection Page
!define directory_bgcolor "FFFFFF"	; RGB Background Color for Directory textbox
!define smp_bgcolor "FFFFFF"		; RGB Background of Startmenu List and Textbox
;!define dets_bgcolor "FFFFFF"		; Background Color of Details Screen while files are being extracted
!define file_license ".\License.txt"	; Choose the file with the license agreement

!define eng_sec1_desc "These are the basic files needed by UltraStar Deluxe" 		; English Description of Base components
!define ger_sec1_desc "Dies sind die von UltraStar Deluxe benötigten Grunddateien" 	; German Description of Base components

!define eng_sec2_desc "This will add the example song Dead Smiling Pirates - I 18 from the CreativeCommons database" 	; English Description of the Example Song
!define ger_sec2_desc "Dies fügt den Beispielsong Dead Smiling Pirates - I 18 aus der CreativeCommons Datenbank hinzu" 	; German Description of the Example Song

!define eng_sec1 "Base components" 							; English Name of the component section1
!define ger_sec1 "Basiskomponenten" 							; German Name of the component section1

!define eng_sec2 "Example Song" 							; English Name of the component section2
!define ger_sec2 "Beispielsong" 							; German Name of the component section2

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

;!define MUI_FINISHPAGE_RUN .\Ultrastar.exe		; Do not work as i expected :(
;!define MUI_FINISHPAGE_RUN_NOTCHECKED			; 
;!define MUI_FINISHPAGE_SHOWREADME .\documentation.pdf	;

!define MUI_FINISHPAGE_LINK_LOCATION http://www.ultrastardeluxe.org
!define MUI_FINISHPAGE_NOREBOOTSUPPORT
!define MUI_FINISHPAGE_TEXT_LARGE
!define MUI_FINISHPAGE_TEXT "$(fp_text)"

; The other (multi) language Strings are at the bottom of this file

; --------------------------------------------------
; Do not change anything from here on ...
; ... expect you are mog and know what you are doing
; --------------------------------------------------

Name "${p_name} V.${version}"
Brandingtext "${p_name} Installation"
OutFile "Install ${p_name} V.${version}.exe"

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

!define MUI_ICON "${icon_inst}"		; Icon for Installation
!define MUI_UNICON "${icon_uninst}"	; Icon for Uninstallation

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${bmp_header}" 		 ; Header Bitmap of the installation (Size: 150x57 px)
!define MUI_HEADERIMAGE_UNBITMAP "${bmp_header}"	 ; Header Bitmap of the uninstallation (Size: 150x57 px)
!define MUI_WELCOMEFINISHPAGE_BITMAP "${bmp_side}"	 ; Left Side Bitmap of Welcome & Finish Page while Installation (Size: 164x314 px)
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${bmp_side}"	 ; Left Side Bitmap of Welcome & Finish Page while Uninstallation (Size: 164x314 px)
!define MUI_BGCOLOR "FFFFFF"				 ; RGB Background color (for header, welcome & finish page)
!define MUI_WELCOMEFINISHPAGE_INI "${mui_ini}"		 ; Installation Options for Welcome & Finish Page (Installation)
!define MUI_UNWELCOMEFINISHPAGE_INI "${mui_ini}"	 ; Installation Options for Welcome & Finish Page (Uninstallation)
!define MUI_LICENSEPAGE_BGCOLOR "${license_bgcolor}"	 ; Background Color of Licence agreement
!define MUI_COMPONENTSPAGE_CHECKBITMAP "${bmp_check}" 	 ; Bitmap of Checks at Components Selection Page
!define MUI_DIRECTORYPAGE_BGCOLOR "${directory_bgcolor}" ; RGB Background Color for Directory textbox
!define MUI_STARTMENUPAGE_BGCOLOR "${smp_bgcolor}"	 ; RGB Background of Startmenu List and Textbox
;!define MUI_INSTFILESPAGE_COLORS "${dets_bgcolor}"	 ; Background Color of Details Screen while files are being extracted

!define MUI_FINISHPAGE_NOAUTOCLOSE			 ; Allows user to check the log file of installation (Comment out if unwanted)
!define MUI_UNFINISHPAGE_NOAUTOCLOSE			 ; Allows user to check the log file of uninstallation (Comment out if unwanted)

!define MUI_ABORTWARNING
!define MUI_ABORTWARNING_TEXT $(str_abort)		 ; Abort Warning message
!define MUI_ABORTWARNING_CANCEL_DEFAULT			 ; Default: Cancel abort (Comment out if unwanted)


;Language Dialog Box Settings
;!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
;!define PRODUCT_UNINST_ROOT_KEY "HKLM"
;!define MUI_LANGDLL_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
;!define MUI_LANGDLL_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
;!define MUI_LANGDLL_REGISTRY_VALUENAME "NSIS:Language"

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
!insertmacro MUI_PAGE_FINISH

; Pages for MUI Uninstallation

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

; Language files

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "German"

; ---------------------------------------------------------
; Section1: Main components of UltraStar Deluxe
; ---------------------------------------------------------

LangString DESC_Section1 ${LANG_ENGLISH} "${eng_sec1_desc}"	; Adds the description to section1
LangString DESC_Section1 ${LANG_GERMAN} "${ger_sec1_desc}"

LangString sec1 ${LANG_ENGLISH} "${eng_sec1}"			; Name of section1
LangString sec1 ${LANG_GERMAN} "${ger_sec1}"

Section $(sec1) Section1
  SectionIn RO			; readonly
  SetOutPath $INSTDIR
  SetOverwrite try

; Create required directories:
  
  CreateDirectory "$INSTDIR\Covers"
  CreateDirectory "$INSTDIR\Languages"
  CreateDirectory "$INSTDIR\Plugins"
  CreateDirectory "$INSTDIR\Skins"
  CreateDirectory "$INSTDIR\Skins\Classic"
  CreateDirectory "$INSTDIR\Skins\Deluxe"
  CreateDirectory "$INSTDIR\Songs"
  CreateDirectory "$INSTDIR\Sounds"
  CreateDirectory "$INSTDIR\Themes"

; Extract files to the directories:

  File "..\InstallerDependencies\bass.dll"
  File "..\InstallerDependencies\Changelog.german.txt"
  File "..\InstallerDependencies\Changelog.txt"
  File "..\InstallerDependencies\documentation.pdf"
  File "..\InstallerDependencies\License.txt"
  File "..\InstallerDependencies\ReadMe.txt"
  File "..\InstallerDependencies\SDL.dll"
  File "..\InstallerDependencies\smpeg.dll"
  File "..\InstallerDependencies\sqlite3.dll"
  
  File "..\ScoreConverter.exe"
  File "..\Ultrastar.exe"

  SetOutPath $INSTDIR\Covers\"
  File "..\Covers\Covers.ini"
  File "..\Covers\NoCover.jpg"

  SetOutPath $INSTDIR\Languages\"
  File "..\Languages\Dutch.ini"
  File "..\Languages\English.ini"
  File "..\Languages\French.ini"
  File "..\Languages\German.ini"
  File "..\Languages\readme.txt"
  File "..\Languages\Swedish.ini"

  SetOutPath $INSTDIR\Plugins\"
  File "..\Plugins\Blind.dll"
  File "..\Plugins\Duell.dll"
  File "..\Plugins\Hold_The_Line.dll"
  File "..\Plugins\Until5000.dll"

  SetOutPath $INSTDIR\Skins\Classic\"
  File "..\Skins\Classic\Star.ini"
  File "..\Skins\Classic\[button]13.jpg"
  File "..\Skins\Classic\[button]alt.jpg"
  File "..\Skins\Classic\[button]az.jpg"
  File "..\Skins\Classic\[button]e.jpg"
  File "..\Skins\Classic\[button]enter.jpg"
  File "..\Skins\Classic\[button]esc.jpg"
  File "..\Skins\Classic\[button]j.jpg"
  File "..\Skins\Classic\[button]m.jpg"
  File "..\Skins\Classic\[button]navi.jpg"
  File "..\Skins\Classic\[button]p.jpg"
  File "..\Skins\Classic\[effect]goldenNoteStar.jpg"
  File "..\Skins\Classic\[effect]perfectNoteStar.jpg"
  File "..\Skins\Classic\[helper]rectangle.jpg"
  File "..\Skins\Classic\[icon]error.jpg"
  File "..\Skins\Classic\[icon]question.jpg"
  File "..\Skins\Classic\[icon]Star.jpg"
  File "..\Skins\Classic\[icon]stats.jpg"
  File "..\Skins\Classic\[icon]video.jpg"
  File "..\Skins\Classic\[mainbutton]Exit.jpg"
  File "..\Skins\Classic\[mainbutton]Multi.jpg"
  File "..\Skins\Classic\[mainbutton]Options.jpg"
  File "..\Skins\Classic\[mainbutton]Solo.jpg"
  File "..\Skins\Classic\[mainbutton]Stats.jpg"
  File "..\Skins\Classic\[main]Bar.jpg"
  File "..\Skins\Classic\[main]Bar1.jpg"
  File "..\Skins\Classic\[main]Button.jpg"
  File "..\Skins\Classic\[main]Button2.jpg"
  File "..\Skins\Classic\[main]Button3.jpg"
  File "..\Skins\Classic\[main]ButtonEditor.jpg"
  File "..\Skins\Classic\[main]Logo.jpg"
  File "..\Skins\Classic\[main]songCover.jpg"
  File "..\Skins\Classic\[main]square.jpg"
  File "..\Skins\Classic\[menu]jumpToBg.jpg"
  File "..\Skins\Classic\[menu]PopUpBg.JPG"
  File "..\Skins\Classic\[menu]PopUpFg.JPG"
  File "..\Skins\Classic\[menu]songMenuBg.jpg"
  File "..\Skins\Classic\[menu]songMenuBorder.jpg"
  File "..\Skins\Classic\[menu]songMenuButtonBG.jpg"
  File "..\Skins\Classic\[menu]songMenuSelectBG.jpg"
  File "..\Skins\Classic\[party]Joker.jpg"
  File "..\Skins\Classic\[party]playerButton.jpg"
  File "..\Skins\Classic\[party]playerTeamButton.jpg"
  File "..\Skins\Classic\[party]pointer.bmp"
  File "..\Skins\Classic\[party]roundBG1.jpg"
  File "..\Skins\Classic\[party]roundBG2.jpg"
  File "..\Skins\Classic\[party]roundBG3.jpg"
  File "..\Skins\Classic\[party]roundBG4.jpg"
  File "..\Skins\Classic\[party]roundTeamButton.jpg"
  File "..\Skins\Classic\[party]scoreBG1.jpg"
  File "..\Skins\Classic\[party]scoreBG2.jpg"
  File "..\Skins\Classic\[party]scoreDecoration.jpg"
  File "..\Skins\Classic\[party]teamPoints.jpg"
  File "..\Skins\Classic\[party]winDecoration.jpg"
  File "..\Skins\Classic\[party]winTeamButton1.jpg"
  File "..\Skins\Classic\[party]winTeamButton2.jpg"
  File "..\Skins\Classic\[party]winTeamButton3.jpg"
  File "..\Skins\Classic\[score]box.jpg"
  File "..\Skins\Classic\[score]level.jpg"
  File "..\Skins\Classic\[score]levelround.jpg"
  File "..\Skins\Classic\[score]line.jpg"
  File "..\Skins\Classic\[sing]lineBonusPopUp.jpg"
  File "..\Skins\Classic\[sing]LyricsBall.bmp"
  File "..\Skins\Classic\[sing]lyricsHelpBar.bmp"
  File "..\Skins\Classic\[sing]notesBgLeft.bmp"
  File "..\Skins\Classic\[sing]notesBgMid.bmp"
  File "..\Skins\Classic\[sing]notesBgRight.bmp"
  File "..\Skins\Classic\[sing]notesLeft.bmp"
  File "..\Skins\Classic\[sing]notesMid.bmp"
  File "..\Skins\Classic\[sing]notesRight.bmp"
  File "..\Skins\Classic\[sing]p.jpg"
  File "..\Skins\Classic\[sing]scoreBg.jpg"
  File "..\Skins\Classic\[sing]singBarBack.jpg"
  File "..\Skins\Classic\[sing]singBarBar.jpg"
  File "..\Skins\Classic\[sing]singBarFront.jpg"
  File "..\Skins\Classic\[sing]textBar.jpg"
  File "..\Skins\Classic\[song]BGFade.jpg"
  File "..\Skins\Classic\[song]EqualizerBG.jpg"
  File "..\Skins\Classic\[song]selection.jpg"
  File "..\Skins\Classic\[stat]detailBG1.jpg"
  File "..\Skins\Classic\[stat]mainBG1.jpg"
  File "..\Skins\Classic\[stat]mainBG2.jpg"
  File "..\Skins\Classic\[stat]mainBG3.jpg"

  SetOutPath $INSTDIR\Skins\Deluxe\"
  File "..\Skins\Deluxe\Blue.ini"
  File "..\Skins\Deluxe\Fall.ini"
  File "..\Skins\Deluxe\Summer.ini"
  File "..\Skins\Deluxe\Winter.ini"
  File "..\Skins\Deluxe\[bg-load]blue.jpg"
  File "..\Skins\Deluxe\[bg-load]fall.jpg"
  File "..\Skins\Deluxe\[bg-load]summer.jpg"
  File "..\Skins\Deluxe\[bg-load]winter.jpg"
  File "..\Skins\Deluxe\[bg-main]blue.jpg"
  File "..\Skins\Deluxe\[bg-main]fall.jpg"
  File "..\Skins\Deluxe\[bg-main]summer.jpg"
  File "..\Skins\Deluxe\[bg-main]winter.jpg"
  File "..\Skins\Deluxe\[button]13.jpg"
  File "..\Skins\Deluxe\[button]alt.jpg"
  File "..\Skins\Deluxe\[button]az.jpg"
  File "..\Skins\Deluxe\[button]enter.jpg"
  File "..\Skins\Deluxe\[button]esc.jpg"
  File "..\Skins\Deluxe\[button]j.jpg"
  File "..\Skins\Deluxe\[button]m.jpg"
  File "..\Skins\Deluxe\[button]navi.jpg"
  File "..\Skins\Deluxe\[button]p.jpg"
  File "..\Skins\Deluxe\[effect]goldenNoteStar.jpg"
  File "..\Skins\Deluxe\[effect]perfectNoteStar.jpg"
  File "..\Skins\Deluxe\[helper]buttonFade.jpg"
  File "..\Skins\Deluxe\[helper]rectangle.jpg"
  File "..\Skins\Deluxe\[icon]cd.jpg"
  File "..\Skins\Deluxe\[icon]error.jpg"
  File "..\Skins\Deluxe\[icon]main.jpg"
  File "..\Skins\Deluxe\[icon]options.jpg"
  File "..\Skins\Deluxe\[icon]party.jpg"
  File "..\Skins\Deluxe\[icon]question.jpg"
  File "..\Skins\Deluxe\[icon]score.jpg"
  File "..\Skins\Deluxe\[icon]search.jpg"
  File "..\Skins\Deluxe\[icon]songmenu.jpg"
  File "..\Skins\Deluxe\[icon]stats.jpg"
  File "..\Skins\Deluxe\[icon]video.jpg"
  File "..\Skins\Deluxe\[main]button.jpg"
  File "..\Skins\Deluxe\[main]buttonf.jpg"
  File "..\Skins\Deluxe\[main]mainBar.jpg"
  File "..\Skins\Deluxe\[main]playerNumberBox.jpg"
  File "..\Skins\Deluxe\[main]selectbg.jpg"
  File "..\Skins\Deluxe\[main]songCover.jpg"
  File "..\Skins\Deluxe\[main]songSelection1.jpg"
  File "..\Skins\Deluxe\[main]songSelection2.jpg"
  File "..\Skins\Deluxe\[menu]jumpToBg.jpg"
  File "..\Skins\Deluxe\[menu]PopUpBg.JPG"
  File "..\Skins\Deluxe\[menu]PopUpFg.JPG"
  File "..\Skins\Deluxe\[menu]songMenuBg.jpg"
  File "..\Skins\Deluxe\[menu]songMenuSelectBg.jpg"
  File "..\Skins\Deluxe\[party]Joker.jpg"
  File "..\Skins\Deluxe\[party]playerButton.jpg"
  File "..\Skins\Deluxe\[party]playerTeamButton.jpg"
  File "..\Skins\Deluxe\[party]pointer.bmp"
  File "..\Skins\Deluxe\[party]roundBG1.jpg"
  File "..\Skins\Deluxe\[party]roundBG2.jpg"
  File "..\Skins\Deluxe\[party]roundBG3.jpg"
  File "..\Skins\Deluxe\[party]roundBG4.jpg"
  File "..\Skins\Deluxe\[party]roundTeamButton.jpg"
  File "..\Skins\Deluxe\[party]scoreBG1.jpg"
  File "..\Skins\Deluxe\[party]scoreBG2.jpg"
  File "..\Skins\Deluxe\[party]scoreDecoration.jpg"
  File "..\Skins\Deluxe\[party]teamPoints.jpg"
  File "..\Skins\Deluxe\[party]winDecoration1.jpg"
  File "..\Skins\Deluxe\[party]winTeamButton1.jpg"
  File "..\Skins\Deluxe\[party]winTeamButton2.jpg"
  File "..\Skins\Deluxe\[party]winTeamButton3.jpg"
  File "..\Skins\Deluxe\[score]box.jpg"
  File "..\Skins\Deluxe\[score]endcap.jpg"
  File "..\Skins\Deluxe\[score]level.jpg"
  File "..\Skins\Deluxe\[score]levelRound.jpg"
  File "..\Skins\Deluxe\[score]Line.jpg"
  File "..\Skins\Deluxe\[sing]lineBonusPopUp.jpg"
  File "..\Skins\Deluxe\[sing]LyricsBall.bmp"
  File "..\Skins\Deluxe\[sing]lyricsHelpBar.bmp"
  File "..\Skins\Deluxe\[sing]notesBgLeft.bmp"
  File "..\Skins\Deluxe\[sing]notesBgMid.bmp"
  File "..\Skins\Deluxe\[sing]notesBgRight.bmp"
  File "..\Skins\Deluxe\[sing]notesLeft.bmp"
  File "..\Skins\Deluxe\[sing]notesMid.bmp"
  File "..\Skins\Deluxe\[sing]notesRight.bmp"
  File "..\Skins\Deluxe\[sing]p.jpg"
  File "..\Skins\Deluxe\[sing]scoreBg.jpg"
  File "..\Skins\Deluxe\[sing]singBarBack.jpg"
  File "..\Skins\Deluxe\[sing]singBarBar.jpg"
  File "..\Skins\Deluxe\[sing]singBarFront.jpg"
  File "..\Skins\Deluxe\[sing]textBar.jpg"
  File "..\Skins\Deluxe\[sing]timeBar.jpg"
  File "..\Skins\Deluxe\[sing]timeBar1.jpg"
  File "..\Skins\Deluxe\[sing]timeBarBG.jpg"
  File "..\Skins\Deluxe\[special]bar1.jpg"
  File "..\Skins\Deluxe\[special]bar2.jpg"
  File "..\Skins\Deluxe\[stat]detailBG1.jpg"
  File "..\Skins\Deluxe\[stat]mainBG1.jpg"
  File "..\Skins\Deluxe\[stat]mainBG2.jpg"
  File "..\Skins\Deluxe\[stat]mainBG3.jpg"

  SetOutPath $INSTDIR\Sounds\"
  File "..\Sounds\Common back.mp3"
  File "..\Sounds\Common start.mp3"
  File "..\Sounds\credits-outro-tune.mp3"
  File "..\Sounds\dismissed.mp3"
  File "..\Sounds\menu swoosh.mp3"
  File "..\Sounds\option change col.mp3"
  File "..\Sounds\rimshot022b.mp3"
  File "..\Sounds\select music change music 50.mp3"
  File "..\Sounds\select music change music.mp3"
  File "..\Sounds\wome-credits-tune.mp3"
  
  SetOutPath $INSTDIR\Themes\"
  File "..\Themes\Classic.ini"
  File "..\Themes\Deluxe.ini"

; Create shortcuts

  SetOutPath $INSTDIR\

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  CreateDirectory "${p_name}"
;  CreateShortCut "$STARTMENU.lnk" "$INSTDIR\Ultrastar.exe"
;  CreateShortCut "$DESKTOP.lnk" "$INSTDIR\Ultrastar.exe"
;  CreateShortCut "$SMPROGRAMS.lnk" "$INSTDIR\Ultrastar.exe"
;  WriteIniStr "$INSTDIR\${PRODUCT_NAME}.url" "InternetShortcut" "URL" "${PRODUCT_WEB_SITE}"
  CreateDirectory "$SMPROGRAMS\$ICONS_GROUP"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\Website.lnk" "http://www.ultrastardeluxe.org/"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_uninstall).lnk" "$INSTDIR\Uninstall.exe"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_shortcut).lnk" "$INSTDIR\Ultrastar.exe"

  !insertmacro MUI_STARTMENU_WRITE_END

; Create Uninstaller:

  WriteUninstaller $INSTDIR\Uninstall.exe

  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "${p_name}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"

SectionEnd

; ---------------------------------------------------------
; Section2: Example Song "Dead Smiling Pirates"
; ---------------------------------------------------------

LangString DESC_Section2 ${LANG_ENGLISH} "${eng_sec2_desc}"
LangString DESC_Section2 ${LANG_GERMAN} "${ger_sec2_desc}"

LangString sec2 ${LANG_ENGLISH} "${eng_sec2}"
LangString sec2 ${LANG_GERMAN} "${ger_sec2}"

Section /o $(sec2) Section2
  SetOverwrite try
  SetOutPath "$INSTDIR"
  CreateDirectory "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]"
  SetOutPath "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\"

;  CreateDirectory "$INSTDIR\Songs\"
;  SetOutPath "$INSTDIR\Songs\"
  
;  InetLoad::load "http://192.168.88.200/demosong.zip" "$INSTDIR\Songs\demosong.zip"
;    Pop $0
;    StrCmp $0 "OK" dlok
;    MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
;  dlok:

;  ZipDLL::extractall "$INSTDIR\Songs\demosong.zip" "$INSTDIR\Songs"
  
  File "..\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18 [BG].jpg"
  File "..\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18 [CO].jpg"
  File "..\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18.ogg"
  File "..\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18.txt"
  File "..\Songs\Dead Smiling Pirates - I 18 [DEMO]\License.txt"

SectionEnd

; ---------------------------------------------------------
; Section3: Uninstallation Wizard
; ---------------------------------------------------------

Section Uninstall
 !insertmacro MUI_STARTMENU_GETFOLDER "Application" $ICONS_GROUP

; Delete "$INSTDIR\${PRODUCT_NAME}.url"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Deinstallieren.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Website.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\UltraStar Deluxe spielen.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Play UltraStar Deluxe.lnk"
; Delete "$SMPROGRAMS.lnk"
; Delete "$DESKTOP.lnk"
; Delete "$STARTMENU.lnk"

 Delete "$INSTDIR\*.*"
 RMDir /r "$INSTDIR\Covers"
 RMDir /r "$INSTDIR\Languages"
 RMDir /r "$INSTDIR\Plugins"
 RMDir /r "$INSTDIR\Skins\Classic"
 RMDir /r "$INSTDIR\Skins\Deluxe"
 RMDir /r "$INSTDIR\Skins"
 RMDir /r "$INSTDIR\Songs"
 RMDir /r "$INSTDIR\Sounds"
 RMDir /r "$INSTDIR\Themes"
 RMDir /r "$INSTDIR\Screenshots"
 RMDir /r "$INSTDIR\Playlists"

 RMDir "$SMPROGRAMS\$ICONS_GROUP"

 DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"

 Delete "$INSTDIR\Uninstall.exe"
 RMDIR $INSTDIR"

SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${Section1} $(DESC_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${Section2} $(DESC_Section2)
!insertmacro MUI_FUNCTION_DESCRIPTION_END

; -----------------------------------------------------------------------
; Language Strings for Installation / Uninstallation Wizard
; These Strings can be edited, like the variables at the top
; BUG: Some of the LangStrings do not work - do not know why :(
; I commented them out of the whole source. Maybe someone else can fix it
; -----------------------------------------------------------------------

LangString str_continue ${LANG_GERMAN} "Dies wird UltraStar Deluxe installieren. Fortsetzen?"
LangString str_continue ${LANG_ENGLISH} "This will install UltraStar Deluxe. Continue?"

LangString str_abort ${LANG_ENGLISH} "Are you sure to abort Installation?"
LangString str_abort ${LANG_GERMAN} "Wollen Sie die Installation wirklich abbrechen?"

LangString uninst_begin ${LANG_ENGLISH} "This will uninstall UltraStar Deluxe completely (with Songs!). Continue ?"
LangString uninst_begin ${LANG_GERMAN} "Dies wird UltraStar Deluxe vollständig deinstallieren (inkl. Songs!). Fortfahren?"

LangString uninst_success ${LANG_ENGLISH} "We are sad because the uninstallation finished successfully! Hope you enjoyed UltraStar Deluxe."
LangString uninst_success ${LANG_GERMAN} "Wir sind traurig, da die Deinstallation erfolgreich verlief. Wir hoffen du hast UltraStar Deluxe genossen."

LangString str_header ${LANG_ENGLISH} "Custom options for Installation"
LangString str_header ${LANG_GERMAN} "Eigene Optionen zur Installation"

LangString str_header_subtitle ${LANG_ENGLISH} ""
LangString str_header_subtitle ${LANG_GERMAN} ""

LangString sm_shortcut ${LANG_GERMAN} "UltraStar Deluxe spielen"  ; Name for start Icon in startmenu
LangString sm_shortcut ${LANG_ENGLISH} "Play UltraStar Deluxe"

LangString sm_uninstall ${LANG_GERMAN} "Deinstallieren"		  ; Name for uninstall icon in startmenu
LangString sm_uninstall ${LANG_ENGLISH} "Uninstall"

LangString wp_title ${LANG_GERMAN} "Willkommen zur Installationsroutine von UltraStar Deluxe"	; Title String in Welcome Page
LangString wp_title ${LANG_ENGLISH} "Welcome to the UltraStar Deluxe Setup Wizard"

LangString wp_text ${LANG_GERMAN} "Dieser Assistent wird Sie durch die Installation von UltraStar Deluxe begleiten. UltraStar Deluxe ist ein kostenloses quelloffenes Karaokespiel, welches Singstar ähnelt. Diese Installationsroutine enthält den Bonus Song 'Dead Smiling Pirates - I 18' aus der CreativeCommons Datenbank, der optional installiert werden kann.\n\r\n\rDas UltraStar Deluxe Team wünscht viel Spaß\n\rProjekthomepage: http://www.ultrastardeluxe.org\n\rProject Forum: http://forum.ultrastardeluxe.org"
LangString wp_text ${LANG_ENGLISH} "This wizard will guide you through the Installation of UltraStar Deluxe. UltraStar Deluxe is a free open source Karaoke game, which can be compared with Singstar. These Installation Wizard includes the bonus track 'Dead Smiling Pirates - I 18' from the CreativeCommons database and which can be installed seperatly.\n\r\n\rThe UltraStar Deluxe Team wishes you fun\n\rProject website: http://www.ultrastardeluxe.org\n\rProject Forum: http://forum.ultrastardeluxe.org"

LangString fp_showreadme ${LANG_GERMAN} "Dokumentation ansehen (PDF)"	; "Show Readme" String in Finish Page
LangString fp_showreadme ${LANG_ENGLISH} "See documentation (PDF)"

LangString fp_text ${LANG_GERMAN}  "UltraStar wurde erfolgreich auf Ihrem System installiert.\n\rBesuchen Sie unsere Projektwebseite um die neusten Updates und News zu erhalten."
LangString fp_text ${LANG_ENGLISH}  "UltraStar was installed successfully on you system.\n\rVisit out project website to get latest news and updates."

LangString fp_link ${LANG_GERMAN} "Projektwebseite"	; Link to developers website - String in Finish Page
LangString fp_link ${LANG_ENGLISH} "Project website"

LangString uncp_text ${LANG_GERMAN} "Willkommen beim Deinstallations-Assistent für UltraStar Deluxe" 
LangString uncp_text ${LANG_ENGLISH} "Welcome to the UltraStar Deluxe Uninstall Wizard"

; ------------------------------------------------------------------
; Functions for the beginning of the installation and uninstallation
; ------------------------------------------------------------------

; Function for Installation

Function .onInit
     !insertmacro MUI_LANGDLL_DISPLAY
    ; MessageBox MB_YESNO $(str_continue) IDYES continue
    ; Abort 		; Do not work as I expected :(
    ; continue:	
FunctionEnd

; Function for Uninstallation

Function un.onInit
    !insertmacro MUI_LANGDLL_DISPLAY
   ; MessageBox MB_YESNO "$(uninst_begin)" IDYES continue
   ;   Abort ; causes uninstaller to quit.
   ; continue:
FunctionEnd