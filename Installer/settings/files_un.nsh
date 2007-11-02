; This list contains the files that will be uninstalled

; Delete provided Game Exe and provided Tools
 Delete "$INSTDIR\ScoreConverter.exe"
 Delete "$INSTDIR\${exe}.exe"

; Delete other provided/created stuff
 Delete "$INSTDIR\bass.dll"
 Delete "$INSTDIR\Changelog.german.txt"
 Delete "$INSTDIR\Changelog.txt"
 Delete "$INSTDIR\documentation.pdf"
 Delete "$INSTDIR\License.txt"
 Delete "$INSTDIR\ReadMe.txt"
 Delete "$INSTDIR\SDL.dll"
 Delete "$INSTDIR\smpeg.dll"
 Delete "$INSTDIR\sqlite3.dll"
 Delete "$INSTDIR\config.ini"
 Delete "$INSTDIR\Error.log"
 Delete "$INSTDIR\covers.cache"
 Delete "$INSTDIR\Uninstall.exe"

${If} ${AtLeastWinVista}

; Delete gdf.dll
 Delete "$WINDIR\gdf.dll"

${EndIf}

; Delete provided Covers 
 Delete "$INSTDIR\Covers\Covers.ini"
 Delete "$INSTDIR\Covers\NoCover.jpg"

; Delete provided languages
 Delete "$INSTDIR\Languages\Catalan.ini"
 Delete "$INSTDIR\Languages\Dutch.ini"
 Delete "$INSTDIR\Languages\English.ini"
 Delete "$INSTDIR\Languages\French.ini"
 Delete "$INSTDIR\Languages\German.ini"
 Delete "$INSTDIR\Languages\Italian.ini"
 Delete "$INSTDIR\Languages\Norwegian.ini"
 Delete "$INSTDIR\Languages\readme.txt"
 Delete "$INSTDIR\Languages\Serbian.ini"
 Delete "$INSTDIR\Languages\Spanish.ini"
 Delete "$INSTDIR\Languages\Swedish.ini"
 Delete "$INSTDIR\Languages\Portuguese.ini"

; Delete provided plugins
 Delete "$INSTDIR\Plugins\Blind.dll"
 Delete "$INSTDIR\Plugins\Duell.dll"
 Delete "$INSTDIR\Plugins\Hold_The_Line.dll"
 Delete "$INSTDIR\Plugins\Until5000.dll"

; Delete provided Classic Skin
 Delete "$INSTDIR\Skins\Classic\Star.ini"
 Delete "$INSTDIR\Skins\Classic\[button]13.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]alt.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]az.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]e.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]enter.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]esc.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]j.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]m.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]navi.jpg"
 Delete "$INSTDIR\Skins\Classic\[button]p.jpg"
 Delete "$INSTDIR\Skins\Classic\[effect]goldenNoteStar.jpg"
 Delete "$INSTDIR\Skins\Classic\[effect]perfectNoteStar.jpg"
 Delete "$INSTDIR\Skins\Classic\[helper]rectangle.jpg"
 Delete "$INSTDIR\Skins\Classic\[icon]error.jpg"
 Delete "$INSTDIR\Skins\Classic\[icon]question.jpg"
 Delete "$INSTDIR\Skins\Classic\[icon]Star.jpg"
 Delete "$INSTDIR\Skins\Classic\[icon]stats.jpg"
 Delete "$INSTDIR\Skins\Classic\[icon]video.jpg"
 Delete "$INSTDIR\Skins\Classic\[mainbutton]Exit.jpg"
 Delete "$INSTDIR\Skins\Classic\[mainbutton]Multi.jpg"
 Delete "$INSTDIR\Skins\Classic\[mainbutton]Options.jpg"
 Delete "$INSTDIR\Skins\Classic\[mainbutton]Solo.jpg"
 Delete "$INSTDIR\Skins\Classic\[mainbutton]Stats.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]Bar.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]Bar1.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]Button.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]Button2.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]Button3.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]ButtonEditor.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]Logo.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]songCover.jpg"
 Delete "$INSTDIR\Skins\Classic\[main]square.jpg"
 Delete "$INSTDIR\Skins\Classic\[menu]jumpToBg.jpg"
 Delete "$INSTDIR\Skins\Classic\[menu]PopUpBg.JPG"
 Delete "$INSTDIR\Skins\Classic\[menu]PopUpFg.JPG"
 Delete "$INSTDIR\Skins\Classic\[menu]songMenuBg.jpg"
 Delete "$INSTDIR\Skins\Classic\[menu]songMenuBorder.jpg"
 Delete "$INSTDIR\Skins\Classic\[menu]songMenuButtonBG.jpg"
 Delete "$INSTDIR\Skins\Classic\[menu]songMenuSelectBG.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]Joker.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]playerButton.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]playerTeamButton.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]pointer.bmp"
 Delete "$INSTDIR\Skins\Classic\[party]roundBG1.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]roundBG2.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]roundBG3.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]roundBG4.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]roundTeamButton.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]scoreBG1.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]scoreBG2.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]scoreDecoration.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]teamPoints.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]winDecoration.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]winTeamButton1.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]winTeamButton2.jpg"
 Delete "$INSTDIR\Skins\Classic\[party]winTeamButton3.jpg"
 Delete "$INSTDIR\Skins\Classic\[score]box.jpg"
 Delete "$INSTDIR\Skins\Classic\[score]level.jpg"
 Delete "$INSTDIR\Skins\Classic\[score]levelround.jpg"
 Delete "$INSTDIR\Skins\Classic\[score]line.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]lineBonusPopUp.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]LyricsBall.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]lyricsHelpBar.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]notesBgLeft.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]notesBgMid.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]notesBgRight.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]notesLeft.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]notesMid.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]notesRight.bmp"
 Delete "$INSTDIR\Skins\Classic\[sing]p.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]scoreBg.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]singBarBack.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]singBarBar.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]singBarFront.jpg"
 Delete "$INSTDIR\Skins\Classic\[sing]textBar.jpg"
 Delete "$INSTDIR\Skins\Classic\[song]BGFade.jpg"
 Delete "$INSTDIR\Skins\Classic\[song]EqualizerBG.jpg"
 Delete "$INSTDIR\Skins\Classic\[song]selection.jpg"
 Delete "$INSTDIR\Skins\Classic\[stat]detailBG1.jpg"
 Delete "$INSTDIR\Skins\Classic\[stat]mainBG1.jpg"
 Delete "$INSTDIR\Skins\Classic\[stat]mainBG2.jpg"
 Delete "$INSTDIR\Skins\Classic\[stat]mainBG3.jpg"

; Delete provided Deluxe Skin
 Delete "$INSTDIR\Skins\Deluxe\Blue.ini"
 Delete "$INSTDIR\Skins\Deluxe\Fall.ini"
 Delete "$INSTDIR\Skins\Deluxe\Summer.ini"
 Delete "$INSTDIR\Skins\Deluxe\Winter.ini"
 Delete "$INSTDIR\Skins\Deluxe\[bg-load]blue.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-load]fall.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-load]summer.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-load]winter.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-main]blue.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-main]fall.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-main]summer.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[bg-main]winter.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]13.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]alt.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]az.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]enter.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]esc.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]j.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]m.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]navi.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[button]p.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[effect]goldenNoteStar.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[effect]perfectNoteStar.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[helper]buttonFade.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[helper]rectangle.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]cd.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]error.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]main.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]options.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]party.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]question.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]score.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]search.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]songmenu.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]stats.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[icon]video.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]button.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]buttonf.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]mainBar.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]playerNumberBox.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]selectbg.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]songCover.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]songSelection1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[main]songSelection2.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[menu]jumpToBg.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[menu]PopUpBg.JPG"
 Delete "$INSTDIR\Skins\Deluxe\[menu]PopUpFg.JPG"
 Delete "$INSTDIR\Skins\Deluxe\[menu]songMenuBg.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[menu]songMenuSelectBg.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]Joker.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]playerButton.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]playerTeamButton.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]pointer.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[party]roundBG1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]roundBG2.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]roundBG3.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]roundBG4.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]roundTeamButton.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]scoreBG1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]scoreBG2.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]scoreDecoration.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]teamPoints.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]winDecoration1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]winTeamButton1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]winTeamButton2.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[party]winTeamButton3.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[score]box.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[score]endcap.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[score]level.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[score]levelRound.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[score]Line.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]lineBonusPopUp.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]LyricsBall.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]lyricsHelpBar.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]notesBgLeft.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]notesBgMid.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]notesBgRight.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]notesLeft.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]notesMid.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]notesRight.bmp"
 Delete "$INSTDIR\Skins\Deluxe\[sing]p.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]scoreBg.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]singBarBack.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]singBarBar.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]singBarFront.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]textBar.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]timeBar.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]timeBar1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[sing]timeBarBG.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[special]bar1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[special]bar2.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[stat]detailBG1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[stat]mainBG1.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[stat]mainBG2.jpg"
 Delete "$INSTDIR\Skins\Deluxe\[stat]mainBG3.jpg"

; Delete provided sounds
 Delete "$INSTDIR\Sounds\Common back.mp3"
 Delete "$INSTDIR\Sounds\Common start.mp3"
 Delete "$INSTDIR\Sounds\credits-outro-tune.mp3"
 Delete "$INSTDIR\Sounds\dismissed.mp3"
 Delete "$INSTDIR\Sounds\menu swoosh.mp3"
 Delete "$INSTDIR\Sounds\option change col.mp3"
 Delete "$INSTDIR\Sounds\rimshot022b.mp3"
 Delete "$INSTDIR\Sounds\select music change music 50.mp3"
 Delete "$INSTDIR\Sounds\select music change music.mp3"
 Delete "$INSTDIR\Sounds\wome-credits-tune.mp3"
  
; Delete provided Themes
 Delete "$INSTDIR\Themes\Classic.ini"
 Delete "$INSTDIR\Themes\Deluxe.ini"

; Delete provided Songs
 Delete "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18 [BG].jpg"
 Delete "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18 [CO].jpg"
 Delete "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18.ogg"
 Delete "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\Dead Smiling Pirates - I 18.txt"
 Delete "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]\License.txt"

 Delete "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\Steven Dunston - Northern Star [BG].jpg"
 Delete "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\Steven Dunston - Northern Star [CO].jpg"
 Delete "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\Steven Dunston - Northern Star.mp3"
 Delete "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\Steven Dunston - Northern Star v1.4.txt"
 Delete "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]\License.txt"

; Delete only empty directories:

StrCpy $0 "$INSTDIR\Covers"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Languages"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Plugins"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Skins\Classic"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Skins\Deluxe"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Skins"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Songs\Dead Smiling Pirates - I 18 [DEMO]"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Songs\Steven Dunston - Northern Star [DEMO]"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Songs"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Sounds"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Themes"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Screenshots"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR\Playlists"
Call un.DeleteIfEmpty

StrCpy $0 "$INSTDIR"
Call un.DeleteIfEmpty



; Delete created Icons in startmenu

 SetShellVarContext all

 Delete "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Deinstallieren.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Website.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Internetseite.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\UltraStar Deluxe spielen.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Play UltraStar Deluxe.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Readme.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Lies mich.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Lizenz.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\License.lnk"

; Delete created Icon on Desktop

Delete "$Desktop\Play UltraStar Deluxe.lnk"
Delete "$Desktop\UltraStar Deluxe spielen.lnk"

StrCpy $0 "$SMPROGRAMS\$ICONS_GROUP"
Call un.DeleteIfEmpty

