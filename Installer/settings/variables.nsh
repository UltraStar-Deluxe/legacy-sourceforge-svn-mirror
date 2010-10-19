; These are the common used variables
; for the USdx Challenge, Medley & Duet Edition Installation Wizard

!define version "r9"						; Current version of UltraStar Deluxe Challenge, Medley & Duet Edition
!define p_name "UltraStar Deluxe CMD Edition" 				; Just the name of the program
!define publisher "USDX Team"					; Publisher
!define homepage "http://www.ultrastardeluxe.org/"		; Project Homepage
!define forum "http://forum.ultrastardeluxe.org/"		; Forum Homepage

!define exe "UltraStar CMDe"						; Current name of start exe (must also be defined in functions.nsh)

;!define demosong "http://ultrastardeluxe.xtremeweb-hosting.net/installer/songs/song.zip" ; URL from where the demo song "I18" will be downloaded	
;!define demosong2 "http://ultrastardeluxe.xtremeweb-hosting.net/installer/songs/song2.zip"  ; URL from where the demo song "Northern Star" will be downloaded	

; Theme URLs:

;!define dl_orange "http://ultrastardeluxe.xtremeweb-hosting.net/installer/themes/orange.zip"
;!define dl_vistar "http://ultrastardeluxe.xtremeweb-hosting.net/installer/themes/vistar.zip"
;!define dl_streetlight "http://ultrastardeluxe.xtremeweb-hosting.net/installer/themes/streetlight.zip"

; Other Language Strings (except Section Language Strings)
; can be found at .\langstrings.nsh

!define eng_sec1_desc "These are the basic files needed by UltraStar Deluxe" 		; English Description of Base components
!define ger_sec1_desc "Dies sind die von UltraStar Deluxe benötigten Grunddateien" 	; German Description of Base components	

;!define eng_sec3_desc "You can choose which optional themes should also be installed."  ; English Description of "Optional Themes"-Section
;!define ger_sec3_desc "Hier können optionale Motive zum Installieren gewählt werden."   ; German Description of "Optional Themes"-Section

;!define eng_g2Section1_desc "Install the demo song 'Dead Smiling Pirates - I 18'."	; English Description of the Example Song "I 18"
;!define ger_g2Section1_desc "Installiert das freie Beispiellied 'Dead Smiling Pirates - I 18'."   	; German Description of the Example Song "I 18"

;!define eng_g2Section2_desc "Install the demo song 'Steven Dunston - Northern Star'."		; English Description of the Example Song "Northern Star"
;!define ger_g2Section2_desc "Installiert das freie Beispiellied 'Steven Dunston - Northern Star'."	; German Description of the Example Song "Northern Star"

;!define eng_g2Section3_desc "Install the demo song 'Joshua Morin - On the run'." ; English Description of the Example Song "On the run"
;!define ger_g2Section3_desc "Installiert das freie Beispiellied 'Joshua Morin - On the run'."; German Description of the Example Song "On the run"

!define eng_sec1 "Base components" 							; English Name of the component section1
!define ger_sec1 "Basiskomponenten" 							; German Name of the component section1

;!define eng_sec2 "Demo Songs" 							; English Name of the component section2
;!define ger_sec2 "Beispiellieder" 							; German Name of the component section2

;!define eng_sec3 "Optional Themes"							; English Name of the component section group1
;!define ger_sec3 "Optionale Motive"							; German Name of the component section group1

; Group Section Descriptions:

;!define eng_g1Sec1_desc "This will install the optional theme 'Orange' by Skar."
;!define ger_g1Sec1_desc "Dies installiert das optionale Motiv 'Orange' von Skar."

;!define eng_g1Sec2_desc "This will install the optional theme 'Streetlight' by Skar."
;!define ger_g1Sec2_desc "Dies installiert das optionale Motiv 'Streetlight' von Skar."

;!define eng_g1Sec3_desc "This will install the optional theme 'Vistar' by Skar."
;!define ger_g1Sec3_desc "Dies installiert das optionale Motiv 'Vistar' von Skar."

; Skin:

!define gdf_path "$WINDIR\gdf.dll"		; Path to gdf.dll for Vista Game Explorer
!define bmp_header "..\InstallerDependencies\images\header.bmp"	; Bitmap of the Installation Header (Size: 150x57 px)
!define bmp_side "..\InstallerDependencies\images\left.bmp"		; Bitmap on the left side of Welcome & Finish Page (Size: 164x314 px)
!define mui_ini ".\settings\io.ini"		; Installation Options for Welcome & Finish Page
!define license_bgcolor "FFFFFF"		; RGB Background Color for Licence agreement
!define bmp_check "..\InstallerDependencies\images\modern.bmp"		; Bitmap of Checks at Components Selection Page
!define directory_bgcolor "FFFFFF"		; RGB Background Color for Directory textbox
!define smp_bgcolor "FFFFFF"			; RGB Background of Startmenu List and Textbox
;!define dets_bgcolor "FFFFFF"			; Background Color of Details Screen while files are being extracted
!define file_license "..\License.txt"	; Choose the file with the license agreement