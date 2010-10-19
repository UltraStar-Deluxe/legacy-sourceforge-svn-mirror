Ultrastar Deluxe Challenge, Medley & Duet Edition Readme.txt
----------------------------
1. Authors
2. Release Notes
3. Command-Line Parameters
4. Controls
5. License
----------------------------

SF.Net Page: http://sourceforge.net/projects/ultrastardx/
German Ultrastar Forum: http://www.ultra-star.de

This is just a little introduction, for more information press the TAB key in every screen of the game.

----------------------------
1. Authors
----------------------------
This game was introduced by Corvus5 who has written most of the code by himself.
Basing on the official release 0.5.0 Mota and Whiteshark started to write little patches and modifications, and released a package named ultra-star.dl.am Mod.
This modification was continued at Sourceforge.net by the Ultrastar Deluxe Team:
     Blindy
     Mog
     Mota
     Sawyer
     Whiteshark
And Ultrastar Deluxe 1.0.1a was the Result (December 2007).

The Challenge, Medley & Duet Edition was created in the year 2010 by brunzel who has joined the Ultrastar Deluxe Team in December 2009.
A lot of the changes and new function are the result of suggestions from the German Ultrastar forum.

----------------------------
2. Release Notes
----------------------------

New features and functions:
+ Challenge mode:
  - Sing with up to 9 players in a compitition!
  
+ Medley support:
  - Sing shortet versions of your songs! The program tries to recognize suitable sections automatically. You can also set tags for manual definitions.
  - new tags:
	* MedleyStartBeat: Start of the medley-section at this beat
	* MedleyEndBeat: End of the medley-section at this beat
	* CalcMedley: if 'Off' then the program will exclude this song from the medley function
	* PreviewStart: The beginning of the preview in song selection (in seconds)
  - Start the medleys over the menu (press "M" in song selection screen) or just press TAB for a full shortcut list.
  - Medleys are usable in the party-modes.
  
+ Duet support:
  - Sing with 2 or 4 players a song with 2 independed lyrics (voices).
  - Duet Syntax: just add a "P1" before the lyrics for player 1 and a "P2" in front of the lyrics of player 2:
    <header>
	P1
	<notes and lyrics of player 1>
	P2
	<notes and lyrics of player 2>
	E

+ WebCam support:
  - Enable and adjust the webcam setting in the recording options screen.
  - Press afterwards "W" in singscreen to switch between webcam and video.
  
Please read the changelog for more information.

Some hints:

- To change the path to the song directory add to config.ini:
  [Path]
  Songs=[SongFolder] (e.g. C:\Program Files\Ultrastar\Songs)

- To take a screenshot press "PrintScreen" Key
  Screenshots are saved in the directory "Screenshots".

- To Enable 4 to 6 Playermode 2 Screens are needed.
  Disable the fullscreen mode, extend your desktop horizontaly, set the resolution to fill one
  screen. 
  Add to Config.ini:
  [Graphics]
  Screens=2
  
- This Version has also the support of 4 and 6 players on one screen.

- Press Alt + F[1..12] in NameScreen to Save a Playername
  Press F[1..12] to Load a Playername

- To enable benchmark run the game with -benchmark parameter.


----------------------------
3. Command-Line Parameters
----------------------------
Command-Line Parameters are passed to the game adding it to the Path of a Shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.

-Benchmark         : Create a benchmark.log file with start timings.
-NoLog    	   : Do not create any .log files
-Language [ID]     : Load Language [ID] on startup. Example: -Language german

-Songpath [Path]   : Some as config Songpath. Example: -SongPath "C:\Ultrastar Songs"
-ConfigFile [File] : Load Configfile [File] instead of config.ini. Path to the file have to exist. Example: -ConfigFile config.SongCreation.ini
-ScoreFile [File]  : Use [File] instead of Ultrastar.db. Path to the file have to exist. Example: -ScoreFile HouseParty.db

-FullScreen        : Start the game in Fullscreen Mode
-Depth [16/32]     : Force Depth 16 or 32. Example: -Depth 16
-Resolution [ID]   : Force resolution. Example: -Resolution 800x600
-Screens [1/2]     : Force 1 or 2 Screen Mode. Example: -Screens 2

Some Examples:

Start with Resolution 1024x768 32 Bit Depth and Fullscreen:
ultrastar.exe -Resolution 1024x768 -Depth 32 -Fullscreen

Start without logging and polish Language
ultrastar.exe -NoLog -Language polish

Start with custom config File and Score DB:
ultrastar.exe -ConfigFile C:\Ultrastar\Configs\PartyConfig.ini -ScoreFile C:\Ultrastar\Scores\PartyScores.db


----------------------------
4. Controls
----------------------------
Press the TAB key to see a small description and keymapping in every screen.

Use the Arrowkeys to navigate through the Screens.
Use Enter to select and Escape to go to the previous screen.
In Songscreen you can use R, Shift + R or Strg + R to select a random song/category
Use Alt + [Letter] to jump to a songs artist with the first letter [Letter]
Use Alt + Shift + [Letter] to jump to a song title with the first letter [Letter]
Press J to open the "Search for a Song" Interface


----------------------------
5. License
----------------------------
Ultrastar Deluxe is licensed under the terms of the GNU General Public License 2.0
See License.txt for more Information.