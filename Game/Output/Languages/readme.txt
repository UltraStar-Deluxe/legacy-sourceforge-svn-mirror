.o0 Ultrastar Deluxe in your Language 0o.

-----------------------
   Table of Contents
-----------------------
1. Introduction
2. Statistic Wild-Cards
3. Texts to Add



-----------------------
1. Introduction:
-----------------------
To translate USD to a new language, take the English Language File, or another one that is up to date and Edit the Texts behind the Equal Mark(=).

-----------------------
2. Statistic Wild-Cards:
-----------------------
Here are some Information about the Wild-Cars in the Language Texts for the Statistic Screens (STAT_...):
Information that will replace the Wild-Cards:

STAT_OVERVIEW_INTRO:
  Format:
    %0:d Ultrastar Version
    %1:d Day of Reset (A1)
    %2:d Month of Reset (A2)
    %3:d Year of Reset (A3)

STAT_OVERVIEW_SONG:
  Format:
    %0:d Count Songs (A1)
    %1:d Count of Sung Songs (A2)
    %2:d Count of UnSung Songs
    %3:d Count of Songs with Video (A3)
    %4:s Name of the most popular Song

STAT_OVERVIEW_PLAYER:
  Format:
    %0:d Count Players (A1)
    %1:s Best Player (Result)
    %2:d Best Players Score
    %3:s Best Score Player (Result2)
    %4:d Best Score

STAT_FORMAT_SCORES:
  Format:
    %0:s Singer
    %1:d Score
    %2:s Difficulty
    %3:s Song Artist   
    %4:s Song Title

STAT_FORMAT_SINGERS:
  Format:
    %0:s Singer
    %1:d Average Score


STAT_FORMAT_SONGS:
  Format:
    %0:s Artist
    %1:s Title
    %2:d Times Sung

STAT_FORMAT_BANDS:
  Format:
    %0:s Artist Name
    %1:d Times Sung

Some further Explantation about the Wild-Cards:
%x:[.y]z

Where X is the Number of the Wildcard,
Y is optional, it is the number of digits for Decimal Numbers (Z=d). So if y is 2 there and the number is only 0 to 9 there will be a Zero added in front of the number.
z can be d for Numbers and s for texts

For the date thing in STAT_OVERVIEW_INTRO you may use %1:.2d for the day and %2:.2d for the Month.

-----------------------
3. Texts to Add:
-----------------------
use the Languages.xls (big thx to Thursday for the english translation of the help section)