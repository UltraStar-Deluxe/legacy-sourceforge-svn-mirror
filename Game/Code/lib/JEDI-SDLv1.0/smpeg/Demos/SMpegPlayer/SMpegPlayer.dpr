program SMpegPlayer;
{******************************************************************}
{                                                                  }
{       Object Pascal Example of using smpeg and SDL_Mixer         }
{             Conversion of Console Smpeg player                   }
{                                                                  }
{                                                                  }
{ The original files are : Found on internet                       }
{                                                                  }
{ The original Pascal code is : SMpegPlayer.dpr                    }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{   SMpegPlayer : Shows how to load and play an Mpeg file using    }
{                 smpeg and SDL_Mixer for the sound                }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, smpeg and OpenGL somewhere         }
{   in your path .                                                 }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   This demo shows how to load and play an mpeg file using smpeg  }
{   with SDL_Mixer                                                 }
{   You will need Smpeg and SDL_Mixer libraris order for this demo }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   July   02 2001 - DL : Initial translation.                     }
{                                                                  }
{ November 23 2002 - DL : Fix PollMPeg as suggested by             }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  sdl,
  sdl_mixer,
  smpeg;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;
  BPP = 0;

  TITLE = 'JEDI-SDL Console MpegPlayer';

var
  //  Screen Surface
  screen : PSDL_Surface;

  // Audio Specs
  aspec : TSDL_AudioSpec;
  format : Uint16;
  freq, chan : integer;
  Islooping : Boolean;

function OpenMpeg( FileName : string ) : PSMPEG;
var
  Handle : PSMPEG;
begin
  // Create a new Mpeg
  handle := SMPEG_new( PChar( fileName ), nil, 0 );
  if handle = nil then
  begin
    //Display Error
    Halt;
  end;

  // Disable Audio
  SMPEG_enableaudio( handle, 0 );

  // Query Mixer
  Mix_QuerySpec( freq, format, chan );
  aspec.freq := freq;
  aspec.format := format;
  aspec.channels := chan;

  // Tell Smpeg what we want
  Smpeg_actualSpec( handle, @aspec );

  // Hook the mixer audio playing function
  Mix_HookMusic( @SMPeg_PlayAudioSDL, handle );

  // Reenable Audio
  SMPEG_enableaudio( handle, 1 );

  // Set Max Volume
  SMPEG_setvolume( handle, 100 );

  //Set up a video surface to display MPeg in
  SMPEG_setdisplay( Handle, Screen, nil, nil );

  // Reenable Video
  SMPEG_enablevideo( handle, 1 );

  // Retuen the handle in case we need it somewhere else
  Result := handle;
end;

// Free Mpeg

procedure FreeMPeg( handle : PSMPEG );
begin
  SMPEG_delete( handle );
end;

// Loop Mpeg

procedure LoopMpeg( handle : PSMPEG );
begin
  if IsLooping then
  begin
    SMPEG_loop( handle, 1 );
    isLooping := true;
  end
  else
  begin
    SMPEG_loop( handle, 0 );
    isLooping := false;
  end;
end;

// Play Mpeg

procedure PlayMpeg( handle : PSMPEG );
begin
  SMpeg_play( handle );
end;

// Stop Mpeg

procedure StopMpeg( handle : PSMPEG );
begin
  SMpeg_stop( handle );
end;

function PollMPeg( handle : PSMPEG ) : TSMpegStatus;
begin
  Result := SMPEG_status( handle );
end;

function WaitMpeg( interval : Uint32; param : pointer ) : Uint32;
var
  e : TSDL_Event;
  handle : PSMPEG;
begin
  handle := PSMPEG( param );

  // Has it stopped? If so, throw a User Event
  if PollMPeg( handle ) = STATUS_SMPEG_STOPPED then
  begin
    e.type_ := SDL_USEREVENT;
    e.user.code := 1;
    e.user.data1 := nil;
    e.user.data2 := nil;
    SDL_PushEvent( @e );
  end;

  result := interval;
end;

procedure RunIntro( fileName : string );
var
  e : TSDL_Event;
  handle : PSMPEG;
  td : PSDL_TimerID;
  done : Boolean;
begin
  done := false;
  // Create a Movie
  handle := OpenMpeg( fileName );

  // Play the Movie
  PlayMPeg( Handle );

  // Create a timer to see if the Movie has stopped
  td := SDL_AddTimer( 1000, @WaitMPeg, handle );

  // wait for Movie to finish
  while not done do
  begin
    // wiat for out event to happen
    SDL_WaitEvent( @e );
    case e.type_ of
      // Check to see if user want to skip the movie
      SDL_KEYDOWN :
        begin
          if e.key.keysym.sym = SDLK_ESCAPE then
            Done := true;
        end;

      SDL_USEREVENT :
        begin
          if PollMpeg( handle ) = STATUS_SMPEG_STOPPED then
            Done := true;
        end

    end;
  end;

  // Stop the movie
  StopMpeg( handle );

  // Remove WaitMpegTimer
  SDL_RemoveTimer( td );

  // Unhook mixer audio playback function
  Mix_HookMusic( nil, nil );

  // Free out MPEG
  FreeMPeg( handle );
end;

begin
  // Make sure we at least have a parameter
  if ParamCount <> 1 then
  begin
    Halt( 1 );
  end;
  
  // Initialize SDL
  if SDL_Init( SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_TIMER ) < 0 then
  begin
    // Display and error
    Halt( 1 );
  end;

  // Open the Mixer before SDL_SetVideo to avoid the poping sound
  Mix_OpenAudio( 22050, AUDIO_S16, 2, 1024 );

  SDL_WM_SetCaption( TITLE, nil );

  // Set the video Mode
  screen := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, BPP, SDL_DOUBLEBUF or
    SDL_ANYFORMAT );
  if screen = nil then
  begin
    // Display and error
    Halt( 1 );
  end;

  RunIntro( ParamStr( 1 ) );

  Mix_CloseAudio;

  SDL_Quit;
end.

