#!/bin/bash
# check if the MinGW/Msys grep is used and not Borland's or FPC's version
GREP_DIR=`which grep`
if ([ "$GREP_DIR" != "/bin/grep.exe" ] && [ "$GREP_DIR" != "/usr/bin/grep.exe" ]); then
  echo "Incorrect version of grep ($GREP_DIR)"
  echo "Make sure mingw/msys precede FPC and Delphi in the PATH environment."
  exit 1
fi
cd src/mediaplugins/ffmpeg
./autogen.sh
FFMPEG_LIBDIR="libffmpeg"
./configure ffmpeg_CFLAGS="-I${FFMPEG_LIBDIR}/include" ffmpeg_LIBS="-L${FFMPEG_LIBDIR}/lib -lavcodec -lavformat -lavutil" libswscale_CFLAGS="-I${FFMPEG_LIBDIR}/include" libswscale_LIBS="-L${FFMPEG_LIBDIR}/lib -lswscale"
make compile
