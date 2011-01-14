#!/bin/bash
# check if the MinGW/Msys grep is used and not Borland's or FPC's version
GREP_DIR=`which grep`
if ([ "$GREP_DIR" != "/bin/grep.exe" ] && [ "$GREP_DIR" != "/usr/bin/grep.exe" ]); then
  echo "Incorrect version of grep ($GREP_DIR)"
  echo "Make sure mingw/msys precede FPC and Delphi in the PATH environment."
  exit 1
fi
cd src/mediaplugins/ffmpeg
#./autogen.sh
FFMPEG_DIR="libffmpeg"
FFMPEG_LIB="${FFMPEG_DIR}/lib"
FFMPEG_INCLUDE="${FFMPEG_DIR}/include"
# remove MSVC compatibility files inttypes.h and stdint.h
if [ -e "${FFMPEG_INCLUDE}/inttypes.h" ]; then
  mkdir -p "${FFMPEG_INCLUDE}/intheader"
  mv -f "${FFMPEG_INCLUDE}/inttypes.h" "${FFMPEG_INCLUDE}/stdint.h" "${FFMPEG_INCLUDE}/intheader"
fi
./configure ffmpeg_CFLAGS="-I${FFMPEG_INCLUDE}" ffmpeg_LIBS="-L${FFMPEG_LIB} -lavcodec -lavformat -lavutil" libswscale_CFLAGS="-I${FFMPEG_INCLUDE}" libswscale_LIBS="-L${FFMPEG_LIB} -lswscale"
make compile
