#!/bin/sh
AUTOGEN_DIR=dists/autogen
#libtoolize
aclocal -I ${AUTOGEN_DIR}/m4 && autoconf
automake -a
cd src/mediaplugins
./autogen.sh

