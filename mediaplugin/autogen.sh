#!/bin/sh
AUTOGEN_DIR=dists/autogen
aclocal -I ${AUTOGEN_DIR}/m4 && autoconf
cd src/plugins/media/
./autogen.sh

