#!/bin/sh
AUTOGEN_DIR=../../../../dists/autogen
libtoolize --force --copy
aclocal -I ${AUTOGEN_DIR}/m4 && autoconf
automake -a

