#!/bin/sh
#
# Author: UltraStar Deluxe Team

USDX_BUNDLE="`echo "$0" | sed -e 's/\/Contents\/MacOS\/UltraStarDeluxe//'`"
USDX_RESOURCES="$USDX_BUNDLE/Contents/Resources"

export "DYLD_LIBRARY_PATH=$USDX_RESOURCES/dylib"
exec "$USDX_RESOURCES/bin/ultrastardx"
