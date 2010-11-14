#
# ultrastardx configure.ac script
#
# by UltraStar Deluxe Team
#
# Execute "autogen.sh" or "make reconf"
# to create the configure script.

# Require autoconf >= 2.61
AC_PREREQ(2.61)

# Init autoconf
AC_INIT([usdx-ffmpeg-plugin], [1.1])

# Specify a source-file so autoconf can check if the source-dir exists
AC_CONFIG_SRCDIR(.)

# Set the path to install-sh
AC_CONFIG_AUX_DIR(../../../../dists/autogen)

# show features and packages in one list
AC_PRESERVE_HELP_ORDER

AC_CANONICAL_SYSTEM

# -----------------------------------------
# find tools
# -----------------------------------------

AM_INIT_AUTOMAKE([foreign])

# options for make command
AC_PROG_MAKE_SET
AM_PROG_LIBTOOL

# -----------------------------------------
# functions
# -----------------------------------------

# ADD_DEFINE(DEFINE)
# adds a preprocessor definition
AC_DEFUN([ADD_DEFINE],
[
    CPPFLAGS="-D$1 $CPPFLAGS"
])

# -----------------------------------------
# check for compilers
# -----------------------------------------

# find and test the C compiler (for C-libs and wrappers)
AC_PROG_CC
AC_LANG([C])

# find and test the C++ compiler (for C-libs and wrappers)
AC_PROG_CXX
AC_LANG([C++])

case "${target_os}" in
  darwin*)
    CPPFLAGS="$CPPFLAGS -arch i386"
    CFLAGS="$CFLAGS -arch i386"
    CXXFLAGS="$CXXFLAGS -arch i386"
	LDFLAGS="$LDFLAGS -dynamic"
    LIB_EXT=.dylib
    ;;
  cygwin*|mingw*)
	LDFLAGS="$LDFLAGS -no-undefined"
    LIB_EXT=.dll
    ;;
  *)
    LIB_EXT=.so
    ;;
esac
AC_SUBST(LIB_EXT)

PLUGIN_DIR="../../../../game/plugins/media"
AC_SUBST(PLUGIN_DIR)

# -----------------------------------------
# check for libraries
# -----------------------------------------

# find pkg-config
PKG_PROG_PKG_CONFIG()

# find FFmpeg
PKG_CHECK_MODULES([ffmpeg], [libavcodec libavformat libavutil], [
    CPPFLAGS="$ffmpeg_CFLAGS $CPPFLAGS"
    #AC_CHECK_LIB([avcodec], [img_convert], [HAVE_IMG_CONVERT="yes"])
    AC_CHECK_HEADERS(ffmpeg/avformat.h, [
      # old include layout
      ADD_DEFINE(HAVE_FFMPEG_INCLUDE_DIR)
    ])
  ])
# find FFMpeg's swscale lib (just if FFMpeg is compiled in GPL mode)
PKG_CHECK_MODULES([libswscale], [libswscale], 
  [ 
    ADD_DEFINE(HAVE_SWSCALE) 
  ], [])
    
# -----------------------------------------
# create output files
# -----------------------------------------

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

