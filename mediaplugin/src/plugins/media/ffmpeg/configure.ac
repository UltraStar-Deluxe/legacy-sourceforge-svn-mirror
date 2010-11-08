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

# -----------------------------------------
# find tools
# -----------------------------------------

LT_INIT

# options for make command
AC_PROG_MAKE_SET
# find tool for ln -s (e.g. uses cp -p for FAT-filesystems)
#AC_LN_S
# find a program for recursive dir creation
#AC_PROG_MKDIR_P
# find the best install tool
#AC_PROG_INSTALL
# some other useful tools
#AC_PROG_AWK
#AC_PROG_SED
#AC_PROG_GREP
AC_PROG_LIBTOOL
#AC_PROG_RANLIB
#AC_PROG_EGREP

AM_INIT_AUTOMAKE([foreign])

# -----------------------------------------
# check for compilers
# -----------------------------------------

AC_CANONICAL_HOST

# find and test the C compiler (for C-libs and wrappers)
AC_PROG_CC
AC_LANG([C])

# find and test the C++ compiler (for C-libs and wrappers)
AC_PROG_CXX
AC_LANG([C++])


# find pkg-config
PKG_PROG_PKG_CONFIG()
if [[ x$PKG_CONFIG = x ]]; then
   AC_MSG_ERROR([
!!! pkg-config was not found on your system. 
!!! It is needed to determine the versions of your libraries.
!!! Install it and try again.])
fi


# -----------------------------------------
# check for libraries
# -----------------------------------------

# find FFMpeg
PKG_CHECK_MODULES([ffmpeg], [libavcodec libavformat libavutil], [
    #AC_CHECK_HEADERS(libavcodec/avcodec.h ffmpeg/avcodec.h)
    #AC_CHECK_HEADERS(libavutil/avutil.h ffmpeg/avutil.h)
  ])
# find FFMpeg's swscale lib (just if FFMpeg is compiled in GPL mode)
PKG_CHECK_MODULES([libswscale], [libswscale], 
  [
    HAVE_SWSCALE="yes"
  ], [
    HAVE_SWSCALE="no"
  ])

# -----------------------------------------
# create output files
# -----------------------------------------

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

