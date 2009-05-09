# $URL$
# $Id$

add_subdirectory(lib)

##
# CONFIGURATION
##

pascal_configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.inc.in
  ${CMAKE_CURRENT_BINARY_DIR}/config-${FPC_PLATFORM}.inc)

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/paths.inc
  "INSTALL_DATADIR = '${CMAKE_INSTALL_PREFIX}/share/ultrastardx';")

##
# BUILD
##

#if(DELPHI)
#  PACKAGES ctypes
#endif(DELPHI)
#if(WIN32)
#  PACKAGES bass midi
#endif(WIN32)

set(depends)
if(libprojectM_USE_CWRAPPER)
  list(APPEND depends projectM-cwrapper)
endif(libprojectM_USE_CWRAPPER)

pascal_add_module(ultrastardx ultrastardx.dpr
  #FLAGS -vut
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../game
  UNITS
    base/*.pas
    #macosx/*.pas
    media/*.pas
    menu/*.pas
    screens/*.pas
    lib/other/*.pas
    ../plugins/SDK/*.pas
  INCLUDES
    ${CMAKE_CURRENT_BINARY_DIR}/config-${FPC_PLATFORM}.inc
    ${CMAKE_CURRENT_BINARY_DIR}/paths.inc
  PACKAGES
    collections
    ffmpeg
    fft
    freetype
    sdl
    sdl_image
    opengl
    portaudio
    portmixer
    projectm
    png
    samplerate
    sqlite3
    zlib
  DEPENDS ${depends}
)
