# $URL$
# $Id$

add_subdirectory(lib)

##
# CONFIGURATION
##

pascal_configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.inc.in
  ${CMAKE_CURRENT_BINARY_DIR}/config-${FPC_PLATFORM}.inc)

# write path to shared data to paths.inc
if(UNIX AND NOT APPLE)
  if(IS_ABSOLUTE ${DATADIR})
    set(usdx_install_datadir "${DATADIR}")
  else()
    set(usdx_install_datadir "${CMAKE_INSTALL_PREFIX}/${DATADIR}")
  endif()
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/paths.inc
    "INSTALL_DATADIR = '${usdx_install_datadir}';")
endif()

##
# BUILD
##

# includes used by ultrastardx
set(usdx_includes
  ${CMAKE_CURRENT_BINARY_DIR}/config-${FPC_PLATFORM}.inc
)
if(UNIX AND NOT APPLE)
  list(APPEND usdx_includes ${CMAKE_CURRENT_BINARY_DIR}/paths.inc)
endif()

# packages by ultrastardx
set(usdx_packages
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
)
if(WIN32)
  list(APPEND usdx_packages bass midi)
endif(WIN32)
#if(DELPHI)
#  list(APPEND usdx_packages ctypes)
#endif(DELPHI)

# dependencies by ultrastardx
set(usdx_depends)
if(libprojectM_USE_CWRAPPER)
  list(APPEND usdx_depends projectM-cwrapper)
endif()

# add target for ultrastardx binary
pascal_add_module(ultrastardx ultrastardx.dpr
  #FPC_FLAGS -Si -Cs2000000 # maybe set stack size?
  OUTPUT_DIR ${USDX_GAME_DIR}
  UNITS
    base/*.pas
    macosx/*.pas
    media/*.pas
    menu/*.pas
    screens/*.pas
    lib/other/*.pas
    ../plugins/SDK/*.pas
  INCLUDES ${usdx_includes}
  PACKAGES ${usdx_packages}
  DEPENDS ${usdx_depends}
)
