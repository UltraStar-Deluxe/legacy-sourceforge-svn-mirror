add_subdirectory(lib)

##
# CONFIGURATION
##

include(ConfigurePasFile)  
configure_pas_file(${CMAKE_CURRENT_SOURCE_DIR}/config.inc.in
	${CMAKE_CURRENT_BINARY_DIR}/config-${FPC_PLATFORM}.inc)

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/paths.inc 
     "INSTALL_DATADIR = '/usr/local/share/ultrastardx';")

##
# BUILD
##

#if(DELPHI)
#  PACKAGES ctypes bass midi
#endif(DELPHI)

set(depends)
if(libprojectM_USE_CWRAPPER)
  list(APPEND depends projectM-cwrapper)
endif(libprojectM_USE_CWRAPPER)

pascal_add_module(ultrastardx ultrastardx.dpr
#  FLAGS -vut
  UNITS 
    base/*.pas 
    menu/*.pas
    screens/*.pas 
    media/*.pas
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
    sdl sdl_image opengl 
    portaudio portmixer 
    projectm
    png
    samplerate
    sqlite3
    zlib
  DEPENDS ${depends}
)
