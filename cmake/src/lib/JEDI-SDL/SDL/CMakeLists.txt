# $URL$
# $Id$

# Flags for the treatment of a linker issue with SDL on Mac OS X.
# Symbol _SDK_main is undefined
set(fpc_lib_flags)
if(APPLE)
  set(fpc_lib_flags -k\"-U _SDL_main\")
endif(APPLE)

pascal_add_package(sdl
  UNITS Pas/*.pas
  INCLUDES Pas/*.inc
  FPC_LIBRARY_FLAGS ${fpc_lib_flags}
)
