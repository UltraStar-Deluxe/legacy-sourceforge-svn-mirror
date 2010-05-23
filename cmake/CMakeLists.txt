# UltrastarDX CMake script
# by UltraStar Deluxe Team
#
# CMake primers:
# - Tutorial (Linux Magazin, German):
#   http://www.linux-magazin.de/heft_abo/ausgaben/2007/02/mal_ausspannen
# - Official CMake site:
#   http://www.cmake.org/ 
# - Official CMake 2.6 documentation
#   http://www.cmake.org/cmake/help/cmake2.6docs.html
# - CMake Useful Variables (CMake Wiki):
#   http://www.cmake.org/Wiki/CMake_Useful_Variables
#
# $URL$
# $Id$

##
# CMAKE SETTINGS
##

# min. version and policies
cmake_minimum_required(VERSION 2.6)
# module and script directory
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/dists/cmake/Modules)

##
# PROJECT SETTINGS
##

project(UltrastarDX)

set(CPACK_PACKAGE_NAME "ultrastardx")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_PACKAGE_VERSION_PATCH "0")

set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}_beta1")

set(PACKAGE_WEBSITE "http://www.ultrastardeluxe.org/")
set(PACKAGE_IRC "#ultrastardx at quakenet.org")

##
# MAIN DEPENDENCIES
##

find_package(PPC 2.2.2 REQUIRED)
include(UsePascal)
include(ExpandVersion)
include(FileUtils)

##
# LIBRARIES
##

# for some reason pkg_check_modules does not quit the configuration 
# phase if REQUIRED is specified and the module is not found.
function(pkg_check_required_modules prefix modules)
  pkg_check_modules(${prefix} REQUIRED ${modules})
  if(NOT ${prefix}_FOUND)
    message(FATAL_ERROR "A required module is not available. Install it and try again.")
  endif(NOT ${prefix}_FOUND)
endfunction(pkg_check_required_modules)

if(NOT WIN32)
  find_package(PkgConfig REQUIRED)

  #find_package(OpenGL)
  #pkg_check_required_modules(opengl gl)

  pkg_check_required_modules(sdl sdl)
  #find_package(SDL REQUIRED)

  # find SDL_image
  message(STATUS "Looking for SDL_image")
  find_package(SDL_image)
  if(SDLIMAGE_LIBRARY)
    message(STATUS "Looking for SDL_image - found")
  else(SDLIMAGE_LIBRARY)
    message(FATAL_ERROR "Looking for SDL_image - missing")
  endif(SDLIMAGE_LIBRARY)
  # hide SDL_image from GUI
  mark_as_advanced(SDLIMAGE_LIBRARY SDLIMAGE_INCLUDE_DIR)

  pkg_check_required_modules(libpng libpng12)
  #find_package(PNG REQUIRED)
  pkg_check_required_modules(freetype freetype2)
  #find_package(Freetype REQUIRED)
  pkg_check_required_modules(sqlite3 sqlite3)

  # find FFMpeg
  # Note: do not use the min/max version parameters with ffmpeg
  # otherwise it might fail in ubuntu due to a wrong version number 
  # format in ffmpeg's .pc-files.
  # For example: 0d.51.1.2 instead of the correct 51.1.2.
  #   A check for version >=52.0.0 will return version 0d.51.1.2 
  #   although it is lower because pkg-config is confused by the 0d.
  # Use ${libav...}_VERSION_INT for version-checking instead
  pkg_check_required_modules(libavcodec libavcodec)
  expand_version(libavcodec_VERSION)
  pkg_check_required_modules(libavformat libavformat)
  expand_version(libavformat_VERSION)
  pkg_check_required_modules(libavutil libavutil)
  expand_version(libavutil_VERSION)
  set(ffmpeg_FOUND TRUE)

  # find FFMpeg's swscale lib (just if FFMpeg is compiled in GPL mode)
  pkg_check_modules(libswscale libswscale)
  expand_version(libswscale_VERSION)

  option(ENABLE_PROJECTM "Enable ProjectM support if available" OFF)
  set(libprojectM_USE_CWRAPPER FALSE)
  if(ENABLE_PROJECTM)
    # find projectM version
    set(libprojectM_MODULE "libprojectM >= 0.98")
    pkg_check_modules(libprojectM ${libprojectM_MODULE})

    if(libprojectM_FOUND)
      # get projectM data-dir (for preset- and font-dir)
      execute_process(
        COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=pkgdatadir ${libprojectM_MODULE}
        OUTPUT_VARIABLE libprojectM_DATADIR
        OUTPUT_STRIP_TRAILING_WHITESPACE)

      # check if we need the c-wrapper
      if(NOT libprojectM_VERSION VERSION_LESS "1.0.0")
        set(libprojectM_USE_CWRAPPER TRUE)
      endif()
    endif(libprojectM_FOUND)
  endif(ENABLE_PROJECTM)
  expand_version(libprojectM_VERSION)

  # find portaudio
  pkg_check_required_modules(portaudio portaudio-2.0)
  expand_version(portaudio_VERSION)

  # find portmixer
  pkg_check_modules(portmixer portmixer)

  # find lua 5.1
  # (K)Ubuntu uses lua5.1.pc, Mac OS X and other 
  # linux distributions use lua.pc
  pkg_check_modules(lua lua5.1)
  set(lua_LIB_NAME "lua5.1")
  if(NOT lua_FOUND)
    pkg_check_required_modules(lua "lua >= 5.1")
    set(lua_LIB_NAME "lua")
  endif(NOT lua_FOUND)
endif(NOT WIN32)

##
# INITIALIZATION
##

include(FileUtils)

set(USDX_GAME_DIR ${CMAKE_CURRENT_BINARY_DIR}/game)

# init runtime environment for out-of-source builds
if(NOT WIN32)
if(NOT ${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  # create game dir
  file(MAKE_DIRECTORY ${USDX_GAME_DIR})

  # symlink directories which are shared by all out-of-source builds
  foreach(subdir covers fonts languages resources sounds themes visuals)
    file_symlink(${CMAKE_CURRENT_SOURCE_DIR}/game/${subdir} ${USDX_GAME_DIR}/${subdir})
  endforeach(subdir)
endif()
endif(NOT WIN32)

##
# COMPILATION
##

add_subdirectory(src)

macro(add_configuration_target target build_type)
ADD_CUSTOM_TARGET(${target}
  COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_BINARY_DIR}
    ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE:STRING=${build_type} ${PROJECT_SOURCE_DIR}
  COMMAND ${CMAKE_BUILD_TOOL}
  COMMAND ${CMAKE_COMMAND} -E echo "Configuration switched: ${build_type}"
)
endmacro()

# convenience targets to switch between debug and release configuration
add_configuration_target(debug Debug)
add_configuration_target(release Release)

##
# INSTALLATION
##

set(BINDIR "bin" CACHE STRING "Where to install binaries. Either relative to CMAKE_INSTALL_PREFIX or absolute.")
set(DATADIR "share/ultrastardx" CACHE STRING "Where to install game content. Either relative to CMAKE_INSTALL_PREFIX or absolute.")

get_target_property(usdx_exe ultrastardx PASCAL_LOCATION)
install(PROGRAMS ${usdx_exe} DESTINATION ${BINDIR})

install(
  DIRECTORY 
    artwork
    game/covers
    game/fonts
    game/languages
    game/plugins
    game/resources
    game/sounds
    game/themes
  DESTINATION ${DATADIR}
  PATTERN .svn EXCLUDE
  PATTERN .PLACEHOLDER EXCLUDE
)
install(
  FILES
    COPYING.txt
  DESTINATION ${DATADIR}
)

#if(APPLE)
#  include(MacOSXBundle)
#endif(APPLE)

# add "uninstall" target
# See: http://www.vtk.org/Wiki/CMake_FAQ#Can_I_do_.22make_uninstall.22_with_CMake.3F
CONFIGURE_FILE(
  "${CMAKE_CURRENT_SOURCE_DIR}/dists/cmake/Modules/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)

ADD_CUSTOM_TARGET(uninstall
  "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")


##
# CPACK
##

# CPack vars

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Karaoke program that evaluates your performance")
set(CPACK_PACKAGE_VENDOR "UltraStar Deluxe Team")
set(CPACK_PACKAGE_FILE_NAME "ultrastardx-${CPACK_PACKAGE_VERSION}")
set(CPACK_PACKAGE_EXECUTABLES "ultrastardx" "ultrastardx")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "ultrastardx ${CPACK_PACKAGE_VERSION}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "ultrastardx-${CPACK_PACKAGE_VERSION}-src")
set(CPACK_SOURCE_GENERATOR "TGZ")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.txt")

if(WIN32 AND NOT UNIX)
  set(CPACK_NSIS_DISPLAY_NAME "UltraStar Deluxe") 
  set(CPACK_NSIS_HELP_LINK ${PACKAGE_WEBSITE})
  set(CPACK_NSIS_URL_INFO_ABOUT ${PACKAGE_WEBSITE})
  set(CPACK_NSIS_CONTACT "help@ultrastardx.org")
  set(CPACK_NSIS_MODIFY_PATH OFF)
  set(CPACK_GENERATOR "ZIP;NSIS")
else(WIN32 AND NOT UNIX)
  set(CPACK_STRIP_FILES TRUE)
endif(WIN32 AND NOT UNIX)


set(CPACK_SOURCE_IGNORE_FILES
  "~"
  "\\\\.svn"
  "\\\\.history"
  "\\\\.exe$"
  "\\\\.a$"
  "\\\\.dll$"
  "\\\\.dof$"
  "\\\\.zip$"
  "\\\\.gz$"
  "\\\\.bz2$"
  "\\\\.tmp$"
  "\\\\.dcu$"
  "\\\\.ppu$"
  "\\\\.o$"
  "\\\\.obj$"
  "link\\\\.res$"
  "CMakeFiles"
  "Makefile$"
  "CPack"
  "install_manifest\\\\.txt$"
  "CMakeCache\\\\.txt$"
  "^${PROJECT_SOURCE_DIR}/[^/]*\\\\.cmake$"
  "^${PROJECT_SOURCE_DIR}/src/.*\\\\.cmake$"
  "^${PROJECT_SOURCE_DIR}/game/ultrastardx$"
  "^${PROJECT_SOURCE_DIR}/src/paths\\\\.inc$"
  "^${PROJECT_SOURCE_DIR}/src/config-[^w].*\\\\.inc$"
  "^${PROJECT_SOURCE_DIR}/build/[^d]"
)
if(NOT ${PROJECT_SOURCE_DIR} STREQUAL ${PROJECT_BINARY_DIR})
  list(APPEND CPACK_SOURCE_IGNORE_FILES "^${PROJECT_BINARY_DIR}")
endif()

include(CPack)

##
# SUMMARY
##


message("
!! Configuration of ${CPACK_PACKAGE_NAME} ${CPACK_PACKAGE_VERSION} done!
!!
!! Type \"make\" to compile and 
!!      \"make install\" to install it afterwards.
!!
!! For further information on ${CPACK_PACKAGE_NAME} visit:
!!   ${PACKAGE_WEBSITE}
!!
!! In case you find a bug send a bugreport to:
!!   ${PACKAGE_BUGREPORT}
!! You might as well ask for help at the IRC-Channel
!!   ${PACKAGE_IRC}
!!")
