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

set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")

set(PACKAGE_WEBSITE "http://www.ultrastardeluxe.org/")
set(PACKAGE_IRC "#ultrastardx at quakenet.org")

##
# MAIN DEPENDENCIES
##

find_package(PPC 2.2.0 REQUIRED)
include(UsePascal)
include(ExpandVersion)
include(FileUtils)

##
# LIBRARIES
##

if(NOT WIN32)
  find_package(PkgConfig REQUIRED)

  #find_package(OpenGL)
  #pkg_check_modules(opengl REQUIRED gl)

  pkg_check_modules(sdl REQUIRED sdl)
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

  pkg_check_modules(libpng REQUIRED libpng12)
  #find_package(PNG REQUIRED)
  pkg_check_modules(freetype REQUIRED freetype2)
  #find_package(Freetype REQUIRED)
  pkg_check_modules(sqlite3 REQUIRED sqlite3)

  # find FFMpeg
  # Note: do not use the min/max version parameters with ffmpeg
  # otherwise it might fail in ubuntu due to a wrong version number 
  # format in ffmpeg's .pc-files.
  # For example: 0d.51.1.2 instead of the correct 51.1.2.
  #   A check for version >=52.0.0 will return version 0d.51.1.2 
  #   although it is lower because pkg-config is confused by the 0d.
  # Use ${libav...}_VERSION_INT for version-checking instead
  pkg_check_modules(libavcodec REQUIRED libavcodec)
  expand_version(libavcodec_VERSION)
  pkg_check_modules(libavformat REQUIRED libavformat)
  expand_version(libavformat_VERSION)
  pkg_check_modules(libavutil REQUIRED libavutil)
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
  pkg_check_modules(portaudio REQUIRED portaudio-2.0)
  expand_version(portaudio_VERSION)

  # find portmixer
  pkg_check_modules(portmixer portmixer)
endif(NOT WIN32)

##
# INITIALIZATION
##

include(FileUtils)

set(USDX_GAME_DIR ${CMAKE_CURRENT_BINARY_DIR}/game)
set(USDX_PLUGIN_DIR ${USDX_GAME_DIR}/plugins)

# init runtime environment for out-of-source builds
if(NOT WIN32)
if(NOT ${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  # create game dir
  file(MAKE_DIRECTORY ${USDX_GAME_DIR})

  # symlink directories which are shared by all out-of-source builds
  foreach(subdir covers fonts languages resources sounds themes visuals)
    file_symlink(${CMAKE_CURRENT_SOURCE_DIR}/game/${subdir} ${USDX_GAME_DIR}/${subdir})
  endforeach(subdir)

  # copy directories which are either changed by the build process
  # or while execution
  file_copy_dir(${CMAKE_CURRENT_SOURCE_DIR}/game/plugins ${USDX_PLUGIN_DIR})
endif()
endif(NOT WIN32)

##
# COMPILATION
##

add_subdirectory(src)
add_subdirectory(plugins)

##
# INSTALLATION
##

get_target_property(usdx_exe ultrastardx PASCAL_LOCATION)
install(PROGRAMS ${usdx_exe} DESTINATION bin)

set(USDX_INSTALL_DATA_DIR share/ultrastardx)

install(
  DIRECTORY 
    artwork
    game/covers
    game/fonts
    game/languages
    game/resources
    game/sounds
    game/themes
  DESTINATION ${USDX_INSTALL_DATA_DIR}
  PATTERN .svn EXCLUDE
)
install(
  DIRECTORY ${USDX_PLUGIN_DIR}
  DESTINATION ${USDX_INSTALL_DATA_DIR}
  USE_SOURCE_PERMISSIONS
  PATTERN .svn EXCLUDE
)
install(
  FILES
    COPYING.txt
  DESTINATION ${USDX_INSTALL_DATA_DIR}
)
# install fink libs
if(APPLE)
  if(EXISTS ${usdx_exe})
    include(AnalyzeDylibDeps)
    analyze_dylib_deps(${usdx_exe} libdeps)
    set(USDX_DYLIB_DIR ${CMAKE_CURRENT_BINARY_DIR}/dylib)

    # install() does not follow symlinks so we have to copy the libs at cmake time
    file(MAKE_DIRECTORY ${USDX_DYLIB_DIR})
    foreach(dylib ${libdeps})
      get_filename_component(dylib_name ${dylib} NAME)
      file_copy_if_different(${dylib} ${USDX_DYLIB_DIR}/${dylib_name})
    endforeach()

    install(DIRECTORY ${USDX_DYLIB_DIR}/ DESTINATION dylib USE_SOURCE_PERMISSIONS)
  else()
    message("
!! To create a Mac OS X bundle, first compile with 
!!   \"make\" then 
!!   reconfigure with cmake (e.g. \"cmake ..\") and finally call 
!!   \"cpack -G Bundle\"
")
  endif()
endif(APPLE)

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

#if(DEFINED DATA_INSTALL_DIR)
#	set(SHAREPATH ${DATA_INSTALL_DIR}/ultrastardx/)
#else(DEFINED DATA_INSTALL_DIR)
#	set(SHAREPATH share/ultrastardx/)
#endif(DEFINED DATA_INSTALL_DIR)
#set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

# CPack vars

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Karaoke program that evaluates your performance")
set(CPACK_PACKAGE_VENDOR "UltraStar Deluxe Team")
set(CPACK_PACKAGE_FILE_NAME "ultrastardx-${CPACK_PACKAGE_VERSION}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "ultrastardx-src-${CPACK_PACKAGE_VERSION}")
set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_PACKAGE_EXECUTABLES "ultrastardx" "ultrastardx")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.txt")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "ultrastardx ${CPACK_PACKAGE_VERSION}")

if(APPLE)
  set(CPACK_BUNDLE_NAME UltraStarDeluxe)
  set(CPACK_BUNDLE_ICON ${CMAKE_CURRENT_SOURCE_DIR}/icons/ultrastardx.icns)
  set(CPACK_BUNDLE_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/src/macosx/Info.plist)
  set(CPACK_BUNDLE_STARTUP_COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/dists/bundle-startup.sh)
  #CPACK_PACKAGE_ICON
endif(APPLE)

if(WIN32 AND NOT UNIX)
  set(CPACK_NSIS_DISPLAY_NAME "Ultrastar Deluxe") 
  set(CPACK_NSIS_HELP_LINK ${PACKAGE_WEBSITE})
  set(CPACK_NSIS_URL_INFO_ABOUT ${PACKAGE_WEBSITE})
  set(CPACK_NSIS_CONTACT "help@ultrastardx.org")
  set(CPACK_NSIS_MODIFY_PATH OFF)
  set(CPACK_GENERATOR "ZIP;NSIS")
else(WIN32 AND NOT UNIX)
  set(CPACK_STRIP_FILES "bin/ultrastardx")
endif(WIN32 AND NOT UNIX)

set(CPACK_SOURCE_IGNORE_FILES
  "~"
  "\\\\.svn"
  "\\\\.exe$"
  "\\\\.a$"
  "\\\\.dll$"
  "\\\\.dof$"
  "\\\\.cmake$"
  "\\\\.zip$"
  "\\\\.gz$"
  "\\\\.bz2$"
  "\\\\.tmp$"
  "\\\\.sh$"
  "\\\\.dcu$"
  "\\\\.ppu$"
  "\\\\.o$"
  "Makefile"
  "CMakeFiles"
  "debug"
  "release$"
  "Debug$"
  "Release$"
  "CPack"
  "^${PROJECT_SOURCE_DIR}/bin/[a-z]"
  "^${PROJECT_SOURCE_DIR}/install_manifest.txt"
  "^${PROJECT_SOURCE_DIR}/CMakeCache.txt"
  "^${PROJECT_SOURCE_DIR}/ultrastardx\\\\."
)

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
!! IMPORTANT:
!! This is an UNSUPPORTED ALPHA release for developers only.
!!
!! DO NOT EXPECT THE MAKEFILE OR THE PROGRAM ITSELF TO WORK
!!
!! If you want to contribute, visit the IRC-Channel instead:
!!   ${PACKAGE_IRC}
!!
!! PLEASE DO NOT SEND BUGREPORTS FOR THIS VERSION.
!!")

# TODO: insert this in the public beta release
#!!! In case you find a bug send a bugreport to:
#!!!   ${PACKAGE_BUGREPORT}
#!!! You might as well ask for help at the IRC-Channel
#!!!   ${PACKAGE_IRC}
