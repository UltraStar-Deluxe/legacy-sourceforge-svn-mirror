# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

# - Find Pascal compiler
#
# This module defines
#  PPC_FOUND:              If false, do not try to use PPC.
#  PASCAL_COMPILER_TYPE:   Either FPC or DELPHI (latter not yet supported)
#  PASCAL_COMPILER_WORKS:  True if the compiler works
#  PASCAL_FLAGS_...:       Some default flags for several build types
#    PASCAL_FLAGS
#    PASCAL_LIBRARY_FLAGS
#    PASCAL_FLAGS_DEBUG
#    PASCAL_FLAGS_RELEASE
#    PASCAL_FLAGS_RELWITHDEBINFO
#    PASCAL_FLAGS_MINSIZEREL
#  PASCAL_LIBRARY_BUILD_WORKS: Compiler/linker combination is able to build libraries.
#  PASCAL_LIBRARY_SUFFIX:  Library suffix (e.g. .dll or .so)
#  PASCAL_LIBRARY_PREFIX:  Library prefix (e.g. lib)
#  FPC_PLATFORM:           Target platform (OS): linux, freebsd, openbsd, netbsd, darwin, win32, ...
#  FPC_PROCESSOR:          Target processor (CPU): i386, x86_64, powerpc, arm
#  FPC_CPLATFORM:          Compiler platform (OS)
#  FPC_CPROCESSOR:         Compiler processor (CPU)
#  FPC_TARGET:             String "FPC_PROCESSOR-FPC_PLATFORM"

##
# Get FPC compiler info
##

set(current_pkg PPC)
set(ppc_check_dir ${CMAKE_BINARY_DIR}/ppc_check)

# - creates file with code in ppc_check_dir and compiles it with
#   the given flags. The result will be stored in result_var.
#   If NO_CLEAN is given, the ppc_check_dir will not be removed
#   after the check.
#
# ppc_check(file code flags result_var NO_CLEAN)
#
function(ppc_check file code flags result_var)
  # create compiler check directory
  file(MAKE_DIRECTORY ${ppc_check_dir}) 

  file(WRITE ${ppc_check_dir}/${file} "${code}")

  execute_process(
    COMMAND ${PASCAL_COMPILER} ${flags} ${file}
    WORKING_DIRECTORY ${ppc_check_dir}
    RESULT_VARIABLE check_result
    ERROR_VARIABLE check_output
    OUTPUT_VARIABLE check_output
  )

  # remove compiler check directory
  list(FIND ARGN "NO_CLEAN" no_clean)
  if(no_clean EQUAL -1)
    file(REMOVE_RECURSE ${ppc_check_dir}) 
  endif()

  list(FIND ARGN "SHOW_ERROR" show_error)
  if(show_error GREATER -1)
    if(check_result AND NOT ${current_pkg}_FIND_QUIETLY)
      message("${check_output}")
    endif()
  endif()

  set(${result_var} ${check_result} PARENT_SCOPE)
endfunction(ppc_check)

# use a foreach statement so we can leave this section with break()
# if an error occurs.
foreach(once 1)
  # clear error message
  set(ppc_error)

  # skip tests if compiler was previously found
  if(PASCAL_COMPILER_WORKS)
    break()
  endif(PASCAL_COMPILER_WORKS)

  # Find FreePascal executable
  set(ppc_bin_path
    "$ENV{SystemDrive}/fpc/*/bin/*"
    "$ENV{ProgramFiles}/fpc/*/bin/*"
    "$ENV{SystemDrive}/lazarus/fpc/*/bin/*"
    "$ENV{ProgramFiles}/lazarus/fpc/*/bin/*"
  )
  find_program(PASCAL_COMPILER NAMES fpc ppc386 ppc PATHS ${ppc_bin_path})
  if(NOT PASCAL_COMPILER)
    set(ppc_error "Executable not found")
    break()
  endif(NOT PASCAL_COMPILER)

  if(NOT ${current_pkg}_FIND_QUIETLY)
    message(STATUS "Check for working Pascal compiler: ${PASCAL_COMPILER}")
  endif(NOT ${current_pkg}_FIND_QUIETLY)

  # retrieve FPC version
  execute_process(COMMAND ${PASCAL_COMPILER} -iV
                  OUTPUT_VARIABLE FPC_VERSION 
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(FPC_VERSION ${FPC_VERSION} CACHE INTERNAL "")

  # check version
  if(${current_pkg}_FIND_VERSION_EXACT)
    if(NOT FPC_VERSION VERSION_EQUAL ${current_pkg}_FIND_VERSION)
      set(ppc_error "Required =${${current_pkg}_FIND_VERSION} but found ${FPC_VERSION}")
      break()
    endif(NOT FPC_VERSION VERSION_EQUAL ${current_pkg}_FIND_VERSION)
  endif(${current_pkg}_FIND_VERSION_EXACT)
  if(${current_pkg}_FIND_VERSION)
    if(FPC_VERSION VERSION_LESS ${current_pkg}_FIND_VERSION)
      set(ppc_error "Required >=${${current_pkg}_FIND_VERSION} but found ${FPC_VERSION}")
      break()
    endif(FPC_VERSION VERSION_LESS ${current_pkg}_FIND_VERSION)
  endif(${current_pkg}_FIND_VERSION)

  # retrieve FPC platform info
  execute_process(COMMAND ${PASCAL_COMPILER} -iTO
                  OUTPUT_VARIABLE FPC_PLATFORM
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(FPC_PLATFORM ${FPC_PLATFORM} CACHE INTERNAL "")
  execute_process(COMMAND ${PASCAL_COMPILER} -iTP
                  OUTPUT_VARIABLE FPC_PROCESSOR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(FPC_PROCESSOR ${FPC_PROCESSOR} CACHE INTERNAL "")
  execute_process(COMMAND ${PASCAL_COMPILER} -iSO
                  OUTPUT_VARIABLE FPC_CPLATFORM
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(FPC_CPLATFORM ${FPC_CPLATFORM} CACHE INTERNAL "")
  execute_process(COMMAND ${PASCAL_COMPILER} -iSP
                  OUTPUT_VARIABLE FPC_CPROCESSOR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(FPC_CPROCESSOR ${FPC_CPROCESSOR} CACHE INTERNAL "")
  set(FPC_TARGET "${FPC_PROCESSOR}-${FPC_PLATFORM}" CACHE INTERNAL "")

  ##
  # Compiler checks
  ##

  set(check_flags "-l -v0ie")

  # check whether FPC works and can compile a program
  ppc_check(simple.pas "program simple;\nbegin\nend."
            "${check_flags}" check_result SHOW_ERROR)
  if(check_result)
    set(ppc_error "Cannot compile simple test-program")
    break()
  endif(check_result)

  # check if FPC can link with standard libraries
  ppc_check(link.pas "program link;\nuses SysUtils;\nbegin\nWriteLn('Test');\nend."
            "${check_flags}" check_result SHOW_ERROR)
  if(check_result)
    set(ppc_error "Cannot link with standard libraries")
    break()
  endif(check_result)

  # check whether FPC's linker knows (or at least doesn't crash with) "-z noexecstack".
  # FPC does not set the NX-flag on stack memory. Binaries generated with FPC
  # might crash on platforms that require the stack to be non-executable.
  # So we will try to find a workaround here.
  # See http://bugs.freepascal.org/view.php?id=11563
  ppc_check(noexecstack.pas "program noexecstack;\nbegin\nend."
            "${check_flags};-k-z noexecstack" check_result)
  if(NOT check_result)
    set(noexecstack_flag "-k\"-z noexecstack\"")
  endif(NOT check_result)

  # check if compiler is able to build libraries
  set(lib_build_works TRUE)
  ppc_check(testlib.pas "library link;\nbegin\nend."
            "${check_flags}" check_result
            NO_CLEAN)
  if(check_result)
    # The linker (ld) cannot link the library. Ubuntu with ld 2.19.1 
    # is known to fail here because of a bug so do not break here.
    if(NOT ${current_pkg}_FIND_QUIETLY)
      message(STATUS "Pascal compiler cannot build shared libraries!")
    endif(NOT ${current_pkg}_FIND_QUIETLY)
    set(lib_build_works FALSE)
  else(check_result)
    # find generated library
    find_library(PASCAL_TESTLIB_PATH testlib
                 PATHS ${ppc_check_dir}
                 NO_DEFAULT_PATH)
    # do not show library name in GUI
    mark_as_advanced(PASCAL_TESTLIB_PATH)
    # extract prefix and suffix from library name
    if(PASCAL_TESTLIB_PATH)
      get_filename_component(lib_suffix ${PASCAL_TESTLIB_PATH} EXT)
      get_filename_component(testlib_filename ${PASCAL_TESTLIB_PATH} NAME_WE)
      string(REGEX REPLACE "^(.*)testlib.*" "\\1" lib_prefix ${testlib_filename})
      mark_as_advanced(PASCAL_LIBRARY_PREFIX PASCAL_LIBRARY_SUFFIX)
    endif(PASCAL_TESTLIB_PATH)

    # set cache entries
    set(PASCAL_LIBRARY_SUFFIX ${lib_suffix} CACHE STRING "Pascal library suffix.")
    set(PASCAL_LIBRARY_PREFIX ${lib_prefix} CACHE STRING "Pascal library prefix.")
  endif(check_result)
  # remove test directory
  file(REMOVE_RECURSE ${ppc_check_dir}) 

  # cache results
  set(PASCAL_COMPILER_WORKS TRUE CACHE INTERNAL "")
  set(PASCAL_LIBRARY_BUILD_WORKS ${lib_build_works} CACHE INTERNAL "")
  set(PASCAL_COMPILER_TYPE FPC CACHE INTERNAL "")

  if(NOT ${current_pkg}_FIND_QUIETLY)
    message(STATUS "Check for working Pascal compiler: ${PASCAL_COMPILER} -- works")
  endif(NOT ${current_pkg}_FIND_QUIETLY)
endforeach(once)

##
# Set Flags
##

# default flags.
# Notes:
# - The flags defined in fpc.cfg are evaluated first in a first pass.
#   This also applies those flags that are enabled by defining -dDEBUG or -dRELEASE.
#   In a second pass all command line options are evaluated.
#   This way all flags defined in fpc.cfg can be undefined with -<flag>-
# - FPC strips binaries by default. Stripping binaries is considered bad style,
#   for example Gentoo reacts to stripped binaries with a quality warning.
#   In addition the cmake installer strips the binaries if required.
#   Disable stripping with -Xs-.
set(PASCAL_FLAGS
    "-Xs- -v0Binwe ${noexecstack_flag}"
    CACHE STRING "Flags used by the Pascal compiler during all build types.")
separate_arguments(PASCAL_FLAGS)

# default library flags. Some Linux distribution's security policies require -fPIC, 
# otherwise linking will fail.
if(UNIX)
  # FPC supports PIC for Darwin and Linux/x86_64 but Linux/i386 is broken with 2.2.2. 
  # See http://community.freepascal.org:10000/bboards/message?message_id=314214&forum_id=24092
  if(NOT FPC_PLATFORM STREQUAL linux OR NOT FPC_PROCESSOR STREQUAL i386)
    set(PIC_flag "-fPIC")
  endif()
endif(UNIX)
set(PASCAL_LIBRARY_FLAGS
    "${PIC_flag}"
    CACHE STRING "Flags used by the Pascal compiler during library builds.")
separate_arguments(PASCAL_LIBRARY_FLAGS)

# Some debug flags to keep in mind: -gv -gw(2/3) -gh -pg
set(debug_info_flags "-g -gl")

# default debug flags. Define DEBUG but remove some ill-suited flags set in fpc.cfg (-Crtoi).
set(PASCAL_FLAGS_DEBUG
    "-dDEBUG -Cr-t-o-i- ${debug_info_flags}"
    CACHE STRING "Flags used by the Pascal compiler during debug builds.")
separate_arguments(PASCAL_FLAGS_DEBUG)

set(release_base_flags "-dRELEASE -vn-")

# default release flags. Define RELEASE but remove some ill-suited flags set in fpc.cfg (-Xs)
# Enable optimization, reduce verbosity by disabling notes.
set(PASCAL_FLAGS_RELEASE
    "${release_base_flags} -O2"
    CACHE STRING "Flags used by the Pascal compiler during release builds.")
separate_arguments(PASCAL_FLAGS_RELEASE)

set(PASCAL_FLAGS_RELWITHDEBINFO
    "${release_base_flags} -O2 ${debug_info_flags}"
    CACHE STRING "Flags used by the Pascal compiler during Release with Debug Info builds.")
separate_arguments(PASCAL_FLAGS_RELWITHDEBINFO)

set(PASCAL_FLAGS_MINSIZEREL
    "${release_base_flags} -Os"
    CACHE STRING "Flags used by the Pascal compiler during release minsize builds.")
separate_arguments(PASCAL_FLAGS_MINSIZEREL)

##
# set PPC_FOUND and show error messages
##

set(fail_message "No working Pascal compiler found: ${ppc_error}!")
if(PASCAL_COMPILER_WORKS)
  set(${current_pkg}_FOUND TRUE)
else(PASCAL_COMPILER_WORKS)
  if(${current_pkg}_FIND_REQUIRED)
    message(FATAL_ERROR "${fail_message}")
  else(${current_pkg}_FIND_REQUIRED)
    if(NOT ${current_pkg}_FIND_QUIETLY)
      message(STATUS "${fail_message}")
    endif(NOT ${current_pkg}_FIND_QUIETLY)
  endif(${current_pkg}_FIND_REQUIRED)
endif(PASCAL_COMPILER_WORKS)
