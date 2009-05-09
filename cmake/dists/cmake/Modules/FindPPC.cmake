# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

# - Find Pascal compiler
#
# This module defines
#  PPC_FOUND, If false, do not try to use PPC.

##
# Find FreePascal executable
##

set(PPC_BIN_PATH
  "$ENV{SystemDrive}/fpc/*/bin/*" 
  "$ENV{ProgramFiles}/fpc/*/bin/*" 
  "$ENV{SystemDrive}/lazarus/fpc/*/bin/*" 
  "$ENV{ProgramFiles}/lazarus/fpc/*/bin/*"
)
find_program(PPC fpc PATHS ${PPC_BIN_PATH})

##
# Get FPC compiler info
##

set(PPC_CHECK_DIR ${CMAKE_BINARY_DIR}/ppc_check)

# - creates file with code in PPC_CHECK_DIR and compiles it with
#   the given flags. The result will be stored in result_var.
#   If NO_CLEAN is given, the PPC_CHECK_DIR will not be removed
#   after the check.
#
# PPC_CHECK(file code flags result_var NO_CLEAN)
#
function(PPC_CHECK file code flags result_var)
  # create compiler check directory
  file(MAKE_DIRECTORY ${PPC_CHECK_DIR}) 

  file(WRITE ${PPC_CHECK_DIR}/${file} "${code}")

  execute_process(
    COMMAND ${PPC} ${flags} ${file}
    WORKING_DIRECTORY ${PPC_CHECK_DIR}
    RESULT_VARIABLE check_result
    ERROR_QUIET
    OUTPUT_QUIET)

  # remove compiler check directory
  list(FIND ARGN "NO_CLEAN" no_clean)

  if(no_clean EQUAL -1)
    file(REMOVE_RECURSE ${PPC_CHECK_DIR}) 
  endif(no_clean EQUAL -1)

  set(${result_var} ${check_result} PARENT_SCOPE)
endfunction(PPC_CHECK)

# use a foreach statement so we can leave this section with break()
# if an error occurs.
foreach(once 1)
  set(PPC_WORKS)
  set(PPC_ERROR)

  if(NOT PPC)
    set(PPC_ERROR "Executable not found")
    break()
  endif(NOT PPC)

  # retrieve FPC version
  execute_process(COMMAND ${PPC} -iV  
                  OUTPUT_VARIABLE FPC_VERSION 
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  # check version
  if(PPC_FIND_VERSION_EXACT)
    if(NOT FPC_VERSION VERSION_EQUAL PPC_FIND_VERSION)
      set(PPC_ERROR "Required =${PPC_FIND_VERSION} but found ${FPC_VERSION}")
      break()
    endif(NOT FPC_VERSION VERSION_EQUAL PPC_FIND_VERSION)
  endif(PPC_FIND_VERSION_EXACT)
  if(PPC_FIND_VERSION)
    if(FPC_VERSION VERSION_LESS PPC_FIND_VERSION)
      set(PPC_ERROR "Required >=${PPC_FIND_VERSION} but found ${FPC_VERSION}")
      break()
    endif(FPC_VERSION VERSION_LESS PPC_FIND_VERSION)
  endif(PPC_FIND_VERSION)

  # retrieve FPC platform info
  execute_process(COMMAND ${PPC} -iTO 
                  OUTPUT_VARIABLE FPC_PLATFORM
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${PPC} -iTP 
                  OUTPUT_VARIABLE FPC_PROCESSOR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${PPC} -iSO 
                  OUTPUT_VARIABLE FPC_CPLATFORM
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${PPC} -iSP 
                  OUTPUT_VARIABLE FPC_CPROCESSOR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(FPC_TARGET "${FPC_PROCESSOR}-${FPC_PLATFORM}")


  #-v -l     verbose/banner
  #-n        ignore fpc.cfg
  #-Fi -I    include-dir
  #-k
  #-Fl       linker-dir
  #-Fo -Fu   object-/unit-dir
  #-Xt -Xc
  #-Ciort    range checks
  #-Cs -Ch   stack/heap size
  #-Cg -fPIC PIC code 
  #-E
  #-g -gv -gw(2/3) -gh -gl -pg
  #-O
  #-T -Xd -XP    cross-compiling
  #-d -u     define/undefine
  #-Xs       strip
  #-B        build all modules
  #-Dd -Dv   description + DLL-version

  #PFLAGS_BASE_DEFAULT    := -Si -Sg- -Sc- -v0Binwe
  #PFLAGS_DEBUG_DEFAULT   := -Xs- -g -gl -dDEBUG_MODE
  #PFLAGS_RELEASE_DEFAULT := -Xs- -O2

  ##
  # Compiler checks
  ##

  set(default_flags "")

  # check whether FPC works and can compile a program
  ppc_check(simple.pas "program simple;\nbegin\nend." 
            "${default_flags}" check_result)
  if(check_result)
    set(PPC_ERROR "Cannot compile simple test-program")
    break()
  endif(check_result)

  # check if FPC can link with standard libraries
  ppc_check(link.pas "program link;\nuses SysUtils;\nbegin\nWriteLn('Test');\nend."
            "${default_flags}" check_result)
  if(check_result)
    set(PPC_ERROR "Cannot link with standard libraries")
    break()
  endif(check_result)

  # check whether FPC's linker knows (or at least doesn't crash with) 
  #   "-z noexecstack"
  # FPC does not set the NX-flag on stack memory. Binaries generated with FPC
  # might crash on platforms that require the stack to be non-executable.
  # So we will try to find a workaround here.
  # See http://bugs.freepascal.org/view.php?id=11563
  set(noexecstack_flags -k"-z noexecstack")
  ppc_check(noexecstack.pas "program noexecstack;\nbegin\nend."
            "${default_flags} ${noexecstack_flags}" check_result)
  if(check_result)
    set(noexecstack_flags)
  endif(check_result)

  # check prefixes and suffixes
  ppc_check(testlib.pas "library link;\nbegin\nend."
            "${default_flags}" check_result
            NO_CLEAN)
  # find generated library
  find_library(PPC_TEST_LIBPATH testlib 
               PATHS ${PPC_CHECK_DIR}
               NO_DEFAULT_PATH)
  # do not show library name in GUI
  mark_as_advanced(PPC_TEST_LIBPATH)
  # extract prefix and suffix from library name
  if(PPC_TEST_LIBPATH)
    get_filename_component(PPC_LIBRARY_SUFFIX ${PPC_TEST_LIBPATH} EXT CACHE)
    get_filename_component(libfilename ${PPC_TEST_LIBPATH} NAME_WE)
    string(REGEX REPLACE "^(.*)testlib.*" "\\1" PPC_LIBRARY_PREFIX ${libfilename})
  endif(PPC_TEST_LIBPATH)
  # remove library test directory
  file(REMOVE_RECURSE ${PPC_CHECK_DIR}) 
  if(check_result)
    set(PPC_ERROR "Cannot link with standard libraries")
    break()
  endif(check_result)

  ##
  # Check results
  ##

  #set(PFLAGS ${noexecstack_flags} "-B" "-FE../bin" "-Cs2000000" "-vwi" "-O2" "-Fl/opt/local/lib" ${hwengine_project})

  set(PPC_WORKS TRUE)
endforeach(once)

# handle the QUIETLY and REQUIRED arguments and set PPC_FOUND to TRUE if
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(PPC "Could NOT find PPC: ${PPC_ERROR}!" PPC PPC_WORKS)

mark_as_advanced(PPC)
