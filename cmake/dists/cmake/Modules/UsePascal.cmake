# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

# - Pascal module build macros
#
# ADD_PASCAL_MODULE(
#   <name>
#   [PROGRAM | LIBRARY | UNIT]
#   <source>
#   FLAGS ...
#   DEPENDS depend [...]
#   UNITS    file1 [...]
#   INCLUDES file1 [...]
#   OBJECT_DIRS dir1 [...]
#   PACKAGES pkg1 [...]
#   EXCLUDE_FROM_ALL
#   REBUILD_ALL
# )

include(MacroParseArguments)
include(LazarusGenerator)

function(PASCAL_ADD_PACKAGE name)
  set(ARG)
  parse_arguments(ARG 
    "UNITS;INCLUDES;OBJECT_DIRS"
    ""
    ${ARGN}
  )

  set(unit_abs_list)
  foreach(unit ${ARG_UNITS})
    get_filename_component(unit_abs ${unit} ABSOLUTE)
    list(APPEND unit_abs_list ${unit_abs})
  endforeach(unit)
  set(inc_abs_list)
  foreach(inc ${ARG_INCLUDES})
    get_filename_component(inc_abs ${inc} ABSOLUTE)
    list(APPEND inc_abs_list ${inc_abs})
  endforeach(inc)

  set_property(GLOBAL PROPERTY PASCAL_PKG_${name}_EXISTS TRUE)
  set_property(GLOBAL PROPERTY PASCAL_PKG_${name}_UNITS    "${unit_abs_list}")
  set_property(GLOBAL PROPERTY PASCAL_PKG_${name}_INCLUDES "${inc_abs_list}")
  set_property(GLOBAL PROPERTY PASCAL_PKG_${name}_OBJECT_DIRS "${ARG_OBJECT_DIRS}")
endfunction(PASCAL_ADD_PACKAGE)

macro(PASCAL_ADD_MODULE)
  set(ARG)
  parse_arguments(ARG 
    "FLAGS;DEPENDS;UNITS;INCLUDES;OBJECT_DIRS;PACKAGES"
    "EXCLUDE_FROM_ALL;REBUILD_ALL"
    ${ARGN}
  )

  # check parameter count
  list(LENGTH ARG_DEFAULT_ARGS defargs_cnt)
  if(defargs_cnt LESS 2)
    message(FATAL_ERROR "ADD_PASCAL_MODULE called with incorrect number of arguments")
  endif(defargs_cnt LESS 2)

  set(arg_index 0)

  # get target parameter
  list(GET ARG_DEFAULT_ARGS ${arg_index} target)
  math(EXPR arg_index "${arg_index}+1")

  # determine file type
  list(GET ARG_DEFAULT_ARGS ${arg_index} type)
  set(type_list "PROGRAM;LIBRARY;UNIT")
  list(FIND type_list ${type} type_index)
  if(type_index EQUAL -1)
    set(type AUTO)
  else(type_index EQUAL -1)
    math(EXPR arg_index "${arg_index}+1")
  endif(type_index EQUAL -1)

  # get source parameter
  if(arg_index EQUAL 2)
    if(defargs_cnt LESS 3)
      message(FATAL_ERROR "ADD_PASCAL_MODULE called with incorrect number of arguments")
    endif(defargs_cnt LESS 3)
  endif(arg_index EQUAL 2)
  list(GET ARG_DEFAULT_ARGS ${arg_index} source)

  # autodetect type
  if(type STREQUAL AUTO)
    file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/${source} source_lines)
    string(TOUPPER "${source_lines}" source_lines)
    foreach(line ${source_lines})
      string(REGEX MATCH "^[ \t]*PROGRAM[ \t]+[A-Za-z0-9_]+[ \t]*;" line_match "${line}")
      if(line_match) 
        set(type PROGRAM)
        break()
      endif(line_match) 

      string(REGEX MATCH "^[ \t]*LIBRARY[ \t]+[A-Za-z0-9_]+[ \t]*;" line_match "${line}")
      if(line_match) 
        set(type LIBRARY)
        break()
      endif(line_match) 

      string(REGEX MATCH "^[ \t]*UNIT[ \t]+[A-Za-z0-9_]+[ \t]*;" line_match "${line}")
      if(line_match) 
        set(type UNIT)
        break()
      endif(line_match) 
    endforeach(line)

    if(type STREQUAL AUTO)
      message(FATAL_ERROR "Autodetection of Pascal source-type failed")
    endif(type STREQUAL AUTO)
  endif(type STREQUAL AUTO)

  # init list of additional "make clean" files
  set(extra_clean_files "")

  # set ppu and obj output dir
  set(unit_out_dir ${CMAKE_CURRENT_BINARY_DIR}/${FPC_TARGET})
  list(APPEND extra_clean_files ${unit_out_dir})

  # create output name
  set(output_name ${target})
  if(type STREQUAL PROGRAM)
    set(output "${CMAKE_CURRENT_BINARY_DIR}/${output_name}${CMAKE_EXECUTABLE_SUFFIX}")
    list(APPEND extra_clean_files ${output})
  endif(type STREQUAL PROGRAM)
  if(type STREQUAL LIBRARY)
    set(output "${CMAKE_CURRENT_BINARY_DIR}/${PPC_LIBRARY_PREFIX}${output_name}${PPC_LIBRARY_SUFFIX}")
    list(APPEND extra_clean_files ${output})
  endif(type STREQUAL LIBRARY)
  if(type STREQUAL UNIT)
    get_filename_component(source_name ${source} NAME_WE)
    set(output "${unit_out_dir}/${source_name}.o")
    # no need to add output to clean files
  endif(type STREQUAL UNIT)

  # set additional "make clean" files (Note: set_directory_properties() has no APPEND option)
  set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${extra_clean_files}")

  # set working dir
  set(work_dir ${CMAKE_CURRENT_SOURCE_DIR})

  # init unit/include/object dir list
  set(unit_dirs "")
  set(inc_dirs "")
  set(obj_dirs "")

  # add packages
  foreach(pkg ${ARG_PACKAGES})
    set(pkg_exists)
    get_property(pkg_exists GLOBAL PROPERTY PASCAL_PKG_${pkg}_EXISTS)
    if(NOT pkg_exists)
      message(FATAL_ERROR "Unknown pascal package \"${pkg}\". See pascal_add_package().")
    endif(NOT pkg_exists)

    set(pkg_units)
    get_property(pkg_units GLOBAL PROPERTY PASCAL_PKG_${pkg}_UNITS)
    list(APPEND units ${pkg_units})

    set(pkg_incs)
    get_property(pkg_incs GLOBAL PROPERTY PASCAL_PKG_${pkg}_INCLUDES)
    list(APPEND incs ${pkg_incs})

    set(pkg_obj_dirs)
    get_property(pkg_obj_dirs GLOBAL PROPERTY PASCAL_PKG_${pkg}_OBJECT_DIRS)
    list(APPEND obj_dirs ${pkg_obj_dirs})
  endforeach(pkg ${ARG_PACKAGES})

  # add unit/include/object dirs passed as macro parameter
  list(APPEND units ${ARG_UNITS})
  # extract unit directories
  foreach(unit ${units})
    get_filename_component(unit_dir ${unit} PATH)    
    list(APPEND unit_dirs ${unit_dir})
  endforeach(unit)
  list(REMOVE_DUPLICATES unit_dirs)

  list(APPEND incs ${ARG_INCLUDES})
  # extract inc directories
  foreach(inc ${incs})
    get_filename_component(inc_dir ${inc} PATH)    
    list(APPEND inc_dirs ${inc_dir})
  endforeach(inc)
  list(REMOVE_DUPLICATES inc_dirs)

  list(APPEND obj_dirs ${ARG_OBJECT_DIRS})
  list(REMOVE_DUPLICATES obj_dirs)

  # process unit/include/object directories
  set(dir_flag_list)
  foreach(flag_dir ${unit_dirs})
    get_filename_component(flag_abs_dir ${flag_dir} ABSOLUTE)
    file(RELATIVE_PATH flag_rel_dir ${work_dir} ${flag_abs_dir})
    list(APPEND dir_flag_list -Fu${flag_rel_dir})
  endforeach(flag_dir)
  foreach(flag_dir ${inc_dirs})
    get_filename_component(flag_abs_dir ${flag_dir} ABSOLUTE)
    file(RELATIVE_PATH flag_rel_dir ${work_dir} ${flag_abs_dir})
    list(APPEND dir_flag_list -Fi${flag_rel_dir})
  endforeach(flag_dir)
  foreach(flag_dir ${obj_dirs})
    get_filename_component(flag_abs_dir ${flag_dir} ABSOLUTE)
    file(RELATIVE_PATH flag_rel_dir ${work_dir} ${flag_abs_dir})
    list(APPEND dir_flag_list -Fo${flag_rel_dir})
  endforeach(flag_dir)

  # add *.pas/*.pp of each unit-dir and *.inc of each inc-dir
  # to the depends list
  set(depends)
  list(APPEND depends ${units})
  list(APPEND depends ${incs})

  # check if we are to add the target to ALL
  set(custom_target_all "ALL")
  if(ARG_EXCLUDE_FROM_ALL)
    set(custom_target_all)
  endif(ARG_EXCLUDE_FROM_ALL)

  # check if even unchanged units should be rebuild
  if(ARG_REBUILD_ALL)
    list(APPEND PFLAGS -B)
  endif(ARG_REBUILD_ALL)

  # set flags
  list(APPEND PFLAGS ${ARG_FLAGS})

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

  # lower-case project type (just for user info)
  string(TOLOWER ${type} proj_type_lowcase)

  # timestamp filename
  set(target_timestamp ${CMAKE_CURRENT_BINARY_DIR}/${target}_timestamp)

  # output filename
  get_filename_component(output_file ${output} NAME)

  # add command
  add_custom_command(
    OUTPUT ${target_timestamp}
    # delete and create units dir (needed for proper update)
#    COMMAND ${CMAKE_COMMAND} -E remove_directory ${unit_out_dir}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${unit_out_dir}
    # build
    COMMAND ${PPC} 
#            -l- -vi-
            ${PFLAGS} 
            ${dir_flag_list}
            -FU${unit_out_dir}
            -o${output}
            ${source}
    # update timestamp
    COMMAND ${CMAKE_COMMAND} -E touch ${target_timestamp}
    WORKING_DIRECTORY ${work_dir}
    MAIN_DEPENDENCY ${source}
    COMMENT "Building Pascal ${proj_type_lowcase} ${output_file}"
    DEPENDS ${ARG_DEPENDS} ${depends}
  )

  # add target
  add_custom_target(${target} ${custom_target_all} 
    COMMENT "Checking dependencies of target ${target}"
    DEPENDS ${target_timestamp}
  )

  # we cannot set the TYPE target property so specify our own property
  set_target_properties(${target} PROPERTIES PASCAL_TYPE ${type})

  # similar to the target property LOCATION we specify the output file
  # of the target
  set_target_properties(${target} PROPERTIES PASCAL_LOCATION ${output})

  generate_lazarus_project(
    ${source}
    ${output}
    ${unit_out_dir}
    "${ARG_UNITS}"
    "${unit_dirs}"
    "${ARG_INCLUDES}"
    "${inc_dirs}"        
    "${obj_dirs}"
    "${PFLAGS}"
  )
endmacro(PASCAL_ADD_MODULE)
