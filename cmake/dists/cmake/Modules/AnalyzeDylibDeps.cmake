# otool: Mac OS X object file displaying tool
find_program(OTOOL otool)

function(extract_dylib_deps dylib deplist_name)
  execute_process(
    COMMAND ${OTOOL} -L ${dylib}
    OUTPUT_VARIABLE otool_out
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  string(REPLACE "\n" ";" otool_out ${otool_out})
  set(deplist "")
  foreach(line ${otool_out})
    if(line MATCHES version)
      # extract dylib-name
      string(REGEX REPLACE "^[\t ]*([^ ]+) .*" "\\1" libname "${line}")
      # filter out system libs
      if(NOT libname MATCHES ^/usr/lib AND
         NOT libname MATCHES ^/System/Library)
        list(APPEND deplist ${libname})
      endif()
    endif()
  endforeach()
  set(${deplist_name} "${deplist}" PARENT_SCOPE)
endfunction(extract_dylib_deps)

function(ANALYZE_DYLIB_DEPS module deplist_name)
  # init queue (contains libs to analyze)
  set(queue "")
  # add module as first "lib" to analyze
  list(APPEND queue ${module})

  # init dep_list
  set(deps "")

  # analyze each lib enqueued until queue is empty
  while(queue)
    # dequeue front element
    list(GET queue 0 current_lib)
    list(REMOVE_AT queue 0)

    # get dependencies of current dylib
    extract_dylib_deps(${current_lib} current_deps)
    # extract dependencies not already in the deps list
    foreach(lib ${current_deps})
      list(FIND deps ${lib} lib_in_deps)
      if(lib_in_deps EQUAL -1)
        list(APPEND queue ${lib})
        list(APPEND deps ${lib})
      endif()
    endforeach()
  endwhile()

  set(${deplist_name} "${deps}" PARENT_SCOPE)
endfunction(ANALYZE_DYLIB_DEPS)
