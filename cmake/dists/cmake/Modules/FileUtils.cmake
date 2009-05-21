# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

function(FILE_SYMLINK old new)
  if(NOT EXISTS ${old})
    message(FATAL_ERROR "File \"${old}\" does not exist!")    
  endif(NOT EXISTS ${old})
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink "${old}" "${new}")
  # ignore "link already exists" errors
  if(NOT EXISTS ${new})
    message(FATAL_ERROR "Symlink \"${old}\" to \"${new}\" failed!")
  endif(NOT EXISTS ${new})
endfunction(FILE_SYMLINK)

function(FILE_COPY_DIR src dst)
  if(NOT EXISTS ${src})
    message(FATAL_ERROR "Directory \"${src}\" does not exist!")    
  endif(NOT EXISTS ${src})
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${src}" "${dst}"
    RESULT_VARIABLE result
  )
  if(result)
    message(FATAL_ERROR "Copy \"${src}\" to \"${dst}\" failed!")
  endif(result)
endfunction(FILE_COPY_DIR)

function(FILE_COPY src dst)
  if(NOT EXISTS ${src})
    message(FATAL_ERROR "File \"${src}\" does not exist!")    
  endif(NOT EXISTS ${src})
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy "${src}" "${dst}"
    RESULT_VARIABLE result
  )
  if(result)
    message(FATAL_ERROR "Copy \"${src}\" to \"${dst}\" failed!")
  endif(result)
endfunction(FILE_COPY)

function(FILE_COPY_IF_DIFFERENT src dst)
  if(NOT EXISTS ${src})
    message(FATAL_ERROR "File \"${src}\" does not exist!")    
  endif(NOT EXISTS ${src})
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${src}" "${dst}"
    RESULT_VARIABLE result
  )
  if(result)
    message(FATAL_ERROR "Copy \"${src}\" to \"${dst}\" failed!")
  endif(result)
endfunction(FILE_COPY_IF_DIFFERENT)
