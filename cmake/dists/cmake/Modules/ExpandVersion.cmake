# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

# - Macro for version management 
#
# EXPAND_VERSION(VAR)
#   Adds major, minor and patch version variables with prefix ${VAR}.
#   VAR is normally set to the version variable set by pkg-config
#   for example MyModule_VERSION and must be in the format
#     major[.minor[.patch]].
#   Non-integer version components are ignored. For example
#   0d.52.0.1, 52.rel.0.1 and 52.0.1 result in the same version
#   components.
#   Defines
#     ${VAR}_MAJOR: major version component (A)
#     ${VAR}_MINOR: minor version component (I)
#     ${VAR}_PATCH: patch version component (P)
#     ${VAR}_INT:   version as integer presentation (AAAIIIPPP) 

macro(EXPAND_VERSION VAR)
  # replace "." and "-" with the list delimiter
  string(REGEX REPLACE "[.-]" ";" comps "${${VAR}}")

  # extract valid version components
  set(valid_comps)
  foreach(comp ${comps})
    if(comp MATCHES "^[0-9]+$")    
      list(APPEND valid_comps ${comp})  
    endif(comp MATCHES "^[0-9]+$")    
  endforeach(comp)  

  # append zeros in case the version-string has less than
  # three components
  list(APPEND valid_comps 0 0 0)

  # assign valid components to version variables  
  list(GET valid_comps 0 ${VAR}_MAJOR)
  list(GET valid_comps 1 ${VAR}_MINOR)
  list(GET valid_comps 2 ${VAR}_PATCH)

  math(EXPR ${VAR}_INT "${${VAR}_MAJOR}*1000000 + ${${VAR}_MINOR}*1000 + ${${VAR}_PATCH}")
endmacro(EXPAND_VERSION)
