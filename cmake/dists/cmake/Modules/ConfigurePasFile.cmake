# Copyright (c) 2009 Tobias Gunkel <tobigun@at@users.sourceforge.net>
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty provided
# the copyright notice and this notice are preserved.

# - Macro for handling Pascal configuration files
#
# CONFIGURE_PAS_FILE(InputFile OutputFile)
#   Works as configure_file(InputFile OutputFile) but in addition
#   it replaces occurences of {$CMAKEDEFINE(VAR) XYZ} with
#     {$DEFINE XYZ} if the value of VAR is true  
#     {$UNDEF XYZ} if the value of VAR is false  

macro(CONFIGURE_PAS_FILE)
  file(READ ${ARGV0} config_file)
  string(REGEX MATCHALL "{\\$CMAKEDEFINE\\([^\\)]+\\)" pasdefine_list "${config_file}")
  foreach(pasdefine ${pasdefine_list})
    string(REGEX REPLACE "{\\$CMAKEDEFINE\\(([^\\)]+)\\)" "\\1" pasdefine_var ${pasdefine})
    if (${pasdefine_var})
      string(REPLACE ${pasdefine} "{$DEFINE" config_file "${config_file}")
    else (${pasdefine_var})
      string(REPLACE ${pasdefine} "{$UNDEF" config_file "${config_file}")
    endif (${pasdefine_var})	
  endforeach(pasdefine)
  string(CONFIGURE "${config_file}" config_file)
  file(WRITE ${ARGV1} "${config_file}")
endmacro(CONFIGURE_PAS_FILE)
