# $URL$
# $Id$
pascal_add_package(projectm
  UNITS *.pas
  INCLUDES *.inc
  OBJECT_DIRS ${CMAKE_CURRENT_BINARY_DIR}
)
add_subdirectory(cwrapper)
