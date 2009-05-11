# $URL$
# $Id$
pascal_add_module(hold_the_line Hold_The_Line.dpr
  OUTPUT_DIR ${USDX_PLUGIN_DIR}
  PACKAGES sdl opengl
  EXCLUDE_FROM_ALL
)
add_dependencies(plugins hold_the_line)
