# $URL$
# $Id$
pascal_add_module(team_duell TeamDuell.dpr
  OUTPUT_DIR ${USDX_PLUGIN_DIR}
  PACKAGES sdl opengl
  EXCLUDE_FROM_ALL
)
add_dependencies(plugins team_duell)
