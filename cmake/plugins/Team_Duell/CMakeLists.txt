# $URL$
# $Id$
pascal_add_module(team_duell TeamDuell.dpr
  PACKAGES sdl opengl
)
add_dependencies(plugins team_duell)
