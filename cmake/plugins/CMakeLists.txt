# $URL$
# $Id$

# create an option to exclude plugins target from ALL target
option(BUILD_PLUGINS "Build UltraStar Deluxe party-mode plugins." ON)
set(plugins_tgt_all)
if(BUILD_PLUGINS)
  set(plugins_tgt_all ALL)
endif(BUILD_PLUGINS)
# create a plugins target
add_custom_target(plugins ${plugins_tgt_all})

# add plugins to target
add_subdirectory(5000Points)
add_subdirectory(Blind)
add_subdirectory(Duell)
add_subdirectory(Dont_Get_Worse)
add_subdirectory(Team_Duell)
