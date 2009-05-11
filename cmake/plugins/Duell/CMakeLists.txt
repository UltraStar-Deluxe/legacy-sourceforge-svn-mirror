# $URL$
# $Id$
pascal_add_module(duell Duell.dpr OUTPUT_DIR ${USDX_PLUGIN_DIR} EXCLUDE_FROM_ALL)
add_dependencies(plugins duell)
