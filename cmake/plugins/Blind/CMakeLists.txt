# $URL$
# $Id$
pascal_add_module(blind Blind.dpr OUTPUT_DIR ${USDX_PLUGIN_DIR} EXCLUDE_FROM_ALL)
add_dependencies(plugins blind)
