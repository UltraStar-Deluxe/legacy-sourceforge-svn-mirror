# $URL$
# $Id$
include_directories(${libprojectM_INCLUDEDIR}/libprojectM)
add_definitions(-DPROJECTM_VERSION_INT=${libprojectM_VERSION_INT})

set(projectm_cwrapper_srcs projectM-cwrapper.cpp)
if(WIN32)
  add_library(projectM-cwrapper EXCLUDE_FROM_ALL SHARED ${projectm_cwrapper_srcs})
else(WIN32)
  add_library(projectM-cwrapper EXCLUDE_FROM_ALL STATIC ${projectm_cwrapper_srcs})
endif(WIN32)
