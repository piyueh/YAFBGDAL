# =============================================================================
# \file CMakeLists.txt
# \brief 
# \author Pi-Yueh Chuang
# \version alpha
# \date 2018-10-30
# =============================================================================

CMAKE_MINIMUM_REQUIRED (VERSION 2.8)

# =============================================================================
# DEFAULT VARIABLE VALUES
# =============================================================================

# set default flags for release and debug modes
SET(CMAKE_CXX_FLAGS_DEBUG "-g" CACHE STRING "")
SET(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG" CACHE STRING "")
SET(CMAKE_SHARED_LINKER_FLAGS "-Wl,--no-undefined" CACHE STRING "")

# set default build mode
SET(CMAKE_BUILD_TYPE DEBUG CACHE STRING "")


# =============================================================================
# PROJECT SETTINGS
# =============================================================================

# start generator
PROJECT(YAFBGDAL Fortran)

# use GNU standard installation directory heirarcy
INCLUDE(GNUInstallDirs)

# other cmake setting
SET(CMAKE_VERBOSE_MAKEFILE ON)
SET(CMAKE_COLOR_MAKEFILE ON)
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")

# add options
OPTION(BUILD_SHARED_LIBS "Whether to build shared library." ON)

# Library
ADD_SUBDIRECTORY(src)

# Examples
ADD_SUBDIRECTORY(examples)

# enable `make test`
ENABLE_TESTING()

# vi: ft=cmake
