# =============================================================================
# libyafbgdal
# =============================================================================

# find GDAL library
FIND_PACKAGE(GDAL REQUIRED)

# alias to src folder
SET(SRC ${PROJECT_SOURCE_DIR}/src)

# source files
SET(SOURCE
    ${SRC}/c_conv_type.f90
    ${SRC}/cpl_conv.f90
    ${SRC}/cpl_port.f90
    ${SRC}/cpl_string.f90
    ${SRC}/ogr_core.f90
    ${SRC}/ogr_srs_api.f90
    ${SRC}/ogr_api.f90
    ${SRC}/gdal.f90)

# library
ADD_LIBRARY(yafbgdal ${SOURCE})

# force to put *.mod and *.smod to ${CMAKE_BINARY_DIR}/include
# force to use rpath even after installation
SET_TARGET_PROPERTIES(yafbgdal
    PROPERTIES 
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

# include directory
TARGET_INCLUDE_DIRECTORIES(yafbgdal PRIVATE ${CMAKE_BINARY_DIR}/include)

# RPATH link: gdal
TARGET_LINK_LIBRARIES(yafbgdal PUBLIC ${GDAL_LIBRARY})

# when `make install`, copy include/*.mod to installation path
# note: not sure if we should install *.smod
INSTALL(DIRECTORY ${CMAKE_BINARY_DIR}/include 
    DESTINATION ${CMAKE_INSTALL_PREFIX}
    FILES_MATCHING PATTERN "*.mod")

# installation of the library
INSTALL(TARGETS yafbgdal
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
