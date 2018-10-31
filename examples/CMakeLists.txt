# =============================================================================
# examples
# =============================================================================

# alias to the the example directory
SET(EXAMPLE_SRC ${PROJECT_SOURCE_DIR}/examples)

# add executable
ADD_EXECUTABLE(read_files_from_CMD ${EXAMPLE_SRC}/read_files_from_CMD.f90)

# include directory
TARGET_INCLUDE_DIRECTORIES(read_files_from_CMD 
    PRIVATE ${CMAKE_BINARY_DIR}/include)

# library linkage
TARGET_LINK_LIBRARIES(read_files_from_CMD yafbgdal)

# force to use rpath
#SET_TARGET_PROPERTIES(read_files_from_CMD
#    PROPERTIES 
#    INSTALL_RPATH_USE_LINK_PATH TRUE
#    INSTALL_RPATH ${CMALE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR})

# installation
INSTALL(TARGETS read_files_from_CMD
    RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/examples)

# vi: ft=cmake
