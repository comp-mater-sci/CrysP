# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(FH5 Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(FH5_NAME "FH5")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 

list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)

list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)


set(LIBSRC 
FH5Constants.f90  
FH5.f90  
FH5Simple.f90
)

add_library(${FH5_NAME} STATIC ${LIBSRC})

#
# Installation of the outputs in the build tree
#

set(FH5_INCLUDE_DIR "include") # Just one directory needed in the package
set(FH5_LIBRARY_DIR "lib")

# Note: Static libraries are always treated as ARCHIVE targets.
install(TARGETS ${FH5_NAME} 
  EXPORT "FH5Targets"
  LIBRARY DESTINATION  ${FH5_LIBRARY_DIR}
  ARCHIVE DESTINATION  ${FH5_LIBRARY_DIR}
  INCLUDES DESTINATION ${FH5_INCLUDE_DIRS}
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")
install(EXPORT "FH5Targets" DESTINATION "cmake")

# Generate package
include(CMakePackageConfigHelpers)
configure_package_config_file("../FH5Config.cmake.in" "FH5Config.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
	PATH_VARS FH5_INCLUDE_DIR FH5_LIBRARY_DIR
	)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/FH5Config.cmake" DESTINATION ".")

# Export the package
export(PACKAGE FH5)


