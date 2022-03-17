# $Id$
#
cmake_minimum_required(VERSION 3.5)

project(FCRI Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

# Library: static, single-threaded
set(CRI "CRI")
# Library: static, multi-threaded
set(CRI_MT "CRI_mt")
# Library: dynamic, single-threaded
set(CRI_SHARED "CRI_shared")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 

list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)

list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

#
# Library files
#

set(LIBSRC 
criAlgorithm.f90
criArray.f90
criConfigReader.f90
criErrcodes.f90
criIterUtils.f90
criLibCRIVersion.f90
criLinearMap.f90
criLog.f90
criMathUtils.f90
criMkTemp.f90
criNamedRange.f90
criNumerics.f90
criPath.f90
criRange.f90
criRuntime.f90
criTest.f90
criTimers.f90
criUncomment.f90
criVersion.f90
)

add_library(${CRI} STATIC ${LIBSRC})
add_library(${CRI_MT} STATIC ${LIBSRC})

add_library(${CRI_SHARED} SHARED ${LIBSRC})
set_target_properties(${CRI_SHARED} PROPERTIES OUTPUT_NAME ${CRI})

target_compile_options(${CRI_MT} PUBLIC "-parallel" "-qopenmp") #HGH: originally -openmp

#
# Installation of the outputs in the build tree
#

set(FCRI_INCLUDE_DIRS "include" "include/fcri")
set(FCRI_INCLUDE_DIR "include") # Just one directory needed in the package
set(FCRI_LIBRARY_DIR "lib")

# Note: Static libraries are always treated as ARCHIVE targets.
install(TARGETS ${CRI} ${CRI_MT} ${CRI_SHARED}
  EXPORT "FCRITargets"
  LIBRARY DESTINATION  ${FCRI_LIBRARY_DIR}
  ARCHIVE DESTINATION  ${FCRI_LIBRARY_DIR}
  INCLUDES DESTINATION ${FCRI_INCLUDE_DIRS}
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")
install(DIRECTORY "${CMAKE_SOURCE_DIR}/include/" DESTINATION "include/fcri" 
	FILES_MATCHING PATTERN "*.fpp" PATTERN ".*" EXCLUDE)

install(EXPORT "FCRITargets" DESTINATION "cmake")

# Generate package
include(CMakePackageConfigHelpers)
configure_package_config_file("../FCRIConfig.cmake.in" "FCRIConfig.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
	PATH_VARS FCRI_INCLUDE_DIR FCRI_LIBRARY_DIR
	)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/FCRIConfig.cmake" DESTINATION ".")

# Export the package
export(PACKAGE FCRI)


