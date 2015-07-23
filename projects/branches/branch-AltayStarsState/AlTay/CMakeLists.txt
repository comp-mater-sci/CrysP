# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(altay Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../build/cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

include_directories("include")

#
# Dependencies
#
set(FCRI_DIR "../FCRI/${CMAKE_BUILD_TYPE}")
find_package(FCRI REQUIRED)
include_directories(${FCRI_INCLUDE_DIRS})
message("FCRI includes: ${FCRI_INCLUDE_DIRS}")
message("FCRI libs: ${FCRI_LIBRARIES}")

set(FH5_DIR "../FH5/${CMAKE_BUILD_TYPE}")
find_package(FH5 REQUIRED)
include_directories(${FH5_INCLUDE_DIR})
message("FH5 includes: ${FH5_INCLUDE_DIR}")
message("FH5 libs: ${FH5_LIBRARIES}")
#
# HDF5
#
find_package("HDF5" REQUIRED COMPONENTS C Fortran HL Fortran_HL)
message("HDF5_FOUND: ${HDF5_FOUND}")
include_directories(${HDF5_INCLUDE_DIR})
message("HDF5 includes: ${HDF5_INCLUDE_DIR}")
message("HDF5 libs: ${HDF5_LIBRARIES}")

add_subdirectory(src)


