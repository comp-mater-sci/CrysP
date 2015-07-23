# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(FCRI Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../build/cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

include_directories("include")
#
# Dependencies
#
set(FCRI_DIR "../FCRI/${CMAKE_BUILD_TYPE}")
find_package(FCRI REQUIRED)
include_directories(${FCRI_INCLUDE_DIR} ${FCRI_INCLUDE_DIR}/fcri)
set(FCRI_INCLUDE_DIRS ${FCRI_INCLUDE_DIR} ${FCRI_INCLUDE_DIR}/fcri) 
message("${FCRI_INCLUDE_DIRS}")

#
# HDF5
#
# Linking against HDF5 static libraries: broken, see:
# http://public.kitware.com/Bug/view.php?id=15432
# set(HDF5_USE_STATIC_LIBRARIES ON)
# Components: see http://www.cmake.org/cmake/help/v3.2/module/FindHDF5.html
find_package("HDF5" REQUIRED COMPONENTS C Fortran HL Fortran_HL)
message("HDF5_FOUND: ${HDF5_FOUND}")
include_directories(${HDF5_INCLUDE_DIR})
message("HDF5 includes: ${HDF5_INCLUDE_DIR}")
message("HDF5 libs: ${HDF5_LIBRARIES}")
add_subdirectory(src)

add_subdirectory(src-bin)

