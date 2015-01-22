# $Id: CMakeLists.txt 1658 2014-01-31 23:24:53Z jgawad $
#
cmake_minimum_required(VERSION 2.8)

project(AlTay Fortran)

set(COMMON_DIR "../build/cmake-common")

include("${COMMON_DIR}/basic.cmake")
include("${COMMON_DIR}/xiar.cmake")
include("${COMMON_DIR}/utilities.cmake")
# Provides lists: Fortran_FLAGS, Fortran_FLAGS_DEBUG, Fortran_FLAGS_RELEASE
include("${COMMON_DIR}/fortran-generic.cmake")

set(altay_ROOT_DIR "${CMAKE_BINARY_DIR}/..")
set(CMAKE_INSTALL_PREFIX "${altay_ROOT_DIR}")

include_directories("include")

add_subdirectory(src)

# add_subdirectory(src-test)

