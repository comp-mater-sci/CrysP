# $Id: CMakeLists.txt 1658 2014-01-31 23:24:53Z jgawad $
#
cmake_minimum_required(VERSION 2.8)

project(fopt Fortran)

set(COMMON_DIR "../build/cmake-common")

include("${COMMON_DIR}/basic.cmake")
include("${COMMON_DIR}/xiar.cmake")
include("${COMMON_DIR}/utilities.cmake")
# Provides lists: Fortran_FLAGS, Fortran_FLAGS_DEBUG, Fortran_FLAGS_RELEASE
include("${COMMON_DIR}/fortran-generic.cmake")

set(fopt_ROOT_DIR "${CMAKE_BINARY_DIR}/..")
set(CMAKE_INSTALL_PREFIX "${fopt_ROOT_DIR}")
set(fopt_INCLUDE_DIRS "${fopt_ROOT_DIR}/include")
set(fopt_LIBRARY_DIRS "${fopt_ROOT_DIR}/lib")


add_subdirectory(src)

add_subdirectory(src-test)

