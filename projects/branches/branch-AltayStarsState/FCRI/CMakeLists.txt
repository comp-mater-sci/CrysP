# $Id: CMakeLists.txt 2776 2017-01-04 14:06:04Z jgawad $
#
cmake_minimum_required(VERSION 3.5)

project(FCRI Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../build/cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

include_directories("include")

add_subdirectory(src)

add_subdirectory(src-test)

