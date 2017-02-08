# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(fopt Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../build/cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

add_subdirectory(src)

add_subdirectory(src-test)

