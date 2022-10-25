cmake_minimum_required(VERSION 3.0)

project(altay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

#
# Dependencies
#
set(FCRI_DIR "../FCRI/${CMAKE_BUILD_TYPE}")
find_package(FCRI REQUIRED)
include_directories(${FCRI_INCLUDE_DIR})

add_subdirectory(src)
