cmake_minimum_required(VERSION 3.5)

project(alamDMC Fortran)

# Load typical initialization code. It sets a collection of variables.
include("../common.cmake")

#
# Dependencies
#
set(altay_DIR "../libcrysp/${CMAKE_BUILD_TYPE}")
find_package(altay REQUIRED)
include_directories(${altay_INCLUDE_DIR})

add_subdirectory(src)
