cmake_minimum_required(VERSION 3.0)

project(alamDMC Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

#
# Dependencies
#
set(altay_DIR "../AlTay/${CMAKE_BUILD_TYPE}")
find_package(altay REQUIRED)
include_directories(${altay_INCLUDE_DIR})

add_subdirectory(src) 
