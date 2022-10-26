cmake_minimum_required(VERSION 3.0)

project(alamDMC Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

#
# Dependencies
#
set(FCRI_DIR "../FCRI/${CMAKE_BUILD_TYPE}")
find_package(FCRI REQUIRED)
include_directories(${FCRI_INCLUDE_DIR})

set(altay_DIR "../AlTay/${CMAKE_BUILD_TYPE}")
find_package(altay REQUIRED)
include_directories(${altay_INCLUDE_DIR})

set(fopt_DIR "../fopt/${CMAKE_BUILD_TYPE}")
find_package(fopt REQUIRED)
include_directories(${fopt_INCLUDE_DIR})

add_subdirectory(alamDMC)
add_subdirectory(libAlamDMC) 
add_subdirectory(libAlamYLP)
