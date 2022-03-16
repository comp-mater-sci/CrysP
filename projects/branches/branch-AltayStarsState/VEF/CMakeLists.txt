# $Id: CMakeLists.txt 3369 2020-04-23 08:03:31Z Dries.DeSamblanx $
#
cmake_minimum_required(VERSION 2.8)

project(alamDMC Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../build/cmake-common")
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

set(fng_DIR "../fng/${CMAKE_BUILD_TYPE}")
find_package(fng REQUIRED)
include_directories(${fng_INCLUDE_DIR})

set(slis_DIR "../slis/${CMAKE_BUILD_TYPE}")
# Quick and dirty way, since SLIS does not provide CMake package yet (/if ever/)
get_filename_component(slis_INCLUDE_DIR "${slis_DIR}/include" ABSOLUTE)
get_filename_component(slis_LIB_DIR "${slis_DIR}/lib" ABSOLUTE)
message("slis_INCLUDE_DIR = ${slis_INCLUDE_DIR}")
# Quick and dirty cntd, since SLIS does not provide automatic dependencies yet (/if ever/)
set(Boost_USE_STATIC_LIBS ON)
find_package(Boost REQUIRED COMPONENTS date_time filesystem)

add_subdirectory(alamDMC)
add_subdirectory(libAlamDMC) 
add_subdirectory(libAlamYLP)

include("${COMMON_DIR}/python-bytecode.cmake")

