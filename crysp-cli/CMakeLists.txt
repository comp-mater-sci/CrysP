cmake_minimum_required(VERSION 3.5)

project(crysp-cli Fortran)

# Load typical initialization code. It sets a collection of variables.
include("../common.cmake")

#
# Dependencies
#
set(libcrysp_DIR "../libcrysp/${CMAKE_BUILD_TYPE}")
find_package(libcrysp REQUIRED)

add_subdirectory(src)
