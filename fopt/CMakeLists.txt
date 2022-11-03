cmake_minimum_required(VERSION 3.0)

project(fopt Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

add_subdirectory(src)
