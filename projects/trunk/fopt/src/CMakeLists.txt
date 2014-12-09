# $Id: CMakeLists.txt -1   $
#
cmake_minimum_required(VERSION 2.8)

project(fopt Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "../include")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "../lib")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(LIBSRC
nlls_tr.f90 
nnls_mod.f90 
jacobiFD.f90 
objectiveFx.f90
)

add_library(fopt STATIC ${LIBSRC}) 

