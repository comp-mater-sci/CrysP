# $Id: CMakeLists.txt -1   $
#
cmake_minimum_required(VERSION 2.8)

project(libfopt Fortran)

# Put the module files in a common directory, so other components 
# of this project can find them.
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

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

#
# Installation of the outputs in the build tree
#
install(TARGETS fopt DESTINATION "lib")
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")

