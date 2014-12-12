# $Id: CMakeLists.txt 1658 2014-01-31 23:24:53Z jgawad $
#
cmake_minimum_required(VERSION 2.8)

project(FCRI Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(CRI "CRI")

set(CRI_MT "CRI_mt")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 

list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)

list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

#
# Library libmtmfhmport
#

set(LIBSRC 
criAlgorithm.f90
criAssert.f90
criConfigReader.f90
criErrcodes.f90
criExpandableVector.f90
criIterUtils.f90
criLibCRIVersion.f90
criLinearMap.f90
criLog.f90
criMathUtils.f90
criMkTemp.f90
criNamedRange.f90
criPath.f90
criRange.f90
criRuntime.f90
criTimers.f90
criUncomment.f90
criVersion.f90
)

add_library(${CRI} STATIC ${LIBSRC})
add_library(${CRI_MT} STATIC ${LIBSRC})

set_target_properties(${CRI_MT} PROPERTIES COMPILE_FLAGS "-parallel -openmp")

#
# Installation of the outputs in the build tree
#
install(TARGETS ${CRI} ${CRI_MT} DESTINATION "lib")
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")


