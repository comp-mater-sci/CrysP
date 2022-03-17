# $Id: CMakeLists.txt 1658 2014-01-31 23:24:53Z jgawad $
#
cmake_minimum_required(VERSION 2.8.12)

project(FCRI-test Fortran)

set(CRI_TEST "criTest")

list(APPEND Fortran_FLAGS "-diag-disable 5268")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 

list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)

list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)


set(EXESRC
criTestDriver.f90
criTestAlgorithm.f90
criTestMathUtils.f90
criTestNumerics.f90
criTestPath.f90
criTestRange.f90
criTestUncomment.f90
)

add_executable(${CRI_TEST} ${EXESRC})

target_link_libraries(${CRI_TEST} "CRI")

set_property(TARGET ${CRI_TEST} APPEND PROPERTY INCLUDE_DIRECTORIES ${CMAKE_BINARY_DIR}/include) 

install(TARGETS ${CRI_TEST} DESTINATION "bin")

