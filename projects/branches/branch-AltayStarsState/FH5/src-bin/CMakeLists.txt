# $Id: CMakeLists.txt 1658 2014-01-31 23:24:53Z jgawad $
#
cmake_minimum_required(VERSION 2.8)

project(FH5-samples Fortran)

set(FH5_SAMPLE "FH5sample")

list(APPEND Fortran_FLAGS "-diag-disable 5268")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 

list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)

list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(EXESRC
FH5sample_main.f90  
FH5Sample_File.f90  
)

add_executable(${FH5_SAMPLE} ${EXESRC})

target_link_libraries(${FH5_SAMPLE} "FH5")
target_link_libraries(${FH5_SAMPLE} ${HDF5_LIBRARIES})


set_property(TARGET ${FH5_SAMPLE} APPEND PROPERTY INCLUDE_DIRECTORIES
${CMAKE_BINARY_DIR}/include) 

install(TARGETS ${FH5_SAMPLE} DESTINATION "bin")

