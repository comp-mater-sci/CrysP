# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(AlamDMC Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(AlamDMC_SRCS
abstractModule.f90  
basicModule.f90  
commonConfig.f90
commonUtils.f90
dmcFuture.f90  
dmcUtils.f90
evolutionOutputRecord.f90
incrementationControl.f90 
qrsTypes.f90
stressDrivenEvolutionModule.f90
dmcResultFileOutput.f90
xVectorEvolutionOutputRecord.f90
)

add_library(AlamDMC STATIC ${AlamDMC_SRCS})
add_dependencies(AlamDMC alamYLP)

# Include files located in the same directories as the sources
# and this CMakeLists.txt
target_include_directories(AlamDMC PUBLIC ${CMAKE_CURRENT_LIST_DIR})

install(TARGETS AlamDMC DESTINATION "lib")
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")

