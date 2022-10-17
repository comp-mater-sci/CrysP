# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(AlamDMC Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

if   (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(libalamDMC_FLAGS "-DPEBP_ENABLED")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(libalamDMC_FLAGS "-DPEBP_ENABLED -ffree-line-length-none")
else()
    message(FATAL_ERROR "Compiler type(CMAKE_Fortran_COMPILER_ID) not recognized")
endif()

list(APPEND Fortran_FLAGS ${libalamDMC_FLAGS})

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(AlamDMC_SRCS
abstractModule.f90
alamEvalCached.f90
basicModule.f90
commonConfig.f90
commonUtils.f90
deformationDrivenModule.f90
dmcResultFileOutput.f90
dmcUtils.f90
evolutionOutputRecord.f90
incrementationControl.f90
qrsTypes.f90
resultTable.f90
resultTableRecord.f90
strainDrivenStep.f90
stressDrivenEvolutionModule.f90
stressDrivenModule.f90
substeppingConfig.f90
xVectorEvolutionOutputRecord.f90
YLPResult.f90
)

add_library(AlamDMC STATIC ${AlamDMC_SRCS})
target_link_libraries(AlamDMC alamYLP CRI)

# Include files located in the same directories as the sources
# and this CMakeLists.txt
target_include_directories(AlamDMC PUBLIC ${CMAKE_CURRENT_LIST_DIR})

install(TARGETS AlamDMC DESTINATION "lib")
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")

