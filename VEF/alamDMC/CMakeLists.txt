cmake_minimum_required(VERSION 2.8)

project(alamDMC Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(alamDMC_SRCS
dmcADP.f90
dmcASR.f90 
dmcEWC.f90 
dmcQRS.f90  
dmcUDSA.f90  
dmcYld.f90  
main.f90
)

add_executable(alamDMC  ${alamDMC_SRCS})
target_link_libraries(alamDMC alamYLP AlamDMC altay CRI fopt)
set_target_properties(alamDMC PROPERTIES LINK_FLAGS "-mkl")

install(TARGETS alamDMC DESTINATION "bin")

