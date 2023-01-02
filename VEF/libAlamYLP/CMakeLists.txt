cmake_minimum_required(VERSION 2.8)

project(alamYLP Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(alamYLP_SRCS
nlls_tr.f90
alamYLP.f90 
alamEval.f90
alamYLPConstants.f90
)

add_library(alamYLP STATIC ${alamYLP_SRCS})
target_link_libraries(alamYLP PUBLIC CRI)

install(TARGETS alamYLP DESTINATION "lib")
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")

