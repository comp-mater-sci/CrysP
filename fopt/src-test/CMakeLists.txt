cmake_minimum_required(VERSION 2.8)

project(foptTest Fortran)
set(progname foptTest)

include_directories("${CMAKE_BINARY_DIR}/include")

list(APPEND Fortran_FLAGS "-mkl=sequential")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(PROGSRC
testFunctions.f90
testNllsTR.f90
)

add_executable(${progname} ${PROGSRC}) 
target_link_libraries(${progname} "fopt")

install(TARGETS ${progname} DESTINATION "bin")

