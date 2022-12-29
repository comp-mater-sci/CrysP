# Generic sets of Fortran compiler and linker flag
# All these sets are defined as CMake lists. You will need to "stringify" them
# before passing to the toolchain (see list2string in utilities.cmake).

if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(Fortran_FLAGS "-cpp -I$ENV{MKLROOT}/include -extend-source 132 -Wall")
    set(Fortran_FLAGS_DEBUG "-g -O0 -check all -ftrapuv -debug all -debug-parameters all -traceback")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(Fortran_FLAGS "-cpp -I$ENV{MKLROOT}/include -ffree-line-length-none -Wall -Wno-unused-label")
    set(Fortran_FLAGS_DEBUG "-g -Og -fbacktrace -fcheck=all -fsanitize=undefined")
else()
    message(FATAL_ERROR "Compiler type(CMAKE_Fortran_COMPILER_ID) not recognized")
endif()

set(Fortran_FLAGS_RELEASE -O2)

# Prevent -i_dynamic from being appended to linker flags
set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

