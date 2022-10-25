# Generic sets of Fortran compiler and linker flags, commonly used across many
# projects.
#
# All these sets are defined as CMake lists. You will need to "stringify" them
# before passing to the toolchain (see list2string in utilities.cmake).

# Intel compiler specific



if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
	set(Fortran_FLAGS "-cpp -I$ENV{MKLROOT}/include -extend-source 132")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
	set(Fortran_FLAGS "-cpp -I$ENV{MKLROOT}/include -ffree-line-length-none")
else()
    message(FATAL_ERROR "Compiler type(CMAKE_Fortran_COMPILER_ID) not recognized")
endif()

set(Fortran_FLAGS_DEBUG -g -O0 -check all -ftrapuv "-debug all" "-debug-parameters all" -traceback)

#
# Disabled diagnostics:
#  - Remark #10382: option '-xHOST' setting .... 
set(Fortran_FLAGS_RELEASE -O2)

# Prevent -i_dynamic from being appended to linker flags
set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

