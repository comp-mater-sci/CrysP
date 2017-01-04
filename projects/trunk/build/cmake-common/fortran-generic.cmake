# $Id$
#

# Generic sets of Fortran compiler and linker flags, commonly used across many
# projects.
#
# All these sets are defined as CMake lists. You will need to "stringify" them
# before passing to the toolchain (see list2string in utilities.cmake).

# Intel compiler specific

set(Fortran_FLAGS -fpp "-warn all" -implicitnone "-stand f08" -standard-semantics)

set(Fortran_FLAGS_DEBUG -g -O0 -check all -ftrapuv "-debug all" "-debug-parameters all" -traceback)

#
# Disabled diagnostics:
#  - Remark #10382: option '-xHOST' setting .... 
set(Fortran_FLAGS_RELEASE -O3 -xHost -no-prec-div -diag-disable:10382)

# Prevent -i_dynamic from being appended to linker flags
set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

