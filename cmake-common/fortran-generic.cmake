# Generic sets of Fortran compiler and linker flag

if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set(CMAKE_Fortran_FLAGS "-cpp -I$ENV{MKLROOT}/include -Warn all -stand f18 -qopenmp")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -check all,noarg_temp_created,nouninit -ftrapuv -debug all -debug-parameters all -traceback -fpe0")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "-cpp -I$ENV{MKLROOT}/include -ffree-line-length-none -Wall -Wno-unused-label -ffpe-summary=all -Wunused-parameter -Wconversion-extra -Wimplicit-procedure -fopenmp")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -Og -fbacktrace -fcheck=all -fsanitize=undefined -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999999")
else()
    message(FATAL_ERROR "Compiler type(CMAKE_Fortran_COMPILER_ID) not recognized")
endif()

set(CMAKE_Fortran_FLAGS_RELEASE -O2)

# Prevent -i_dynamic from being appended to linker flags
set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

