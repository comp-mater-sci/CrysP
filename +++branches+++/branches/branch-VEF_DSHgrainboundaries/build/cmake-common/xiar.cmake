# $Id$
#

# For some reasons CMake does not use xiar by default.
# We need xiar if -ipo is chosen, otherwise xiar makes no harm anyway
find_program(XIAR xiar)
if(XIAR) 
	set(CMAKE_Fortran_CREATE_STATIC_LIBRARY
            "${XIAR} cr <TARGET> <LINK_FLAGS> <OBJECTS> "
            "${XIAR} -s <TARGET> ")
endif(XIAR)
MARK_AS_ADVANCED(XIAR)

