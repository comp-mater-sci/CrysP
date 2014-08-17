# $Id: CMakeLists.txt 1658 2014-01-31 23:24:53Z jgawad $
#
cmake_minimum_required(VERSION 2.8)

project(AlTay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "../include")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "../lib")

set(libaltay "altay")
set(progaltay "prog_altay")

set(LibAlTay_FLAGS "-extend-source 132" "-align dcommons" "-warn nodeclarations")

list(APPEND Fortran_FLAGS ${LibAlTay_FLAGS})

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 

list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)

list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

# Dependencies

get_filename_component(FCRI_DIR "../../FCRI" ABSOLUTE)
set(FCRI_INCLUDE ${FCRI_DIR}/${CMAKE_BUILD_TYPE}/include)
set(FCRI_LIBDIR ${FCRI_DIR}/${CMAKE_BUILD_TYPE}/lib)
message("FCRI_INCLUDE = ${FCRI_INCLUDE}")

include_directories("${FCRI_INCLUDE}")
link_directories("${FCRI_LIBDIR}")


set(CORESRC
Alg0A.for 
Alg1.for 
Alg2.for 
Dynfil.for 
macroKinematic.f90 
microstr.f90 
ioconfig.f90 
miscutils.f90 
Pancak2.for 
Sliprat.for 
TBH.for 
cubAccess.f90 
curAccess.f90 
smtAccess.f90 
texFormats.f90 
texFormatConstants.f90 
altayHardLawSimple.f90 
altayHard.f90 
altayHardTypes.f90 
altayHardLawDSH.f90 
altayDSHState.f90
)

#
# Library libaltay
#

set(LIBSRC 
${CORESRC}
altayConfig.f90  
altayRCM.f90 
altaySub.f90
)

add_library(${libaltay} STATIC ${LIBSRC})

# set_target_properties(${libaltay} PROPERTIES INCLUDE_DIRECTORIES "${FCRI_INCLUDE}")
set_target_properties(${libaltay} PROPERTIES COMPILE_DEFINITIONS "ALTAY_SUBROUTINE;NO_STDOUT;RCM_ENABLED;PEBP_ENABLED;")

#
# AlTay stand-alone
#
set(PROGSRC
${CORESRC}
MainA1.for
)

add_executable(${progaltay} ${PROGSRC}) 
# set_target_properties(${progaltay} PROPERTIES INCLUDE_DIRECTORIES "${FCRI_INCLUDE}")
set_target_properties(${progaltay} PROPERTIES OUTPUT_NAME "altay")
set_target_properties(${progaltay} PROPERTIES COMPILE_DEFINITIONS "MAINDIRECT;FINALCUB_ENABLED;PEBP_ENABLED;FINALBPM_ENABLED;INTERMEDIATEBPM_DISABLED")
# Set different output for module files
set_target_properties(${progaltay} PROPERTIES Fortran_MODULE_DIRECTORY "${progaltay}_modules")
target_link_libraries(${progaltay} "CRI")

