# $Id$
#
cmake_minimum_required(VERSION 2.8)

project(AlTay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(libaltay "altay")
set(progaltay "prog_altay")

set(LibAlTay_FLAGS "-extend-source 132")

list(APPEND Fortran_FLAGS ${LibAlTay_FLAGS})

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(CORESRC
altaySimul.f90 
altayTaylor.f90 
altayAlgorithms.f90 
altayDynfil.f90 
altayMacroKinematic.f90 
altayMesostructure.f90 
altayIOConfig.f90 
altayMiscutils.f90 
altayPancake.f90 
altaySliprate.f90 
altayTBH.f90 
altayCubAccess.f90 
altayCurAccess.f90 
altaySmtAccess.f90 
altayTexFormats.f90 
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

set_target_properties(${libaltay} PROPERTIES COMPILE_DEFINITIONS "ALTAY_SUBROUTINE;NO_STDOUT;RCM_ENABLED;PEBP_ENABLED;")

#
# AlTay stand-alone
#
set(PROGSRC
${CORESRC}
MainA1.f90
)

add_executable(${progaltay} ${PROGSRC}) 
set_target_properties(${progaltay} PROPERTIES OUTPUT_NAME "altay")
set_target_properties(${progaltay} PROPERTIES COMPILE_DEFINITIONS "MAINDIRECT;FINALCUB_ENABLED;PEBP_ENABLED;FINALBPM_ENABLED;INTERMEDIATEBPM_DISABLED")
# Set different output for module files
set_target_properties(${progaltay} PROPERTIES Fortran_MODULE_DIRECTORY "${progaltay}_modules")
target_link_libraries(${progaltay} "CRI")

#
# Installation of the outputs in the build tree
#

set(altay_INCLUDE_DIRS "include")
set(altay_INCLUDE_DIR "include") # Just one directory needed in the package
set(altay_LIBRARY_DIR "lib")

# Note: Static libraries are always treated as ARCHIVE targets.
install(TARGETS ${libaltay}
  EXPORT "altayTargets"
  LIBRARY DESTINATION  ${altay_LIBRARY_DIR}
  ARCHIVE DESTINATION  ${altay_LIBRARY_DIR}
  INCLUDES DESTINATION ${altay_INCLUDE_DIRS}
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")

install(EXPORT "altayTargets" DESTINATION "cmake")

install(TARGETS ${progaltay} DESTINATION "bin")

# Generate package
include(CMakePackageConfigHelpers)
configure_package_config_file("../altayConfig.cmake.in" "altayConfig.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
	PATH_VARS altay_INCLUDE_DIR altay_LIBRARY_DIR
	)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/altayConfig.cmake" DESTINATION ".")

# Export the package
export(PACKAGE altay)


