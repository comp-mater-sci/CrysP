# $Id$
#
cmake_minimum_required(VERSION 2.8.12)

project(AlTay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(libaltay "altay")
set(progaltay "prog_altay")

set(LibAlTay_FLAGS "-extend-source 132" "-align dcommons" "-warn nodeclarations")

list(APPEND Fortran_FLAGS ${LibAlTay_FLAGS})

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(LIBSRC
Alg0A.f90
altayAlgorithms.f90
altayConfig.f90
altayCRSSTypes.f90
altayCubAccess.f90
altayCurAccess.f90
altayDeformationMechanismData_preconfigured.f90
altayDeformationMechanism.f90
altayDeformationMechanismConstants.f90
#altayDSHState.f90
altayGrainCluster.f90
altayHard.f90
# altayHardLawDSH.f90
altayHardLawKM.f90
altayHardLawSimple.f90
altayHardTypes.f90
altayHDF5Access.f90
altayHDF5Context.f90
altayHDF5PersitenceScheme.f90
altayIOContext.f90
altayMacroKinematic.f90
altayMaterial.f90
altayMesostructure.f90
altayMillerIndices.f90
altayMiscutils.f90
altayNativePersistenceScheme.f90
altayODFTypes.f90
altayPancake.f90
altayPhase.f90
altayRCM.f90
altaySimulation.f90
altaySliprate.f90
altayState.f90
altayStatePersistence.f90
altayStatePersistenceUtils.f90
altayStateTypes.f90
altayTaylor.f90
altayTBH.f90
# altaySub.f90
altayTexAccess.f90
altayTexFormats.f90
altayTexFormatConstants.f90
Dynfil.f90
ioconfig.f90
altaySmtAccess.f90
)

#
# Library libaltay
#


add_library(${libaltay} STATIC ${LIBSRC})
# FIXME: add PEBP_ENABLED once altayDSHState is upgraded
# FIXME: add ALTAY_SUBROUTINE once the API is fixed.
# FIXME: remove TESTING_ENABLED once the code reached production-ready level.
set_target_properties(${libaltay} PROPERTIES COMPILE_DEFINITIONS "NO_STDOUT;RCM_ENABLED;TESTING_ENABLED;")
add_dependencies(${libaltay} CRI FH5)
#
# AlTay stand-alone
#
set(PROGSRC
MainA1.f90
)

add_executable(${progaltay} ${PROGSRC}) 
set_target_properties(${progaltay} PROPERTIES OUTPUT_NAME "altay")
# FIXME: add PEBP_ENABLED;FINALBPM_ENABLED;INTERMEDIATEBPM_DISABLED once
# altayDSHState is upgraded
set_target_properties(${progaltay} PROPERTIES COMPILE_DEFINITIONS "MAINDIRECT;FINALCUB_ENABLED;")

target_include_directories(${progaltay} PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})

# Set different output for module files
set_target_properties(${progaltay} PROPERTIES Fortran_MODULE_DIRECTORY "${progaltay}_modules")
target_link_libraries(${progaltay} ${libaltay} CRI FH5 ${HDF5_LIBRARIES})

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


