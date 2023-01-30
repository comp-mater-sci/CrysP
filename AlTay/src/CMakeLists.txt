cmake_minimum_required(VERSION 3.0)

project(AlTay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(libaltay "altay")

list(APPEND Fortran_FLAGS ${LibAlTay_FLAGS})

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

add_library(${libaltay} STATIC
criAlgorithm.f90
criConfigReader.f90
criLinearMap.f90
criMathUtils.f90
criNamedRange.f90
criNumerics.f90
criPath.f90
criRange.f90
criRuntime.f90
criUncomment.f90
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
altayCurAccess.f90 
altaySmtAccess.f90 
altayTexFormats.f90 
altayDefinitions.f90
altayLog.f90
altayConfig.f90  
altayRCM.f90 
altaySub.f90
hardening/altayHard.f90
hardening/altayHardTypes.f90
hardening/altayHardLawDSH.f90
hardening/altayHardLawSwift.f90
hardening/altayHardLawVoce.f90
)


set_target_properties(${libaltay} PROPERTIES COMPILE_DEFINITIONS "ALTAY_SUBROUTINE;NO_STDOUT;RCM_ENABLED;PEBP_ENABLED;")

#
# Installation of the outputs in the build tree
#
set(altay_INCLUDE_DIRS "include" "include/altay")
set(altay_INCLUDE_DIR "include")
set(altay_LIBRARY_DIR "lib")

# Note: Static libraries are always treated as ARCHIVE targets.
install(TARGETS ${libaltay}
  EXPORT "altayTargets"
  LIBRARY DESTINATION  ${altay_LIBRARY_DIR}
  ARCHIVE DESTINATION  ${altay_LIBRARY_DIR}
  INCLUDES DESTINATION ${altay_INCLUDE_DIRS}
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")
install(DIRECTORY "${CMAKE_SOURCE_DIR}/include/" DESTINATION "include/altay" 
	FILES_MATCHING PATTERN "*.fpp" PATTERN ".*" EXCLUDE)

install(EXPORT "altayTargets" DESTINATION "cmake")

# Generate package
include(CMakePackageConfigHelpers)
configure_package_config_file("../altayConfig.cmake.in" "altayConfig.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
	PATH_VARS altay_INCLUDE_DIR altay_LIBRARY_DIR
	)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/altayConfig.cmake" DESTINATION ".")

# Export the package
export(PACKAGE altay)
