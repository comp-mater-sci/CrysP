cmake_minimum_required(VERSION 3.0)

project(AlTay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(libaltay "altay")

list(APPEND Fortran_FLAGS ${LibAlTay_FLAGS})

add_library(${libaltay} STATIC
criConfigReader.f90
criLinearMap.f90
criMathUtils.f90
criNamedRange.f90
criNumerics.f90
criRange.f90
criUncomment.f90
altaySimul.f90 
taylor.f90 
altayAlgorithms.f90 
altayDynfil.f90 
altayMacroKinematic.f90 
altayMesostructure.f90 
altaySliprate.f90 
simplex.f90 
altayCurAccess.f90 
definitions.f90
logging.f90
altayConfig.f90  
altaySub.f90
parameters.f90
hardening/hardening.f90
hardening/model/hardening_model.f90
hardening/model/hardening_model_swift.f90
hardening/model/hardening_model_voce.f90
hardening/model/dislocation_substructural_hardening/hardening_model_dsh.f90
hardening/model/dislocation_substructural_hardening/hardening_model_bp.f90
hardening/model/dislocation_substructural_hardening/hardening_model_pebp_screw.f90
hardening/model/dislocation_substructural_hardening/hardening_model_pebp_loop.f90
)


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
