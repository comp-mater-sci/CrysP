cmake_minimum_required(VERSION 3.0)

project(AlTay Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

set(libaltay "altay")

list(APPEND Fortran_FLAGS ${LibAlTay_FLAGS})

add_library(${libaltay} STATIC
file_io.f90
utils.f90
logging.f90
altayConfig.f90  
altay.f90
parameters.f90
macro/simulation.f90
meso/meso.f90
meso/meso_model.f90
meso/taylor_ambiguity.f90
meso/simplex.f90 
meso/models/alamel.f90
meso/models/full_constraints_taylor.f90
meso/cluster.f90
meso/relaxation.f90
micro/grain.f90 
micro/slip_systems.f90
micro/micro.f90
micro/hardening_model.f90
micro/models/none.f90
micro/models/swift.f90
micro/models/voce.f90
micro/models/dsh.f90
micro/models/dsh_edge.f90
micro/models/dsh_screw.f90
micro/models/dsh_loop.f90
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
