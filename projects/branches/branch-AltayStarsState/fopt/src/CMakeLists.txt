# $Id: CMakeLists.txt 2919 2017-04-03 09:56:15Z jgawad $
#
cmake_minimum_required(VERSION 2.8)

project(fopt Fortran)

# Put the module files in a common directory, so other components 
# of this project can find them.
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list2string(Fortran_FLAGS CMAKE_Fortran_FLAGS) 
list2string(Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_DEBUG)
list2string(Fortran_FLAGS_RELEASE CMAKE_Fortran_FLAGS_RELEASE)

set(AUXLIBSRC
mkl_rci_stub.f90
)

set(LIBSRC
nlls_tr.f90 
nnls_mod.f90 
jacobiFD.f90 
objectiveFx.f90
)

# We have to force mkl_rci_stub to be built before fopt.
# CMake dependency scanner does not realize that mkl_rci_stub.f90 provides
# Fortran modules that are used in fopt.
add_library(mkl_rci_stub OBJECT ${AUXLIBSRC})

add_library(fopt STATIC ${LIBSRC} $<TARGET_OBJECTS:mkl_rci_stub>)

add_dependencies(fopt mkl_rci_stub)

#
# Installation of the outputs in the build tree
#
set(fopt_INCLUDE_DIR "include") # Just one directory needed in the package
set(fopt_LIBRARY_DIR "lib")

# Note: Static libraries are always treated as ARCHIVE targets.
install(TARGETS fopt
  EXPORT "foptTargets"
  LIBRARY DESTINATION  ${fopt_LIBRARY_DIR}
  ARCHIVE DESTINATION  ${fopt_LIBRARY_DIR}
  INCLUDES DESTINATION ${fopt_INCLUDE_DIR}
)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")
install(EXPORT "foptTargets" DESTINATION "cmake")

# Generate package
include(CMakePackageConfigHelpers)
configure_package_config_file("../foptConfig.cmake.in" "foptConfig.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
	PATH_VARS fopt_INCLUDE_DIR fopt_LIBRARY_DIR
	)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/foptConfig.cmake" DESTINATION ".")

# Export the package
export(PACKAGE fopt)


