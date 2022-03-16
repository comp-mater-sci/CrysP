# $Id$

# Define variables to be used in CMakeLists files in subdirectories:
# * ${PROJECT_NAME}_ROOT_DIR: output directory
# * ${CMAKE_INSTALL_PREFIX}: directory where the outputs will be installed
# * ${PROJECT_NAME}_CMAKE_DIR: directory where the generated export files will
#   be stored (used in making a project package)

macro(basicFortranSetup projectname)
	include("${COMMON_DIR}/basic.cmake") #MB: sets build type: Debug, Release
	include("${COMMON_DIR}/xiar.cmake")
	include("${COMMON_DIR}/utilities.cmake")
	# Provides lists: Fortran_FLAGS, Fortran_FLAGS_DEBUG, Fortran_FLAGS_RELEASE
	include("${COMMON_DIR}/fortran-generic.cmake")

	set(${projectname}_ROOT_DIR "${CMAKE_BINARY_DIR}/..")
	set(CMAKE_INSTALL_PREFIX "${${projectname}_ROOT_DIR}")
	set(${projectname}_CMAKE_DIR "${CMAKE_INSTALL_PREFIX}/cmake")
endmacro(basicFortranSetup)

basicFortranSetup(${PROJECT_NAME})

