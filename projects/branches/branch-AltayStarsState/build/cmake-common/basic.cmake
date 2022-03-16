# $Id: basic.cmake 2069 2015-02-20 17:22:41Z jgawad $
#
# Pre-defined configuration for projects with simple Debug and Release configuration

if(NOT CMAKE_BUILD_TYPE)
	set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      	"Choose the type of build, options are: Debug Release."
      	FORCE)
	message(STATUS "Assuming Release configuration")
endif(NOT CMAKE_BUILD_TYPE)

