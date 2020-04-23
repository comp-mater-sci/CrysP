cmake_minimum_required(VERSION 3.7)
project(slis VERSION 1.0 LANGUAGES CXX Fortran)

# Load typical initialization code. It sets a collection of variables.
set(COMMON_DIR "../build/cmake-common")
include("${COMMON_DIR}/basic-fortran-project.cmake")

# set(Boost_USE_STATIC_LIBS ON)
find_package(Boost REQUIRED COMPONENTS date_time filesystem)

set(LIBSLIS_SRC digest.cpp  signature.cpp  slisapi.cpp  slis_v1.cpp 
	        stdafx.cpp  tokenapi.cpp  token_v1.cpp )

# Static library
add_library(slis STATIC ${LIBSLIS_SRC})
target_compile_definitions(slis PUBLIC BOOST_ALL_STATIC_LINK)
# Note: this should be handled by target_compile_features, but it is too
# complicated...
target_compile_options(slis PUBLIC "-std=c++11")
target_include_directories(slis PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(slis PRIVATE Boost::date_time Boost::filesystem)

# Shared library
# set(Boost_USE_STATIC_LIBS OFF)
add_library(slis_shared SHARED ${LIBSLIS_SRC})
target_compile_definitions(slis_shared PUBLIC BOOST_ALL_STATIC_LINK)
# Note: this should be handled by target_compile_features, but it is too
# complicated...
target_compile_options(slis_shared PUBLIC "-std=c++11")
target_include_directories(slis_shared PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(slis_shared PRIVATE Boost::date_time Boost::filesystem)
set_target_properties(slis_shared PROPERTIES OUTPUT_NAME slis)

# Python compilation
include("${COMMON_DIR}/python-bytecode.cmake")

add_subdirectory(slisadmin)

add_subdirectory(libfslis)

export(TARGETS slis fslis FILE slisConfig.cmake)

install(TARGETS slis slis_shared
	DESTINATION "lib")


