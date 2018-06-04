cmake_minimum_required(VERSION 3.7)
project(fslis VERSION 1.0 LANGUAGES Fortran)

set(LIBFSLIS_SRC fslis.f90)
add_library(fslis STATIC ${LIBFSLIS_SRC})
target_link_libraries(fslis PUBLIC slis)

install(TARGETS fslis 
	DESTINATION "lib")

