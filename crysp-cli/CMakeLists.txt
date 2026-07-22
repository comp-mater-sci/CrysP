cmake_minimum_required(VERSION 3.5)

project(crysp-cli Fortran)

# Load typical initialization code. It sets a collection of variables.
include("../common.cmake")

#
# Dependencies
#
set(libcrysp_DIR "../libcrysp/${CMAKE_BUILD_TYPE}")
find_package(libcrysp REQUIRED)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list(APPEND Fortran_FLAGS ${alamDMC_FLAGS})

add_executable(crysp
src/basicModule.f90
src/commonUtils.f90
src/dmcADP.f90
src/dmcASR.f90
src/dmcQRS.f90
src/dmcYLD.f90
src/crysp.f90
src/file_io.f90
)

target_link_libraries(crysp PRIVATE libcrysp)
set_target_properties(crysp PROPERTIES LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

install(TARGETS crysp RUNTIME DESTINATION bin)

