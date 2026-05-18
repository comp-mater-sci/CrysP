project(alamDMC LANGUAGES C Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

if   (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
elseif   (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(cryspy_FLAGS "-ffree-line-length-0")
else()
    message(FATAL_ERROR "Compiler type(CMAKE_Fortran_COMPILER_ID) not recognized")
endif()

list(APPEND Fortran_FLAGS ${alamDMC_FLAGS})

add_executable(cryspy
basicModule.f90
commonUtils.f90
dmcADP.f90
dmcASR.f90
dmcQRS.f90
dmcYld.f90
cryspy.f90
file_io.f90
)

target_link_libraries(cryspy altay)
set_target_properties(cryspy PROPERTIES LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

install(TARGETS cryspy RUNTIME DESTINATION bin)
