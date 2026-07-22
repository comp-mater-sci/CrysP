set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list(APPEND Fortran_FLAGS ${alamDMC_FLAGS})

add_executable(crysp
basicModule.f90
commonUtils.f90
dmcADP.f90
dmcASR.f90
dmcQRS.f90
dmcYLD.f90
crysp.f90
file_io.f90
)

target_link_libraries(crysp PRIVATE libcrysp)
set_target_properties(crysp PROPERTIES LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

install(TARGETS crysp RUNTIME DESTINATION bin)
