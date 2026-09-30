cmake_minimum_required(VERSION 3.5)

project(libcrysp
        VERSION 1.0.0
        LANGUAGES Fortran C)

include("../common.cmake")


set(CMAKE_C_FLAGS "-O3 -L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -ldl")

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

add_library(crysp STATIC
    src/libcrysp.f90
    src/model.f90
    src/input.f90
    src/utils/base_defs.f90
    src/utils/math_utils.f90
    src/utils/conversions.f90
    src/utils/logging.f90
    src/utils/serialization.f90
    src/macro/material.f90
    src/macro/trust_region.c
    src/macro/macro.f90
    src/macro/deformation.f90
    src/macro/mode.f90
    src/macro/incrementation.f90
    src/meso/meso.f90
    src/meso/meso_model.f90
    src/meso/taylor_ambiguity.f90
    src/meso/simplex.f90
    src/meso/models/alamel.f90
    src/meso/models/full_constraints_taylor.f90
    src/meso/cluster.f90
    src/meso/relaxation.f90
    src/micro/grain.f90
    src/micro/slip_systems.f90
    src/micro/micro.f90
    src/micro/constitutive_model.f90
    src/micro/models/isotropic_state.f90
    src/micro/models/none.f90
    src/micro/models/swift.f90
    src/micro/models/voce.f90
    src/micro/models/hockett_sherby.f90
    src/micro/models/dsh.f90
    src/micro/models/dsh_edge.f90
    src/micro/models/dsh_screw.f90
    src/micro/models/dsh_loop.f90
)

include(CMakePackageConfigHelpers)

# Unit tests (run with `ctest --test-dir build --output-on-failure`).
option(CRYSP_BUILD_TESTS "Build the libCrysP unit tests" ON)
if(CRYSP_BUILD_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

# This tells CMake exactly where the headers are depending on how it's consumed
target_include_directories(crysp PUBLIC
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>  # Used when building locally
    $<INSTALL_INTERFACE:include>                            # Used after running 'make install'
)

set_target_properties(crysp PROPERTIES
    LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl"
    EXPORT_NAME "libcrysp"
)

#Group targets into export set
install(TARGETS crysp
        EXPORT cryspTargets
        ARCHIVE DESTINATION  lib
        INCLUDES DESTINATION include
)

export(EXPORT cryspTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/libcryspTargets.cmake"
    NAMESPACE crysp::
)
#Automatically generate the Targets file
install(EXPORT cryspTargets
        FILE cryspTargets.cmake
        DESTINATION lib/cmake/libcrysp
        NAMESPACE crysp::
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")

set(CONFIG_CONTENT "
include(\"\${CMAKE_CURRENT_LIST_DIR}/libcryspTargets.cmake\")"
)

# Write the string directly to the build folder
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/libcryspConfig.cmake" "${CONFIG_CONTENT}")
# Install the generated config file
install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/libcryspConfig.cmake"
    DESTINATION lib/cmake/libcrysp
)
