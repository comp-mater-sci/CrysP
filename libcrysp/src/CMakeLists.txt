set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include")

list(APPEND Fortran_FLAGS ${Libcrysp_FLAGS})

add_library(libcrysp STATIC
    libcrysp.f90
    utils/base_defs.f90
    utils/math_utils.f90
    utils/conversions.f90
    utils/logging.f90
    utils/parameters.f90
    macro/trust_region.c
    macro/macro.f90
    macro/deformation.f90
    macro/mode.f90
    macro/incrementation.f90
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
    micro/constitutive_model.f90
    micro/models/none.f90
    micro/models/swift.f90
    micro/models/voce.f90
    micro/models/hockett_sherby.f90
    micro/models/dsh.f90
    micro/models/dsh_edge.f90
    micro/models/dsh_screw.f90
    micro/models/dsh_loop.f90
)

include(CMakePackageConfigHelpers)

target_link_libraries(libcrysp)

# This tells CMake exactly where the headers are depending on how it's consumed
target_include_directories(libcrysp PUBLIC
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>  # Used when building locally
    $<INSTALL_INTERFACE:include>                            # Used after running 'make install'
)

set_target_properties(libcrysp PROPERTIES LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

#Group targets into export set
install(TARGETS libcrysp
        EXPORT libcryspTargets
        LIBRARY DESTINATION  lib
        ARCHIVE DESTINATION  lib
        INCLUDES DESTINATION include
)

export(EXPORT libcryspTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/libcryspTargets.cmake"
)
#Automatically generate the Targets file
install(EXPORT libcryspTargets
        FILE libcryspTargets.cmake
        DESTINATION lib/cmake/libcrysp
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

# Export the package
export(PACKAGE libcrysp)
