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

target_link_libraries(libcrysp)
set_target_properties(libcrysp PROPERTIES LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

#
# Installation of the outputs in the build tree
#
set(libcrysp_INCLUDE_DIR "include")
set(libcrysp_LIBRARY_DIR "lib")

# Note: Static libraries are always treated as ARCHIVE targets.
install(TARGETS libcrysp
  EXPORT "libcryspTargets"
  LIBRARY DESTINATION  ${libcrysp_LIBRARY_DIR}
  ARCHIVE DESTINATION  ${librysp_LIBRARY_DIR}
  INCLUDES DESTINATION ${libcrysp_INCLUDE_DIRS}
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION "include")
install(EXPORT "libcryspTargets" DESTINATION "cmake")

# Generate package
include(CMakePackageConfigHelpers)
configure_package_config_file("../libcryspConfig.cmake.in" "libcryspConfig.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
	PATH_VARS libcrysp_INCLUDE_DIR libcrysp_LIBRARY_DIR
	)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/libcryspConfig.cmake" DESTINATION ".")

# Export the package
export(PACKAGE libcrysp)
