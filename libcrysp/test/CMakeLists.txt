# Unit tests for libCrysP.
#
# Each test is a small stand-alone Fortran program linked against the library and registered with CTest. A test passes if the
# program exits with status 0 (see testing.f90). Run them with `ctest --test-dir build --output-on-failure` after building.
#
# Keeping one executable per test topic gives process isolation: libCrysP reports errors with `error stop`, so a failure in one
# test cannot take down the others. A test that *expects* an abort can be judged on its output instead of its exit status with
# PASS_REGULAR_EXPRESSION / FAIL_REGULAR_EXPRESSION (see test_parameter below).

# Keep test module files out of the library's module directory so they are never installed alongside the library.
set(TEST_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/include")

# Static libraries do not propagate their LINK_FLAGS, so replicate the MKL link line used by the library and the CLI.
set(TEST_LINK_FLAGS "-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")

add_library(crysp_testing STATIC testing.f90)
target_link_libraries(crysp_testing PUBLIC crysp)
target_include_directories(crysp_testing PUBLIC ${TEST_MODULE_DIR})
set_target_properties(crysp_testing PROPERTIES Fortran_MODULE_DIRECTORY ${TEST_MODULE_DIR})

# Register a test program `<name>.f90` as CTest test `<name>`.
function(crysp_add_test name)
    add_executable(${name} ${name}.f90)
    target_link_libraries(${name} PRIVATE crysp_testing)
    set_target_properties(${name} PROPERTIES
        Fortran_MODULE_DIRECTORY ${TEST_MODULE_DIR}
        LINK_FLAGS "${TEST_LINK_FLAGS}"
    )
    add_test(NAME ${name} COMMAND ${name})
endfunction()

crysp_add_test(test_parameter)
crysp_add_test(test_material_roundtrip)

# test_parameter deliberately ends by aborting (shape mismatch must be detected), so its exit status is always non-zero.
# Judge it on its output instead: any failed check prints "FAILED", and the abort must have logged the exception.
set_tests_properties(test_parameter PROPERTIES
    FAIL_REGULAR_EXPRESSION "FAILED"
    PASS_REGULAR_EXPRESSION "EXCEPTION serialization, parameter_to_real_matrix"
)
