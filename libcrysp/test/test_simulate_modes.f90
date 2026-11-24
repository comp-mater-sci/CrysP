!> Input validation of crysp_simulate_stress_modes and crysp_simulate_strain_modes.
!>
!> Each check fails before the material is touched, so a default material suffices: a check that wrongly passed would go on to
!> simulate on it and fail with a different code (or hang). Modes are validated one at a time as they are simulated, so a rejected
!> mode must come first.
program test_simulate_modes
    use base_defs, only: DP
    use libcrysp, only: crysp_simulate_stress_modes, crysp_simulate_strain_modes
    use crysp_status, only: Status, CRYSP_ERR_DIMS, CRYSP_ERR_ARG
    use crysp_material, only: Material
    use testing

    implicit none

    type(Material), target:: mat
    type(Status):: stat
    real(DP):: unit_modes(6,3), strain_modes(6,3), stresses(6,3), stress_mode_deviations(3)

    unit_modes = 0._DP
    unit_modes(1,:) = 1._DP
    strain_modes = unit_modes

    !Wrong number of components
    stat = crysp_simulate_stress_modes(mat, unit_modes(1:4,:), strain_modes(1:4,:), stresses(1:4,:), stress_mode_deviations(1:4))
    call check_equal(stat%code, CRYSP_ERR_DIMS, 'stress modes: 4 components rejected')
    stat = crysp_simulate_strain_modes(mat, unit_modes(1:4,:), stresses(1:4,:))
    call check_equal(stat%code, CRYSP_ERR_DIMS, 'strain modes: 4 components rejected')

    !Outputs with fewer modes than the input
    stat = crysp_simulate_stress_modes(mat, unit_modes, strain_modes(:,1:2), stresses, stress_mode_deviations)
    call check_equal(stat%code, CRYSP_ERR_DIMS, 'stress modes: mismatched strain_modes rejected')
    stat = crysp_simulate_stress_modes(mat, unit_modes, strain_modes, stresses(:,1:2), stress_mode_deviations)
    call check_equal(stat%code, CRYSP_ERR_DIMS, 'stress modes: mismatched stresses rejected')
    stat = crysp_simulate_stress_modes(mat, unit_modes, strain_modes, stresses, stress_mode_deviations(1:2))
    call check_equal(stat%code, CRYSP_ERR_DIMS, 'stress modes: mismatched residuals rejected')
    stat = crysp_simulate_strain_modes(mat, unit_modes, stresses(:,1:2))
    call check_equal(stat%code, CRYSP_ERR_DIMS, 'strain modes: mismatched stresses rejected')

    !A purely hydrostatic mode is rejected, and named in the message.
    unit_modes(1:3,1) = 2._DP
    stat = crysp_simulate_stress_modes(mat, unit_modes, strain_modes, stresses, stress_mode_deviations)
    call check_equal(stat%code, CRYSP_ERR_ARG, 'stress modes: hydrostatic mode rejected')
    call check(index(stat%message, 'mode 1') > 0, 'stress modes: message names the rejected mode, got "' // trim(stat%message) // '"')

    !No modes at all is not an error.
    stat = crysp_simulate_stress_modes(mat, unit_modes(:,1:0), strain_modes(:,1:0), stresses(:,1:0), stress_mode_deviations(1:0))
    call check_equal(stat%code, 0, 'stress modes: zero modes accepted')
    stat = crysp_simulate_strain_modes(mat, unit_modes(:,1:0), stresses(:,1:0))
    call check_equal(stat%code, 0, 'strain modes: zero modes accepted')

    call finish()
end program
