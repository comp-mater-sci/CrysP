module crysp_grain
    use conversions
    use crysp_status
    use constitutive_model
    use crysp_parameters
    use iso_c_binding

    implicit none

    private
    public:: Grain, &
             Grain_C, &
             TexturePoint, &
             assignment(=)


    !>Texture-related state variables for single grain
    !>
    !> @note
    !> Grain deliberately does not extend [[State]], even though its serialize/deserialize interface matches structurally.
    !> A grain cannot serialize itself without context: it holds a pointer to the constitutive model of its phase, which must be resolved
    !> to an index by the caller. Threading a serialization context through the whole State hierarchy for this one case was deemed
    !> overkill, so Grain instead takes the surrounding phases as an explicit argument to its serialization operations.
    !> @endnote
    type:: Grain
        type(Phase), pointer:: phase
        real(DP), dimension(3, 3):: orientation
        real(DP):: stress_increment
        class(HardeningState), allocatable:: hardening_state
    contains
        procedure:: init   => grain_init
        procedure:: deform => grain_deform
        procedure:: serialize => grain_serialize
        procedure:: deserialize => grain_deserialize
        procedure:: to_texture_point => grain_to_texture_point
    end type grain

    !> C-compatible counterpart of [[Grain]].
    !>
    !> `phase` refers to the element of the material's C-side phase list that backs this grain, as [[Grain]] points to its
    !> [[Phase]]. It does not own that element. The phase is resolved at material level, which holds both the Fortran and the C
    !> phase list: the grain conversions below leave it to the material. `orientation` is the rotation matrix, not Euler angles.
    type, bind(C):: Grain_C
        type(C_PTR):: phase !! type(Phase_C). Resolved at material level.
        real(C_DOUBLE), dimension(3,3):: orientation
        real(C_DOUBLE):: stress_increment
        type(HardeningState_C):: hardening_state
    end type

    interface assignment(=)
        module procedure grain_to_c, grain_from_c
    end interface

contains

    !> Convert a grain to its C-compatible counterpart. The caller takes ownership of the hardening state, see
    !> [[constitutive_model:HardeningState_C]].
    !>
    !> The phase is resolved at material level, which holds both phase lists: `gr_c%phase` is left null here and must be set by
    !> the material.
    subroutine grain_to_c(gr_c, gr)
        type(Grain_C), intent(out):: gr_c
        type(Grain), intent(in):: gr

        gr_c%phase = C_NULL_PTR !Resolved at material level.
        gr_c%orientation = real(gr%orientation,C_DOUBLE)
        gr_c%stress_increment = real(gr%stress_increment, C_DOUBLE)
        gr_c%hardening_state = gr%hardening_state
    end subroutine

    !> Restore a grain from its C-compatible counterpart.
    !>
    !> The phase is resolved at material level.
    !> state in `gr_c` is freed, see [[constitutive_model:hard_stat_from_c]].
    !> `gr_c` is assumed to be well-formed: it is not validated here.
    subroutine grain_from_c(gr, gr_c)
        type(Grain), intent(inout):: gr
        type(Grain_C), intent(in):: gr_c

        gr%orientation = real(gr_c%orientation, DP)
        gr%stress_increment = real(gr_c%stress_increment, DP)
        gr%hardening_state = gr%phase%model%make_hardening_state()
        gr%hardening_state = gr_c%hardening_state
    end subroutine

    !Initialize grain.
    subroutine grain_init(this, orientation, phase_obj, state)
        class(Grain), intent(out):: this
        real(DP), dimension(:), intent(in):: orientation
        type(Phase), target, intent(in):: phase_obj
        class(HardeningState), intent(in):: state

        this%orientation = euler_to_rotation_matrix(orientation)
        this%phase => phase_obj
        this%hardening_state = state
    end subroutine

    function grain_deform(this, v_grad, t, slip_rates, stress) result(stat)
        class(Grain), intent(inout):: this
        real(DP), dimension(3,3), intent(in):: v_grad !! Externally imposed velocity gradient.
        real(DP), intent(in):: t
        real(DP), dimension(size(this%hardening_state%crss,2)), intent(in):: slip_rates
        real(DP), dimension(3,3), intent(in):: stress !! Stress in the global frame
        type(Status):: stat

        real(DP):: spin(3,3), &
                   rot_inc(3,3), &
                   imposed_spin_rate(3,3), &
                   orientation_old(3,3), &
                   crss_old(2, size(this%hardening_state%crss,2)), &
                   stress_new(3,3)

        crss_old = this%hardening_state%crss
        orientation_old = this%orientation

        !Spin consists of a part counteracting the spin component of the slip systems and imposed spin rate
        !Note that imposed_spin_rate is actually an approximation of the 'external' spin caused by the velocity gradient.
        !To get the exact value, you must compute the deformation gradient increment as e^Lt, perform polar decomposition to get R
        !And take the matrix logarithm and divide by t.
        !This is needed because the stretch in L interacts nonlinearly with the rotation.
        !For small t, this effect is however negligible. This has been tested extensively.
        imposed_spin_rate = (v_grad - transpose(v_grad)) / 2._DP
        spin = imposed_spin_rate - (spin_to_tensor(matmul(this%phase%model%spin_coeffs, slip_rates)) .fromframe. this%orientation)
        rot_inc = matrix_exponential(spin*t)
        this%orientation = matmul(rot_inc, this%orientation)

        stat = this%phase%model%deform(this%hardening_state, t, slip_rates)
        if (failed(stat)) return

        !Upper bound on stress increment is max relative increment of CRSS multiplied by rotated stress tensor.
        stress_new = maxval(this%hardening_state%crss/crss_old) * (stress .toframe. transformation_matrix(this%orientation, orientation_old))
        this%stress_increment = norm2(stress_new-stress) / norm2(stress)
    end function
end module
