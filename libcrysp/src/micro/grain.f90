module crysp_grain
    use conversions
    use logging
    use constitutive_model
    use iso_c_binding

    implicit none

    private
    public:: Grain


    !>Texture-related state variables for single grain
    !>
    !> @note
    !> Grain deliberately does not extend [[State]], even though its serialize/deserialize interface matches structurally.
    !> A grain cannot serialize itself without context: it holds a pointer to the constitutive model of its phase, which must be resolved
    !> to an index by the caller. Threading a serialization context through the whole State hierarchy for this one case was deemed
    !> overkill, so Grain instead takes the surrounding phases as an explicit argument to its serialization operations.
    !> @endnote
    type:: Grain
        type(ConstitutiveModel), pointer:: phase
        real(DP), dimension(3, 3):: orientation
        real(DP):: stress_increment
        class(HardeningState), allocatable:: hardening_state
    contains
        procedure:: init   => grain_init
        procedure:: deform => grain_deform
        procedure:: size => grain_get_size
        procedure:: serialize => grain_serialize
        procedure:: deserialize => grain_deserialize
    end type grain

contains

    !Initialize grain.
    subroutine grain_init(this, orientation, model)
        class(Grain), intent(out):: this
        real(DP), dimension(:), intent(in):: orientation
        class(ConstitutiveModel), target, intent(in):: model
        class(HardeningState), intent(in):: state

        this%orientation = euler_to_rotation_matrix(orientation)
        this%model => model
        this%state = model%get_state()
    end subroutine

    subroutine grain_deform(this, v_grad, t, slip_rates, stress)
        class(Grain), intent(inout):: this
        real(DP), dimension(3,3), intent(in):: v_grad !! Externally imposed velocity gradient.
        real(DP), intent(in):: t
        real(DP), dimension(size(this%state%crss,2)), intent(in):: slip_rates
        real(DP), dimension(3,3), intent(in):: stress !! Stress in the global frame

        real(DP):: spin(3,3), &
                   rot_inc(3,3), &
                   imposed_spin_rate(3,3), &
                   orientation_old(3,3), &
                   crss_old(2, size(this%state%crss,2)), &
                   stress_new(3,3)

        crss_old = this%state%crss
        orientation_old = this%orientation

        !Spin consists of a part counteracting the spin component of the slip systems and imposed spin rate
        !Note that imposed_spin_rate is actually an approximation of the 'external' spin caused by the velocity gradient.
        !To get the exact value, you must compute the deformation gradient increment as e^Lt, perform polar decomposition to get R
        !And take the matrix logarithm and divide by t.
        !This is needed because the stretch in L interacts nonlinearly with the rotation.
        !For small t, this effect is however negligible. This has been tested extensively.
        imposed_spin_rate = (v_grad - transpose(v_grad)) / 2._DP
        spin = imposed_spin_rate - (spin_to_tensor(matmul(this%model%spin_coeffs, slip_rates)) .fromframe. this%orientation)
        rot_inc = matrix_exponential(spin*t)
        this%orientation = matmul(rot_inc, this%orientation)

        call this%model%deform(this%state, t, slip_rates)

        !Upper bound on stress increment is max relative increment of CRSS multiplied by rotated stress tensor.
        stress_new = maxval(this%state%crss/crss_old) * (stress .toframe. transformation_matrix(this%orientation, orientation_old))
        this%stress_increment = norm2(stress_new-stress) / norm2(stress)
    end subroutine


    pure function get_size(this) result(size)
        class(Grain), intent(in):: this
        integer:: size

        size = 3 + this%hardening_state%get_size()
    end function

    pure function grain_serialize(this, phases) result(params)
        class(Grain), target, intent(in):: this
        class(ConstitutiveModel), dimension(:), intent(in):: phases
        type(Parameter), dimension(this%size()):: params

        integer:: i

        do (i=1,size(phases))
            if (c_associated(c_loc(phases(i)), c_loc(this%model))) then
                params(1) = i
                exit
            end if
        end do

        params(2) = this%orientation
        params(3) = this%stress_increment
        params(4:) = this%hardening_state%serialize()
    end subroutine

    pure subroutine grain_deserialize(this, phases, params)
        class(HardeningState), target, intent(out):: this
        class(ConstitutiveModel), intent(in):: phases
        type(Parameter), dimension(this%size()), intent(in):: params

        integer:: phase_id

        phase_id = params(1)
        this%model => phases(phase_id)

        this%orientation = params(2)
        this%stress_increment = params(3)
        call this%hardening_state%deserialize(params(4:))
    end subroutine
end module
