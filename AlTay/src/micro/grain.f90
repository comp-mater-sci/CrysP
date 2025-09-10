module grain_module
    use conversions
    use logging
    use constitutive_model

    implicit none

    private
    public  ::  Grain

    !>Texture-related state variables for single grain
    type:: Grain
        real(DP), dimension(3, 3):: orientation
        class(ConstitutiveModel), pointer:: model
        class(HardeningState), allocatable:: state
    contains
        procedure:: init   => grain_init
        procedure:: deform => grain_deform
    end type grain

contains

    !Initialize grain.
    subroutine grain_init(this, orientation, model, state)
        class(Grain), intent(out):: this
        real(DP), dimension(:), intent(in):: orientation
        class(ConstitutiveModel), target, intent(in):: model
        class(HardeningState), intent(in):: state

        this%orientation = euler_to_tensor(orientation)
        this%model => model
        this%state = state
    end subroutine

    subroutine grain_deform(this, v_grad, t, slip_rates)
        class(Grain), intent(inout):: this
        real(DP), dimension(3,3), intent(in):: v_grad !! Externally imposed velocity gradient.
        real(DP), intent(in):: t
        real(DP), dimension(size(this%state%crss,2)), intent(in):: slip_rates

        real(DP):: spin(3,3), &
                   rot_inc(3,3), &
                   imposed_spin_rate(3,3)


        !Spin consists of a part counteracting the spin component of the slip systems and imposed spin rate
        !Note that imposed_spin_rate is actually an approximation of the 'external' spin caused by the velocity gradient.
        !To get the exact value, you must compute the deformation gradient increment as e^Lt, perform polar decomposition to get R
        !And take the matrix logarithm and divide by t.
        !This is needed because the stretch in L interacts nonlinearly with the rotation.
        !For small t, this effect is however negligible. This has been tested extensively.
        imposed_spin_rate = (v_grad - transpose(v_grad)) / 2._DP
        spin = UNIT_MATRIX_3X3+(spin_to_tensor(matmul(this%model%spin_coeffs, slip_rates)) .fromframe. this%orientation) - imposed_spin_rate

        this%orientation = matmul(spin, this%orientation)

        !rot_inc = matrix_exponential(spin*t)
        !this%orientation = matmul(this%orientation, rot_inc) !Opposite order due to passive convention

        call this%model%deform(this%state, t, slip_rates)
    end subroutine
end module
