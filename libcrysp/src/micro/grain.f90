module crysp_grain
    use conversions
    use logging
    use constitutive_model

    implicit none

    private
    public:: Grain, &
             GrainState

    !> Hardening state object specific to each grain.
    !>
    !> Concrete constitutive models are to extend this type to include fields for whatever grain-specific state they want to track.
    !> @note
    !> It may seem much nicer to simply create subtypes of [[Grain]] with additional fields for hardening state in the concrete constitutive models
    !> but this leads to problems at the meso level because a Cluster must keep a list of Grains that belong to it
    !> and Fortran does not allow lists of heterogeneous type.
    !> @endnote
    type, extends(State):: GrainState
        real(DP), dimension(3, 3):: orientation
        real(DP), dimension(:,:), allocatable:: crss
        real(DP):: stress_increment
    contains
        serialize => grain_serialize
        deserialize => grain_deserialize
    end type

    !>Texture-related state variables for single grain
    type:: Grain
        type(Phase), pointer:: phase
        class(GrainState), allocatable:: state
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

        this%orientation = euler_to_rotation_matrix(orientation)
        this%model => model
        this%state = state
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

    pure function grain_serialize(this) result(params)
        class(GrainState), intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        params = [serialize(this%orientation), &
                  serialize(this%crss), &
                  serialize(this%stress_increment)]
    end function

    function grain_deserialize(this, params) result(params_)
        class(GrainState), intent(out):: this
        type(Parameter), dimension(:), intent(in):: params
        type(Parameter), dimension(:), allocatable:: params_

        this%orientation = params(1)
        this%crss = params(2)
        this%stress_increment = params(3)
        params_ = params .pop. 3
    end function
end module
