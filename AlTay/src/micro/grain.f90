module grain_module
    use utils
    use logging
    use slip_systems

    implicit none

    private
    public  ::  Grain, &
                HardeningState

    !>Abstract container for hardening state data.
    !>Each hardening model may extend this type to implement the grain-specific state it wants to track.
    !>Must be implemented this way because at the higher levels we want to mix grains of different phases (and thus types of state)
    !and Fortran does not allow list of heterogeneous type.
    type, abstract:: HardeningState
    end type

    !>Texture-related state variables for single grain
    type:: Grain
        real(DP), dimension(3, 3):: orientation
        type(SlipSystem), dimension(:), allocatable:: slip_systems
        class(HardeningState), allocatable:: hardening_state
    contains
        procedure:: init              => grain_init
        procedure:: get_taylor_coeffs => grain_get_taylor_coeffs
        procedure:: get_spin_coeffs   => grain_get_spin_coeffs
        procedure:: get_crss          => grain_get_crss
    end type grain

contains

    !Initialize grain.
    subroutine grain_init(this, deformation_mechanism, orientation)
        class(Grain), target, intent(inout)::    this
        integer, dimension(:,:,:), intent(in)::  deformation_mechanism
        real(DP), dimension(:), intent(in)::     orientation

        integer:: i, &
                  n_slip_systems

        n_slip_systems = size(deformation_mechanism, 3)

        !Initialize grain orientation matrix
        this%orientation = from_euler_angles(orientation)

        !Allocate slip systems and spin coefficients
        allocate(this%slip_systems(n_slip_systems))

        !Initialize each of the slip systems
        do i = 1, n_slip_systems
            call this%slip_systems(i)%init(deformation_mechanism(:,:,i))
        end do
    end subroutine

    function grain_get_taylor_coeffs(this) result(coeffs)
        class(Grain), intent(in):: this
        real(DP), dimension(5, size(this%slip_systems)):: coeffs

        integer:: i

        do i = 1, size(this%slip_systems)
            coeffs(:,i) = this%slip_systems(i)%taylor_coeffs
        end do
    end function

    function grain_get_spin_coeffs(this) result(coeffs)
        class(Grain), intent(in):: this
        real(DP), dimension(3, size(this%slip_systems)):: coeffs

        integer:: i

        do i = 1, size(this%slip_systems)
            coeffs(:,i) = this%slip_systems(i)%spin_coeffs
        end do
    end function

    function grain_get_crss(this) result(crss)
        class(Grain), intent(in):: this
        real(DP), dimension(2, size(this%slip_systems)):: crss

        integer:: i

        do i = 1, size(this%slip_systems)
            crss(:,i) = this%slip_systems(i)%crss
        end do
    end function
end module
