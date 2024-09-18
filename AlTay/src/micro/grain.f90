module grain_module
    use utils
    use logging
    use slip_systems

    implicit none
    private

    character(*), parameter:: MOD_NAME = 'dynfil'

    !>Texture-related state variables for single grain
    type:: grain
        real(DP)::                  sum_slip = 0._DP
        real(DP), dimension(3, 3):: orientation, &
                                    stress_state
        type(SlipSystem), dimension(:), allocatable:: slip_systems
        real(DP), dimension(5):: imposed_strain

    contains
        procedure:: init => grain_init
        procedure:: get_work_rate => grain_get_work_rate
        procedure:: get_taylor_coeffs => grain_get_taylor_coeffs
        procedure:: get_spin_coeffs => grain_get_spin_coeffs
        procedure:: get_crss => grain_get_crss
        procedure:: set_crss => grain_set_crss
    end type grain

    integer                                    :: nrStep = 0       !<State variable: step number.

    public  ::  grain, &
                nrStep

contains


    !Initialize grain.
    subroutine grain_init(this, deformation_mechanism, orientation)
        class(Grain), target, intent(inout)::                                           this
        integer, dimension(:,:,:), intent(in)::                                         deformation_mechanism
        real(DP), dimension(:), intent(in)::                                            orientation

        integer:: i, &
                  n_slip_systems

        n_slip_systems = size(deformation_mechanism, 3)

        !Initialize grain orientation matrix
        this%orientation = from_euler_angles(orientation)
        this%sum_slip = 0._DP

        !Allocate slip systems and spin coefficients
        allocate(this%slip_systems(n_slip_systems))

        !Initialize each of the slip systems
        do i = 1, n_slip_systems
            call this%slip_systems(i)%init(deformation_mechanism(:,:,i))
        end do
    end subroutine

    real(DP) function grain_get_work_rate(this) result(work_rate)
        class(Grain), intent(in):: this

        integer:: i

        work_rate = 0._DP
        do i = 1, size(this%slip_systems)
            work_rate = work_rate+this%slip_systems(i)%get_work_rate()
        end do
    end function

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

    subroutine grain_set_crss(this, crss)
        class(Grain), intent(inout):: this
        real(DP), dimension(2, size(this%slip_systems)):: crss

        integer:: i

        do i = 1, size(this%slip_systems)
            this%slip_systems(i)%crss = crss(:,i)
        end do

    end subroutine

end module
