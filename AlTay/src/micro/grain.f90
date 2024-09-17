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
        real(DP), dimension(:,:), allocatable:: spin_coeffs

        !Pointers to useful quantities
        !These are allocated at the cluster level for efficiency.
        !Technically these pointers are superfluous but they convenient for many calculations and also improve performance.
        real(DP), dimension(:), pointer, contiguous:: overstress, &
                                                      rss
        real(DP), dimension(:,:), pointer:: crss, &
                                            taylor_coeffs


        real(DP), dimension(5):: imposed_strain

    contains
        procedure:: init => grain_init
        procedure:: get_work_rate => grain_get_work_rate
    end type grain

    integer                                    :: nrStep = 0       !<State variable: step number.

    public  ::  grain, &
                nrStep

contains


    !Initialize grain.
    subroutine grain_init(this, deformation_mechanism, orientation, rss, crss, taylor_coeffs)
        class(Grain), target, intent(inout)::                                           this
        integer, dimension(:,:,:), intent(in)::                                         deformation_mechanism
        real(DP), dimension(:), intent(in)::                                            orientation
        real(DP), dimension(:), target, intent(in)::       rss
        real(DP), dimension(:,:), target, intent(in)::    crss
        real(DP), dimension(:,:), target, intent(inout)::    taylor_coeffs

        integer:: i, &
                  n_slip_systems

        n_slip_systems = size(deformation_mechanism, 3)

        !Assign pointers
        this%rss            => rss
        this%crss           => crss
        this%taylor_coeffs  => taylor_coeffs

        !Initialize grain orientation matrix
        this%orientation = from_euler_angles(orientation)
        this%sum_slip = 0._DP

        !Allocate slip systems and spin coefficients
        allocate(this%slip_systems(n_slip_systems))
        allocate(this%spin_coeffs(3, n_slip_systems))

        !Initialize each of the slip systems
        do i = 1, n_slip_systems
            call this%slip_systems(i)%init(deformation_mechanism(:,:,i), &
                                           taylor_coeffs(:,i),           &
                                           this%spin_coeffs(:,i),        &
                                           rss(i),                       &
                                           crss(:,i))
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

end module
