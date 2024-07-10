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
        real(DP), dimension(3, 3):: stress, &
                                    orientation
        type(SlipSystem), dimension(:), allocatable:: slip_systems
    end type grain

    type(Grain), dimension(:), allocatable, target:: grains             !<State variable: array of grains/orientations.
    integer                                    :: nrStep = 0       !<State variable: step number.
    real(DP):: deformation_gradient(3, 3)

    public  ::  grain, &
                grains,       &
                nrStep,     &
                dynfil_init,    &
                dynfil_finalize, &
                deformation_gradient

contains
    subroutine dynfil_init(fname, n_slip_systems)
        character(*), intent(in)    :: fname
        integer, intent(in):: n_slip_systems
        integer                     :: nunit, info, nrec, nstap, i
        character(40):: title
        real(DP):: angles(3), stap, weight, initial_sum_slip
        character(*), parameter:: PROC_NAME = 'load_texture'

        open(newunit = nunit, file = trim(fname), status='old',form='formatted',iostat = info)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open textrure file')

        nrec = 0
        read (nunit, 94, iostat = info) nrec, title
94      format(I5, 5x, A)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file header')
        if (nrec > 0) allocate(grains(nrec))

        do i = 1, nrec
            read(nunit, 96, iostat = info) angles(3), angles(2), angles(1), stap, nstap, weight, initial_sum_slip
96          format(4F10.0, I5, 5X, 2F10.0)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read boundary segment')
            angles = angles/RAD_TO_DEG

            grains(i)%sum_slip = initial_sum_slip
            grains(i)%orientation = from_euler_angles(angles)
            allocate(grains(i)%slip_systems(n_slip_systems))
        enddo

        close(nunit)
    end subroutine

        !>Puts the module variables into initial state and deallocates the storage.
    subroutine DYNFIL_finalize(info)
        integer, intent(out)    :: info
        info = 0
        NRSTEP = 0
        if (allocated(grains)) deallocate(grains, stat = info)
    end subroutine

end module
