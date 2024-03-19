module altayDynfil
    use utils
    use logging
    use criMathUtils
    use slip_systems

    implicit none
    private

    character(*), parameter:: MOD_NAME = 'dynfil'

    !>Texture-related state variables for single grain
    type:: grain
        real(DP)                    :: tGEW     = 1._DP, &
                                       tGAM     = 0._DP
        real(DP), dimension(3, 3)    :: tT       = 0._DP, &
                                        boundary_reference_frame !> Rotation matrix for boundary frame in ACTIVE notation (for performance)
    end type grain

    type(grain), dimension(:), allocatable     :: grains             !<State variable: array of grains/orientations.
    integer                                    :: nrStep = 0       !<State variable: step number.
    real(DP):: deformation_gradient(3, 3)

    public  ::  grain, &
                grains,       &
                nrStep,     &
                dynfil_init,    &
                dynFil_getGrain,    &
                dynFil_setGrain,    &
                dynfil_finalize, &
                read_microstructure, &
                deformation_gradient

contains
    subroutine dynfil_init(fname)
        character(*), intent(in)    :: fname
        integer                     :: nunit, info, nrec, nstap, i
        character(40):: title
        real(DP):: angles(3), stap, weight, gam
        character(*), parameter:: PROC_NAME = 'load_texture'

        open(newunit = nunit, file = trim(fname), status='old',form='formatted',iostat = info)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open textrure file')

        nrec = 0 
        read (nunit, 94, iostat = info) nrec, title 
94      format(I5, 5x, A)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file header')
        if (nrec > 0) allocate(grains(nrec))
        
        do i = 1, nrec
            read(nunit, 96, iostat = info) angles(3), angles(2), angles(1), stap, nstap, weight, gam
96          format(4F10.0, I5, 5X, 2F10.0)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read boundary segment')
            angles = angles/RAD_TO_DEG
            grains(i) = grain(weight, gam, euler_angles_to_rotation_matrix(angles), 0._DP)
        enddo
    
        close(nunit)
    end subroutine

    subroutine read_microstructure(file_name, initial_deformation_gradient)
        character(len=*), intent(in):: file_name
        real(DP), dimension(3, 3), intent(in):: initial_deformation_gradient 
        integer           :: file_handle, &
                             n_boundaries, &
                             i, j
        real(DP):: transformation_matrix(3, 3), &
                   angles(3)
        character(len = 40)  :: TitMic !<Microstructure title

        open (newunit = file_handle, file = file_name, status='old')
        read (file_handle, '(I5, 5x, A)') n_boundaries, TitMic  ! read number of grain boundaries and file title

        do i = 1, n_boundaries
            read (file_handle, '(3f10.0)') angles(3), angles(2), angles(1)  !read Euler angles from microstructure file in order: phi2, PHI, phi1
            !Calculate the transformation matrix
            !Cols 1 and 2 hold two non-parallel vectors within the initial GB (grain boundary) plane.
            !Col 3 holds a vector out of the initial GB plane (not necessarily perpendicular to the GB plane).
            transformation_matrix = matmul(initial_deformation_gradient, transpose(euler_angles_to_rotation_matrix(angles/RAD_TO_DEG)))

            !Assign boundaries to a pair of grains
            do j = 2*i-1, size(grains)-1, 2*n_boundaries
                grains(j)%boundary_reference_frame = transformation_matrix
            end do
        enddo

        close(unit = file_handle)
    end subroutine read_microstructure

    !>Puts the module variables into initial state and deallocates the storage.
    subroutine DYNFIL_finalize(info)
        integer, intent(out)    :: info
        info = 0
        NRSTEP = 0
        if (allocated(grains)) deallocate(grains, stat = info)
    end subroutine

    !> Get the record data for i-th grain
    subroutine DYNFIL_getGrain(i, T, GEW, gam)
        integer, intent(in)                             :: i
        real(DP), intent(out)                   :: GEW, gam
        real(DP), dimension(3, 3), intent(out)   :: T

        GEW     = grains(i)%tGEW
        gam = grains(i)%tgam
        T       = grains(i)%tT
    end subroutine

    !> Put the record data for i-th grain
    subroutine DYNFIL_setGrain(i, T, GAM)
        integer, intent(in)                     :: i
        real(DP), intent(in)                    :: GAM
        real(DP), dimension(3, 3), intent(in)    :: T

        grains(i)%tGAM    = GAM
        grains(i)%tT      = T
    end subroutine
end module
