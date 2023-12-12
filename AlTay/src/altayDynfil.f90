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
                                        tTAX     = UNIT_MATRIX_3X3, &
                                        tZERO    = 0._DP, &
                                        boundary_transformation_matrix, &
                                        boundary_reference_frame
    end type grain

    type:: matFrame
        real(DP), dimension(3, 3)   :: FALG   = UNIT_MATRIX_3X3
        real(DP), dimension(3, 3)   :: CIJ0   = UNIT_MATRIX_3X3
        real(DP), dimension(3, 3)   :: TAX0   = UNIT_MATRIX_3X3
        real(DP), dimension(3)     :: GAXES  = 1._DP
    end type

    type(grain), dimension(:), allocatable     :: DFIL             !<State variable: array of grains/orientations.
    type(matFrame), public, protected          :: mf               !<State variable: material (frame) global geometry
    integer                                    :: nrStep = 0       !<State variable: step number.

    public  ::  grain, &
                DFIL,       &
                nrStep,     &
                dynfil_init,    &
                dynFil_getGlobal,    &
                dynFil_setGlobal,    &
                dynFil_getGrain,    &
                dynFil_setGrain,    &
                dynfil_finalize, &
                read_microstructure

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
        if (nrec > 0) allocate(dfil(nrec))
        
        do i = 1, nrec
            read(nunit, 96, iostat = info) angles(3), angles(2), angles(1), stap, nstap, weight, gam
96          format(4F10.0, I5, 5X, 2F10.0)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read boundary segment')
            angles = angles*pi_deg
            dfil(i) = grain(weight, gam, rotmat(angles), mf%tax0, 0._DP, 0._DP, 0._DP)
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
            transformation_matrix = matmul(initial_deformation_gradient, transpose(rotmat(deg2rad(angles))))

            !Assign boundaries to a pair of grains
            do j = 2*i-1, size(DFIL)-1, 2*n_boundaries
                DFIL(j)%boundary_transformation_matrix = transformation_matrix
            end do
        enddo

        close(unit = file_handle)
    end subroutine read_microstructure

    !>Puts the module variables into initial state and deallocates the storage.
    subroutine DYNFIL_finalize(info)
        integer, intent(out)    :: info
        info = 0
        mf = matFrame()
        NRSTEP = 0
        if (allocated(DFIL)) deallocate(DFIL, stat = info)
    end subroutine

    !> Extract the global material data
    subroutine DYNFIL_getGlobal(F, CIJ)
        real(DP), dimension(3, 3), intent(out)   :: CIJ, F

        F = mf%FALG
        CIJ = mf%CIJ0
    end subroutine

    !> Write the global material data
    subroutine DYNFIL_setGlobal(F, axes, CIJ, tax)
        real(DP), dimension(3), intent(in)      :: axes
        real(DP), dimension(3, 3), intent(in)    :: CIJ, tax, F

        mf%FALG = F
        mf%GAXES = AXES
        mf%CIJ0 = CIJ
        mf%TAX0 = TAX
    end subroutine

    !> Get the record data for i-th grain
    subroutine DYNFIL_getGrain(i, T, GEW, gam, TAX, ZERO)
        integer, intent(in)                             :: i
        real(DP), intent(out)                   :: GEW, gam
        real(DP), dimension(3, 3), intent(out)   :: TAX, T, ZERO

        GEW     = DFIL(i)%tGEW
        gam = DFIL(i)%tgam
        T       = DFIL(i)%tT
        TAX     = DFIL(i)%tTAX
        ZERO    = DFIL(i)%tZERO
    end subroutine

    !> Put the record data for i-th grain
    subroutine DYNFIL_setGrain(i, T, GAM, TAX, ZERO)
        integer, intent(in)                     :: i
        real(DP), intent(in)                    :: GAM
        real(DP), dimension(3, 3), intent(in)    :: TAX, T, ZERO

        DFIL(i)%tGAM    = GAM
        DFIL(i)%tT      = T
        DFIL(i)%tTAX    = TAX
        DFIL(i)%tZERO   = ZERO
    end subroutine

    !Determine the boundary reference frame and the transformation matrices between boundary frame and crystal frame and the
    !relative weight of the cluster.
end module
