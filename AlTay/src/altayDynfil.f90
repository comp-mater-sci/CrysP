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

    public  ::  DFIL,       &
                nrStep,     &
                dynfil_init,    &
                dynFil_getGlobal,    &
                dynFil_setGlobal,    &
                dynFil_getGrain,    &
                dynFil_setGrain,    &
                dynfil_finalize, &
                read_microstructure, &
                update_cluster_state

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
            dfil(i) = grain(weight, gam, euler_angles_to_rotation_matrix(angles), mf%tax0, 0._DP, 0._DP, 0._DP)
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
            transformation_matrix = matmul(initial_deformation_gradient, transpose(euler_angles_to_rotation_matrix(deg2rad(angles))))

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
    subroutine update_cluster_state(grain_, deformation_gradient, von_mises_strain_mode)
        type(Grain), intent(inout):: grain_
        real(DP), intent(in):: deformation_gradient(3, 3), &
                               von_mises_strain_mode(3, 3)
        real(DP)::             Tprinc(3, 3)
        real(DP):: GRPAR(3, 3), PrDir(2, 3), TDCGr(3, 3), vec1(3), vec2(3), AL(3)  ! TDC is the normalized von-Mises equivalent strain rate
        real(DP):: u, dlength, dot1, dot2, TGANGLE, GEWF
        integer:: i
       
        GRPAR = matmul(deformation_gradient, grain_%boundary_transformation_matrix)
        ! Calculation of volume affected by the surface
        AL = norm2(GRPAR, 1)
        vec1 = cross(GRPAR(:,2), GRPAR(:,3))
        ! The factor 0.25 is there so that for equiaxed grains, GEWF below becomes 1/3; 
        ! for very flattened grains, it should tend to 1.
        u = abs(sum(GRPAR(:,1)*vec1))*0.25D0/product(AL)

        if (minloc(AL, 1) == 3) then
            GEWF = u*(4.D0*(AL(1)-AL(3))*(AL(2)-AL(3))*AL(3)  &
                    +2.0D0*(AL(1)+AL(2)-2.0_dp*AL(3))*AL(3)**2 &
                    +4.D0*AL(3)**3/3.D0)
        else
            GEWF = u*merge(2.D0*(AL(1)-AL(2))*AL(2)**2+4.D0*AL(2)**3/3.D0, &
                           2.D0*(AL(2)-AL(1))*AL(1)**2+4.D0*AL(1)**3/3.D0, &
                           AL(1) >= AL(2))
        endif

        ! Construction of orientation matrices for frames associated to the interfaces
        ! Orientation of interfaces containing axes
        Tprinc(1, 1:3)=GRPAR(1:3, 1)
        Tprinc(3, 1:3)=cross(GRPAR(:,1), GRPAR(:,2))
        Tprinc(2, 1:3)=cross(Tprinc(3, :), Tprinc(1, :))
        ! Normalisation
        do i = 1, 3
            Tprinc(i, :)=Tprinc(i, :)/norm2(Tprinc(i, :))
        enddo

        dlength = norm2(von_mises_strain_mode)
        TDCGr = rotateSRTensorFrom(von_mises_strain_mode, Tprinc)

        dot1 = sum(RELAXATIONS(:,:,1) * TDCGr) / sqrt(2.0D0) / dlength
        dot2 = sum(RELAXATIONS(:,:,2) * TDCGr) / sqrt(2.0D0) / dlength

        if(abs(dot1) < 0.000001_DP .and. abs(dot2) >= 0.000001_DP) then
            !  Need to rotate current frame (represented by Tprinc) with 90 degree to let relaxation-2 be the orthogonal one
            vec1 = Tprinc(2, 1:3)
            Tprinc(2, 1:3)=-Tprinc(1, 1:3)
            Tprinc(2, 1:3)=vec1
        elseif(abs(dot1) >= 0.000001_DP .and. abs(dot2) >= 0.000001_DP) then
            ! need to rotate by a angle < 90 (this angle could be positive or negative)
            tgangle = dot2/dot1
            PrDir = 0.0_DP
            PrDir(1, 1)=1.D0/sqrt(1.D0+tgangle**2)
            PrDir(1, 2)=tgangle/sqrt(1.D0+tgangle**2)
            PrDir(2, 1)=-PrDir(1, 2)
            PrDir(2, 2)=PrDir(1, 1)
            ! Prdir(n, :) is vector-n in the GB frame
            ! Transform these two vector in the Sample's frame
            vec1 = 0.0_DP
            vec2 = 0.0_DP
            do i = 1, 3
                vec1(i)=vec1(i)+sum(Tprinc(:,i)*PrDir(1, :))
                vec2(i)=vec2(i)+sum(Tprinc(:,i)*PrDir(2, :))
            enddo
            Tprinc(1, 1:3)=vec1
            Tprinc(2, 1:3)=vec2
            
        endif

        grain_%boundary_reference_frame = Tprinc
        grain_%tgew = GEWF        
    end subroutine

end module
