module altayDynfil
    use utils
    use logging
    use criMathUtils

    implicit none
    private

    character(*), parameter :: MOD_NAME = 'dynfil'

    !>Texture-related state variables for single grain
    type :: grain
        real(DP)                    :: tGEW     = 1._DP, &
                                       tGAM     = 0._DP
        real(DP), dimension(3,3)    :: tT       = 0._DP
        real(DP), dimension(3,3)    :: tTAX     = unit_sr_matrix
        real(DP), dimension(3,3)    :: tZERO    = 0._DP
    end type grain

    type :: matFrame
        real(DP),dimension(3,3)   :: FALG   = unit_sr_matrix
        real(DP),dimension(3,3)   :: CIJ0   = unit_sr_matrix
        real(DP),dimension(3,3)   :: TAX0   = unit_sr_matrix
        real(DP),dimension(3)     :: GAXES  = 1._DP
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
                dynfil_finalize

contains
    subroutine dynfil_init(fname)
        character(*), intent(in)    :: fname
        integer                     :: nunit, info, nrec, nstap, i
        character(40) :: title
        real(DP) :: angles(3), stap, weight, gam
        character(*), parameter :: PROC_NAME = 'load_texture'

        open(newunit=nunit,file=trim(fname),status='old',form='formatted',iostat=info)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Unable to open textrure file')

        nrec = 0 
        read (nunit, 94, iostat=info) nrec,title 
94      format(I5,5x,A)
        if (info /= VEF_OK) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read texture file header')
        if (nrec > 0) allocate(dfil(nrec))
        
        do i = 1, nrec
            read(nunit,96,iostat=info) angles(3),angles(2),angles(1),stap,nstap,weight,gam
96          format(4F10.0,I5,5X,2F10.0)
            if (info /= 0) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Could not read boundary segment')
            angles = angles * pi_deg
            dfil(i) = grain(weight, gam, rotmat(angles), mf%tax0, 0._DP)
        enddo
    
        close(nunit)
    end subroutine

    !>Puts the module variables into initial state and deallocates the storage.
    subroutine DYNFIL_finalize(info)
        integer, intent(out)    :: info
        info = 0
        mf = matFrame()
        NRSTEP = 0
        if (allocated(DFIL)) deallocate(DFIL,stat=info)
    end subroutine

    !> Extract the global material data
    subroutine DYNFIL_getGlobal(F, CIJ)
        real(DP), dimension(3,3), intent(out)   :: CIJ, F

        F = mf%FALG
        CIJ = mf%CIJ0
    end subroutine

    !> Write the global material data
    subroutine DYNFIL_setGlobal(F, axes, CIJ, tax)
        real(DP), dimension(3), intent(in)      :: axes
        real(DP), dimension(3,3), intent(in)    :: CIJ, tax, F

        mf%FALG = F
        mf%GAXES = AXES
        mf%CIJ0 = CIJ
        mf%TAX0 = TAX
    end subroutine

    !> Get the record data for i-th grain
    subroutine DYNFIL_getGrain(i, T, GEW, GAM, TAX, ZERO)
        integer, intent(in)                             :: i
        real(DP), intent(out)                   :: GEW,GAM
        real(DP), dimension(3,3), intent(out)   :: TAX, T, ZERO

        GEW     = DFIL(i)%tGEW
        GAM     = DFIL(i)%tGAM
        T       = DFIL(i)%tT
        TAX     = DFIL(i)%tTAX
        ZERO    = DFIL(i)%tZERO
    end subroutine

    !> Put the record data for i-th grain
    subroutine DYNFIL_setGrain(i, T, GEW, GAM, TAX, ZERO)
        integer, intent(in)                     :: i
        real(DP), intent(in)                    :: GEW,GAM
        real(DP), dimension(3,3), intent(in)    :: TAX, T, ZERO

        DFIL(i)%tGEW    = GEW
        DFIL(i)%tGAM    = GAM
        DFIL(i)%tT      = T
        DFIL(i)%tTAX    = TAX
        DFIL(i)%tZERO   = ZERO
    end subroutine
end module
