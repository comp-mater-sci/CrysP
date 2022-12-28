module altayDynfil
    use altayMiscutils
    use altay_definitions
    use criMathUtils
    use altayIOConfig

    implicit none
    private

    !>Texture-related state variables for single grain
    type :: grain
        real(dp)                    :: tFI1     = 0.D0, tPHI    = 0.D0, tFI2 = 0.D0    !<Euler angles
        real(dp)                    :: tGEW     = 1.D0, tGAM    = 0.D0
        real(dp), dimension(3)      :: tAXES    = 1.D0, tEULR   = 0.D0
        real(dp), dimension(3,3)    :: tT       = 0.D0
        real(dp), dimension(3,3)    :: tF       = unitMatrix
        real(dp), dimension(3,3)    :: tCIJ     = unitMatrix
        real(dp), dimension(3,3)    :: tTAX     = unitMatrix
        real(dp), dimension(3,3)    :: tZERO    = 0.D0, tRHO    = 0.D0
    end type grain

    type :: matFrame
        real(dp),dimension(3,3)   :: FALG   = unitMatrix
        real(dp),dimension(3,3)   :: CIJ0   = unitMatrix
        real(dp),dimension(3,3)   :: TAX0   = unitMatrix
        real(dp),dimension(3)     :: GAXES  = 1.D0, GEULR = 0.D0
    end type

    type(grain), dimension(:), allocatable     :: DFIL             !<State variable: array of grains/orientations.
    type(matFrame)                             :: mf               !<State variable: material (frame) global geometry
    character(len=40)                          :: filetitle = ''   !<State variable: title of the input texture file
    integer                                    :: nrStep = 0       !<State variable: step number.

    public  ::  DFIL,       &
                mf,         &
                nrStep,     &
                fileTitle,  &
                initFields, &
                dynFil0,    &
                dynFil2,    &
                dynFil3,    &
                dynFil4,    &
                dynFil5,    &
                dynfil_finalize

contains

    !> Allocate the memory block for the state variables.
    subroutine DYNFIL0(npoint,keepstate,istat)

        integer, intent(in)                     :: npoint       !<Number of elements to be allocated
        logical, intent(in)                     :: keepstate    !<Flag: preserve contenst of DFIL on reallocation.
        integer, intent(out)                    :: istat        !<Exit code
        type(grain), dimension(:), allocatable  :: tmp
        integer                                 :: ntransf

        istat = 1
        ! Error handling
        if (npoint <= 0) then
            if(NLIST.eq.1) write(IMP,100)
            return
        endif

        if (.not. allocated(DFIL)) then
            allocate(DFIL(npoint),stat=istat)
        else
            ! DFIL is previously allocated
            if (size(DFIL) == npoint) then
                istat = 0
                return
            endif
            if (keepstate) then
                ! Transfer npoints
                allocate(tmp(npoint),stat=istat)
                if (istat == 0) then
                    ntransf = min(npoint,size(DFIL))
                    tmp(1:ntransf) = DFIL(1:ntransf)
                    call move_alloc(tmp,DFIL)
                endif
            else
                deallocate(DFIL)
                allocate(DFIL(npoint),stat=istat)
            endif
        endif
        ! Error handling
        if (istat /= 0 .and. NLIST.eq.1) write(IMP,101)

100     format('DYNFIL0: error: requested number of grains is zero.')
101     format('DYNFIL0: error: allocation of memory failed.')
    end subroutine

    !>Puts the module variables into initial state and deallocates the storage.
    subroutine DYNFIL_finalize(info)
        integer, intent(out)    :: info
        info = 0
        mf = matFrame()
        filetitle = ''
        NRSTEP = 0
        if (allocated(DFIL)) deallocate(DFIL,stat=info)
    end subroutine

    !> Extract the global material data
    subroutine DYNFIL2(n, F, AXES, EULR, CIJ, TAX)
        integer, intent(out)                            :: n
        real(dp), dimension(3), intent(out)     :: AXES, EULR
        real(dp), dimension(3,3), intent(out)   :: CIJ, TAX, F

        n = nrstep
        F = mf%FALG
        AXES = mf%GAXES
        EULR = mf%GEULR
        CIJ = mf%CIJ0
        TAX = mf%TAX0
    end subroutine

    !> Write the global material data
    subroutine DYNFIL3(n, F, axes, eulr, CIJ, tax)
        integer,intent(in)                              :: n
        real(dp), dimension(3), intent(in)      :: axes, eulr
        real(dp), dimension(3,3), intent(in)    :: CIJ, tax, F

        nrstep = n
        mf%FALG = F
        mf%GAXES = AXES
        mf%GEULR = EULR
        mf%CIJ0 = CIJ
        mf%TAX0 = TAX
    end subroutine

    !> Get the record data for i-th grain
    subroutine DYNFIL4(i, FI1, PHI, FI2, T, GEW, GAM, F, AXES, EULR, CIJ, TAX, ZERO)
        integer, intent(in)                             :: i
        real(dp), intent(out)                   :: FI1,PHI,FI2,GEW,GAM
        real(dp), dimension(3), intent(out)     :: AXES, EULR
        real(dp), dimension(3,3), intent(out)   :: CIJ, TAX, F, T, ZERO

        FI1     = DFIL(i)%tFI1
        PHI     = DFIL(i)%tPHI
        FI2     = DFIL(i)%tFI2
        GEW     = DFIL(i)%tGEW
        GAM     = DFIL(i)%tGAM
        AXES    = DFIL(i)%tAXES
        EULR    = DFIL(i)%tEULR
        T       = DFIL(i)%tT
        F       = DFIL(i)%tF
        CIJ     = DFIL(i)%tCIJ
        TAX     = DFIL(i)%tTAX
        ZERO    = DFIL(i)%tZERO
    end subroutine

    !> Put the record data for i-th grain
    subroutine DYNFIL5(i, FI1, PHI, FI2, T, GEW, GAM, F, AXES, EULR, CIJ, TAX, ZERO)
        integer, intent(in)                             :: i
        real(dp), intent(in)                    :: FI1,PHI,FI2,GEW,GAM
        real(dp), dimension(3), intent(in)      :: AXES, EULR
        real(dp), dimension(3,3), intent(in)    :: CIJ, TAX, F, T, ZERO

        DFIL(i)%tFI1    = FI1
        DFIL(i)%tPHI    = PHI
        DFIL(i)%tFI2    = FI2
        DFIL(i)%tGEW    = GEW
        DFIL(i)%tGAM    = GAM
        DFIL(i)%tAXES   = AXES
        DFIL(i)%tEULR   = EULR
        DFIL(i)%tT      = T
        DFIL(i)%tF      = F
        DFIL(i)%tCIJ    = CIJ
        DFIL(i)%tTAX    = TAX
        DFIL(i)%tZERO   = ZERO
    end subroutine

    !> Set the computed fields in grain structure.
    subroutine initFields(mf,gr)
        type(matFrame), intent(in)  :: mf
        type(grain), intent(inout)  :: gr

        gr%tT       = rotmat(gr%tfi1,gr%tPHI,gr%tfi2)
        gr%tAXES    = mf%GAXES
        gr%tEULR    = mf%GEULR
        gr%tF       = mf%FALG
        gr%tCIJ     = mf%CIJ0
        gr%tTAX     = mf%TAX0
        gr%tZERO    = 0.D0
        gr%tRHO     = 0.D0
    end subroutine
end module
