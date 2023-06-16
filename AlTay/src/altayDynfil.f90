module altayDynfil
    use altayMiscutils
    use definitions
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
        real(dp), dimension(3,3)    :: tF       = unit_sr_matrix
        real(dp), dimension(3,3)    :: tCIJ     = unit_sr_matrix
        real(dp), dimension(3,3)    :: tTAX     = unit_sr_matrix
        real(dp), dimension(3,3)    :: tZERO    = 0.D0, tRHO    = 0.D0
    end type grain

    type :: matFrame
        real(dp),dimension(3,3)   :: FALG   = unit_sr_matrix
        real(dp),dimension(3,3)   :: CIJ0   = unit_sr_matrix
        real(dp),dimension(3,3)   :: TAX0   = unit_sr_matrix
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
                dynfil_init,    &
                dynFil_getGlobal,    &
                dynFil_setGlobal,    &
                dynFil_getGrain,    &
                dynFil_setGrain,    &
                dynfil_finalize

contains

    !> Allocate the memory block for the state variables.
    subroutine dynfil_init(npoint,keepstate,istat)

        integer, intent(in)                     :: npoint       !<Number of elements to be allocated
        logical, intent(in)                     :: keepstate    !<Flag: preserve contenst of DFIL on reallocation.
        integer, intent(out)                    :: istat        !<Exit code
        type(grain), dimension(:), allocatable  :: tmp
        integer                                 :: ntransf

        istat = 1
        ! Error handling
        if (npoint <= 0) then
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

100     format('dynfil_init: error: requested number of grains is zero.')
101     format('dynfil_init: error: allocation of memory failed.')
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
    subroutine DYNFIL_getGlobal(n, F, AXES, EULR, CIJ, TAX)
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
    subroutine DYNFIL_setGlobal(n, F, axes, eulr, CIJ, tax)
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
    subroutine DYNFIL_getGrain(i, T, GEW, GAM, F, AXES, EULR, CIJ, TAX, ZERO)
        integer, intent(in)                             :: i
        real(dp), intent(out)                   :: GEW,GAM
        real(dp), dimension(3), intent(out)     :: AXES, EULR
        real(dp), dimension(3,3), intent(out)   :: CIJ, TAX, F, T, ZERO

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
    subroutine DYNFIL_setGrain(i, T, GEW, GAM, F, AXES, EULR, CIJ, TAX, ZERO)
        integer, intent(in)                     :: i
        real(dp), intent(in)                    :: GEW,GAM
        real(dp), dimension(3), intent(in)      :: AXES, EULR
        real(dp), dimension(3,3), intent(in)    :: CIJ, TAX, F, T, ZERO

        type(EulerAngles) :: eu

        eu = EulerAnglesType(T)

        DFIL(i)%tFI1    = eu%FI1
        DFIL(i)%tPHI    = eu%PHI
        DFIL(i)%tFI2    = eu%FI2
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
