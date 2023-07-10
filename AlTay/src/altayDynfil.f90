module altayDynfil
    use definitions
    use criMathUtils

    implicit none
    private

    !>Texture-related state variables for single grain
    type :: grain
        real(dp)                    :: tGEW     = 1.D0, &
                                        tGAM    = 0.D0
        real(dp), dimension(3,3)    :: tT       = 0.D0
        real(dp), dimension(3,3)    :: tTAX     = unit_sr_matrix
        real(dp), dimension(3,3)    :: tZERO    = 0.D0
    end type grain

    type :: matFrame
        real(dp),dimension(3,3)   :: FALG   = unit_sr_matrix
        real(dp),dimension(3,3)   :: CIJ0   = unit_sr_matrix
        real(dp),dimension(3,3)   :: TAX0   = unit_sr_matrix
        real(dp),dimension(3)     :: GAXES  = 1.D0
    end type

    type(grain), dimension(:), allocatable     :: DFIL             !<State variable: array of grains/orientations.
    type(matFrame), public, protected          :: mf               !<State variable: material (frame) global geometry
    integer                                    :: nrStep = 0       !<State variable: step number.

    public  ::  DFIL,       &
                nrStep,     &
                grain,      &
                dynfil_init,    &
                dynFil_getGlobal,    &
                dynFil_setGlobal,    &
                dynFil_getGrain,    &
                dynFil_setGrain,    &
                dynfil_finalize

contains

    !> Allocate the memory block for the state variables.
    subroutine dynfil_init(grains)
        type(grain), dimension(:), allocatable, intent(in)  :: grains
        integer :: i

        dfil = grains
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
        real(dp), dimension(3,3), intent(out)   :: CIJ, F

        F = mf%FALG
        CIJ = mf%CIJ0
    end subroutine

    !> Write the global material data
    subroutine DYNFIL_setGlobal(F, axes, CIJ, tax)
        real(dp), dimension(3), intent(in)      :: axes
        real(dp), dimension(3,3), intent(in)    :: CIJ, tax, F

        mf%FALG = F
        mf%GAXES = AXES
        mf%CIJ0 = CIJ
        mf%TAX0 = TAX
    end subroutine

    !> Get the record data for i-th grain
    subroutine DYNFIL_getGrain(i, T, GEW, GAM, TAX, ZERO)
        integer, intent(in)                             :: i
        real(dp), intent(out)                   :: GEW,GAM
        real(dp), dimension(3,3), intent(out)   :: TAX, T, ZERO

        GEW     = DFIL(i)%tGEW
        GAM     = DFIL(i)%tGAM
        T       = DFIL(i)%tT
        TAX     = DFIL(i)%tTAX
        ZERO    = DFIL(i)%tZERO
    end subroutine

    !> Put the record data for i-th grain
    subroutine DYNFIL_setGrain(i, T, GEW, GAM, TAX, ZERO)
        integer, intent(in)                     :: i
        real(dp), intent(in)                    :: GEW,GAM
        real(dp), dimension(3,3), intent(in)    :: TAX, T, ZERO

        DFIL(i)%tGEW    = GEW
        DFIL(i)%tGAM    = GAM
        DFIL(i)%tT      = T
        DFIL(i)%tTAX    = TAX
        DFIL(i)%tZERO   = ZERO
    end subroutine

end module
