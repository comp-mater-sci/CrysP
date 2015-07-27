module altayStateTypes
use altayTexAccess
use criErrcodes
use criMathUtils
use altayTexAccess
use altayHardTypes
use altayCRSSTypes
use altayHardLaw_KM
#ifdef PEBP_ENABLED
use altayHardLaw_DSH, only: DSHStateVariable => StatVar
#endif
implicit none




    !> Basic state of any individual grain
    type :: GrainState
        
        !> Associated orientation
        type(DiscreteOrientation),pointer  :: orientation => null()
        
        !> Accumulated shear deformation (including both slip and twinning),
        !> from a reference (virgin) state up to the current state. 
        double precision                   :: accumulatedshear = 0.0D0

#ifdef PEBP_ENABLED
        !> State variables of the DSH hardening law.
        type(DSHStateVariable)             :: dsh_state
#endif

        !> State variables of the Kocks-Mecking hardening law.
        type(KMStateVariables)             :: km_state
        
        !> \fixme: decide whether the crss is actually needed.
        !> Collection of CRSS per grain (useful for some/all hardening models?)
        ! type(CRSSData)                   :: crss   
        
    end type
    

    type :: GrainStateCollection
        
        type(GrainState), dimension(:), allocatable :: grainstate
        
    contains
    
        procedure :: initialize => GrainStateCollection_initialize
        
    end type
    
    
    type :: MesostructureState
        
        double precision, dimension (3,3) :: deformationgradient = unit_sr_matrix

    contains
    
        procedure :: update => MesostructureState_update      
        
    end type



    !> Container for the state variables
    type :: altayStateVariables
        
        type(MesostructureState)                     :: mesostructure
        
        !> Orientations of discrete ODF. 
        type(DiscreteODF)                       :: texture
        
        !> Collection of the state of grains
        type(GrainStateCollection)              :: grainstates
                
    end type


    !> \todo is this structure usefull? keep, modify or remove?
    type :: ClusterState
        
        !type(Grain),dimension(:),pointer :: grain !=> null()
        
        !> \todo add the appropriate grain boundary data: euler angles + weighting factor
        !type(EulerAngles) :: GBeuler

    end type


contains
    
    subroutine GrainStateCollection_initialize( this, texture, info)
    implicit none
    class(GrainStateCollection), intent(inout) :: this
    type(DiscreteODF), intent(in), target      :: texture
    integer, intent(out)                       :: info
    !
    ! Number of grains in the collection
    integer :: n_grains = 0
    ! Running index, error code
    integer :: i, ierr
    !
        info = criErr_BadDims
        !Allocation
        n_grains = size(texture%orientations)
        if (n_grains <= 0) return
        ! Two paths: initialize from scratch or refresh.
        if (.not. allocated(this%grainstate)) then
            info = criErr_MemAlloc
            allocate(this%grainstate(n_grains), stat=ierr)
            if (ierr /= 0) return
        else
            ! The size must conform with n_grains
            if (size(this%grainstate) /= n_grains) return
        endif
        !Pointer assignments - keep the order as is
        do i=1,n_grains
            this%grainstate(i)%orientation => texture%orientations(i)
        end do
        info = criSuccess
        !
    end subroutine

    subroutine MesostructureState_update( this, incremental_defgrad, info)
    implicit none
    class(MesostructureState), intent(inout)     :: this
    double precision, dimension(3,3), intent(in) :: incremental_defgrad
    integer, intent(out)                         :: info
    !
        info = criError
        this%deformationgradient = matmul(incremental_defgrad,this%deformationgradient)
        info = criSuccess
        !
    end subroutine

    
end module