module altayStateTypes
use altayTexAccess
implicit none


    type :: GrainState
        
        !> Associated orientation
        type(DiscreteOrientation),pointer  :: orientation => null()
        
        !> Accumulated shear deformation (including both slip and twinning),
        !> from a reference (virgin) state up to the current state. 
        double precision                  :: accumulatedshear = 0.0D0
        
        !> \todo types related to the substructural state variables to be added.
        
    end type
    

    type :: GrainStateCollection
        
        type(GrainState), dimension(:), allocatable :: grainstate
        
    contains
    
        procedure :: initialize => GrainStateCollection_initialize
        
    end type
    
    
    type :: MesostructureState
        
        double precision, dimension (3,3) :: deformationgradient = unit_sr_matrix
        
        !> \todo this component is not yet exploited in the datastructure. If it were to be exploited,
        !>  it should render the SAVEd modular data structure 'TmatGr' (of altayMesostructure
        !>  module) obsolete.
        type(EulerAngles), dimension(:), allocatable   :: GrainBoundaryEuler_initial
        
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
    !> Number of grains in the collection
    integer :: n_grains = 0
    !> Running index
    integer :: i
    !
        info = criErr_BadDims
        !Allocation
        n_grains = size(texture%orientations)
        if (.not.(n_grains > 0)) return
        allocate(this%grainstate(n_grains), stat=info)
        if (info/=criSuccess) return
        !Pointer assignments - keep the order as is
        do i=1,n_grains
            this%grainstate(i)%orientation => texture%orientations(i)
        end do
        !
    end subroutine
    
end module