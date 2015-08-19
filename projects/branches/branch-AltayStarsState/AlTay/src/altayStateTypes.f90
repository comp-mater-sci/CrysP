!
! $Id$
!
#include "criMacros.fpp"
!> Data types for state variables
!>
!> The state variables may contain:
!>     * **Plain data members**. 
!>     * **Allocatable members** describe optional state variables, or the variables
!>       that are conditionally relevant.
!>     * **Pointers**. They generally DO NOT describe actual state,
!>       but are added to simplify processing or to establish link with either other
!>       components of state or non-state data. 
module altayStateTypes
use criErrcodes
use criMathUtils
use criIterUtils
use altayMesostructure
use altayODFTypes
use altayMaterialTypes
use altayHardTypes
!use altayCRSSTypes
use altayHardLaw_KM
#ifdef PEBP_ENABLED
use altayHardLaw_DSH, only: DSHStateVariable
#endif
implicit none

    !> State variables related to hardening of a single grain
    type :: HardeningStateVariables
        
#ifdef PEBP_ENABLED
        !> State variables of the DSH hardening law.
        type(DSHStateVariable),allocatable      :: dsh_state
#endif

        !> State variables of the Kocks-Mecking hardening law.
        type(KMStateVariables),allocatable      :: km_state
        
        !> \todo: decide whether the crss is actually needed.
        !> Collection of CRSS per grain (useful for some/all hardening models?)
        ! type(CRSSData)                   :: crss   
        
    end type



    !> Basic state of any individual grain
    type :: GrainState
        
        !> Phase where the grain belongs to
        type(PhaseData),pointer             :: phase => null()
        
        !> Associated orientation
        type(DiscreteOrientation)           :: orientation
        
        !> Accumulated shear deformation (including both slip and twinning),
        !> from a reference (virgin) state up to the current state. 
        double precision                   :: accumulatedshear = 0.0D0
        
        type(HardeningStateVariables),allocatable       :: hardening_state
        
    end type
    
    
    !> Collection of grain state objects
    type :: GrainStateCollection
        
        !> Array of grain state objects
        type(GrainState), dimension(:), allocatable :: grains
        
        !> Auxiliary pointer to phase data used in GrainState objects.
        !> It simplifies operations that require knowledge (1) how many
        !> phases are present and (2) what are the pointers.
        !> Shape is [1:nphases]
        type(PhaseData), dimension(:),pointer       :: phases
    end type


   !> State variables of a single cluster
    type :: ClusterState
        
        !> Vector of indices of the cluster components inside array of grain state.
        integer,dimension(:),allocatable    :: idx
        
        type(PtrInterfaceData),dimension(:),allocatable      :: interfaces

    end type


    !> Collection of cluster state objects.
    type :: ClusterStateCollection
        
        type(ClusterState),dimension(:),allocatable          :: clusters

    end type
    
    
    !> State of the mesostructure
    type :: MesostructureState
        
        double precision, dimension (3,3) :: deformationgradient = unit_sr_matrix
    end type



    !> Container for the state variables
    type :: altayStateVariables
        
        type(MesostructureState)                :: mesostructure

        !> Collection of the state of grains
        type(GrainStateCollection)              :: grainstates
        
        type(ClusterStateCollection)            :: clusterstates
        
        !>@{ \name Auxiliary members that are not considered as state variables
        
        !> Pointer to single material object.
        type(MaterialData),pointer              :: material => null()
        
        !>@}
        
    end type

    interface initialize
        module procedure GrainStateCollection_initialize_blocks
    end interface


contains


    !> Initialize grain state collection from state-independent material
    !> properties and discrete ODFs of individual phases. The initialization
    !> populates the grains field by making blocks of grains that belong
    !> to particular phases.
    !>
    !> The resultant GrainStateCollection object will have the layout
    !> implied by concatenating discrete ODFs of the phases into blocks.
    !> For instance, if two phases are used, grains field of
    !> GrainStateCollection will have the layout as depicted below:
    !>     grain_1   -> phase_1
    !>     ...
    !>     grain_n   -> phase_1
    !>     grain_n+1 -> phase_2
    !>     ...
    !>     grain_m   -> phase_2
    !>
    !> \note The block layout may be cache-inefficient. 
    subroutine GrainStateCollection_initialize_blocks(this, odfs, material, info)
    implicit none
    type(GrainStateCollection), intent(out)     :: this
    !> 
    !> Order of DiscreteODF objects in the array must correspond to the
    !> order of phases in the `material` parameter.
    type(DiscreteODF),dimension(:),intent(in)   :: odfs
    type(MaterialData),intent(in),target        :: material
    integer, intent(out)                        :: info
    !
    ! Number of grains and phases in the collection
    integer :: n_grains, n_phases
    ! Running index, error code
    integer :: idx, i, j, ierr
    !
        info = criErr_BadDims
        ! Check if the number of ODFs matches the number of phases
        ! and if non-zero number of grains is used.
        n_phases = material%nphases()
        if ( (n_phases <= 0) .or. (size(odfs,dim=1) /= n_phases) .or. (any(size(odfs) <= 0))) return
        !
        this%phases => material%phases
        n_grains = sum(size(odfs))
        if (n_grains <= 0) return
        !
        info = criErr_MemAlloc
        allocate(this%grains(n_grains), stat=ierr)
        if (ierr /= 0) return
        ! Set up individual grains: orientations and association with the pahse
        idx = 1
        do i = lbound(material%phases, dim=1), ubound(material%phases, dim=1)
            do j = lbound(odfs(i)%orientations, dim=1), ubound(odfs(i)%orientations,dim=1)
                this%grains(idx)%orientation = odfs(i)%orientations(j)
                this%grains(idx)%phase => material%phases(i)
                idx = idx + 1
            enddo
        enddo
        info = criSuccess
        !
    end subroutine


    !> Check if GrainStateCollection object contains all necessary components.
    pure logical function GrainStateCollection_isValid(this) result(is_ok)
    implicit none
    type(GrainStateCollection), intent(in)     :: this
    !
        CHOOSE(is_ok, allocated(this%grains), (size(this%grains) > 0) .and. associated(this%phases), .false.)
        CHOOSE(is_ok, is_ok, size(this%phases) > 0, .false.)
    !
    end function


    !> Construct forward mapping (vector of indices) between components of
    !> GrainStateCollection and PhaseData objects. The vector constitutes a map:
    !> grain_index -> phase_id
    !>
    !> The result `map` is a vector of the length eqal to the number of 
    !> elements in `this%grains`. The vector elements are indices of phases
    !>  in `this%phases` that are pointed to by respective elements of 
    !> `this%grains(:)%phase`.
    pure subroutine GrainStateCollection_forwardMapping(this, map, info)
    implicit none
    type(GrainStateCollection), intent(in)          :: this
    integer,dimension(:),allocatable, intent(out)   :: map
    integer,intent(out)                             :: info
    !
    integer :: n_grains, ierr, i, j
    integer :: default_idx, min_phase_idx, max_phase_idx
    !
        info = criSuccess
        n_grains = size(this%grains)
        ! Mark for non-existing phase
        default_idx = lbound(this%phases, dim=1) - 1
        min_phase_idx = lbound(this%phases, dim=1)
        max_phase_idx = ubound(this%phases, dim=1)
        !
        RETURN_ON_WITH(allocate(map(n_grains), stat=ierr), ierr /= 0, info=criErr_MemAlloc)
        if (n_grains < 1) return
        ! Visit all grains and determine 
        do  i = 1, n_grains
            map(i) = default_idx
            ! Check if the grain is associated with a particular phase
            do j = min_phase_idx, max_phase_idx
                if (associated(this%grains(i)%phase, this%phases(j))) then
                    map(i) = j
                    exit
                endif
            enddo
            ! Verify if the match was found:
            if (map(i) == default_idx) exit
        enddo
        RETURN_IF_WITH(i <= n_grains, info = criError)
    !
    end subroutine


    !> Calculate vector of indices of objects in GrainStateCollection that are 
    !> associated with a given phase. The vector constitutes a map: 
    !> phase_id -> grain_indices
    pure subroutine GrainStateCollection_reverseMapping(this, phase_id, map, info)
    implicit none
    type(GrainStateCollection), intent(in)          :: this
    integer,intent(in)                              :: phase_id
    integer,dimension(:),allocatable, intent(out)   :: map
    integer,intent(out)                             :: info
    !
    integer :: n_grains, ierr, i, j
    integer,dimension(:),allocatable :: idx
    !
        info = criSuccess
        n_grains = size(this%grains)
        ! Pre-allocate map.
        RETURN_ON_WITH(allocate(map(n_grains), stat=ierr), ierr /= 0, info=criErr_MemAlloc)
        ! Check if there is any work to do. If none, map of zero elements is returned.
        if (n_grains < 1) return
        
        ! Note: the loop below could be implemented by which_indices, but expression 
        ! this%grains(:)%phase triggers error:
        ! "A component with POINTER attribute may NOT be to the right of an array component"
        ! in  call which_indices(associated(this%grains(:)%phase, this%phases(i)), map)
        j = 0
        do  i = 1, n_grains
            ! Check if the grain is associated with a particular phase
            if (associated(this%grains(i)%phase, this%phases(phase_id))) then
                j = j + 1
                map(j) = i
            endif
        enddo
        if (j < n_grains) then
            ! shrink the map, possibly to zero elements
            idx = map(:j)
            map = idx
        endif
    !
    end subroutine
            
    !> Extract DiscreteODF objects from GrainStateCollection object
    subroutine GrainStateCollection_DiscreteODF(this, odf, phase_id, info)
    implicit none
    type(GrainStateCollection), intent(in)  :: this
    type(DiscreteODF),intent(out)           :: odf
    integer,intent(in)                      :: phase_id
    integer,intent(out)                     :: info
    !
    integer :: n_grains
    integer,dimension(:),allocatable :: reverse_map
    !
        info = criErr_BadArgs
        if (.not. GrainStateCollection_isValid(this)) return
        call GrainStateCollection_reverseMapping(this, phase_id, reverse_map, info)
        if (info /= criSuccess) return
        n_grains = size(reverse_map)
        RETURN_IF(info /= criSuccess, info = DiscreteODF_resize(odf, n_grains))
        if (n_grains > 0) then
            odf%title = this%grains(reverse_map(1))%phase%name
            odf%orientations = this%grains(reverse_map)%orientation
        endif
    !
    end subroutine


    
    pure subroutine MesostructureState_update(this, incremental_defgrad, info)
    implicit none
    type(MesostructureState), intent(inout)     :: this
    double precision, dimension(3,3), intent(in) :: incremental_defgrad
    integer, intent(out)                         :: info
    !
        this%deformationgradient = matmul(incremental_defgrad,this%deformationgradient)
        info = criSuccess
        !
    end subroutine

end module