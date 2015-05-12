! $Id$

module altayGrainCluster
use altayStateTypes
use altayState
use altayPancake
use altaySliprate
use altayMacroKinematic
implicit none

    
    type :: GrainClusterComponent
        
        !> todo consider adding pointer to both 'old' and 'new' states
        type(DiscreteOrientation),pointer            :: grain => null()
          
        type(Pancak2Solution),pointer  :: solution => null()
        
        !> CRSS of all deformation systems
        type(CRSSData),pointer         :: crss => null()
        
        type(SlipratSolution),pointer  :: sliprat_solution => null()
        
    end type
    
    !> todo this should not be a parameter
    integer,parameter,private :: ngrains_per_cluster = 2
    
    type :: GrainClusterSolution
        
        type(GrainClusterComponent),dimension(ngrains_per_cluster) :: components
        
    end type
    
#ifdef ENABLE_DEVEL_GrainClusterSolutionX
    !
    ! ALTERNATIVE. 
    ! This would be more compact and elegant, but setting up a cluster requires
    ! vector subscript. Sadly, we need a construct that is forbidden by Fortran 
    ! standard. 
    !
    ! -->>
    type :: GrainClusterSolutionX
        
        type(DiscreteOrientation),dimension(:), pointer :: grains => null()
          
        type(Pancak2Solution),dimension(:), pointer :: solutions => null()
        
        !> CRSS of all deformation systems
        type(CRSSData),dimension(:), pointer        :: crss => null()
        
    end type
    ! <<--
    
#endif
    
    
    contains
    
    
    subroutine GrainClusterSolution_init(this, state, crss_data, solution_data,indices, info)
    implicit none
    type(GrainClusterSolution),intent(out)                  :: this
    type(altayStateData),target,intent(in)                  :: state
    type(CRSSData),dimension(:),target,intent(in)           :: crss_data
    type(Pancak2Solution),dimension(:),target,intent(in)    :: solution_data
    integer,dimension(:),intent(in)                         :: indices
    integer,intent(out)                                     :: info
    !
    integer :: i
        info = criErr_BadArgs
        
        ! TODO: extend the checks: size of crss_data and solution_data muust match
        ! TODO: put the checks into conditional compilation. Rationale:
        !       checking the sizes would have a considerable runtime overhead.
        if (any(indices > altayStateData_size(state)) .or. &
            any(indices < 0) .or. (size(indices) > ngrains_per_cluster)) return
        ! Assign the pointers
        do i=1, ngrains_per_cluster
            this%components(i)%grain => state%old%texture%orientations(i)
            this%components(i)%solution => solution_data(i)
            this%components(i)%crss => crss_data(i)
        enddo
        info = criSuccess
    !
    end subroutine
    
    
    !TESTING -->>
    subroutine dummy()
    implicit none
    type(GrainClusterSolution)          :: cluster
    type(altayStateData),target         :: state
    type(CRSSData),dimension(:),allocatable,target           :: crss_data
    type(Pancak2Solution),dimension(:),allocatable,target    :: solution_data
    integer,dimension(2)                :: indices
    integer :: info
    integer :: i, ngrains
    
        !
        ! In the initialization
        !
        info = altayStateData_init(state)
        ! expand texture, make 10 grains
        ngrains = 10
        info = DiscreteODF_resize(state%old%texture, ngrains, keep_state=.false.)
        ! make assembly
        info = altayStateData_assemble(state)
        !
        ! Inside call to SIMUL
        !
        allocate(crss_data(ngrains))
        allocate(solution_data(ngrains))
        
        !
        ! In main SIMUL loop over the clusters
        !
        do i = 1,ngrains,2
            indices = [i, i+1]
            call GrainClusterSolution_init(cluster, state, crss_data, solution_data, indices, info)
        enddo
        
    end subroutine
    
    !<<--
    
    
#ifdef ENABLE_DEVEL_GrainClusterSolutionX
    !
    ! ALTERNATIVE
    !
    ! -->>
    
    subroutine GrainClusterSolutionX_init(this, state, grains, indices, info)
    implicit none
    type(GrainClusterSolutionX),intent(out)      :: this
    type(altayStateData),intent(in),target      :: state
    type(DiscreteOrientation),dimension(:),intent(in),target  :: grains
    integer,dimension(:),intent(in)             :: indices
    integer,intent(out)                         :: info
    !
    ! integer :: i,j
        info = criErr_BadArgs
        if (any(indices > altayStateData_size(state)) .or. &
            any(indices < 0)) return
        ! Set the indices of the grains
        
        ! NOTE:  Fortran standard explicitly forbids data-target from being an array 
        ! section with a vector subscript. (Constraint C724 in Fortran 2008).
        ! See: https://software.intel.com/en-us/forums/topic/270679
        ! We cannot make assigmnent like this:
        ! this%grains => state%old%texture%orientations(indices)
        this%grains => orientations
    !
    end subroutine
    
    
    subroutine dummyX()
    implicit none
    type(GrainClusterSolutionX)          :: cluster
    type(altayStateData),target         :: state

    integer,dimension(2)                :: indices
    integer :: info
    
        info = altayStateData_init(state)
        ! expand texture, make 10 grains
        info = DiscreteODF_resize(state%old%texture, 10, keep_state=.false.) 
        ! make assembly
        info = altayStateData_assemble(state)
        
        call GrainClusterSolutionX_init(cluster, state, state%old%texture%orientations(indices), indices, info)
    
    end subroutine
    ! <<--
#endif
    
    Subroutine LinProg_solver(this, DM_data, MacroDefRate, MacroDefState, info)
        implicit none
        type(GrainClusterSolution), intent(inout)    :: this
        type(DeformationMechanismData), intent(in)   :: DM_data
        type(DeformationRate), intent(in)            :: MacroDefRate
        type(DeformationState), intent(in)           :: MacroDefState
        integer, intent(out)                         :: info    
        !
        integer :: i
        double precision, dimension(3,3)                 :: T_cluster
        type(EulerAngles), dimension(Pancak2_max_grains) :: grain_euler
        type(CRSSdata), dimension(Pancak2_max_grains)    :: grain_CRSS
        !
        !>todo: add corresponding field to GrainClusterSolution type
        !T_cluster = this%?
        !
        !Initializations
        do i = 1,Pancak2_max_grains
            if (i <= ngrains_per_cluster) then !initialize from 'this'
                !initialize grain_euler
                grain_euler(i) = this%components(i)%grain%euler
                !initialize grain_CRSS
                grain_CRSS(i) = this%components(i)%crss
            else !zero initializations
                !initialize grain_euler
                grain_euler(i) = Arr2EulerAngles([0.D0,0.D0,0.D0])
                !initialize grain_CRSS
                call CRSSData_init(grain_CRSS(i), CRSSData_size(this%components(i)%crss), info)
            end if
        end do
        !
        do i = 1,ngrains_per_cluster
            !
            call Pancak2(i, ngrains_per_cluster, &
                T_cluster, &
                grain_euler, grain_CRSS, DM_data,   &
                MacroDefRate, MacroDefState, &
                this%components(i)%solution)
        end do
        !
        info = criSuccess    
       
    end subroutine
    
    
end module