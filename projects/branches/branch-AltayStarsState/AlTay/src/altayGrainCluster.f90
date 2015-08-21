!
! $Id$
!


module altayGrainCluster
use altayStateTypes
use altayState
use altayPancake
use altaySliprate
use altayMacroKinematic
implicit none

    
    !> todo consider relocating type definition to altaySDVTypes
    !> \todo are the pointer attributes desirable?
    type :: GrainClusterComponent
        
        !> \todo consider removing this component; this componenent is usefull in view
        !>       of LinProg_solver-interface, but redundant and even undesirable for 
        !>       altayGrainCluster_solver
        !> todo consider adding pointer to both 'old' and 'new' states
        type(GrainState),pointer       :: grain => null()
          
        type(Pancak2Solution),pointer  :: solution => null()
        
        !> CRSS of all deformation systems
        type(CRSSData),pointer         :: crss => null()
        
        type(SlipratSolution),pointer  :: sliprat_solution => null()
        
    end type
    
    
    !> todo consider relocating type definition to altaySDVTypes
    type :: GrainClusterSolution
        
        type(GrainClusterComponent),dimension(:), allocatable :: components
        
    end type


contains
    
    
    !> /todo To keep or not to keep? altayGrainCluster_solver seems to offer better interface
    Subroutine LinProg_solver(this, MacroDefRate, info)
    implicit none
    type(GrainClusterSolution), intent(inout)    :: this
    type(DeformationRate), intent(in)            :: MacroDefRate
    integer, intent(out)                         :: info    
    !
    integer :: i
    integer,parameter :: ngrains_per_cluster = 2
    double precision, dimension(3,3)                 :: T_cluster
    type(EulerAngles), dimension(Pancak2_max_grains) :: grain_euler
    type(CRSSdata), dimension(Pancak2_max_grains)    :: grain_CRSS
    type(DeformationMechanismData),dimension(Pancak2_max_grains) :: grain_DM_data
        !
        !>todo: add corresponding field to GrainClusterSolution type
        !T_cluster = this%?
        !
        !Initializations
        do i = 1,Pancak2_max_grains
            if (i <= ngrains_per_cluster) then !initialize from 'this'
                !initialize grain_euler
                grain_euler(i) = this%components(i)%grain%orientation%euler
                !initialize grain_CRSS
                grain_CRSS(i) = this%components(i)%crss
                !initialize grain_DM_data
                grain_DM_data(i) = this%components(i)%grain%phase%deformationmechanism
            else !zero initializations
                !initialize grain_euler
                grain_euler(i) = Arr2EulerAngles([0.D0,0.D0,0.D0])
                !initialize grain_CRSS
                call CRSSData_init(grain_CRSS(i), CRSSData_size(this%components(i)%crss), info)
                !initialize grain_DM_data
                call DeformationMechanismData_init(grain_DM_data(i), 0, 0, info)
            end if
        end do
        !
        do i = 1,ngrains_per_cluster
            !
            call Pancak2(i, ngrains_per_cluster, &
                T_cluster, &
                grain_euler, grain_CRSS, grain_DM_data,   &
                MacroDefRate, &
                this%components(i)%solution)
        end do
        !
        info = criSuccess    
       
    end subroutine
    
    
    Subroutine altayGrainCluster_solver(cluster_state, MacroDefRate, solution, info)
    implicit none
    type(ClusterState), intent(in) :: cluster_state
    type(DeformationRate),intent(in) :: MacroDefRate
    type(GrainClusterSolution), intent(out) :: solution
    integer, intent(out) :: info
        !
        ! -1- Set component(s) of type CRSSData
        !     Make call(s) to altayHard_getCRSS
        !
        ! -2- Set component of type Pancak2Solution
        !     Make call(s) to PANCAK2, OR: 1 call to a pancak2-wrapper routine
        !
        ! -3- Set component(s) of type SlipratSolution
        !     Make call(s) to sliprat...
        !
    end subroutine


end module