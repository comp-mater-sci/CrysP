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
    
    
    !> /todo Consider relocating to Pancak2 module.
    Subroutine LinProg_solver(this, state, MacroDefRate, info)
    implicit none
    type(GrainClusterSolution), intent(inout)    :: this
    type(ClusterState), intent(in)               :: state
    type(DeformationRate), intent(in)            :: MacroDefRate
    integer, intent(out)                         :: info    
    !
    integer :: i
    integer :: ngrains_per_cluster
    double precision, dimension(3,3)                 :: T_cluster
    type(EulerAngles), dimension(Pancak2_max_grains) :: grain_euler
    type(CRSSdata), dimension(Pancak2_max_grains)    :: grain_CRSS
    type(DeformationMechanismData),dimension(Pancak2_max_grains) :: grain_DM_data
        !
        !
        ngrains_per_cluster = size(state%idx)
        !
        !Set T_cluster
        select case (ngrains_per_cluster)
        case (1) !Taylor
            T_cluster = 0.D0 ! Irrelevant
        case (2) !Alamel
            if (.not.(allocated(state%interfaces))) then
                info = criErr_NullPtr
                return
            end if
            if (size(state%interfaces,dim=1) /= 1 ) then
                info = criErr_BadDims
                return
            end if
            i = lbound(state%interfaces(:),dim=1)
            T_cluster = state%interfaces(i)%ptr%trafo%matrix
        case default
            info = criErr_BadDims
            return
        end select
        !
        !Initializations
        do i = 1,Pancak2_max_grains
            if (i <= ngrains_per_cluster) then !initialize from 'this'
                !initialize grain_euler
                !> \todo adopt proper acces to ???%grain%orientation%euler
                grain_euler(i) = this%components(i)%grain%orientation%euler
                !initialize grain_CRSS
                grain_CRSS(i) = this%components(i)%crss
                !initialize grain_DM_data
                !> \todo adopt proper acces to ???%grain%phase%deformationmechanism
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
        call LinProg_solver(solution, cluster_state, MacroDefRate, info)
        !
        ! -3- Set component(s) of type SlipratSolution
        !     Make call(s) to sliprat...
        !
    end subroutine


end module