!
! $Id$
!
#include "criMacros.fpp"


module altayGrainCluster
use altayStateTypes
use altayState
use altayPancake
use altaySliprate
use altayMacroKinematic
use altaySDVTypes
implicit none

    
    type :: GrainClusterSolution
        
        type(GrainSDV),dimension(:),allocatable :: components
        
    end type

contains
    
    
    !> /todo Consider relocating to Pancak2 module.
    Subroutine LinProg_solver(this, cluster_state, grain_states, MacroDefRate, info)
    implicit none
    type(GrainClusterSolution), intent(inout)    :: this
    type(ClusterState), intent(in)               :: cluster_state
    type(GrainStateCollection), intent(in)       :: grain_states
    type(DeformationRate), intent(in)            :: MacroDefRate
    integer, intent(out)                         :: info    
    !
    integer :: i
    integer :: n_grains
    integer :: n_interfaces
    double precision, dimension(3,3)                 :: T_cluster
    type(EulerAngles), dimension(Pancak2_max_grains) :: grain_euler
    type(CRSSdata), dimension(Pancak2_max_grains)    :: grain_CRSS
    type(DeformationMechanismData),dimension(Pancak2_max_grains) :: grain_DM_data
        !
        !
        ALLOCATED_SIZE(n_grains,cluster_state%idx)
        !
        !Set T_cluster
        select case (n_grains)
        case (1) !Taylor
            T_cluster = 0.D0 ! Irrelevant
        case (2) !Alamel
            ALLOCATED_SIZE(n_interfaces,cluster_state%interfaces)
            RETURN_IF_WITH(n_interfaces /= 1, info = criErr_BadDims)
            i = lbound(cluster_state%interfaces(:),dim=1)
            T_cluster = cluster_state%interfaces(i)%ptr%trafo%matrix
        case default
            info = criErr_BadDims
            return
        end select
        !
        !Initializations
        do i = 1,n_grains
            associate (grain => grain_states%grains(cluster_state%idx(i)))
                !initialize grain_euler
                grain_euler(i) = grain%orientation%euler
                !initialize grain_CRSS
                grain_CRSS(i) = this%components(i)%crss
                !initialize grain_DM_data
                grain_DM_data(i) = grain%phase%deformationmechanism
            end associate
        end do
        !
        do i = 1,n_grains
            !
            call Pancak2(i, n_grains, &
                T_cluster, &
                grain_euler, grain_CRSS, grain_DM_data,   &
                MacroDefRate, &
                this%components(i)%solution)
        end do
        !
        info = criSuccess    
       
    end subroutine
    
    
    Subroutine altayGrainCluster_solver(cluster_state, grain_states, MacroDefRate, solution, info)
    implicit none
    type(ClusterState), intent(in) :: cluster_state
    type(GrainStateCollection), intent(in) :: grain_states
    type(DeformationRate),intent(in) :: MacroDefRate
    type(GrainClusterSolution), intent(out) :: solution
    integer, intent(out) :: info
    !
    integer :: i
    integer :: n_grains
        !
        ALLOCATED_SIZE(n_grains,cluster_state%idx)
        
        ! -1- Set component(s) of type CRSSData
        !     Make call(s) to altayHard_getCRSS
        !
        ! -2- Set component of type LinearProgrammingSDV
        call LinProg_solver(solution, cluster_state, grain_states, MacroDefRate, info)
        !
        ! -3- Set component(s) of type DeformationRateSDV
        !     Make call(s) to sliprat...
        !
    end subroutine


end module