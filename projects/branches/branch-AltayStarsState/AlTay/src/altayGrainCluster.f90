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



contains
    
    
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