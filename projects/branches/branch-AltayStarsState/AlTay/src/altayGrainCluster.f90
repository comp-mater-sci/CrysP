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
    
#ifndef USE_ALTAYSIMUL
    Subroutine altayGrainCluster_solver(cluster_state, grain_states, MacroDefRate, solution, info)
    implicit none
    type(ClusterState), intent(in) :: cluster_state
    type(GrainStateCollection), intent(in) :: grain_states
    type(DeformationRate),intent(in) :: MacroDefRate
    type(GrainClusterSolution), intent(out) :: solution
    integer, intent(out) :: info
    !
    integer :: i
    !> Number of grains in the cluster
    integer :: n_grains
        !
        ALLOCATED_SIZE(n_grains,cluster_state%idx)
        
        ! -1- Set component(s) of type CRSSData
        do i=1,n_grains
            associate (grain => grain_states%grains(cluster_state%idx(i)))
                call altayHard_getCRSS(grain, solution%components(i)%crss, info)
                if (info /= criSuccess) return
            end associate
        end do
        !
        ! -2- Set component of type LinearProgrammingSDV
        call LinProg_solver(solution, cluster_state, grain_states, MacroDefRate, info)
        if (info /= criSuccess) return
        !
        ! -3- Set component(s) of type DeformationRateSDV
        do i=1,n_grains
            associate (crss_data        => solution%components(i)%crss, &
                       pancak2_solution => solution%components(i)%solution, &
                       sliprat_solution => solution%components(i)%sliprat_solution, &
                       DM_data          => grain_states%grains(cluster_state%idx(i))%&
                                            phase%deformationmechanism)
                call sliprat(sliprat_solution,MacroDefRate,pancak2_solution,crss_data,DM_data)
            end associate
        end do
        !
    end subroutine
#endif

end module