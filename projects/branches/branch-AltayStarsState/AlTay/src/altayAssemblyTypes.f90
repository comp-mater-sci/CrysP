!
! $Id$
!
#include "criMacros.fpp"

module altayAssemblyTypes
implicit none

    !> Simple description of assembly for multi-phase clusters
    type ::AssemblyMultiPhaseDirective
        !> Ids of Phases involved in cluster. Shape: [1:n_grains_per_cluster]
        integer,dimension(:),allocatable :: phase_ids
        
        !> Number of clusters to be created
        integer :: number_instances = 0 
    end type
    
end module
