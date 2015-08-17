!
! $Id$
!
#include "criMacros.fpp"
!> Collection of procedures that build complex assemblies
!>
!> 1. Create clusters of state variables
!> 2. Combine state-independent and state-dependent material properties
!> into state variables
module altayAssembly
use criErrcodes
use altayAssemblyTypes
use altayStateTypes
use altayMesostructure
use altayMaterialTypes
use altayODFTypes
implicit none
    
    type,private :: GrainMap
        integer,dimension(:),allocatable :: map
    end type
    
contains

    !> Construct "bamboo-type" clusters of n grains (n = [1, ...]) that contain
    !> n-1 grain interfaces.
    !>
    !> Note that both Taylor (1 grain) and Alamel (2 grains) clusters are
    !> special cases of bamboo-type clusters.
    subroutine altayAssembly_ClusterAssembly_basic(state, directives, info)
    implicit none
    type(altayStateVariables),intent(inout),target :: state
    type(AssemblyMultiPhaseDirective),dimension(:),intent(in) :: directives
    integer,intent(out) :: info
    !
    ! Indices of the "current" interface inside the phases and between phases.
    ! and the number of interfaces in the array.
    ! Shape: [1:n_phases+n_interphases]
    integer,dimension(:),allocatable :: mesostructure_idx, mesostructure_size
    ! Pointers to mesostructure data
    ! Shape: [1:n_phases + n_interphases]
    type(PtrMesostructureData),dimension(:),allocatable :: mesostructure_data
    ! reverse maps: phase_id -> grain_id. Shape: [1:n_phases]
    type(GrainMap),dimension(:),allocatable :: grain_maps
    ! Indices of the "current" grain and number of elements
    ! Shape: [1:n_phases]
    integer,dimension(:),allocatable :: grain_idx, grain_maps_size
    ! For the initial check only. Shape: [1:n_phases]
    integer,dimension(:),allocatable :: required_maps_size
    integer :: n_phases, n_intephases, n_interfaces, i, j, k, phase_id, tmp
    integer :: n_grains, n_clusters, cluster_idx, n_cluster_grains, n_cluster_interfaces
    !
        info = criErr_BadArgs
        ALLOCATED_SIZE(n_phases,state%material%phases)
        if (any(directives(:)%number_instances <= 0)) return
        ! Initial situation: we have N grains in total, N_1 in phase 1, N_2 in phase 2 etc.
        ! Verify if the design given by the directives can be implemented. 
        n_grains = 0
        allocate(grain_maps_size(n_phases), source=0)
        allocate(required_maps_size(n_phases), source=0)
        do i = 1, size(directives)
            ALLOCATED_SIZE(tmp, directives(i)%phase_ids) 
            associate(phase_ids => directives(i)%phase_ids)
                n_grains = n_grains + tmp * directives(i)%number_instances
                do j = 1, tmp
                    required_maps_size(phase_ids(j)) = required_maps_size(phase_ids(j)) + directives(i)%number_instances
                enddo
            end associate
        enddo
        ! 
        allocate(grain_maps(n_phases))
        ! Prepare data: indices of phases
        do phase_id = 1, n_phases
            call GrainStateCollection_reverseMapping(state%grainstates, phase_id, grain_maps(phase_id)%map, info)
            ALLOCATED_SIZE(grain_maps_size(phase_id), grain_maps(phase_id)%map)
        end do
        ! Check if the number of grains in all phases permits the design
        RETURN_IF(any(required_maps_size > grain_maps_size), info = criErr_BadArgs)
        !
        n_clusters = sum(directives(:)%number_instances)
        allocate(state%clusterstates%clusters(n_clusters))
        
        !> \fixme altayAssembly_ClusterAssembly_basic: don't assume that phase_id starts from 1
        ALLOCATED_SIZE(n_phases, state%material%phases)
        ALLOCATED_SIZE(n_intephases, state%material%interphase_interfaces)
        n_interfaces = n_phases + n_intephases
        !
        ! Set up helper structures
        allocate(mesostructure_data(n_interfaces))
        allocate(mesostructure_idx(n_interfaces),source=1)
        allocate(mesostructure_size(n_interfaces),source=0)
        do i = 1, n_phases
            mesostructure_data(i)%ptr => state%material%phases(i)%intraphase_interfaces
            ALLOCATED_SIZE(mesostructure_size(i), state%material%phases(i)%intraphase_interfaces%interfaces)
        enddo
        j = n_phases + 1
        do i = 1, n_intephases
            mesostructure_data(j)%ptr => state%material%interphase_interfaces(i)
            ALLOCATED_SIZE(mesostructure_size(j), state%material%interphase_interfaces(i)%interfaces)
            j = j + 1
        enddo

        ! Let's start from 1st grain in each phase
        allocate(grain_idx(n_phases),source=1)
        cluster_idx = 1
        ! Loop over directives
        do i = 1, size(directives)
            associate (phase_ids => directives(i)%phase_ids)
                n_cluster_grains = size(phase_ids)
                n_cluster_interfaces = n_cluster_grains - 1
                do j = 1, directives(i)%number_instances
                    associate(cluster => state%clusterstates%clusters(cluster_idx))
                        !
                        ! Set cluster elements: grains
                        allocate(cluster%idx(n_cluster_grains))
                        do k = 1, n_cluster_grains
                            phase_id = phase_ids(k)
                            ! check pre-condition: the index must not exceed the size of the map
                            RETURN_IF(grain_idx(phase_id) > grain_maps_size(phase_id), info = criErr_BadArgs)
                            cluster%idx(k) = grain_maps(phase_id)%map(grain_idx(phase_id))
                            ! Advance the forefront index of phase_id
                            grain_idx(phase_id) = grain_idx(phase_id) + 1
                        enddo
                        !
                        ! Set cluster elements: interfaces
                        allocate(cluster%interfaces(n_cluster_interfaces))
                        do k = 1, n_cluster_interfaces
                            ! Get index dataset for interface between k-th and (k+1)-th grain.
                            ! Loop iteration ensures there is always (k+1)-th grain. 
                            tmp = getIndex(phase_ids(k), phase_ids(k+1), n_phases)
                            associate (interface_idx => mesostructure_idx(tmp))
                                cluster%interfaces(k)%ptr => mesostructure_data(tmp)%ptr%interfaces(interface_idx)
                                ! Advance circular index
                                interface_idx = modulo(interface_idx, mesostructure_size(tmp)) + 1
                            end associate
                        enddo
                    end associate
                    cluster_idx = cluster_idx + 1
                enddo
            end associate
        enddo
        
        ! Check post-condition: the forefront indices of all maps must reach 
        ! the end of the map (size(map) + 1)
        CHOOSE(info, any(grain_idx /= (grain_maps_size + 1)), criErr_BadDims, criSuccess)
    !
    end subroutine
    
    
    !> Calculate the position of element in 1D array that holds (i,j)-th 
    !> element of symmetrical 2D matrix of rank n.
    !>
    !> Rules for convention of ordering elementes in the 1D array: 
    !> 1. diagonal elements of 2D martix (i==j) come first, ordered by i,
    !> 2. elements for which i/=j are stored in in order of ascending i and 
    !>    keeping invariant that (i < j).
    !> The function takes care of relation i<j, but it's user's responsibility
    !> to ensure that (i <= n).
    !> \note The convention here is _different_ from Voigt vector notation.
    !>
    !> Example: for n = 2 (# of elements: n + (n - 1))
    !> 1:(1,1),2:(2,2),3:(1,2)
    !> for n = 3 (# of elements:n + (n - 1) + (n -2))
    !> 1:(1,1),2:(2,2),3:(3,3),4:(1,2),5:(1:3),6:(2:3)
    !> for n = 4:
    !> 1:(1,1),2:(2,2),3:(3,3),4:(4,4),5:(1,2),6:(1:3),7:(1:4),8:(2:3),9:(2:4),10:(3:4)
    !> In general, # of elements: is (n*n + n)/2, since:
    !> # = n + (n - 1) + (n -2) + ... = n*n - (n(n-1))/2 = n*n - (n*n - n)/2 = (n*n + n)/2
    pure integer function getIndex(i, j, n) result(x)
    implicit none
    integer,intent(in) :: i, j !< Indices in 2D matrix
    integer,intent(in) :: n !< rank of the matrix
    !
    integer :: ii, jj
    !
        ! Check if Rule 1 applies
        if (i == j) then
            x = i
        else
            ! Enforce Rule 2)
            if (i < j) then
                ii = i
                jj = j
            else
                ii = j
                jj = i
            endif
            ! Indexation in the blocks where i /= j:
            ! To get to block that starts with i, one has to skip sum_{k=1,i}[n - (k - 1)]
            ! positions.
            ! For instance: 
            ! i=1: n elements
            ! i=2: n + (n-1) elements
            ! Then the position inside this block is j-i.
            ! # elements in sequence up to block that contains elements with index i:
            ! sum_{k=1,i}[(n - k + 1)] = n*i - sum_{k=1,i}[(k-1)] = n*i - (i)(i+1)/2 + i =
            ! = i(n + 1) - i(i+1)/2
            ! Note that futher simplification is possible, but the form above
            ! ensures no integer rounding is involved (i(i+1) is always even).
            x = ii*(n + 1) - ii*(ii+1)/2 + (jj-ii)
        endif
    end function
    
    !> 
    pure integer function getNumberElements(n) result(x)
    implicit none
    integer,intent(in)  :: n
    !
        x = (n*n + n)/2 ! Note: nominator is always even.
    !
    end function
    
end module
