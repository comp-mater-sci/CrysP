!
! $Id$
!

#include "criMacros.fpp"

!> Material model used in AlTay - datatypes
module altayMaterialTypes
use criErrcodes
use altayDeformationMechanism
use altayHardTypes
use altayMesostructure
implicit none

    integer,parameter,private :: phasename_maxlen = 32
    !> Data that characterize state-independent properties of a phase
    type :: PhaseData
        
        character(len=phasename_maxlen) :: name
        
        type(DeformationMechanismData)  :: deformationmechanism

        type(HardeningModelParams)      :: hardening

        type(MesostructureData)         :: intraphase_interfaces
        
    end type


    !> Data that characterize state-independent properties of a material
    type :: MaterialData
        !> Description of phases in the material
        !> Shape: [n_phases]
        type(PhaseData), dimension(:),allocatable           :: phases
        
        !> Description of interfaces between different phases.
        !> Shape: [n_phases-1]
        !>
        !> Consecutive elements describe: (phase_i,phase_j), where phase_i < phase_j
        !> Example: for 2-phases: [1]: (1,2)
        !> Example: for 3-phases: [1]: (1,2), [2]: (1,3), [3]: (2,3)
        type(MesostructureData),dimension(:),allocatable    :: interphase_interfaces
    contains

        !> Provide number of phases in the material.
        procedure,pass(this) :: nphases => MaterialData_size
        
    end type

contains

    pure integer function MaterialData_size(this) result(n)
    implicit none
    class(MaterialData), intent(in):: this
    !
        CHOOSE(n, allocated(this%phases), size(this%phases), 0)
    !
    end function
    
end module
