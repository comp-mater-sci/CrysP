! $Id$

#include "criMacros.fpp"

!> Material model used in AlTay - datatypes
module altayMaterialTypes
use criErrcodes
use altayDeformationMechanism
use altayHardTypes
use altayMesostructure
implicit none

    !> Data type that characterizes a phase
    type :: PhaseData
        
        type(DeformationMechanismData) :: deformationmechanism

        type(HardeningModelParams)      :: hardening

        type(MesostructureData)         :: intraphase_interfaces
        
    end type


    !> Data type that characterizes the material
    type :: MaterialData
        !> shape: [n_phases]
        type(PhaseData), dimension(:),allocatable           :: phases
        type(MesostructureData),dimension(:),allocatable    :: interphase_interfaces
    contains

        !> Provide number of phases in the material.
        procedure,pass(this) :: nphases => MaterialData_size
        
    end type

contains

    integer function MaterialData_size(this) result(n)
    implicit none
    class(MaterialData), intent(in):: this
    !
        CHOOSE(n, allocated(this%phases), size(this%phases), 0)
    !
    end function
    
end module
