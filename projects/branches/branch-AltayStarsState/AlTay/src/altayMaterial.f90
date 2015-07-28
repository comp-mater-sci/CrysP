! $Id$

!> Material model used in AlTay
module altayMaterial
use altayMiscutils
use criErrcodes
use altayDeformationMechanism
use altayHard
use altayConfig
implicit none


    
    !> Data type that characterizes the material
    !> Current implementation is for single-phase materials, so "material"=="phase"
    type :: altayMaterialData
        
        type(DeformationMechanismData) :: deformationmechanism

        type(HardeningModels)   :: hardening

    contains

    procedure, pass(this) :: init => altayMaterialData_initFromConfig

    end type


    contains

    !> Initialized all components of altayMaterialData from configuration cnf
    subroutine altayMaterialData_initFromConfig(this, cnf, info)
    implicit none
    class(altayMaterialData), intent(out):: this
    type(altayConfigData), intent(in)    :: cnf
    integer, intent(out)                 :: info
        !
        info = 0
        !
        ! Initialize deformationmechanism data
        call DeformationMechanismData_init(this%deformationmechanism,cnf%deformationmechanism,info)
        if (info /= criSuccess) then
            write(*,*) 'Cannot initialize deformationmechanism data from configuration.'
            call terminate(stopcode_runtimeerror)
        endif
        !
        ! Initialize hardening
        call HardeningModels_init(this%hardening,cnf%hardening, info)
        if (info /= criSuccess) then
            write(*,*) 'The hardening parameters provided contain flaws.'
            call terminate(stopcode_inputerror)
        endif
        !
    end subroutine 

end module
