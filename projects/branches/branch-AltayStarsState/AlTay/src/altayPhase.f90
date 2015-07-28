! $Id$

!> Phase description in alTay
module altayPhase
use criErrcodes
use altayDeformationMechanism
use altayHard
use altayConfig
implicit none


    
    !> Data type that characterizes a phase
    type :: altayPhaseData
        
        type(DeformationMechanismData) :: deformationmechanism

        type(HardeningModels)   :: hardening

    contains

    procedure, pass(this) :: init => altayPhaseData_initFromConfig

    end type


    contains

    !> Initialized all components of altayPhaseData from configuration cnf
    subroutine altayPhaseData_initFromConfig(this, cnf, info)
    implicit none
    class(altayPhaseData), intent(out)   :: this
    type(altayConfigData), intent(in)    :: cnf
    integer, intent(out)                 :: info
        !
        ! Initialize deformationmechanism data
        call DeformationMechanismData_init(this%deformationmechanism,cnf%deformationmechanism,info)
        if (info /= criSuccess) then
            write(*,*) 'Cannot initialize deformationmechanism data from configuration.'
            return
        endif
        !
        ! Initialize hardening
        call HardeningModels_init(this%hardening,cnf%hardening, info)
        if (info /= criSuccess) then
            write(*,*) 'The hardening parameters provided contain flaws.'
            return
        endif
        !
    end subroutine 

end module
