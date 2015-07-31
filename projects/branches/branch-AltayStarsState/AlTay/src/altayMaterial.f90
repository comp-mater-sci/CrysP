! $Id$

#include "criMacros.fpp"

!> Material model used in AlTay
module altayMaterial
use criErrcodes
use altayMaterialTypes
use altayConfig
use altayHard
implicit none

    interface initialize
        module procedure MaterialData_initFromConfig, PhaseData_initFromConfig
    end interface

contains
    
    !> Initialized all components of altayMaterialData from configuration cnf
    subroutine MaterialData_initFromConfig(this, cnf, info)
    implicit none
    type(MaterialData), intent(out)         :: this
    type(MaterialConfig), intent(in)        :: cnf
    integer, intent(out)                    :: info
    !
    integer :: i, nphases, memstat
    !
        info = criErr_BadArgs
        CHOOSE(nphases, allocated(cnf%phases), size(cnf%phases), 0)
        if (nphases <= 0) return
        info = criErr_MemAlloc
        allocate(this%phases(nphases),stat=memstat)
        if (memstat /= 0) return
        !
        ! Initialize all phases
        do i = 1, nphases
            call PhaseData_initFromConfig(this%phases(i), cnf%phases(i), info)
            if (info /= criSuccess ) return
        end do
        !
    end subroutine 

    
    !> Initialized all components of altayPhaseData from configuration cnf
    subroutine PhaseData_initFromConfig(this, cnf, info)
    implicit none
    type(PhaseData), intent(out)  :: this
    type(PhaseConfig), intent(in)       :: cnf
    integer, intent(out)                :: info
        !
        ! Initialize deformationmechanism data
        call DeformationMechanismData_init(this%deformationmechanism,cnf%deformation_mechanism,info)
        if (info /= criSuccess) then
            write(*,*) 'Cannot initialize deformationmechanism data from configuration.'
            return
        endif
        !
        ! Initialize hardening
        call HardeningModelParams_init(this%hardening,cnf%hardening, info)
        if (info /= criSuccess) then
            write(*,*) 'The hardening parameters provided contain flaws.'
            return
        endif
        !
    end subroutine 
    
end module
