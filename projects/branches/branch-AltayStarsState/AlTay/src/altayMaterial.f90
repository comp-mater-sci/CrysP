!
! $Id$
!

#include "criMacros.fpp"

!> Material model used in AlTay
module altayMaterial
use criErrcodes
use altayMaterialTypes
use altayConfig
use altayHard
implicit none

    interface initialize
        module procedure MaterialData_initFromConfig, &
                         MaterialData_initialize_allocate, &
                         PhaseData_initFromConfig
    end interface

contains
    
    
    !> Initialize MaterialData object to hold n phases.
    subroutine MaterialData_initialize_allocate(this, nphases, info)
    implicit none
    class(MaterialData), intent(out)    :: this
    integer,intent(in)                  :: nphases !< Number of phases. Must be >= 1
    integer,intent(out)                 :: info
    !
    integer :: ierr
    !
        info = criErr_BadArgs
        if (nphases < 1) return
        allocate(this%phases(nphases),stat=ierr)
        info = criErr_MemAlloc
        if (ierr /= 0) return
        allocate(this%interphase_interfaces(nphases-1), stat=ierr)
        if (ierr == 0) info = criSuccess
    !
    end subroutine


    
    !> Initialized all components of altayMaterialData from configuration cnf
    !>
    !> \todo initialize inter-phase interfaces
    subroutine MaterialData_initFromConfig(this, cnf, info)
    implicit none
    type(MaterialData), intent(out)         :: this
    type(MaterialConfig), intent(in)        :: cnf
    integer, intent(out)                    :: info
    !
    integer :: i, nphases
    !
        info = criErr_BadArgs
        CHOOSE(nphases, allocated(cnf%phases), size(cnf%phases), 0)
        call MaterialData_initialize_allocate(this, nphases, info)
        if (info /= criSuccess) return
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
        ! Initialize mesostructure
        !> \todo Implement this in a simular way as it;s done for the components above
        !>       (less intrusive way)
        call this%intraphase_interfaces%interfaces%readfromSMTfile(cnf%intraphase_interfaces%file_path, info)
    end subroutine 
    
end module
