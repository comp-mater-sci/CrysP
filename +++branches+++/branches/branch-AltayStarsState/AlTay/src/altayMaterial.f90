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
    integer :: i, nphases, ninterphases
    !
        info = criErr_BadArgs
        ALLOCATED_SIZE(nphases, cnf%phases)
        call MaterialData_initialize_allocate(this, nphases, info)
        if (info /= criSuccess) return
        !
        ! Initialize all phases
        do i = 1, nphases
            call PhaseData_initFromConfig(this%phases(i), cnf%phases(i), info)
            if (info /= criSuccess ) return
        end do
        ALLOCATED_SIZE(ninterphases, cnf%interphase_interfaces)
        !
        ! Initialize all inter-phases
        do i =1, ninterphases
            call initialize(this%interphase_interfaces(i), cnf%interphase_interfaces(i)%file_path, info)
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
        this%name = cnf%name
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
        call initialize(this%intraphase_interfaces, cnf%intraphase_interfaces%file_path, info)
    !
    end subroutine 
    
end module
