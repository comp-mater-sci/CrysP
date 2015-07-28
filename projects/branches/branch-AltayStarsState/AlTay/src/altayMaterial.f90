! $Id$

!> Material model used in AlTay
module altayMaterial
use criErrcodes
use altayPhase
use altayConfig
implicit none



    !> Data type that characterizes the material
    type :: altayMaterialData
        
        integer                             :: n_phases = 0
        
        !> dimensionality = [n_phases]
        !> Current implementation is for single-phase materials: dimensionality = [1]
        type(altayPhaseData), dimension(1)  :: phase

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
    integer :: i = 0 !< running index
    !integer :: status=0
    !character(len=100) :: error
        !
        this%n_phases = 1
        !
        ! (Re-)allocation
        !if (allocated(this%phase)) deallocate(this%phase)
        !allocate(this%phase(1:this%n_phases),stat=status,errmsg=error)
        !
        ! Initialize all phases
        do i=1,this%n_phases
            call altayPhaseData_initFromConfig(this%phase(i), cnf, info)
            if (info /= criSuccess ) return
        end do
        !
    end subroutine 

end module
