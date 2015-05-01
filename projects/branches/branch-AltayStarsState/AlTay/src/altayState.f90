!
! $Id$
!

!> State variables of AlTay
!>
!> Every procedure that operates on state variable object(s) must follow
!> the guidelines:
!>     * the object must be referenced as the first formal parameter.
!>       It has to be named "this".
!>     * subroutines must return exit status. Named constansts from 
!>       the criErrcodes modules must be used.
!>     * generally, all state transition procedures should use transactional
!>       scheme, e.g. updateState(this, new, info)
module altayState
!
use criErrcodes
use criMathUtils
use altayTexAccess
use altayHardTypes
use altayCRSSTypes
use altayStateTypes
use altayHardLaw_KM
#ifdef PEBP_ENABLED
use altayHardLaw_DSH, only: DSHStateVariable => StatVar
#endif

    !> Container for the state variables
    type :: altayStateVariables
        
        !> \todo Replace with: type(MesostructureState)                 :: mesostructure
        type(MaterialFrame)                     :: frame
        
        !> Collection of crystals (grains). 
        type(TextureData)                       :: texture
        
        !> \fixme: decide whether the crss_ratios should appear as the state variables
        !> At this moment they are just parameters.
        
        !> CRSS applicable to every grain (only for non-hardening model)
        ! type(CRSSData)                          :: crss_ratios
        
        !> \fixme: decide whether the crss_array is actually needed.
        
        !> Collection of CRSS per grain (only for certain hardening models)
        ! type(CRSSData), dimension(:), pointer   :: crss_array => null()

#ifdef PEBP_ENABLED
        !> State variables of the DSH hardening law.
        type(DSHStateVariable),dimension(:),allocatable :: dsh_state
#endif
        !> State variables of the Kocks-Mecking hardening law.
        type(KMStateVariables),dimension(:),allocatable :: km_state
        
    end type


    !> State variables.
    !>
    !> The user update only the state variables in the `new` field
    !> and leave the `old` field unmodified. Once all the updates are
    !>  introduced, the procedure altayStateData_advance must be called.
    type :: altayStateData
        !> Current set of state variables
        type(altayStateVariables), pointer   :: old => null()
        !> New (updated) set of state variables.
        type(altayStateVariables), pointer   :: new => null()

        type(altayStateVariables), dimension(0:1),private :: states
        
    end type


contains


    !> Re-set the `old` and `new` components of altayStateData object.
    !> The state variables are _not_ modified.
    integer function altayStateData_init(this) result(info)
    implicit none
    type(altayStateData),target,intent(inout) :: this
    !
        this%old => this%states(0)
        this%new => this%states(1)
        info = criSuccess
    !
    end function


    !> Advance the state data.
    integer function altayStateData_advance(this) result(info)
    implicit none
    type(altayStateData),target,intent(inout)  :: this
    !
        if (associated(this%old, this%states(1))) then
            this%old => this%states(0)
            this%new => this%states(1)
        else
            this%old => this%states(1)
            this%new => this%states(0)
        endif
        info = criSuccess
    !
    end function


    !> Check consistency of altayStateData object.
    logical function altayStateData_isValid(this) result(is_ok)
    implicit none
    type(altayStateData),target,intent(in)  :: this
    !
        is_ok = .false.
        if (associated(this%old) .and. associated(this%new) .and. &
            .not. associated(this%old,this%new) .and. &
            ( (associated(this%old, this%states(0)) .and. &
               associated(this%new, this%states(1))) &
              .or. &
              (associated(this%old, this%states(1)) .and. &
               associated(this%new, this%states(0))) &
           )) then
            !
            if (allocated(this%old%texture%grains) .and. &
                allocated(this%new%texture%grains)) then
                is_ok = (size(this%old%texture%grains) == size(this%new%texture%grains))
            endif
            !
        endif
    !
    end function


#ifdef TESTING_ENABLED
    !!! TESTING -->
    !> Print diagnostic information about altayStateData object
    subroutine altayStateData_printStatus(this) 
    implicit none
    type(altayStateData),target,intent(in)  :: this
    !
        write(*,fmt=100)
        write(*,*) 'State valid: ', altayStateData_isValid(this)
        write(*,*) 'associated old:', associated(this%old), &
                   'associated new:', associated(this%new)
        if (associated(this%old) .and. associated(this%new)) then
            write(*,*) 'allocated grains, old:', allocated(this%old%texture%grains), &
                       'allocated grains, new:', allocated(this%new%texture%grains)
            write(*,*) 'old->0', associated(this%old, this%states(0)), &
                       'old->1', associated(this%old, this%states(1)) 
            write(*,*) 'new->0', associated(this%new, this%states(0)), &
                       'new->1', associated(this%new, this%states(1)) 
        endif
        write(*,*) 'allocated grains, 0:', allocated(this%states(0)%texture%grains), &
                   'allocated grains, 1:', allocated(this%states(1)%texture%grains)
        write(*,fmt=100)
        !
        100 format(20('-'))
    !
    end subroutine
    !!! <<-- TESTING
#endif


    !> Return the number of crystals in altayStateData object.
    integer function altayStateData_size(this) result(n)
    implicit none
    type(altayStateData),target,intent(in)  :: this
    !
        n = 0
        if (altayStateData_isValid(this)) n = size(this%old%texture%grains)
    !
    end function

    
    integer function altayStateData_assemble(this) result(info)
    type(altayStateData),intent(inout)  :: this
    !
        ! Copy the initial state. We rely on Fortran 2003 automatic allocation 
        ! (standard semantics).
        this%states(1) = this%states(0)
        info = criSuccess
    !
    end function
    
    
end module
