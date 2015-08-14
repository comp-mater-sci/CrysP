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
use criErrcodes
use altayStateTypes
implicit none


    !> State variables.
    !>
    !> The user may only update the state variables in the `new` field
    !> and must leave the `old` field unmodified. Once all the updates are
    !> done, the procedure altayStateData_advance must be called.
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
            if (allocated(this%old%grainstates%grainstate) .and. &
                allocated(this%new%grainstates%grainstate)) then
                is_ok = (size(this%old%grainstates%grainstate) == size(this%new%grainstates%grainstate))
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
            write(*,*) 'allocated grains, old:', allocated(this%old%grainstates%grainstate), &
                       'allocated grains, new:', allocated(this%new%grainstates%grainstate)
            write(*,*) 'old->0', associated(this%old, this%states(0)), &
                       'old->1', associated(this%old, this%states(1)) 
            write(*,*) 'new->0', associated(this%new, this%states(0)), &
                       'new->1', associated(this%new, this%states(1)) 
        endif
        write(*,*) 'allocated grains, 0:', allocated(this%states(0)%grainstates%grainstate), &
                   'allocated grains, 1:', allocated(this%states(1)%grainstates%grainstate)
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
        if (altayStateData_isValid(this)) n = size(this%old%grainstates%grainstate)
    !
    end function

    
    integer function altayStateData_assemble(this) result(info)
    type(altayStateData),intent(inout)  :: this
    !
        ! Copy the initial state. We rely on Fortran 2003 automatic allocation 
        ! (standard semantics).
        this%states(1) = this%states(0)
        !
        ! Initialize the state. It involves setting up pointers, so it has to be 
        ! done per state.
        !> \fixme Check altayStateData_assemble if initialization is correctly done
        ! call this%old%grainstates%initialize(this%old%texture, info)
        ! if (info /= criSuccess) return
        ! call this%new%grainstates%initialize(this%new%texture, info)
        info = criSuccess
    !
    end function
    
    
end module
