
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
!> 
!> Consult readme.txt for general guidelines on AlTay development.
!>
module altayState
!
use criErrcodes
use criMathUtils
use altayMiscutils, only: unitMatrix
use altayHardTypes
use altayCRSSTypes
use altayStateTypes
use altayHardLaw_KM
#ifdef PEBP_ENABLED
use altayHardLaw_DSH, only: DSHStateVariable => StatVar
#endif

    !> Container for the state variables
    type :: altayStateVariables
        
        !> 
        type(MaterialFrame)                     :: frame
        
        !> Collection of crystals (grains). 
        type(TextureData)                       :: texture
        
        !> CRSS applicable to every grain (only for non-hardening model)
        type(CRSSData)                          :: crss_ratios
        
        !> Collection of CRSS per grain (only for certain hardening models)
        type(CRSSData), dimension(:), pointer   :: crss_array => null()

#ifdef PEBP_ENABLED
        !> 
        type(DSHStateVariable),dimension(:),allocatable :: dsh_state
#endif
        !>
        type(KMStateVariables),dimension(:),allocatable :: km_state
        
        ! TODO: check if it is actually needed
        ! integer,private :: active_hard_law = hard_none
        
    end type
    
    !> State variables
    type :: altayStateData
        
        type(altayStateVariables), pointer   :: old => null()

        type(altayStateVariables), pointer   :: new => null()

        type(altayStateVariables), dimension(0:1),private :: states
        
    !contains
    !    procedure :: advance => altayStateData_advance
    end type

    interface
        integer function StateDataUnaryFx(this)
        import :: altayStateData
        type(altayStateData),target,intent(inout)  :: this
        end function
    end interface

    ! DONT USE!
    ! TODO: add explanation why the initialization function does not work
    !       properly. (1) the function returns an object (on stack) that is subsequently 
    !       _copied_ (using the = operator) into the final object. (2) copying of pointers 
    !       is handled in a different way than copying allocatable arrays under F2003 
    !       semantics.
    interface altayStateData
        module procedure altayStateData_init
    end interface
    
contains

    function altayStateData_init() result(obj)
    implicit none
    type(altayStateData),target :: obj
    !
        obj%old => obj%states(0)
        obj%new => obj%states(1)
#ifdef TESTING_ENABLED
        !!! TESTING -->>
        call altayStateData_printStatus(obj)
        !!! <<-- TESTING
#endif
    !
    end function
    
    integer function altayStateData_update(this) result(info)
    implicit none
    type(altayStateData),target,intent(inout) :: this
    !
        this%old => this%states(0)
        this%new => this%states(1)
        info = criSuccess
#ifdef TESTING_ENABLED
        !!! TESTING -->>
        call altayStateData_printStatus(this)
        !!! <<-- TESTING
#endif
    !
    end function
    
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
    
    logical function altayStateData_isValid(this) result(is_ok)
    implicit none
    type(altayStateData),target,intent(in)  :: this
    !
        is_ok = .false.
        if (associated(this%old) .and. associated(this%new) .and. &
            .not. associated(this%old,this%new) .and. &
            ( (associated(this%old, this%states(0)) .and. associated(this%new, this%states(1))) &
              .or. &
              (associated(this%old, this%states(1)) .and. associated(this%new, this%states(0))) &
           )) then
            !
            if (allocated(this%old%texture%grains) .and.allocated(this%new%texture%grains)) then
                is_ok = (size(this%old%texture%grains) == size(this%new%texture%grains))
            endif
            !
        endif
    !
    end function
    
#ifdef TESTING_ENABLED
    !!! TESTING -->
    subroutine altayStateData_printStatus(this) 
    implicit none
    type(altayStateData),target,intent(in)  :: this
    !
        write(*,fmt=100)
        write(*,*) 'State valid: ', altayStateData_isValid(this)
        write(*,*) 'associated old:', associated(this%old), 'associated new:', associated(this%new)
        if (associated(this%old) .and. associated(this%new)) then
            write(*,*) 'allocated grains, old:', allocated(this%old%texture%grains), 'allocated grains, new:', allocated(this%new%texture%grains)
            write(*,*) 'old->0', associated(this%old, this%states(0)), 'old->1', associated(this%old, this%states(1)) 
            write(*,*) 'new->0', associated(this%new, this%states(0)), 'new->1', associated(this%new, this%states(1)) 
        endif
        write(*,*) 'allocated grains, 0:', allocated(this%states(0)%texture%grains), 'allocated grains, 1:', allocated(this%states(1)%texture%grains)
        write(*,fmt=100)
        !
        100 format(20('-'))
    !
    end subroutine
    !!! <<-- TESTING
#endif
    
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
        ! Copy the initial state. We rely on Fortran 2003 automatic allocation (standard semantics).
        this%states(1) = this%states(0)
        info = criSuccess
    !
    end function
    
    
end module
