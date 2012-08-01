!
! $Id$
!
!> RCM: Runtime Control Module
!> The module implements handling of "stop" and other termination conditions.
module altayRCM
      
      integer,parameter,private       :: exception_msg_len = 128

      !>@{ \name Action identifiers
      integer,parameter       :: RCM_CNT  = 0   !< attempt to continue execution
      integer,parameter       :: RCM_RTN  = 1   !< attempt to return (shall not propagate along the call stack)
      integer,parameter       :: RCM_EXT  = 2   !< attempt to return (shall propagate along the call stack)
      integer,parameter       :: RCM_STP  = 10  !< terminate the execution
      !>@}
      
      type RCMException
            
            integer                             :: error_code = 0

            character(len=exception_msg_len)    :: fx = ''

            character(len=exception_msg_len)    :: msg = ''
            
            integer                             :: action = RCM_CNT
                  
      end type
      
      
      !> 
      integer,parameter,private     :: RCM_stack_slice = 32
      
      !> Resizable stack of exceptions. It has shape (1:)
      type(RCMException),dimension(:),allocatable,save,private     :: RCM_stack
      
      !> Stack top pointer. 0 indicates an empty stack
      integer,save,private :: RCM_stack_top = 0
            
contains

      !> Informs whether there are outstanding exceptions on the stack. Returns .false. if there is 
      !> any non-processed exception left.
      logical function RCM_empty()
      implicit none
            RCM_empty = (RCM_stack_top == 0)
      end function

      !> Informs whether there is a
      logical function RCM_signal()
      implicit none
            ! No signal if the module is not initialized yet or the stack is empty.
            RCM_signal = .false.
            if (.not. allocated(RCM_stack)) return
            if (RCM_empty()) return
            if (RCM_stack(RCM_stack_top)%action /= RCM_CNT) RCM_signal = .true.
      end function
      
      
      !> Probes the stack for the error code of the last operation.
      !> Returns 0 if no exception is lying on the stack.
      integer function RCM_topError()
      implicit none
      !
            RCM_topError = 0
            if (.not. RCM_empty()) RCM_topError = RCM_stack(RCM_stack_top)%error_code 
      !
      end function

      !> Throws an exception and puts it on the stack.
      subroutine RCM_throw(ec,fx,msg,action)
      implicit none
      integer,intent(in)            :: ec !< Error code
      character(len=*),intent(in)   :: fx !< Name of the function that raised the exception.
      character(len=*),intent(in)   :: msg 
      integer,intent(in)            :: action !< action identifier
      ! 
      type(RCMException),dimension(:),allocatable :: tmp_stack
      integer :: old_size
      !
            ! prepare the stack if needed
            if (.not. allocated(RCM_stack)) allocate(RCM_stack(RCM_stack_slice))
            RCM_stack_top = RCM_stack_top + 1
            if (size(RCM_stack) < RCM_stack_top) then
                  old_size = size(RCM_stack)
                  ! expand the stack:
                  call move_alloc(RCM_stack,tmp_stack)
                  allocate(RCM_stack(old_size + RCM_stack_slice))
                  RCM_stack(1:old_size) = tmp_stack(1:old_size) ! explicit shape&size aren't really needed, but for sake of clarity...
                  deallocate(tmp_stack)
            endif
            ! The stack is ready for setting the top.
            RCM_stack(RCM_stack_top) = RCMException(ec,fx,msg,action)
      !
      end subroutine
      
      
      !> Catches the top-most exception.
      !> 
      subroutine RCM_catch(ec,fx,msg,action,info)
      implicit none
      integer,intent(out)           :: ec !< Error code
      character(len=*),intent(out)  :: fx !< Name of the function that raised the exception.
      character(len=*),intent(out)  :: msg !< Message/information about the exception 
      integer,intent(out)           :: action !< action identifier
      integer,intent(out)           :: info !< Exit code (0 on successful delivery/)
      !
            info = 1
            if (RCM_empty() .or. (.not. allocated(RCM_stack))) return
            ! Get the information
            ec     = RCM_stack(RCM_stack_top)%error_code
            fx     = RCM_stack(RCM_stack_top)%fx
            msg    = RCM_stack(RCM_stack_top)%msg
            action = RCM_stack(RCM_stack_top)%action
            ! Lower the stack
            RCM_stack_top = RCM_stack_top - 1
            info = 0
      end subroutine

      
      !> Cleans up the excetion stack.
      subroutine RCM_clean()
      implicit none
      !
            RCM_stack_top = 0
      !
      end subroutine
      
end module