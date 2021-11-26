!
! $Id$
!
!> RCM: Runtime Control Module
!> The module implements handling of "stop" and other termination conditions.
module altayRCM
implicit none
private

      integer,parameter,public       :: exception_msg_len = 128
      integer,parameter,public       :: message_msg_len = 512

      !>@{ \name Action identifiers
      integer,parameter,public       :: RCM_CNT  = 0   !< attempt to continue execution
      integer,parameter,public       :: RCM_RTN  = 1   !< attempt to return (shall not propagate along the call stack)
      integer,parameter,public       :: RCM_EXT  = 2   !< attempt to return (shall propagate along the call stack)
      integer,parameter,public       :: RCM_STP  = 10  !< terminate the execution
      !>@}
      
      type :: RCMException
            
            integer                             :: error_code = 0

            character(len=exception_msg_len)    :: fx = ''

            character(len=exception_msg_len)    :: msg = ''
            
            integer                             :: action = RCM_CNT
                  
      end type
      
      
      !> 
      integer,parameter     :: RCM_stack_slice = 16
      
      !> Resizable stack of exceptions. It has shape (1:)
      type(RCMException),dimension(:),allocatable,save     :: RCM_stack
      
      !> Stack top pointer. 0 indicates an empty stack
      integer,save :: RCM_stack_top = 0
      
      interface RCM_catch
            module procedure RCM_catch_exception, RCM_catch_message, RCM_catch_print
      end interface
      
      public :: RCM_empty, RCM_signal, RCM_topError, RCM_throw, RCM_catch,  RCM_clean
      
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
      integer,intent(in)            :: ec       !< Error code
      character(len=*),intent(in)   :: fx       !< Name of the function that raised the exception.
      character(len=*),intent(in)   :: msg      !< Message associated to the exception.
      integer,intent(in)            :: action   !< Action identifier
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
                  ! explicit shape&size aren't really needed, but for sake of clarity...
                  RCM_stack(1:old_size) = tmp_stack(1:old_size) 
                  deallocate(tmp_stack)
            endif
            ! The stack is ready for setting the top.
            RCM_stack(RCM_stack_top) = RCMException(ec,fx,msg,action)
      !
      end subroutine
      
      
      !> Catches the top-most exception.
      !> 
      !> Returns .true. if an exception is caught and .false. otherwise.
      logical function RCM_catch_exception(ec,fx,msg,action) result(info)
      implicit none
      integer,intent(out)           :: ec !< Error code
      character(len=*),intent(out)  :: fx !< Name of the function that raised the exception.
      character(len=*),intent(out)  :: msg !< Message/information about the exception 
      integer,intent(out)           :: action !< action identifier
      
      !
            info = .false.
            !
            if (RCM_empty() .or. (.not. allocated(RCM_stack))) then
                  ! Set (clean) all output variables
                  ec = 0; fx = ''; msg = ''; action = 0; 
            else
                  ! Get the information
                  ec     = RCM_stack(RCM_stack_top)%error_code
                  fx     = RCM_stack(RCM_stack_top)%fx
                  msg    = RCM_stack(RCM_stack_top)%msg
                  action = RCM_stack(RCM_stack_top)%action
                  ! Lower the stack
                  RCM_stack_top = RCM_stack_top - 1
                  info = .true.
            endif
      !
      end function

      !> Catches the top-most exception and presents a formatted message about the exception.
      !>
      !> Returns .true. if an exception is caught and .false. otherwise.
      logical function RCM_catch_message(ec,message) result(info)
      implicit none
      integer,intent(out)           :: ec      !< Error code
      character(len=*),intent(out)  :: message !< Message
      !
      character(len=exception_msg_len) ::  msg,fx  
      integer :: action
      !            
            info = RCM_catch(ec,fx,msg,action)
            if (info) then
                 write(message,fmt=100) trim(fx),ec,trim(msg)
            endif
            100 format('Procedure ',A,1X,'exit code:',I0,1X,'Reason: ',A)
      !
      end function
      
      !> Catches all exceptions and prints a formatted output to unit nunit. 
      !>
      !> Returns .true. if an exception is caught and .false. otherwise.
      logical function RCM_catch_print(nunit) result(info)
      implicit none
      integer,intent(in)      :: nunit
      !
      integer :: ec
      character(len=message_msg_len) :: message
      !
            info = .false.
            do while (RCM_catch_message(ec,message))
                  info = .true.
                  write(nunit,'(A)') trim(message)
            enddo
      !
      end function
      
      
      
      !> Cleans up the excetion stack.
      subroutine RCM_clean()
      implicit none
      !
            RCM_stack_top = 0
      !
      end subroutine
      
end module