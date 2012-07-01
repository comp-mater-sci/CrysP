!
! $Id$
!
!> RCM: Runtime Control Module
!> The module implements handling of "stop" and other termination conditions.
module altayRCM
      
      integer,parameter,private       :: exception_msg_len = 128
            
      type RCMException
            
            integer                             :: error_code
                  
            character(len=exception_msg_len)    :: msg
                  
            integer                             :: action
                  
      end type
      
      !>@{ \name Action identifiers
      integer,parameter       :: RCM_continue   = 0   !< attempt to continue execution
      integer,parameter       :: RCM_return     = 1   !< attempt to return (shall not propagate along the call stack)
      integer,parameter       :: RCM_exit       = 1   !< attempt to return (shall propagate along the call stack)
      integer,parameter       :: RCM_stop       = 2   !< terminate the execution
      !>@}
            
contains
            
            
      
end module