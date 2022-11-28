module tracing
    implicit none
    
    interface vef_trace
        module procedure vef_trace_str, vef_trace_tensor
    end interface

    private
    public  :: vef_trace 

contains

    subroutine vef_trace_str(caller_module, caller_routine, message)
        character(len=*), intent(in)    :: caller_module, caller_routine, message
#ifdef TRACE
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', message 
#endif
    end subroutine

    subroutine vef_trace_tensor(caller_module, caller_routine, tensor)
        character(len=*), intent(in)                :: caller_module, caller_routine
        double precision, dimension(..), intent(in) :: tensor
        double precision                            :: buffer

#ifdef TRACE
        
        select rank(tensor)
            rank(0)
                buffer = tensor
            rank(1)
                buffer = sum(tensor)
            rank(2)
                buffer = sum(tensor)
            rank(3)
                buffer = sum(tensor)
            rank default
                print *, "TRACE Invalid rank!"
                call exit(1)
        end select
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', buffer
#endif
    end subroutine
end module
