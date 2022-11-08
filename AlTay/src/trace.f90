module tracing
    implicit none
    
    private
    public  :: vef_trace    



contains

    subroutine vef_trace(caller_module, caller_routine, message)
        character(len=*), intent(in)    :: caller_module, caller_routine, message
#ifdef TRACE
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', message 
#endif
    end subroutine
end module
