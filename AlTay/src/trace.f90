module tracing
    implicit none
    
    private
    public  :: vef_trace_str,    &
               vef_trace_dbl_arr    



contains

    subroutine vef_trace_str(caller_module, caller_routine, message)
        character(len=*), intent(in)    :: caller_module, caller_routine, message
#ifdef TRACE
        print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', message 
#endif
    end subroutine

subroutine vef_trace_dbl_arr(caller_module, caller_routine, arr)
        character(len=*), intent(in)                :: caller_module, caller_routine
        double precision, dimension(:), intent(in)  ::  arr
        integer                                     :: i
#ifdef TRACE
        do i = 1,size(arr)
            print *, 'TRACE ', caller_module, ', ', caller_routine, ', index ', i, ': ', arr(i) 
        end do
#endif
    end subroutine

end module
