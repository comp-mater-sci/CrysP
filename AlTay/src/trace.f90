module tracing
    implicit none
    
    private
    public  :: vef_trace_str,    &
               vef_trace_dbl_arr, &    
               vef_trace_dbl_mat    



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
            print *, 'TRACE ', caller_module, ', ', caller_routine, ': ', arr(i) 
        end do
#endif
    end subroutine
    
    subroutine vef_trace_dbl_mat(caller_module, caller_routine, mat)
        character(len=*), intent(in)                    :: caller_module, caller_routine
        double precision, dimension(:,:), intent(in)    :: mat
        integer                                         :: i
#ifdef TRACE
        do i = 1,size(mat(1,:))
            call vef_trace_dbl_arr(caller_module, caller_routine, mat(i,:))
        end do
#endif
    end subroutine


end module
