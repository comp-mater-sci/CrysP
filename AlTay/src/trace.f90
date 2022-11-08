module trace
    implicit none
    
    private
    public  :: trace    



contains

    subroutine trace(message)
#ifndef NDEBUG
        character(len=*), intent(in)    :: message

        print *, "TRACE: ", message 
#endif
    end subroutine

end module
