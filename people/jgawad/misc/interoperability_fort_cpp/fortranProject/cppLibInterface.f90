module cppLibInterface
      use,intrinsic :: iso_c_binding
      implicit none
      
      
      
      type,bind(c) :: CType1
            integer(c_size_t)       :: n
            real(c_double)          :: array(10)
      end type
      
      type,bind(c) :: CType2
            integer(c_size_t)       :: n
            type(c_ptr)             :: array
      end type
      
      interface
            subroutine cppFx_simple() bind(c,name="cppFx_simple")
            end subroutine
            
            
            integer(c_int) function cppFx_ArgsIntPtrInt(n,arr) bind(c,name="cppFx_ArgsIntPtrInt")
            use, intrinsic    :: iso_c_binding
            implicit none
            integer(c_size_t),value       :: n
            integer(c_int),dimension(n)   :: arr
            end function
            
            integer(c_int) function cppFx_Fstring(str,len,len_trim) bind(c,name="cppFx_Fstring")
            use, intrinsic    :: iso_c_binding
            implicit none
            character(kind=C_CHAR),intent(in)   :: str(*)
            integer(c_int),value                :: len
            integer(c_int),value                :: len_trim
            end function
            
            integer(c_int) function cppFx_CType1(obj) bind(c,name="cppFx_CType1")
            use, intrinsic    :: iso_c_binding
            import :: CType1
            implicit none
            type(CType1)            :: obj
            end function
            
            integer(c_int) function cppFx_CType2(obj) bind(c,name="cppFx_CType2")
            use, intrinsic    :: iso_c_binding
            import :: CType2
            implicit none
            type(CType2)            :: obj
            end function
            
            ! void * cppFx_allocate(size_t n);
            type(c_ptr) function cppFx_allocate(n) bind(c,name="cppFx_allocate")
            use, intrinsic    :: iso_c_binding
            implicit none
            integer(c_size_t),value    :: n
            end function

            
            
            
      end interface
      
      
end module