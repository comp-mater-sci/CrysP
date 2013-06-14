module cppLibInterface
      use,intrinsic :: iso_c_binding
      implicit none
      integer,parameter :: tiny_array_len = 2
      integer,parameter :: small_array_len = 10
      integer,parameter :: tens_dim = 3
      
      type,bind(c) :: CType1
            integer(c_size_t)       :: n
            real(c_double)          :: array(small_array_len)
      end type
      
      type,bind(c) :: CType2
            integer(c_size_t)       :: n
            type(c_ptr)             :: array
      end type
      
      
      type,bind(c) :: CType3
		real(c_double),dimension(tens_dim,tens_dim) :: tens = 0.D0
	end type

	type,bind(c) :: CType4
		integer(c_size_t)             :: n
            ! There is no way here to declare deferred shape array:
		! type(CType3),dimension(:)     :: array
            ! ... or assumed size arrays:
            ! type(CType3),dimension(*)     :: array
            ! ... or explicit size that is not known at compile time:
            ! type(CType3),dimension(n)     :: array
            ! BUT: we can receive/transfer the array as a C-type pointer
            !      and convert it to/from a native, non-interoperable CType4_fort:
            type(c_ptr)                   :: array
      end type
      
      type :: CType4_fort
            type(CType3),dimension(:),pointer   :: array
      end type

      interface CType4_helper
            module procedure CType4_helper_c2f, CType4_helper_f2c
      end interface
      
      
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
            type(CType1),intent(in)            :: obj
            end function
            
            integer(c_int) function cppFx_CType2(obj) bind(c,name="cppFx_CType2")
            use, intrinsic    :: iso_c_binding
            import :: CType2
            implicit none
            type(CType2),intent(in)            :: obj
            end function
            
            ! void * cppFx_allocate(size_t n);
            type(c_ptr) function cppFx_allocate(n) bind(c,name="cppFx_allocate")
            use, intrinsic    :: iso_c_binding
            implicit none
            integer(c_size_t),value    :: n
            end function

            subroutine cppFx_CType4(obj) bind(c,name="cppFx_CType4")
            use, intrinsic    :: iso_c_binding
            import :: CType4
            implicit none
            type(CType4)            :: obj
            end subroutine
            
            
            ! CType4 * cppFx_allocateCType4(size_t n)
            type(c_ptr) function cppFx_allocateCType4(n) bind(c,name="cppFx_allocateCType4")
            use, intrinsic    :: iso_c_binding
            implicit none
            integer(c_size_t),value :: n
            end function
            
      end interface

contains

      
      function CType4_helper_c2f(cobj) result(fobj)
      implicit none
      type(CType4),intent(in)       :: cobj
      type(CType4_fort)             :: fobj
      !
            call c_f_pointer(cobj%array,fobj%array,[cobj%n])
      !
      end function
      
      function CType4_helper_f2c(fobj) result(cobj)
      implicit none
      type(CType4_fort),intent(in)  :: fobj
      type(CType4)       :: cobj
      !
            cobj%n = size(fobj%array)
            cobj%array = c_loc(fobj%array)
      !
      end function

end module