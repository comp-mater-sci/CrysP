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
      
      ! CType5 is actually a struct with fields size_t n and double * array.
	type,bind(c) :: CType5
		integer(c_size_t)             :: n
            
            type(c_ptr)                   :: array
      end type      
      
      
      
      
      interface CType5_double
            module procedure CType5_double_c2f, CType5_double_f2c
      end interface
      
      
      type,bind(c) :: TypeXNoConstructor
	logical(c_bool)         :: m_flag
	integer(c_size_t)       :: m_lenght
	type(c_ptr)             :: m_array
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

            integer(c_size_t) function cppFx_CType5(cobj) bind(c,name='cppFx_CType5')
            use, intrinsic    :: iso_c_binding
            import :: CType5
            implicit none
            type(CType5),intent(in) :: cobj
            end function

            integer(c_size_t) function cppFx_bool(arr,nelem) bind(c,name='cppFx_bool')
            use, intrinsic    :: iso_c_binding
            logical(c_bool),dimension(*)  :: arr
            integer(c_size_t),value       :: nelem
            end function

            integer(c_size_t) function cppFx_TypeXNoConstructorArray(nelems, array) bind(c,name='cppFx_TypeXNoConstructorArray')
            use, intrinsic    :: iso_c_binding
            import :: TypeXNoConstructor
            integer(c_size_t),value                   :: nelems
            type(TypeXNoConstructor),dimension(*)     :: array
            end function

            integer(c_size_t) function cppFx_TypeXWithConstructorArray(nelems, array) bind(c,name='cppFx_TypeXWithConstructorArray')
            use, intrinsic    :: iso_c_binding
            import :: TypeXNoConstructor
            integer(c_size_t),value                   :: nelems
            type(TypeXNoConstructor),dimension(*)     :: array
            end function
 
      
		integer(c_size_t) function cppFx_TypeXNoConstructor(obj) bind(c,name='cppFx_TypeXNoConstructor')
            use, intrinsic    :: iso_c_binding
            import :: TypeXNoConstructor
            type(TypeXNoConstructor)     :: obj
            end function
      
		integer(c_size_t) function cppFx_TypeXWithConstructor(obj) bind(c,name='cppFx_TypeXWithConstructor')
            use, intrinsic    :: iso_c_binding
            import :: TypeXNoConstructor
            type(TypeXNoConstructor)     :: obj
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
      type(CType4_fort),target,intent(in)  :: fobj
      type(CType4)       :: cobj
      !
            cobj%n = size(fobj%array)
            cobj%array = c_loc(fobj%array)
      !
      end function

      
      function CType5_double_c2f(cobj) result(fobj)
      implicit none
      type(CType5),intent(in)                   :: cobj
      double precision,dimension(:),pointer     :: fobj
      !
            call c_f_pointer(cobj%array,fobj,[cobj%n])
      !
      end function
      
      function CType5_double_f2c(fobj) result(cobj)
      implicit none
      double precision,allocatable,target,dimension(:),intent(in)  :: fobj
      type(CType5)       :: cobj
      !
            cobj%n = size(fobj)
            cobj%array = c_loc(fobj)
      !
      end function

end module