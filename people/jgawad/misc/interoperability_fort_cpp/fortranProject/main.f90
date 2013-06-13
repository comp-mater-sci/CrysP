
subroutine runTests()
use cppLibInterface
implicit none
      integer,parameter :: small_array_len = 10
      integer(c_size_t) :: n, info
      integer(c_int)    :: arr(small_array_len)
      
      character(len=255)      :: string1
      
      type(CType1)      :: obj1
      
      type(CType2)      :: obj2
      double precision,allocatable,dimension(:),target :: obj2_array ! Note: ifort does not insist on "target" modifier
      
      type(c_ptr)       :: ptr_cpp_dynarr
      double precision,dimension(:),pointer :: fptr_cpp_dynarr
      
      
      integer :: i,j
      
      call cppFx_simple()
      
      n = small_array_len
      forall (i=1:n) arr(i) = i
      
      info = cppFx_ArgsIntPtrInt(n,arr)
      write(*,*) 'cppFx_ArgsIntPtrInt: ', info
      
      !------------------------------------------------------------
      string1 = 'Hello from Fortran'
      info = cppFx_Fstring(string1,len(string1),len_trim(string1))
      write(*,*) 'cppFx_Fstring: ', info
      
      !------------------------------------------------------------
      obj1%n = 512
      forall (i=1:10) obj1%array(i) = dble(i)
      
      info = cppFx_CType1(obj1)
      write(*,*) 'cppFx_CType1: ', info

      
      !------------------------------------------------------------
      
      allocate(obj2_array(small_array_len))
      forall (i=1:small_array_len) obj2_array(i) = dble(i)
      obj2.n = small_array_len
      obj2.array = c_loc(obj2_array)
      info = cppFx_CType2(obj2)
      write(*,*) 'cppFx_CType2: ', info

      !------------------------------------------------------------

      ptr_cpp_dynarr = cppFx_allocate(small_array_len)
      call c_f_pointer(ptr_cpp_dynarr,fptr_cpp_dynarr,[small_array_len])
      write(*,*) 'cppFx_CType2 array: ', fptr_cpp_dynarr(:)
      
end subroutine
      
      
program fortran_main
      use cppLibInterface
      implicit none
      
      call runTests()
      
end program