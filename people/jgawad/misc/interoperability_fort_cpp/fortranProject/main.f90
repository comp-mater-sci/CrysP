
subroutine runTests()
use cppLibInterface
implicit none
      
      integer(c_size_t) :: n, info
      integer(c_int)    :: arr(small_array_len)
      
      character(len=255)      :: string1
      
      type(CType1)      :: obj1
      
      type(CType2)      :: obj2
      double precision,allocatable,dimension(:),target :: obj2_array ! Note: ifort does not insist on "target" modifier, although it should...
      
      type(c_ptr)       :: ptr_cpp_dynarr
      double precision,dimension(:),pointer :: fptr_cpp_dynarr
      
      type(CType4)      :: obj3
      type(CType4_fort) :: obj3_fort
      integer,parameter :: tens_vec_len = tens_dim * tens_dim
      double precision,dimension(tens_vec_len) :: tmp_vec
      logical,dimension(tens_dim,tens_dim)          :: tmp_mask

      type(c_ptr)             :: ptr_obj4
      type(CType4),pointer    :: fptr_obj4
      type(CType4_fort)       :: obj4_fort

      double precision,allocatable,dimension(:),target :: obj5
      integer(c_size_t)       :: test_retval
      
      integer :: i,j
      !
      ! Test 1: call to void cppFx_simple(void)
      !
      write(*,*) 'void cppFx_simple(void)'
      call cppFx_simple()
      !------------------------------------------------------------
      
      !
      ! Test 2: call to int cppFx_ArgsIntPtrInt(size_t n, int arr[])
      !
      write(*,100) 'cppFx_ArgsIntPtrInt(size_t n, int arr[])'
      n = small_array_len
      forall (i=1:n) arr(i) = i
      
      info = cppFx_ArgsIntPtrInt(n,arr)
      write(*,*) 'cppFx_ArgsIntPtrInt: ', info
      !------------------------------------------------------------
      
      !
      ! Test 3: call to int cppFx_Fstring( const char * str, int len, int len_trim )
      !
      write(*,100) 'int cppFx_Fstring( const char * str, int len, int len_trim )'
      string1 = 'Hello from Fortran'
      info = cppFx_Fstring(string1,len(string1),len_trim(string1))
      write(*,*) 'cppFx_Fstring: ', info
      !------------------------------------------------------------
      
      !
      ! Test 4: call to int cppFx_CType1(const CType1 & obj);
      !
      write(*,100) 'int cppFx_CType1(const CType1 & obj);'
      obj1%n = 512
      forall (i=1:10) obj1%array(i) = dble(i)
      
      info = cppFx_CType1(obj1)
      write(*,*) 'cppFx_CType1: ', info
      !------------------------------------------------------------

      !
      ! Test 5: call to int cppFx_CType2(const CType2 & obj)
      !
      write(*,100) 'int cppFx_CType2(const CType2 & obj)'
      allocate(obj2_array(small_array_len))
      forall (i=1:small_array_len) obj2_array(i) = dble(i)
      obj2.n = small_array_len
      obj2.array = c_loc(obj2_array)
      info = cppFx_CType2(obj2)
      write(*,*) 'cppFx_CType2: ', info
      !------------------------------------------------------------

      !
      ! Test 6: call to void * cppFx_allocate(size_t n)
      !
      write(*,100) 'void * cppFx_allocate(size_t n)'
      ptr_cpp_dynarr = cppFx_allocate(small_array_len)
      call c_f_pointer(ptr_cpp_dynarr,fptr_cpp_dynarr,[small_array_len])
      write(*,*) 'cppFx_CType2 array: ', fptr_cpp_dynarr(:)
      !------------------------------------------------------------

      !
      ! Test 7: call to void cppFx_CType4(CType4 &)
      !
      write(*,100) 'void cppFx_CType4(CType4 &)'
      allocate(obj3_fort%array(tiny_array_len))
      obj3 = CType4_helper(obj3_fort)
      tmp_mask = .true.
      do i=1,size(obj3_fort%array)
            forall(j=1:tens_vec_len) tmp_vec(j)= dble((i-1)*10+j)
            obj3_fort%array(i)%tens =  reshape(tmp_vec,[tens_dim,tens_dim]) 
            ! obj3_fort%array(i)%tens =  unpack(tmp_vec,tmp_mask,0.D0)
      enddo
      write(*,*) 'Before call to cppFx_CType4'
      do i=1,size(obj3_fort%array)
            do j=1,tens_dim 
                  write(*,330) obj3_fort%array(i)%tens(j,:)
            enddo
            write(*,*) '---'
      enddo
      call cppFx_CType4(obj3)
      
      write(*,*) 'After call to cppFx_CType4'
      do i=1,size(obj3_fort%array)
            do j=1,tens_dim 
                  write(*,330) obj3_fort%array(i)%tens(j,:)
            enddo
            write(*,*) '---'
      enddo
      !------------------------------------------------------------

      !
      ! Test 7: call to CType4 * cppFx_allocateCType4(size_t n)
      !
      write(*,100) 'CType4 * cppFx_allocateCType4(size_t)'
      ptr_obj4 = cppFx_allocateCType4(tiny_array_len)
      call c_f_pointer(ptr_obj4, fptr_obj4)
      obj4_fort = CType4_helper(fptr_obj4)
      do i=1,size(obj4_fort%array)
            do j=1,tens_dim 
                  write(*,330) obj4_fort%array(i)%tens(j,:)
            enddo
            write(*,*) '---'
      enddo
      
      
      !
      ! Test 8: call to size_t cppFx_CType5(const CType5 & cobj)
      !
      write(*,100) 'size_t cppFx_CType5(const CType5 & cobj)'
      allocate(obj5(small_array_len))
      forall (i=1:size(obj5)) obj5(i) = dble(i)
      test_retval = cppFx_CType5(CType5_double(obj5))
      
100 format(10('='),/,'Test: ',A,/)            
      
330 format(3(F10.3,1X))      
333 format(3(3(F10.3,1X),/))
      
end subroutine

      
      
      
subroutine runFStringTest()
use KGmetexFString
implicit none
character(len=128) :: str
!
      interface
            ! void newFStringTest(FStringData &);
            subroutine newFStringTest(fstr) bind(c,name='newFStringTest')
            import :: FStringData
            implicit none
            type(FStringData) :: fstr
            end subroutine
      end interface


      call newFStringTest(fstring(str))
      write(*,*) "'",trim(str),"'"
!
end subroutine
      
program fortran_main
implicit none
!
      call runTests()
      call runFStringTest()
!      
end program