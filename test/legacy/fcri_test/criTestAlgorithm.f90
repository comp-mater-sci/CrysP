#include "criTest.fpp"
#include "criMacros.fpp"
!
!> Tests on the extensions to the Facet potential expression
module criTestAlgorithm
use criAlgorithm
use criTest
implicit none

public criTestAlgorithm_main

contains

      logical function criTestAlgorithm_main() result(stat)
      implicit none

            stat = .true.

            stat = stat .and. test_bounds()

            stat = stat .and. test_replaceAll()

            stat = stat .and. test_isPresent()

            stat = stat .and. test_optionalDefault()

            stat = stat .and. test_CHOOSE()

      end function

      logical function test_bounds() result(res)
      implicit none
      integer,parameter :: n_algorithms = 3
      integer,dimension(:),allocatable :: arr, tst
      logical,dimension(:),allocatable :: tst_val
      integer,dimension(:),allocatable :: tst_lb_idx, tst_ub_idx
      integer,dimension(:),allocatable :: tst_eval
      logical,dimension(:,:),allocatable :: arr_results
      logical,dimension(n_algorithms) :: found

      integer :: i, count
      !
            res = .false.

            arr = [5,7,10,22,33,35,44,50,55,56,60,70,71,72,73,80,90,100]
            tst = [1,4,5,20,60,90,91,100,200]
            tst_val = [.false.,.false.,.true.,.false.,.true.,.true.,.false.,.true.,.false.]

            tst_lb_idx = [1, 1, 1, 4, 11, 17, 18, 18, 19]
            tst_ub_idx = [1, 1, 2, 4, 12, 18, 18, 19, 19]

            allocate(tst_eval(size(tst_val)))
            allocate(arr_results(n_algorithms,size(arr)))


            tst_eval = 0
            !
            ! Test lower bound algorithm

            do i=1, size(tst)
                tst_eval(i) = lower_bound(arr,tst(i))
            enddo
            _TEST('lower_bound, indices', all(tst_eval == tst_lb_idx))

            res = .true.
      !
      end function

      logical function test_replaceAll()
      implicit none
      !
            test_replaceAll = .false.
            ! Test: neutral for empty string (returns empty string)
            _TEST('empty string',(replaceAll('','a','b') == ''))

            ! Test: neutral for strings without "from" (returns a copy of string)
            _TEST('string with no from',(replaceAll('kbx zwd','a','b') == 'kbx zwd'))

            ! Test: single-element string that contains only "from"
            _TEST('single char string with one inst. of from',(replaceAll('a','a','b') == 'b'))

            ! Test:
            _TEST('string with two inst. of from',(replaceAll('abx awd','a','b') == 'bbx bwd'))

            ! Test:
            _TEST('string with three inst. of from',(replaceAll('abx awd gwda','a','b') == 'bbx bwd gwdb'))

            test_replaceAll = .true.
      !
      end function

      logical function test_isPresent()
      implicit none
      !
      character(len=0)  :: empty_string
      character(len=10),dimension(3),parameter      :: list = [character(len=10) :: 'one', 'two', 'three']
      character(len=10),dimension(-1:1),parameter   :: list_nonstandard_shape = [character(len=10) :: 'one', 'two', 'three']
      character(len=10),dimension(0) :: empty_list
      character(len=10),parameter :: val_two_10 = 'two'
      character(len=3),parameter :: val_two_3 = 'two'
      integer :: index
      logical :: is_present
            test_isPresent = .false.
            _TEST('empty string not in empty list', .not.isPresent(empty_string,empty_list))
            _TEST('empty string not in list', .not.isPresent(empty_string,list))
            _TEST('empty string not in shaped list', .not.isPresent(empty_string,list_nonstandard_shape))
            !
            _TEST('value (exact size) not in empty list', .not.isPresent(val_two_10,empty_list))
            _TEST('value (smaller size) not in empty list', .not.isPresent(val_two_3,empty_list))
            !
            _TEST('value (exact size) in list', isPresent(val_two_10,list))
            _TEST('value (smaller size) in list', isPresent(val_two_3,list))
            _TEST('value (exact size) in shaped list', isPresent(val_two_10,list_nonstandard_shape))
            _TEST('value (smaller size) in shaped list', isPresent(val_two_3,list_nonstandard_shape))
            !
            index = -100
            is_present = isPresent(val_two_10,list,index)
            _TEST('value and index (exact size) in list', (is_present .and. (index == 2)))
            index = -100
            is_present = isPresent(val_two_3,list,index)
            _TEST('value and index (smaller size) in list', (is_present .and. (index == 2)))
            ! Note: extent of the array is NOT passed to isPresent
            index = -100
            is_present = isPresent(val_two_10,list_nonstandard_shape,index)
            _TEST('value and index (exact size) in shaped list', (is_present .and. (index == 2)))
            index = -100
            is_present = isPresent(val_two_3,list_nonstandard_shape,index)
            _TEST('value and index (smaller size) in shaped list', (is_present .and. (index == 2)))


      !
      end function

      logical function test_optionalDefault() result(stat)
      implicit none
      !
      logical :: lval
      integer :: ival

            stat = .true.

            stat = stat .and. optionalDefault_absent()

            lval = .true.
            ival = 1

            stat = stat .and. optionalDefault_present(ival, lval)
      !
      end function

      logical function optionalDefault_absent(ival, lval)
      implicit none
      integer,intent(in),optional :: ival
      logical,intent(in),optional :: lval
      !
      logical :: ltest
      integer :: itest

          ltest = .false.
          _TEST('absent logical value, default returned', (optionalDefault(lval,ltest) .eqv. ltest))

          itest = 20
          _TEST('absent integer value, default returned', (optionalDefault(ival,itest) == itest))

          optionalDefault_absent = .true.
      !
      end function


      logical function optionalDefault_present(ival, lval)
      implicit none
      integer,intent(in),optional :: ival
      logical,intent(in),optional :: lval
      !
      logical :: ltest
      integer :: itest

          ltest = .false.
          _TEST('present logical value', (present(lval) .and. optionalDefault(lval,ltest) .eqv. lval))

          itest = 20
          _TEST('present integer value', (present(ival) .and. optionalDefault(ival,itest) == ival))

          optionalDefault_present = .true.
      !
      end function


      logical function test_CHOOSE()
      implicit none
      !
      integer :: yes, no, res, arg
      !
            arg = -1
            yes = 10
            no = 20

            CHOOSE(res, .true., yes, no)
            _TEST('CHOOSE true', res == yes)

            CHOOSE(res, .false., yes, no)
            _TEST('CHOOSE false', res == no)

            CHOOSE(res, arg >= 0, sqrt(dble(arg)), no)
            _TEST('CHOOSE test, runtime error avoided', res == no)
            test_CHOOSE = .true.
      !
      end function


end module
