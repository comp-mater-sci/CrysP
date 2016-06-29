
#include "criTest.fpp"

module criTestExpandableVector
use criExpandableVector
use criTest
implicit none
private

public criTestExpandableVector_main

contains
      
    !> main driver routine of criTestPath      
    logical function criTestExpandableVector_main() result(stat)
    implicit none
      
        stat = test_init()
            
        stat = test_expand() 
        
        stat = test_push()
        
        stat = test_prealloc_push()
        
    end function


      
    logical function test_init()
    implicit none
    
    type(xVector_integer) :: v
    
    !
        test_init = .false.
        ! Test:
        _TEST('size, empty vector', xVector_size(v) == 0)

        ! Test:
        _TEST('capacity, empty vector', xVector_capacity(v) == 0)
        
        test_init = .true.
        
    end function
    
    
    logical function test_expand()
    implicit none
    
    type(xVector_integer),target :: v
    integer :: info
    !
        test_expand = .false.
        call xVector_expand(v, 0, info)
        ! Test:
        _TEST('size, empty after zero expand', xVector_size(v) == 0)
        ! Test:
        _TEST('capacity, empty after zero expand', xVector_capacity(v) == 0)
        
        
        call xVector_expand(v, 1, info)
        ! Test:
        _TEST('size, empty after expand by 1', xVector_size(v) == 0)
        ! Test:
        _TEST('capacity, empty after expand by 1', xVector_capacity(v) == 1)
        
        ! Test: another expand should be ignored
        call xVector_expand(v, 1, info)
        _TEST('size, after expand by 1', xVector_size(v) == 0)
        ! Test:
        _TEST('capacity, after expand by 1', xVector_capacity(v) == 1)

        
        test_expand = .true.
        
    end function
    
    
    logical function test_push()
    implicit none
    
    type(xVector_integer),target :: v
    !
        test_push = .false.

        _TEST('push', xVector_push(v, 1) == criSuccess)
        ! Test:
        _TEST('size, after push #1', xVector_size(v) == 1)
        ! Test:
        _TEST('capacity, after push #1', xVector_capacity(v) == 1)
        
        _TEST('push #2', xVector_push(v, 2) == criSuccess)
        ! Test:
        _TEST('size, after push #2', xVector_size(v) == 2)
        ! Test:
        _TEST('capacity, after push #2', xVector_capacity(v) == 4)
        
        _TEST('elements, after push #2', all(v%values == [1, 2]))
        
        _TEST('push #3', xVector_push(v, 3) == criSuccess)
        ! Test:
        _TEST('size, after push #3', xVector_size(v) == 3)
        ! Test:
        _TEST('capacity, after push #3', xVector_capacity(v) == 4)
        
        _TEST('elements, after push #3', all(v%values == [1, 2, 3]))
        
        test_push = .true.
        
    end function
    
    
    logical function test_prealloc_push()
    implicit none
    type(xVector_integer),target :: v
    integer :: info
    !
        test_prealloc_push = .false.
        
        call xVector_expand(v, 2, info)
        
        _TEST('expand', info  == criSuccess)
        ! Test:
        _TEST('size, after expand', xVector_size(v) == 0)
        ! Test:
        _TEST('capacity, after expand', xVector_capacity(v) == 2)
        
        
        _TEST('push', xVector_push(v, 1) == criSuccess)
        ! Test:
        _TEST('size, after push #1', xVector_size(v) == 1)
        ! Test:
        _TEST('capacity, after push #1', xVector_capacity(v) == 2)
        !
        _TEST('push #2', xVector_push(v, 2) == criSuccess)
        ! Test:
        _TEST('size, after push #2', xVector_size(v) == 2)
        ! Test:
        _TEST('capacity, after push #2', xVector_capacity(v) == 2)
        
        _TEST('elements, after push #2', all(v%values == [1, 2]))
        
        ! Test: push #3 triggers expansion
        _TEST('push #3', xVector_push(v, 3) == criSuccess)
        ! Test:
        _TEST('size, after push #3', xVector_size(v) == 3)
        ! Test: last element
        _TEST('last element, after push #3', v%values(size(v)) == 3)
        ! Test:
        _TEST('capacity, after push #3', xVector_capacity(v) == 6)
        ! Test:
        _TEST('elements, after push #3', all(v%values == [1, 2, 3]))

        test_prealloc_push = .true.
    !
    end function
    
    
    
end module
