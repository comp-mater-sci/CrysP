!
! $Id: criIterUtils.f90 2388 2015-11-06 22:25:10Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of the initial release: 2014-02-16
!>    $Revision: 2388 $
!>    $Date: 2015-11-06 23:25:10 +0100 (Fri, 06 Nov 2015) $
!>
!>    History of modifications: (see svn log)
!>
!
#include "criStdDefs.fpp"
!
!> Operations on subscripts and masks
!>
!> The module provides a set of procedures that simplify some
!> typical vector expressions for which there is neither intrinsic
!> function nor construct available.
module criIterUtils
implicit none

contains
    
    !> Construct a vector of indices from a mask
    !>
    !> The function calculates a vector of indices that correspond to 
    !> the positions of .true. value in the mask. 
    !> 
    !> Note that you CANNOT store the result of the function in an 
    !> allocatable variable. If you need a long-living vector of indices,
    !> consider the subroutine subscriptsFromMask.
    !>
    !> It is possible to provide the lower bound for the indices
    !> by setting optional argument lb. This way you can virtually assign bounds
    !> to the mask as it was mask(lb,:). Note that the mask remains implicitly 
    !> shaped, so no copy-on-entry should be usually needed.
    !>
    !> Examples
    !> --------
    !> which([.false.,.false.,.false.]) is []
    !> which([.true.,.false.,.true.]) is [1,3]
    !> which([.true.,.false.,.true.],-1) is [-1,1]
    !> s = [5,2,1,0,4]
    !> s(which(s > 1)) is [5,2,4]
    !> DON'T DO any of these:
    !> idx = which(s > 1) is INVALID if idx is allocatable
    !> iidx = which(s > 1) puts [1,2,5] in the iidx if iidx is an automatic 
    !> array of extent that conforms with s, but gives no hint how many elements are set.
    pure function which(mask,lb) result(y)
    logical,dimension(:),intent(in) :: mask  !< Mask
    integer,intent(in),optional     :: lb    !< Lower bound of indices
    integer,dimension(:),allocatable :: y
    !
        call which_indices(mask,y,lb)
    !
    end function

    !> Construct a vector of indices from a mask
    !>
    !> The subroutine calculates a vector of indices that correspond to 
    !> the positions of .true. value in the mask and then stores it
    !> in the allocated array. The size of the output array is the number
    !> of .true. elements in the mask.
    !>
    !> It is possible to provide the lower bound for the indices
    !> by setting optional argument lb. This way you can virtually assign bounds
    !> to the mask as it was mask(lb,:). Note that the mask remains implicitly 
    !> shaped, so no copy-on-entry should be usually needed.
    !>
    !> Examples
    !> --------
    !> call which_indices([.false.,.false.,.false.],idx) results in idx=[]
    !> call which_indices([.true.,.false.,.true.],idx) results in idx=[1,3]
    !> call which_indices([.true.,.false.,.true.],idx,-1) is [-1,1]
    pure subroutine which_indices(mask,indices,lb)
    logical,dimension(:),intent(in) :: mask                 !< Mask
    integer,dimension(:),allocatable,intent(out) :: indices !< vector of indices
    integer,intent(in),optional     :: lb                   !< Lower bound of indices
    !
    integer :: nelems,nfound,i,idx
    !
    ! Remark: this subroutine can be implemented by means of pack(),
    ! but this would require a temporary of extent size(mask).
    !
        nelems = count(mask)
        allocate(indices(nelems))
        if (nelems > 0) then
            idx = 1
            if (present(lb)) idx = lb
            nfound = 0
            do i=1, size(mask)
                if (mask(i)) then
                    nfound = nfound + 1
                    indices(nfound) = idx
                    if (nfound == nelems) exit
                endif
                idx = idx + 1
            enddo
        endif
    !
    end subroutine
    
end module    
