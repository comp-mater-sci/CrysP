!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2013-07-02
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criExpandableVector.f90 
!
#include "criStdDefs.fpp"
!
!> Implementation of a simple, expandable vector with constant amortized insertion time.
module criExpandableVector

      type expandableVector
            !> Private data storage
            integer,dimension(:),allocatable    :: xdata
            
            !> Provides view on contents of the vector. 
            integer,dimension(:),pointer        :: values => null()
      end type

contains

      integer function vectorPush_int(v,item) result(info)
      implicit none
      type(expandableVector),intent(inout),target     :: v
      integer,intent(in)                              :: item
      !
      integer,dimension(:),allocatable :: tmp
      integer :: idx_last
      !
            
            if (.not. allocated(v%xdata)) then
                  idx_last = 0
                  v%values => null()
                  allocate(v%xdata(1),stat=info)
                  if (info /= 0) return
            else
                  idx_last = size(v%values)
                  if (idx_last == size(v%xdata)) then
                        allocate(tmp(2*idx_last))
                        tmp(1:idx_last) = v%xdata(1:idx_last)
                        deallocate(v%xdata)
                        call move_alloc(tmp,v%xdata)
                  endif
            endif
            idx_last = idx_last + 1 !< Move the index
            ! ... and place the item
            v%xdata(idx_last) = item
            v%values => v%xdata(1:idx_last)
            info = 0
      !     
      end function

end module
      