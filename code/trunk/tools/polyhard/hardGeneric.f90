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
!>    \date Date of the initial release: 2013-09-13
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!
!
!> The module implements aquisition of hardening data given by a generic model, i.e. 
!> a hardening model that provides a set of discrete (strain,stress) points.
module hardGeneric
use fngErrcodes
implicit none

      type :: genericModel
            !> Arrays for strain-stress data
            double precision, dimension(:), allocatable     :: vEps, vSigma
            
      end type
      
contains
      
      !> Read strain-stress data from the input file. This will allocate
      !> necessary memory in the genericModel 'this' object.
      subroutine readGenericHardConfig(this,inpunit,info)
      implicit none
      type(genericModel),intent(inout)    :: this
      integer,intent(in)                  :: inpunit
      integer,intent(out)                 :: info
      !
      integer :: i,npoints
      !
            read(inpunit,*,err=100) npoints
            info = fngErr_BadDims
            if (npoints <= 0) return
            if (allocated(this%vEps)) deallocate(this%vEps)
            if (allocated(this%vSigma)) deallocate(this%vSigma)
            allocate(this%vEps(npoints), this%vSigma(npoints))
            do i = 1, npoints
                  read(inpunit,*,err=100) this%vEps(i), this%vSigma(i) 
            enddo
            info = fngSuccess
            return
            100   info = fngErr_IORead
            return
      !
      end subroutine
      
end module