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
!> The module implements aquisition of hardening data from Swift hardening law  
module hardSwift
use fngErrcodes
implicit none

      type :: swiftModel
            double precision :: swift_K = 0.D0
            double precision :: swift_n = 0.D0
            double precision :: swift_eps0 = 0.D0
            
      end type

contains
      
      subroutine readSwiftConfig(this,inpunit,info)
      implicit none
      type(swiftModel),intent(inout)      :: this
      integer,intent(in)                  :: inpunit
      integer,intent(out)                 :: info
      !
            ! Read specific data:
            read(inpunit,*,iostat=info,err=100) this%swift_K 
            read(inpunit,*,iostat=info,err=100) this%swift_n
            read(inpunit,*,iostat=info,err=100) this%swift_eps0
            info = fngSuccess
            return
            
      100   info = fngErr_IORead
            return
            
      !
      end subroutine

      
      elemental double precision function swift(eps,K,n,eps0) result(sigma)
      implicit none
      double precision,intent(in) :: eps, K, n, eps0
            sigma = K * ((eps0+eps)**n)
      end function


      
end module