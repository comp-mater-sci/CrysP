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
!>    \date Date of the initial release: 2013-07-07
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!
!> Calculates polynomial interpolation function for a data set.
!>
!> The module uses LAPACK95
module polyApproximation
implicit none

contains
      
      pure subroutine calculateAppoximation(vEps,vSigma,vCoeff,info)
      use LAPACK95
      ! use mkl95_lapack
      implicit none
      double precision,dimension(:),intent(in)   :: vEps   ! {n}
      double precision,dimension(:),intent(in)   :: vSigma ! {n}
      double precision,dimension(:),intent(out)   :: vCoeff ! {n}
      integer,intent(out)                 :: info
      !
      ! mA: [n x n], mB: [n x nrhs]
      double precision,dimension(:,:),allocatable     :: mA, mB
      integer :: n, i
      !
            info = -1
            n = size(vEps)  ! Number of coefficients, so order of polynomial is n-1
            if (any([size(vSigma),size(vCoeff)] /= n)) return
            !        
            allocate(mA(n,n), mB(n,1))
            
            mB(:,1) = vSigma
            !
            forall (i=1:n)
                  mA(:,i) = vEps**(n-i)
            endforall
            !
            call gesv( mA, mB, info=info)
            vCoeff = mB(:,1)
            !
      !
      end subroutine

end module