!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!>    \author     Jerzy Gawad 
!>    Email:      Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    
!>    \date Date of initial release: 2012-03-26
!>    $Revision$
!>    $Date$
!>    History of modifications: (see SVN log).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 
! 
      
!> Calculation of Jacobi matrix by means of finite differences.
!>
module JacobiFD

      double precision,parameter :: JacobiFD_eps = 1e-7

      interface jacobiPrepCFD
            module procedure jacobiPrepCFD_mX, jacobiPrepCFD_invdelta
      end interface
      
      interface jacobiCalcCFD
            module procedure jacobiCalcCFD_mX, jacobiCalcCFD_invdelta
      end interface
      
contains

      !> The subroutine calculates finite difference points at which the function shall be evaluated.
      !> Central Finite Difference stencil is used.
      !>
      !> \f[ \Delta \mathbf{x}_j =  \mathbf{x} \pm \epsilon\mathbf{e} x_j  \f]
      subroutine jacobiPrepCFD_mX(vX,eps,mX,info)
      implicit none
      !> Point at which the jacobian should evaluated. Size of vX = n
      double precision,dimension(:),intent(in)        :: vX
      !> Precision of finite difference scheme
      double precision,intent(in)                     :: eps
      !> Output matrix consisting of [n x 2n] elements. 
      double precision,dimension(:,:),allocatable,intent(out)      :: mX
      !> Exit code: 0 on succcess.
      integer,intent(out)                             :: info
      !
      integer :: n,m,i,j
      double precision :: delta
            n = size(vX)
            m = 2 * n
            ! Input check
            info = 1
            if ((abs(eps) < epsilon(0.D0)) .or. (n < 1)) return
            allocate(mX(n,m))
            !
            j = 1
            do i=1,m,2
                  delta = eps * vX(j) 
                  if (abs(delta) < epsilon(0.D0)) delta = eps ! eps : non-zero if vX(j) is zero
                  mX(:,i) = vX(:)
                  mX(j,i) = mX(j,i) + delta
                  mX(:,i+1) = vX(:)
                  mX(j,i+1) = mX(j,i+1) - delta
                  j = j + 1
            enddo
            info = 0
      end subroutine

      !> The subroutine calculates Jacobi matrix by means of Central Finite Difference stencil.
      !>
      !> The CFD 
      !> \f[
      !> \frac{\partial f_i}{\partial x_j} = 
      !>     \frac{ f(x_1,\ldots,x_j+\Delta x_j,\ldots,x_n)  - f(x_1,\ldots,  x_j-\Delta x_j,\ldots,x_n )}{2 \Delta x_j}
      !> \f]
      subroutine jacobiCalcCFD_mX(mX,mF,mJ,info)
      implicit none
      !> Finite difference points: [n x 2n] matrix.
      double precision,dimension(:,:),intent(in)      :: mX  ! [n x 2n]
      !> Function values at the corresponding finite difference points. 
      !> Functions are represented by m-dimensional vectors inside the matrix.
      double precision,dimension(:,:),intent(in)      :: mF  ! [m x 2n]
      !> Output Jacobi matrix
      double precision,dimension(:,:),intent(out)     :: mJ  ! [m x n]
      !> Exit code: 0 on succcess.
      integer,intent(out)                             :: info
      !
      integer :: i,j, m, n
      double precision :: idelta
      !
            n = size(mJ,dim=2)
            m = size(mJ,dim=1)
            ! Check dimensions
            info = 1
            if ( (size(mF,dim=1) /= m) .or. (size(mF,dim=2) /= 2 *n) ) return
            !
            j = lbound(mF,dim=2)
            do i=lbound(mJ,dim=2), ubound(mJ,dim=2)
                  idelta = 1.D0 / (mX(i,j) - mX(i,j+1))  
                  mJ(:,i) = idelta * (mF(:,j) - mF(:,j+1))
                  j = j + 2
            enddo
            info = 0
      end subroutine
      

      
      !> The subroutine calculates finite difference points at which the function shall be evaluated.
      !>
      !> Central Finite Difference stencil is used.
      !> Additionally, the inverses of the Delta X increments are calculated. Although it requires 
      !> additional storage, subsequent calculations of the Jacobi matrix are easier and faster, in
      !> particular if a large problem is dealt with.
      subroutine jacobiPrepCFD_invdelta(vX,eps,mX,vInvDelta,info)
      implicit none
      !> Point at which the jacobian should evaluated. Size of vX = n
      double precision,dimension(1:),intent(in)        :: vX
      !> Precision of finite difference scheme
      double precision,intent(in)                     :: eps
      !> Output matrix consisting of [n x 2n] finite difference points. 
      !> 
      !> The points are organized as follows. For every odd i:
      !>   * mX(:,i) contains vX + Delta X 
      !>   * mX(:,i+1) contains vX - Delta X 
      !> To put it simple: "forward" points are at odd i, "backward" ones are at even i.
      double precision,dimension(:,:),allocatable,intent(out)      :: mX
      !> Inverses of the Delta X
      double precision,dimension(:),allocatable,intent(out)        :: vInvDelta 
      !> Exit code: 0 on succcess.
      integer,intent(out)                             :: info
      !
      integer :: n,m,i,j,k
      double precision :: delta
            n = size(vX)
            m = 2 * n
            ! Input check
            info = 1
            if ((abs(eps) < epsilon(0.D0)) .or. (n < 1)) return
            allocate(mX(1:n,1:m),vInvDelta(1:n))
            !
            !$omp parallel do private(k,j,delta) if(n > 32)
            do i=1,n
                  k = 2*i
                  j = k - 1
                  !
                  delta = eps * vX(i)  
                  if (abs(delta) < epsilon(0.D0)) delta = eps ! eps : non-zero if vX(j) is zero
                  vInvDelta(i) = 0.5D0 / delta ! Inverse of the delta
                  !
                  mX(:,j) = vX(:)
                  mX(i,j) = mX(i,j) + delta
                  mX(:,k) = vX(:)
                  mX(i,k) = mX(i,k) - delta
            enddo
            !$omp end parallel do
            info = 0
      end subroutine
      
      
      !> The subroutine calculates Jacobi matrix by means of Central Finite Difference stencil.
      !>
      !> The function can accept either the function values (F) evaluated at the FD
      !> points OR differences between the function values (DeltaF) at the relevant FD points.
      !> In some cases the evaluation of DeltaF can be done in a much more efficient
      !> way than the calculations of the function F itself.
      subroutine jacobiCalcCFD_invdelta(vInvDelta,isDeltaF,mF,mJ,info)
      implicit none
      !> Inverse of the Delta x
      double precision,dimension(1:),intent(in)       :: vInvDelta 
      !> Description of the mF contents
      !>
      !> The contents of mF is interpreted according to the following rules:
      !>   * If isDeltaF is False, then mF contains values of the function evaluated 
      !>     at the corresponding finite difference points: R^n -> R^m.
      !>     Dimensionality of the array is then [m x 2n].
      !>   * If isDeltaF is True, then mF contains values of the difference between 
      !>     the function evaluated at the corresponding finite difference points: R^n -> R^m.
      !>     Dimensionality of the array must be [m x n].
      !> Note that the values of the function are represented by m-dimensional vectors inside the matrix.
      logical,intent(in)                              :: isDeltaF
      !> Array of either function values or differences between the function values.
      double precision,dimension(1:,1:),intent(in)      :: mF  
      !> Output Jacobi matrix.
      !> Dimensionality is [m x n]
      double precision,dimension(1:,1:),intent(out)     :: mJ  
      !> Exit code: 0 on succcess.
      integer,intent(out)                             :: info
      !
      integer :: i,j,k, m, n
      !
            n = size(mJ,dim=2)
            m = size(mJ,dim=1)
            ! Check dimensions
            info = 1
            if (.not. isDeltaF) then
                  ! We expect two points in mF per dimension n
                  if ( (size(mF,dim=1) /= m) .or. (size(mF,dim=2) /= 2 *n) ) return
                  !
                  !$omp parallel do private(k,j) if(n > 32)
                  do i = 1, n
                        k = 2*i
                        j = (k - 1)
                        mJ(:,i) = vInvDelta(i) * (mF(:,j) - mF(:,k))
                  enddo
                  !$omp end parallel do
                  info = 0
            else
                  ! We expect one point in mF per dimension n
                  if ( (size(mF,dim=1) /= m) .or. (size(mF,dim=2) /= n) ) return
                  !
                  !$omp parallel do private(k,j) if(n > 32)
                  do i = 1, n
                        mJ(:,i) = vInvDelta(i) * mF(:,i)
                  enddo
                  !$omp end parallel do
                  info = 0
            endif
      !
      end subroutine
      
end module
