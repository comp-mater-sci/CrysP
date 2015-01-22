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
!>    \date Date of first release: 2010-08-24
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTestMathUtils.f90 
!
!
#include "criStdDefs.fpp"
!
subroutine ocrossTest()
use criMathUtils
implicit none

integer  :: i
double precision,dimension(5,5) :: M = 0.D0
double precision,dimension(5)   :: V,U

double precision,dimension(5,1) :: MU =  (/ ( dble(i), i=1,5 ) /)
double precision,dimension(1,5) :: MV =  (/ ( dble(i), i=1,5 ) /)
double precision,dimension(5,5) :: MR = 0.D0

do i=1,5
      V(i) = i*10
      MV(1,i) = V(i)
enddo

write(*,*) 'cri Test application'

write(*,45) 'u', U
write(*,45) 'v', V


M = ocross_product(U,V)
write(*,*) 'M='
write(*,44) (M(i,:) , i=1,5)


write(*,45) 'mu', MU
write(*,45) 'mv', MV

MR = matmul(MU,MV)
write(*,*) 'MR='
write(*,44) (MR(i,:) , i=1,5)

44 format(5(F12.6,1X))
45 format(A,1X,':',1X,5(F12.6,1X))
end subroutine



subroutine chkangle()
use criMathUtils
implicit none
integer :: i, n = 2 * 360
double precision :: alpha, dalpha, alphamax, angle
double precision,dimension(2) :: u,v

alphamax = 360.D0 
n = alphamax
dalpha = alphamax / dble(180)
alpha = -alphamax

u(1) = 1.0
u(2) = 0.0
v = 1.D0

do i=1,n
      ! apply rotation by angle alpha
      v = rotate2D(deg2rad(alpha),u)      
      angle = vec_angle(u,v)
      write(*,'(F10.4,1X,4(F14.8,1X))') alpha, deg2rad(alpha), vec_cosine(u,v), angle, rad2deg(angle)
      alpha = alpha + dalpha     
end do

contains

      function rotate2D(phi,vec)
      implicit none
      double precision,dimension(2) :: rotate2D
      double precision              :: phi
      double precision,dimension(2) :: vec
      !
      double precision,dimension(2,2) :: R
      double precision, dimension(2,1) :: V
      V(:,1) = vec
      
      rotate2D = 0.D0
      R(1,1) = cos(phi)
      R(1,2) = -sin(phi)
      R(2,1) = - R(1,2)
      R(2,2) = R(1,1)
      rotate2D = reshape(matmul(R,V),(/2/))
      end function

end subroutine
