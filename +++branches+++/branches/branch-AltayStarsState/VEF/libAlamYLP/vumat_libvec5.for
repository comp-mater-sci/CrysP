      module Kutils

      contains

!
! Subroutines ported from SKY facet software.

************************************************************************
      SUBROUTINE KNORML5D(Dp,Ap,normDp)
c
c     Objective: To normalise the 5D vector with its norm 
c                i.e., to produce so called strain rate mode or stress mode
c
      IMPLICIT NONE
c
      INTEGER i                        ! I/O handler
      REAL*8 Dp(5),Ap(5)               ! Vector in 5D form &its normalised form
      REAL*8 sumsq, normDp             ! Norm of Dp
c
      sumsq = 0.D0
      DO i = 1, 5
        sumsq = sumsq + Dp(i)*Dp(i)
      END DO
c     
      normDp = dsqrt(sumsq)
      IF (normDp /= 0.0) THEN 
        DO i = 1, 5
          Ap(i) = Dp(i) / normDp
        END DO
      ELSE
        PRINT '(A)', 'DATA ERROR: Normal of vector is found to be zero'
        STOP
      END IF
c ----------------------------------------------------------------------
c     end of subroutine KNORML5D
c ----------------------------------------------------------------------
      RETURN
      END SUBROUTINE
************************************************************************
C***********************************************************************
      SUBROUTINE KVEC5D2MAT(ap, aij)
C      
C     Objective: To transform a tensor from 5D vector notation to matrix form
C
      IMPLICIT NONE
C
      REAL*8 aij(3,3)                  ! Matrix representation of plastic 
C                                        strain mode or deviatoric stress tensor 
      REAL*8 ap(5)                     ! Vector notation of plastic strain mode
C                                        or deviatoric stress tensor
      REAL*8 rt2i, rt23, rt6i          ! 1/root(2), root(2/3) and 1/root(6) 
C
      rt2i = 1.D0/dsqrt(2.D0)
      rt23 = dsqrt(2.D0/3.D0)
      rt6i = 1.D0/dsqrt(6.D0)    
C  
C     Conversion from 5D vector form to matrix form
C
      aij(1,1)= rt2i*ap(1) + rt6i*ap(2)
      aij(2,2)=-rt2i*ap(1) + rt6i*ap(2)
      aij(3,3)=-rt23*ap(2)
      aij(2,3)= rt2i*ap(3)
      aij(3,1)= rt2i*ap(4)
      aij(1,2)= rt2i*ap(5)
      aij(3,2)= aij(2,3)
      aij(1,3)= aij(3,1)
      aij(2,1)= aij(1,2)
c ----------------------------------------------------------------------
c     end of subroutine KVEC5D2MAT
c ----------------------------------------------------------------------
      RETURN
      END SUBROUTINE
C***********************************************************************
C***********************************************************************
      SUBROUTINE KMAT2VEC5D(aij, ap)
C      
C     Objective: To transform the matrix representaion of a given tensor
C     to  five dimensional vector notation
C
      IMPLICIT NONE
C
      REAL*8 aij(3,3)                  ! Matrix representation of plastic 
C                                        strain mode or deviatoric
C                                        stress tensor 
      REAL*8 ap(5)                     ! Vector notation of plastic strain mode
C                                        or deviatoric stress tensor
      REAL*8 hyd                       ! Hydrostatic component of the tensor
      REAL*8 rt2, rt2i, rt32           ! root(2), 1/root(2), and root(3/2) 
C
      rt2 = dsqrt(2.D0)
      rt2i = 1.0/dsqrt(2.D0)
      rt32 = dsqrt(3.D0/2.D0)
C
      hyd = (aij(1,1)+aij(2,2)+aij(3,3))/3.0
      aij(1,1)=aij(1,1)-hyd
      aij(2,2)=aij(2,2)-hyd
      aij(3,3)=aij(3,3)-hyd
C  
C     Conversion from matrix form to 5D vector form
C
      ap(1) = rt2i*(aij(1,1) - aij(2,2))
      ap(2) = -rt32*aij(3,3)
      ap(3) = rt2*aij(2,3)
      ap(4) = rt2*aij(3,1)
      ap(5) = rt2*aij(1,2)
C
      RETURN
      END SUBROUTINE KMAT2VEC5D
C***********************************************************************

      subroutine KROTMAT(bet1, bet2, bet3, aij)
C
C     Objective: To calculate the rotation matrix for given set of euler
C     angles
C
      implicit none
c
      REAL*8 bet1, bet2, bet3          ! 3 Euler angles (fi1, PHI, fi2) in rad.
      REAL*8 aij(3,3)                  ! Rotation matrix obtained using 
C                                        beta1, beta2 and beta3 rotating
C                                        from
C                                        [x1,x2,x3] to principal
C                                        coordinate 
C                                        system [xp1,xp2,xp3]  
C
C     Calculation of rotation matrix
C
      aij(1,1) = cos(bet1)*cos(bet3) - (sin(bet1)*sin(bet3)*cos(bet2))
      aij(1,2) = sin(bet1)*cos(bet3) + (cos(bet1)*sin(bet3)*cos(bet2))
      aij(1,3) = sin(bet3)*sin(bet2)
      aij(2,1) = -cos(bet1)*sin(bet3) - (sin(bet1)*cos(bet3)*cos(bet2))
      aij(2,2) = -sin(bet1)*sin(bet3) + (cos(bet1)*cos(bet3)*cos(bet2))
      aij(2,3) = cos(bet3)*sin(bet2)
      aij(3,1) = sin(bet1)*sin(bet2)
      aij(3,2) = -cos(bet1)*sin(bet2)
      aij(3,3) = cos(bet2)
C
      return
      end subroutine KROTMAT
C***********************************************************************
C
      SUBROUTINE KANG2MAT(bet1, bet2, bet3, bet4, Tij)
C
C     Objective: To transform the polar representation of a given tensor to  
C                matrix representation
C
      IMPLICIT NONE
C
C     Description of variables
C
      REAL*8 bet1, bet2, bet3, bet4    ! 4 angles that represent a tensor (rad.)
      REAL*8 Tij(3,3)                  ! Matrix form of the tensor in [x1,x2,x3]
      INTEGER i, j, k                  ! I/O handlers
      REAL*8 rot(3,3)                  ! Rotation matrix obtained using 
C                                        beta1, beta2 and beta3 rotating from
C                                        [x1,x2,x3] to principal coordinate 
C                                        system [xp1,xp2,xp3]  
      REAL*8 Np(3)                     ! Vector representing the tensor 
C                                        in principal coord. sys. [xp1,xp2,xp3]
      REAL*8 rt2, rt23, rt32, pi, s    ! root(2), root(2/3), root(3/2) & pi
C
      rt2 = dsqrt(2.D0)
      rt23 = dsqrt(2.D0/3.D0)
      rt32 = dsqrt(3.D0/2.D0)
      pi = dacos(-1.D0)
C
C     Calculation of rotation matrix
C
      CALL KROTMAT(bet1, bet2, bet3, rot)
C
C     Calculation of the mode in the principal coordinate system
C
      Np(1) = rt23*cos(bet4 - pi/3.d0)
      Np(2) = rt23*cos(bet4 + pi/3.d0)
      Np(3) = -rt23*cos(bet4)
C
C     Calculation of mode in [x1,x2,x3] coordinate system
C 
      DO i = 1, 3
        DO j = i, 3
          s = 0.d0
          DO k = 1, 3
            s = s + rot(k,i)*rot(k,j)*Np(k)
          END DO
          Tij(i,j) = s
          Tij(j,i) = s
        END DO
      END DO
C
      RETURN
      END SUBROUTINE KANG2MAT
C

      end module


