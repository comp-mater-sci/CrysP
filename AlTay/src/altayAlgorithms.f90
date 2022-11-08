#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif

module altayAlgorithms
    use altayMiscutils, only: terminate, stopcode_runtimeerror
    use criMathUtils
    use altayRCM
    use trace
    
    implicit none

    double precision, parameter :: SQRT_P5 = sqrt(0.5d0)
    double precision, parameter :: RESOLUTION = 0.5e-5

    private  
    public  :: deg2rad,             &
               rotmat,              &
               rotateSRTensorFrom,  &
               KleinKwa,            &
               UPDATC,              &
               GETANG,              &
               Vector5D,            &
               SymMatrix,           &
               Transf

contains
    
    !> Updating of CIJ matrix of ellipsoid
    !> Finv is the inverse of the F-tensor which describes the strain increment.
    subroutine UPDATC(CIJ, Finv)
        double precision, dimension(3,3), intent(in)   :: Finv
        double precision, dimension(3,3), intent(inout)  :: CIJ
        CIJ = matmul(matmul(transpose(Finv), CIJ), Finv)
    end subroutine
    
    !> Transform a 5D-vector in deviatoric (stress/strain-rate) space to a (3,3)-matrix representation of a symmetric and traceless 2nd rank tensor.
    !> Note: The reverse transformation is done by function 'Vector5D'.
    function SymMatrix(vec) result(sym)
        double precision, dimension(5), intent(in) :: vec
        double precision, dimension(3,3)           :: sym 
        double precision, parameter                :: C1 = (sqrt(3.0d0) + 3.0d0) / 6.0d0, & 
                                                      C2 = (3.0d0 - sqrt(3.0d0)) / 6.0d0  
        
        sym(2,2) =  C1 * vec(1) - C2 * vec(2)
        sym(3,3) = -C2 * vec(1) + C1 * vec(2)
        
        sym(1,1) = -sym(2,2) - sym(3,3)
        
        sym(2,3) = SQRT_P5 * vec(3)
        sym(3,1) = SQRT_P5 * vec(4)
        sym(1,2) = SQRT_P5 * vec(5)
        
        sym(3,2) = sym(2,3)
        sym(1,3) = sym(3,1)
        sym(2,1) = sym(1,2)
    end function SymMatrix
    
    !> Transform a (3,3)-matrix representation of a traceless 2nd rank tensor to 5D-vector representation in deviatoric (stress/strain-rate) space.
    !> Notes:
    !>    - Only the symmetric part of 2nd rank tensor is transformed.
    !>    - The reverse transformation is done by function 'SymMatrix'.
    function Vector5D(mat) result(vec)
        double precision, dimension(3,3), intent(in) :: mat
        double precision, dimension(5)               :: vec 
        double precision, parameter                  :: C1 = 0.5d0 * (sqrt(3.0d0) + 1.0d0), &
                                                        C2 = C1 - 1.0d0
        
        vec(1) = C1 * mat(2,2) + C2 * mat(3,3)
        vec(2) = C2 * mat(2,2) + C1 * mat(3,3)
        vec(3) = SQRT_P5 * (mat(2,3) + mat(3,2))
        vec(4) = SQRT_P5 * (mat(3,1) + mat(1,3))
        vec(5) = SQRT_P5 * (mat(1,2) + mat(2,1))
    end function Vector5D
   
    !Calculate the CIJ matrix of an ellipsoid with half axes stored in Gaxes. T defines the orientation of the axes.
    !This version assumes that A is a diagonal matrix
    Subroutine Transf(Gaxes, Aprime, T)
        double precision, dimension(3), intent(in)      :: Gaxes 
        double precision, dimension(3,3), intent(inout) :: Aprime 
        double precision, dimension(3,3), intent(in)    :: T 
        integer                                         :: i, j, k
        double precision                                :: y
        double precision, dimension(3)                  :: A
        double precision, dimension(3,3)                :: X

        A = 1.D0 / Gaxes ** 2

        do j = 1,3
            X(:,j) = t(:,j) * A
        end do

        do i = 1, 3
            do j = 1, 3
                y = 0.0
                do k = 1,3
                    y = y + T(k,i) * X(k,j)
                end do
                Aprime(i, j) = y
            end do
        end do
    end subroutine
  
    !find half-lengths of ellipsoid axes from CIJ matrix
    !store them in prval
    !find Euler angles of these axes, store in GEULR
    Subroutine GETANG(CIJ, prval, GEULR, TMAT)
        
        double precision, dimension(3,3), intent(in)    :: CIJ
        double precision, dimension(3), intent(inout)   :: GEULR, prval
        double precision, dimension(3,3), intent(inout) :: TMAT
        integer                                         :: i
        double precision                                :: CIJTR, enrm 
        double precision, dimension(3,3)                :: prdir, e
        logical                                         :: axisym
        type(EulerAngles)                               :: CEuler

        CIJTR = (CIJ(1,1) + CIJ(2,2) + CIJ(3,3)) / 3.D0
        e = CIJ
        do i = 1, 3
            e(i,i) = e(i,i) - CIJTR
        end do

        call eigenv(e, prval, prdir, enrm, axisym)
        
        RCM_GUARD
      
        prval = prval + CIJTR

        if (prval(1) > prval(2)) call verwis(2, 1, prval, prdir)
        if (prval(2) > prval(3)) call verwis(3, 2, prval, prdir)
        if (prval(1) > prval(2)) call verwis(1, 2, prval, prdir)
        prval = 1.D0 / sqrt(prval)
        TMAT = prdir
        CEuler = EuleranglesType(TMAT)
        GEULR = EulerAngles2Arr(CEuler)
    end subroutine


    !>Utility function to simplify incrementing some value which may roll over
    !>@param val The value to be incremented.
    !>@param inc The amount to increment val by.
    !>@param modulus the value at which to roll over to 0.
    !>@return The incremented value.
    function increment(val, inc, modulus) result(res)
        integer, intent(in) :: val, inc, modulus
        integer             :: res

        res = mod(val + inc, modulus)
    end function
    
    !>Principal values of symmetric tensor with zero trace
    !>The eigenvectors are normalized.
    !>prval contains the principal values
    Subroutine eigenv(e, prval, prdir, enrm, axisym)
        double precision, dimension(3,3), intent(in)    :: e
        logical, intent(inout)                          :: axisym
        double precision, dimension(3,3), intent(inout) :: prdir
        double precision, intent(out)                   :: enrm
        double precision, dimension(3), intent(out)     :: prval
        integer                                         :: i, i1, i2, j, imax, jmax, kmax, ipr, j1, j2, min_ipr
        double precision                                :: xx, a, b, theta, pi, pmax, eta, pp
        double precision, dimension(3)                  :: x
        double precision, dimension(3,3)                :: y

        a=0.0
        do i = 1,3
            do j = 1,3
                if (abs(e(i,j) - e(j,i)) > RESOLUTION) then
                    RCM_RAISE(1,'eigenv','The input tensor is not symmetric', RCM_RTN)
                end if
                a = a + e(i,j)**2
            end do
        end do

        enrm = sqrt(a)
        if (enrm < RESOLUTION) then
            do i = 1,3
                prval(i) = 0.0
                do j = 1,3
                    prdir(i,j) = 0.0
                end do
                prdir(i,i) = 1.D0
            end do
        else 
            a = a / 2
            b = e(1,1) * e(2,3)**2 + e(2,2) * e(3,1)**2 + e(3,3) * e(1,2)**2 - 2 * e(1,2) * e(2,3) * e(3,1) - e(1,1) * e(2,2) * e(3,3)
            call canoni(a, b, x, theta, pi)
      
            RCM_GUARD
            
            pmax = abs(x(1))
            imax = 1
            do i = 2,3
                if (abs(x(i)) > pmax) then
                    pmax = abs(x(i))
                    imax = i
                end if
            end do

            prval(3) = x(imax)
            jmax = mod(imax + 1, 3)
            prval(1) = x(jmax)
            kmax = 6 - imax - jmax
            prval(2) = x(kmax)

            eta = (theta + (imax - 1) * 2 * pi) / 3.D0
            if (eta > pi) eta = eta - 2 * pi

            !Correction on the order of the eigenvalues
            if (abs(prval(2)) >= abs(prval(1))) then
                xx = prval(2)
                prval(2) = prval(1)
                prval(1) = xx
            end if
            
            axisym = abs(prval(2) - prval(1)) < RESOLUTION
            if (axisym) then
                prval(1) = 0.5 * (prval(1) + prval(2))
                prval(2) = prval(1)
                min_ipr = 3
            else
                min_ipr = 2
            end if

            y = e
            do ipr = 3,min_ipr,-1
                do i = 1,3
                    y(i,i) = y(i,i) - prval(ipr)
                end do
                pmax = 0.0
                pp = 0.0
                imax = 0
                jmax = 0
                do i = 1,3
                    i1 = increment(i, 1, 3)
                    i2 = increment(i, 2, 3)
                    do j = 1,3
                        j1 = increment(j, 1, 3)
                        j2 = increment(j, 2, 3)
                        xx = y(i1,j1) * y(i2,j2) - y(i1,j2) * y(i2,j1)
                        if (pmax <= abs(xx)) then 
                            pmax = abs(xx)
                            pp = xx
                            imax = i
                            jmax = j
                        end if 
                    end do
                end do

                ! the minor with the max. value has been identified
                i1 = increment(imax, 1, 3)
                i2 = increment(imax, 2, 3)
                j1 = increment(jmax, 1, 3)
                j2 = increment(jmax, 2, 3)
                prdir(jmax, ipr) = 1.D0
                pmax = -(y(i1,jmax) * y(i2,j2) - y(i2,jmax) * y(i1,j2))
                prdir(j1, ipr) = pmax / pp
                pmax = -(y(i1,j1) * y(i2,jmax) - y(i2,j1) * y(i1,jmax))
                prdir(j2, ipr) = pmax / pp
                call normaliz(prdir(1,ipr), xx)
            end do
            
            if (axisym) then
                !Vectorial product between prdir(,3) and x3 axis
                prdir(1,2) = -prdir(2,3)
                prdir(2,2) = prdir(1,3)
                prdir(3,2) = 0.0
                call normaliz(prdir(1,2), xx)
                if (xx <= 0.7) then
                    !Vectorial product between prdir(,3) and x2 axis
                    prdir(1,2) = prdir(3,3)
                    prdir(2,2) = 0.0
                    prdir(3,2) = -prdir(1,3)
                    call normaliz(prdir(1,2), xx)
                end if 
            end if

            prdir(1,1) = prdir(2,2) * prdir(3,3) - prdir(3,2) * prdir(2,3)
            prdir(2,1) = prdir(3,2) * prdir(1,3) - prdir(1,2) * prdir(3,3)
            prdir(3,1) = prdir(1,2) * prdir(2,3) - prdir(2,2) * prdir(1,3)
            call normaliz(prdir(1,1), xx)
        end if
          do i=1,3
                write (*,*) prval(i)
            end do
    end subroutine
 
    !>Principal values of symmetric tensor with zero trace
    !>The eigenvectors are normalized.
    !>prval contains the principal values
    Subroutine eigenv2(e, prval, prdir, enrm, axisym)
        IMPLICIT double precision (A-H,O-Z)
        integer :: i, i1, i2, j, imax, jmax, kmax, ipr, j1, j2
      dimension e(3,3),x(3),prval(3), y(3,3),prdir(3,3)
      logical axisym,eerste
      SAVE
      a=0.0
      do i=1,3
        a=a+e(i,i)
      enddo
    4 a=0.0
      do 5 i=1,3
      do 6 j=1,3
      xx=abs(e(i,j)-e(j,i))
      if (xx.lt.0.5e-5) goto 7
      RCM_RAISE(1,'eigenv','The input tensor is not symmetric',RCM_RTN)

    7 a=a+e(i,j)**2
    6 continue
    5 continue
      enrm=sqrt(a)
      if (enrm.lt.0.5e-5) goto 33
      a=a*0.5D0
      b=e(1,1)*e(2,3)**2+e(2,2)*e(3,1)**2+e(3,3)*e(1,2)**2-              &
       2.D0*e(1,2)*e(2,3)*e(3,1)-e(1,1)*e(2,2)*e(3,3)
      call canoni(a,b,x,theta,pi)
      
    RCM_GUARD
      pmax=abs(x(1))
      imax=1
      do 10 i=2,3
      if (abs(x(i)).lt.pmax) goto 10
      pmax=abs(x(i))
      imax=i
   10 continue
      prval(3)=x(imax)
      jmax=imax+1
      if (jmax.gt.3) jmax=1
      prval(1)=x(jmax)
      kmax=6-imax-jmax
      prval(2)=x(kmax)
      eta=(theta+(imax-1)*2.D0*pi)/3.D0
      if (eta.gt.pi) eta=eta-2.D0*pi
!
!     Correction on the order of the eigenvalues
!
      if (abs(prval(2)).lt.abs(prval(1))) goto 11
      xx=prval(2)
      prval(2)=prval(1)
      prval(1)=xx
   11 axisym=abs(prval(2)-prval(1)).lt.0.5e-5
      if (.not.axisym) goto 9
      prval(1)=0.5D0*(prval(1)+prval(2))
      prval(2)=prval(1)
    9 do ipr=3,2,-1
        do i=1,3
          do j=1,3
            y(i,j)=e(i,j)
          end do
          y(i,i)=y(i,i)-prval(ipr)
        end do
        pmax=0.0
        pp=0.0
        imax=0
        jmax=0
        do i=1,3
        i1=i+1
        if (i1.gt.3) i1=1
        i2=i1+1
        if (i2.gt.3) i2=1
        do 16 j=1,3
          j1=j+1
          if (j1.gt.3) j1=1
          j2=j1+1
          if (j2.gt.3) j2=1
          xx=y(i1,j1)*y(i2,j2)-y(i1,j2)*y(i2,j1)
          if (pmax.gt.abs(xx)) goto 16
          pmax=abs(xx)
          pp=xx
          imax=i
          jmax=j
   16   continue
        end do
!       the minor with the max. value has been identified
        i1=imax+1
        if (i1.gt.3) i1=1
        i2=i1+1
        if (i2.gt.3) i2=1
        j1=jmax+1
        if (j1.gt.3) j1=1
        j2=j1+1
        if (j2.gt.3) j2=1
        prdir(jmax,ipr)=1.D0
        pmax=-(y(i1,jmax)*y(i2,j2)-y(i2,jmax)*y(i1,j2))
        prdir(j1,ipr)=pmax/pp
        pmax=-(y(i1,j1)*y(i2,jmax)-y(i2,j1)*y(i1,jmax))
        prdir(j2,ipr)=pmax/pp
        call normaliz(prdir(1,ipr),xx)
        if (axisym) goto 17
      end do
      goto 19
!
!     Vectorial product between prdir(,3) and x3 axis
!
   17 prdir(1,2)=-prdir(2,3)
      prdir(2,2)=prdir(1,3)
      prdir(3,2)=0.0
      call normaliz(prdir(1,2),xx)
      if (xx.gt.0.7) goto 19
!
!     Vectorial product between prdir(,3) and x2 axis
!
      prdir(1,2)=prdir(3,3)
      prdir(2,2)=0.0
      prdir(3,2)=-prdir(1,3)
      call normaliz(prdir(1,2),xx)
   19 prdir(1,1)=prdir(2,2)*prdir(3,3)-prdir(3,2)*prdir(2,3)
      prdir(2,1)=prdir(3,2)*prdir(1,3)-prdir(1,2)*prdir(3,3)
      prdir(3,1)=prdir(1,2)*prdir(2,3)-prdir(2,2)*prdir(1,3)
      call normaliz(prdir(1,1),xx)
   21 return
   33 do i=1,3
        prval(i)=0.0
        do j=1,3
          prdir(i,j)=0.0
        end do
        prdir(i,i)=1.D0
      end do
      goto 21
      end subroutine
      !
      subroutine normaliz(prdir,xx)
      IMPLICIT double precision (A-H,O-Z)
      dimension prdir(3)
      integer :: i
      xx=0.0
      do 19 i=1,3
      xx=prdir(i)**2+xx
   19 continue
      xx=sqrt(xx)
      if (xx.gt.0.5d-5) goto 20
      xx=0.0
      return
   20 do 22 i=1,3
      prdir(i)=prdir(i)/xx
   22 continue
      return
      end subroutine
      !
      subroutine canoni(a,b,X,theta,pi)
      IMPLICIT double precision (A-H,O-Z)
!
!     should find the roots of an equation
!
!     x**3 - A x + B = 0
!
!     The roots are suppposed to be real.
!
      dimension x(3)
      if (a.lt.0.5e-11) goto 2
      roota=sqrt(a**3/27.0d0)
      delta=0.5D0*b/roota
      if (abs(delta).lt.(1.0d0+1.0d-6)) goto 1
    2 continue
      RCM_RAISE(1,'CANONI','Two roots seem to be complex',RCM_RTN)
    1 if (delta.gt.1.0) delta=1.D0
      if (delta.lt.-1.0) delta=-1.D0
      theta=acos(delta)
      delta=-2.D0*sqrt(a/3.D0)
      x(1)=delta*cos(theta/3.D0)
      x(2)=delta*cos((theta+2.D0*pi)/3.D0)
      x(3)=delta*cos((theta+4.D0*pi)/3.D0)
      end subroutine
      !
        subroutine verwis(i1,i2,prval,prdir)
                IMPLICIT double precision (A-H,O-Z)
                dimension prval(3),prdir(3,3)
                integer, intent(in) :: i1, i2
                integer :: i
                x=prval(i1)
                prval(i1)=prval(i2)
                prval(i2)=x
                do i=1,3
                        x=prdir(i,i1)
                        prdir(i,i1)=-prdir(i,i2)
                        prdir(i,i2)=x
                end do

        end subroutine
      !
        SUBROUTINE MINV(A,N,D,L,M,NXXX)
                integer, intent(in)                              :: N, NXXX
                integer, dimension(N), intent(inout)             :: M, L
                double precision, dimension(NXXX), intent(inout) :: A
                integer                                          :: K, J, I, JI, NK, KK, IZ, IJ, KI, JP, IK, JR, JK, KJ, JQ
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
      DOUBLE PRECISION D,BIGA,HOLD
!
!        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT
!        10 MUST BE CHANGED TO DABS.
!
!        ...............................................................
!
!        SEARCH FOR LARGEST ELEMENT
!
      D=1.D0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      M(K)=K
      L(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
   10 IF( DABS(BIGA)- DABS(A(IJ))) 15,20,20
   15 BIGA=A(IJ)
      L(K)=I
      M(K)=J
   20 CONTINUE
!
!        INTERCHANGE ROWS
!
      J=L(K)
      IF(J-K) 35,35,25
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
   30 A(JI) =HOLD
!
!        INTERCHANGE COLUMNS
!
   35 I=M(K)
      IF(I-K) 45,45,38
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI) =HOLD
!
!        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
!        CONTAINED IN BIGA)
!
   45 IF(BIGA) 48,46,48
   46 D=0.0
      RETURN
   48 DO 55 I=1,N
      IF(I-K) 50,55,50
   50 IK=NK+I
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
!
!        REDUCE MATRIX
!
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
   60 IF(J-K) 62,65,62
   62 KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
!
!        DIVIDE ROW BY PIVOT
!
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
!
!        PRODUCT OF PIVOTS
!
      D=D*BIGA
!
!        REPLACE PIVOT BY RECIPROCAL
!
      A(KK)=1.D0/BIGA
   80 CONTINUE
!
!        FINAL ROW AND COLUMN INTERCHANGE
!
      K=N
  100 K=(K-1)
      IF(K) 150,150,105
  105 I=L(K)
      IF(I-K) 120,120,108
  108 JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
  110 A(JI) =HOLD
  120 J=M(K)
      IF(J-K) 100,100,125
  125 KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
  130 A(JI) =HOLD
      GO TO 100
  150 RETURN
      END SUBROUTINE

      !

           Subroutine Kleinkwa(N1,N2,M1,M2,A,B,AA,BA,VAL,XV,YV,TOL,RES)
!     N1=number of equations
!     N2=number of unknowns
!     A=coefficient matrix
!     B=right hand sides
!     BA=solution on output
!     AA,VAL,XV,YV=work space
!     RES=residu (sum of squares)
!     M1,M2=dimensions
!
!     We make it a set with a symmetrical matrix, because
!     we want to use STELSEL to solve it.
!
      IMPLICIT double precision (A-H,O-Z)
      integer, intent(in) :: M1, M2
      dimension  A(M1,M2),AA(M2,M2),B(M2),BA(M2)
      dimension VAL(M2),XV(M2),YV(M2)
      integer :: kk, i, j, N2, N1, N
      do kk=1,N2
        x=0.0
        do i=1,N1
          x=x+A(i,kk)*B(i)
        end do
        BA(kk)=x
        do j=1,N2
          y=0.0
          do i=1,N1
            y=y+A(i,kk)*A(i,j)
          end do
          AA(kk,j)=y
        end do
      end do
      call STELSEL(N2,M2,AA,BA,TOL,VAL,XV,YV)
      RES=0.0
      do i=1,N1
        y=0.0
        do j=1,N2
          y=y+A(i,j)*BA(j)
        end do
        RES=RES+(y-B(i))**2
      end do

      end subroutine


      Subroutine STELSEL(N,M,A,R,TOL,VAL,XV,YV)
      IMPLICIT double precision (A-H,O-Z)
      integer, intent(in) :: N, M
      dimension A(M,M),R(M),VAL(M),XV(M),YV(M)
      integer :: j, i 
!
!     to solve the system of equations A * X = R using
!     eigenvalues and eigenvectors
!
!     This method is known as "Singular Value Decomposition Method".
!
!     A s a symmetrical matrix (NxN). Modified during the process.
!
!     The output X is actually stored in R, which is hence modified.
!
!     On return, A (NxN) contains the eigenvectors
!     VAL (N) contains eigenvalues
!     XV, YV : workspace
!
!      write (IMP,203) TOL
! 203  format (' TOL',d20.10)
!      do 50 i=1,N
!      write (IMP,201) r(i),(A(i,j),j=1,N)
! 201  format (d12.4,5x,5d12.4)
!  50  continue
      call tred2(a,N,M,VAL,XV)
      call tqli(VAL,XV,N,M,a)
      do j=1,N
        y=0.0d00
        do i=1,N
          y=y+A(i,j)*R(i)
        end do
        z=VAL(j)
!       write (IMP,202) j,z
! 202   format (' VAL(j)',i5,d20.10)
        if (abs(z).lt.tol) then
             z=0.0d0
        else
             z=y/z
        endif
  !      write (IMP,202) j,z
        YV(j)=z
      end do
      do i=1,N
        y=0.0d00
        do j=1,N
          y=y+A(i,j)*YV(j)
        end do
        R(i)=y
      end do
      end subroutine

      SUBROUTINE tred2(a,n,np,d,e)

      implicit double precision (a-h,o-z)
      INTEGER n,np
      double precision a(np,np),d(np),e(np)
      INTEGER i,j,k,l
      double precision f,g,h,hh,scale
      do i=n,2,-1
        l=i-1
        h=0.
        scale=0.
        if(l.gt.1)then
          do k=1,l
            scale=scale+abs(a(i,k))
          end do
          if(scale.eq.0.)then
            e(i)=a(i,l)
          else
            do k=1,l
              a(i,k)=a(i,k)/scale
              h=h+a(i,k)**2
            end do
            f=a(i,l)
            g=-sign(sqrt(h),f)
            e(i)=scale*g
            h=h-f*g
            a(i,l)=f-g
            f=0.
            do j=1,l
!     Omit following line if finding only eigenvalues
              a(j,i)=a(i,j)/h
              g=0.
              do k=1,j
                g=g+a(j,k)*a(i,k)
              end do
              do k=j+1,l
                g=g+a(k,j)*a(i,k)
              end do
              e(j)=g/h
              f=f+e(j)*a(i,j)
            end do
            hh=f/(h+h)
            do j=1,l
              f=a(i,j)
              g=e(j)-hh*f
              e(j)=g
              do k=1,j
                a(j,k)=a(j,k)-f*e(k)-g*a(i,k)
              end do
            end do
          endif
        else
          e(i)=a(i,l)
        endif
        d(i)=h
      end do
!     Omit following line if finding only eigenvalues.
      d(1)=0.
      e(1)=0.
      do i=1,n
!     Delete lines from here ...
        l=i-1
        if(d(i).ne.0.)then
          do j=1,l
            g=0.
            do k=1,l
              g=g+a(i,k)*a(k,j)
            end do
            do k=1,l
              a(k,j)=a(k,j)-g*a(k,i)
            end do
          end do
        endif
!     ... to here when finding only eigenvalues.
        d(i)=a(i,i)
!     Also delete lines from here ...
        a(i,i)=1.
        do j=1,l
          a(i,j)=0.
          a(j,i)=0.
        end do
!     ... to here when finding only eigenvalues.
      end do

      END SUBROUTINE
      !
!  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      SUBROUTINE tqli(d,e,n,np,z)
      implicit double precision (a-h,o-z)
      INTEGER n,np
      double precision d(np),e(np),z(np,np)
      INTEGER i,iter,k,l,m
      double precision b,c,dd,f,g,p,r,s
      do i=2,n
        e(i-1)=e(i)
      end do
      e(n)=0.
      do l=1,n
        iter=0
1       do m=l,n-1
          dd=abs(d(m))+abs(d(m+1))
          if (abs(e(m))+dd.eq.dd) goto 2
        end do
        m=n
2       if(m.ne.l)then
          if(iter.eq.100) write(*,*) 'too many iterations in tqli'
          iter=iter+1
          g=(d(l+1)-d(l))/(2.D0*e(l))
          r=pythag(g,1.0d00)
          g=d(m)-d(l)+e(l)/(g+sign(r,g))
          s=1.
          c=1.
          p=0.
          do i=m-1,l,-1
            f=s*e(i)
            b=c*e(i)
            r=pythag(f,g)
            e(i+1)=r
            if(r.eq.0.)then
              d(i+1)=d(i+1)-p
              e(m)=0.
              goto 1
            endif
            s=f/r
            c=g/r
            g=d(i+1)-p
            r=(d(i)-g)*s+2.D0*c*b
            p=s*r
            d(i+1)=g+p
            g=c*r-b
!     Omit lines from here ...
            do k=1,n
              f=z(k,i+1)
              z(k,i+1)=s*z(k,i)+c*f
              z(k,i)=c*z(k,i)-s*f
            end do
!     ... to here when finding only eigenvalues.
          end do
          d(l)=d(l)-p
          e(l)=g
          e(m)=0.
          goto 1
        endif
      end do
      END SUBROUTINE


!  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      double precision FUNCTION pythag(a,b)
      implicit double precision (a-h,o-z)
      double precision,intent(in) :: a,b
      double precision absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1.D0+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag=0.
        else
          pythag=absb*sqrt(1.D0+(absa/absb)**2)
        endif
      endif
      END FUNCTION

end module

