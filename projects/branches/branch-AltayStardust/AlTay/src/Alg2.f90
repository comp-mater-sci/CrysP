#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use criMathUtils
      contains
     

      !> The function converts a 5D-vector in deviatoric (stress/strain-rate) space
      !> to a (3,3)-matrix representation of a symmetric and traceless 2nd rank tensor.
      !>
      !> There is a reverse conversion available. \sa SymMat33ToVec5
      pure function Vec5ToSymMat33(vec) result(mat)
      implicit none
      double precision, dimension(5),  intent(in) :: vec
      double precision, dimension(3,3)            :: mat
      double precision, parameter ::           &
          sq22=   sqrt(0.5d0),                 & !0.7071068
          const3= (sqrt(3.0d0)+3.0d0)/6.0d0,   & !0.7886751
          const4= (3.0d0-sqrt(3.0d0))/6.0d0      !0.2113249          
      !
      mat(2,2)=  const3*vec(1)-const4*vec(2)
      mat(3,3)= -const4*vec(1)+const3*vec(2)
      !
      mat(1,1)= -mat(2,2)-mat(3,3)
      !
      mat(2,3)= sq22*vec(3)
      mat(3,1)= sq22*vec(4)
      mat(1,2)= sq22*vec(5)
      !
      mat(3,2)= mat(2,3)   
      mat(1,3)= mat(3,1)       
      mat(2,1)= mat(1,2)       
      !      
      end function Vec5ToSymMat33

    
      !> The function converts a (3,3)-matrix representation of a traceless 2nd rank tensor
      !> to 5D-vector representation in deviatoric (stress/strain-rate) space.
      !> Only the symmetric part of 2nd rank tensor is transformed.
      !>
      !> There is a reverse conversion available. \sa Vec5ToSymMat33
      pure function SymMat33ToVec5(mat) result(vec)
      implicit none
      double precision, dimension(3,3), intent(in) :: mat
      double precision, dimension(5)               :: vec
      double precision, parameter ::           &
          c1= 0.5d0*(sqrt(3.0d0)+1.0d0),       &
          c2= c1-1.0d0,                        &
          c3= sqrt(0.5d0)
      !
      vec(1)= c1*mat(2,2) + c2*mat(3,3)
      vec(2)= c2*mat(2,2) + c1*mat(3,3)
      !
      vec(3)= c3* (mat(2,3)+mat(3,2))
      vec(4)= c3* (mat(3,1)+mat(1,3))
      vec(5)= c3* (mat(1,2)+mat(2,1))
      !      
      end function SymMat33ToVec5
      
      
         
      !> The function converts the vector with dimension 3 into the 
      !>  anti-symmetric rank-two tensor. 
      !>
      !> There is a reverse conversion available. \sa AntiSymMat33ToVec3
      pure function Vec3ToAntiSymMat33(vec) result(mat)
      implicit none
      double precision,dimension(3),intent(in)  :: vec
      double precision,dimension(3,3)           :: mat
      !
      double precision, parameter :: minus = -1.0d0
      !
      mat = 0.0d0
      mat(3,2) = vec(1)
      mat(1,3) = vec(2)
      mat(2,1) = vec(3)
      mat(2,3) = minus * mat(3,2)
      mat(3,1) = minus * mat(1,3)
      mat(1,2) = minus * mat(2,1)
      !
      end function

      
      !> The function converts the anti-symmetric part of rank-two tensor into
      !> vector representation with dimension 3. 
      !>
      !> There is a reverse conversion available. \sa Vec3ToAntiSymMat33
      pure function AntiSymMat33ToVec3(mat) result(vec)
      implicit none
      double precision,dimension(3,3),intent(in)      :: mat
      double precision,dimension(3)                   :: vec
      !
      double precision, parameter :: half = 0.5d0
      !
      vec(1) = half * (mat(3,2)-mat(2,3))
      vec(2) = half * (mat(1,3)-mat(3,1))
      vec(3) = half * (mat(2,1)-mat(1,2))
      !note: same convention in PRETAY; e.g.: "B1(1,IS)=(R(3)*V(2)-R(2)*V(3))/2."
      !
      end function

      
      !
      subroutine UPDATC(CIJ,Finv) 
      double precision, dimension(3,3), intent(in)   :: Finv
      double precision, dimension(3,3), intent(inout):: CIJ
!     Updating of CIJ matrix of ellipsoid
!     Finv is the inverse of the F-tensor which describes the strain
!     increment.
!     CIJ = (Finv)^T * CIJ * Finv
      CIJ = matmul(matmul(transpose(Finv),CIJ),Finv)
      !
      end subroutine

 
      
      Subroutine Transf(Gaxes,Aprime,T)
      IMPLICIT double precision (A-H,O-Z)
!
!     to calculate the CIJ matrix of an ellipsoid with half axes
!     stored in Gaxes. T defines the orientation of the axes.
!
!     This version assumes that A is a diagonal matrix
!
      dimension Gaxes(3),A(3),Aprime(3,3),T(3,3),X(3,3)
      do 2 k=1,3
      A(k)=1.D0/Gaxes(k)**2
   2  continue
      do 1 k=1,3
      do 1 j=1,3
      X(k,j)=t(k,j)*A(k)
   1  continue
      do 3 i=1,3
      do 3 j=1,3
      y=0.0
      do 4 k=1,3
      y=y+T(k,i)*X(k,j)
   4  continue
      Aprime(i,j)=y
   3  continue
      return
      end subroutine
      !
      Subroutine GETANG(CIJ,prval,GEULR,TMAT)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      IMPLICIT double precision (A-H,O-Z)
!
!     find half-lengths of ellipsoid axes from CIJ matrix
!     store them in prval
!     find Euler angles of these axes, store in GEULR
!
      dimension CIJ(3,3),TMAT(3,3),GEULR(3)
      Dimension prval(3),prdir(3,3),e(3,3)
      logical axisym
      type(EulerAngles):: CEuler
      CIJTR=(CIJ(1,1)+CIJ(2,2)+CIJ(3,3))/3.D0
      do 30 i=1,3
      DO 31 j=1,3
      e(i,j)=CIJ(i,j)
   31 continue
      e(i,i)=e(i,i)-CIJTR
   30 continue
      call eigenv(e,prval,prdir,enrm,axisym)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif      
      do 36 i=1,3
      prval(i)=prval(i)+CIJTR
   36 continue
      if (prval(1).gt.prval(2)) call verwis(2,1,prval,prdir)
      if (prval(2).gt.prval(3)) call verwis(3,2,prval,prdir)
      if (prval(1).gt.prval(2)) call verwis(1,2,prval,prdir)
      do 37 i=1,3
      prval(i)=1.D0/sqrt(prval(i))
      do 37 j=1,3
      TMAT(i,j)=prdir(j,i)
   37 continue
      CEuler= EuleranglesType(TMAT)
      GEULR=EulerAngles2Arr(CEuler)
      return
      end subroutine
      !
      Subroutine eigenv(e,prval,prdir,enrm,axisym)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      IMPLICIT double precision (A-H,O-Z)
!     Principal values of symmetric tensor with zero trace
!     The eigenvectors are normalized.
!     prval contains the principal values
!
      dimension e(3,3),x(3),prval(3), y(3,3),prdir(3,3)
      logical axisym,eerste
      SAVE
      a=0.0
      do 3 i=1,3
      a=a+e(i,i)
    3 continue
    4 a=0.0
      do 5 i=1,3
      do 6 j=1,3
      xx=abs(e(i,j)-e(j,i))
      if (xx.lt.0.5e-5) goto 7
#ifndef ALTAY_SUBROUTINE      
      write (*,104)
  104 format (' Eigenv  - the input tensor is not symmetric')
      do 50 ii=1,3
      write (*,110) (e(ii,jj),jj=1,3)
  110 format (3f16.8)
   50 continue
      call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'eigenv','The input tensor is not symmetric',RCM_RTN)
#endif
      
    7 a=a+e(i,j)**2
    6 continue
    5 continue
      enrm=sqrt(a)
      if (enrm.lt.0.5e-5) goto 33
      a=a*0.5D0
      b=e(1,1)*e(2,3)**2+e(2,2)*e(3,1)**2+e(3,3)*e(1,2)**2-              &
       2.D0*e(1,2)*e(2,3)*e(3,1)-e(1,1)*e(2,2)*e(3,3)
      call canoni(a,b,x,theta,pi)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif      
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
    9 do 12 ipr=3,2,-1
      do 13 i=1,3
      do 14 j=1,3
      y(i,j)=e(i,j)
   14 continue
      y(i,i)=y(i,i)-prval(ipr)
   13 continue
      pmax=0.0
      pp=0.0
      imax=0
      jmax=0
      do 15 i=1,3
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
   16 continue
   15 continue
!     the minor with the max. value has been identified
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
   12 continue
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
   33 do 34 i=1,3
      prval(i)=0.0
      do 35 j=1,3
      prdir(i,j)=0.0
   35 continue
      prdir(i,i)=1.D0
   34 continue
      goto 21
      end subroutine
      !
      subroutine normaliz(prdir,xx)
      IMPLICIT double precision (A-H,O-Z)
      dimension prdir(3)
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
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
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
#ifndef ALTAY_SUBROUTINE
      write (*,100)
  100 format(' Subroutine CANONI - 2 Roots seem to be complex')
      call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'CANONI','Two roots seem to be complex',RCM_RTN)
#endif      
    1 if (delta.gt.1.0) delta=1.D0
      if (delta.lt.-1.0) delta=-1.D0
      theta=acos(delta)
      delta=-2.D0*sqrt(a/3.D0)
      x(1)=delta*cos(theta/3.D0)
      x(2)=delta*cos((theta+2.D0*pi)/3.D0)
      x(3)=delta*cos((theta+4.D0*pi)/3.D0)
      return
      end subroutine
      !
      subroutine verwis(i1,i2,prval,prdir)
      IMPLICIT double precision (A-H,O-Z)
      dimension prval(3),prdir(3,3)
      x=prval(i1)
      prval(i1)=prval(i2)
      prval(i2)=x
      do 1 i=1,3
      x=prdir(i,i1)
      prdir(i,i1)=-prdir(i,i2)
      prdir(i,i2)=x
   1  continue
      return
      end subroutine
      !
      SUBROUTINE MINV(A,N,D,L,M,NXXX)                                   
      DIMENSION A(NXXX),L(N),M(N)                                       
!                                                                       
!        ...............................................................
!                                                                       
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      
!        STATEMENT WHICH FOLLOWS.                                       
!                                                                       
      DOUBLE PRECISION A,D,BIGA,HOLD
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
      dimension  A(M1,M2),AA(M2,M2),B(M2),BA(M2)
      dimension VAL(M2),XV(M2),YV(M2)
      do 7 kk=1,N2
      x=0.0
      do 25 i=1,N1
      x=x+A(i,kk)*B(i)
  25  continue
      BA(kk)=x
      do 8 j=1,N2
      y=0.0
      do 9 i=1,N1
      y=y+A(i,kk)*A(i,j)
   9  continue
      AA(kk,j)=y
   8  continue
   7  continue
      call STELSEL(N2,M2,AA,BA,TOL,VAL,XV,YV)
      RES=0.0
      do 1 i=1,N1
      y=0.0
      do 2 j=1,N2
      y=y+A(i,j)*BA(j)
   2  continue
      RES=RES+(y-B(i))**2
   1  continue
      return
      end subroutine






      Subroutine STELSEL(N,M,A,R,TOL,VAL,XV,YV)
      IMPLICIT double precision (A-H,O-Z)
      dimension A(M,M),R(M),VAL(M),XV(M),YV(M)
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
      do 1 j=1,N
      y=0.0d00
      do 2 i=1,N
      y=y+A(i,j)*R(i)
   2  continue
      z=VAL(j)
!      write (IMP,202) j,z
! 202  format (' VAL(j)',i5,d20.10)
      if (abs(z).lt.tol) then
           z=0.0d0
      else
           z=y/z
      endif
!      write (IMP,202) j,z
      YV(j)=z
   1  continue
      do 3 i=1,N
      y=0.0d00
      do 4 j=1,N
      y=y+A(i,j)*YV(j)
   4  continue
      R(i)=y
   3  continue
      return
      end subroutine
      !
      SUBROUTINE tred2(a,n,np,d,e)
      implicit double precision (a-h,o-z)
      INTEGER n,np
      double precision a(np,np),d(np),e(np)
      INTEGER i,j,k,l
      double precision f,g,h,hh,scale
      do 18 i=n,2,-1
        l=i-1
        h=0.
        scale=0.
        if(l.gt.1)then
          do 11 k=1,l
            scale=scale+abs(a(i,k))
11        continue
          if(scale.eq.0.)then
            e(i)=a(i,l)
          else
            do 12 k=1,l
              a(i,k)=a(i,k)/scale
              h=h+a(i,k)**2
12          continue
            f=a(i,l)
            g=-sign(sqrt(h),f)
            e(i)=scale*g
            h=h-f*g
            a(i,l)=f-g
            f=0.
            do 15 j=1,l
!     Omit following line if finding only eigenvalues
              a(j,i)=a(i,j)/h
              g=0.
              do 13 k=1,j
                g=g+a(j,k)*a(i,k)
13            continue
              do 14 k=j+1,l
                g=g+a(k,j)*a(i,k)
14            continue
              e(j)=g/h
              f=f+e(j)*a(i,j)
15          continue
            hh=f/(h+h)
            do 17 j=1,l
              f=a(i,j)
              g=e(j)-hh*f
              e(j)=g
              do 16 k=1,j
                a(j,k)=a(j,k)-f*e(k)-g*a(i,k)
16            continue
17          continue
          endif
        else
          e(i)=a(i,l)
        endif
        d(i)=h
18    continue
!     Omit following line if finding only eigenvalues.
      d(1)=0.
      e(1)=0.
      do 24 i=1,n
!     Delete lines from here ...
        l=i-1
        if(d(i).ne.0.)then
          do 22 j=1,l
            g=0.
            do 19 k=1,l
              g=g+a(i,k)*a(k,j)
19          continue
            do 21 k=1,l
              a(k,j)=a(k,j)-g*a(k,i)
21          continue
22        continue
        endif
!     ... to here when finding only eigenvalues.
        d(i)=a(i,i)
!     Also delete lines from here ...
        a(i,i)=1.
        do 23 j=1,l
          a(i,j)=0.
          a(j,i)=0.
23      continue
!     ... to here when finding only eigenvalues.
24    continue
      return
      END SUBROUTINE
      !
!  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      SUBROUTINE tqli(d,e,n,np,z)
      implicit double precision (a-h,o-z)
      INTEGER n,np
      double precision d(np),e(np),z(np,np)
      INTEGER i,iter,k,l,m
      double precision b,c,dd,f,g,p,r,s
      do 11 i=2,n
        e(i-1)=e(i)
11    continue
      e(n)=0.
      do 15 l=1,n
        iter=0
1       do 12 m=l,n-1
          dd=abs(d(m))+abs(d(m+1))
          if (abs(e(m))+dd.eq.dd) goto 2
12      continue
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
          do 14 i=m-1,l,-1
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
            do 13 k=1,n
              f=z(k,i+1)
              z(k,i+1)=s*z(k,i)+c*f
              z(k,i)=c*z(k,i)-s*f
13          continue
!     ... to here when finding only eigenvalues.
14        continue
          d(l)=d(l)-p
          e(l)=g
          e(m)=0.
          goto 1
        endif
15    continue
      return
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
      return
      END FUNCTION
      
      
      end module
      
