#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      SUBROUTINE MATPROD(C,A,B,N1,N2,N3)
C     MATRIX C=MATRIX A*MATRIX B                                        
      implicit double precision (a-h,o-z)
      DIMENSION A(N1,N2),B(N2,N3),C(N1,N3)
      DO 1 I=1,N1                                                       
      DO 2 J=1,N3                                                       
      X=0.                                                              
      DO 3 K=1,N2                                                       
      X=X+A(I,K)*B(K,J)                                                 
 3    CONTINUE                                                          
      C(I,J)=X                                                          
 2    CONTINUE                                                          
 1    CONTINUE                                                          
      RETURN                                                            
      END                                                               
      subroutine UPDATC(CIJ,F2)
      implicit double precision (a-h,o-z)
C     Updating of CIJ matrix of ellipsoid
C     F2 is the inverse of the F-tensor which describes the strain
C     increment. To be sure, it is first normalised.
C
      dimension CIJ(3,3),F2(3,3),X(3,3)
      y=F2(1,1)*(F2(2,2)*F2(3,3)-F2(2,3)*F2(3,2))
      y=y-F2(1,2)*(F2(2,1)*F2(3,3)-F2(2,3)*F2(3,1))
      y=y+F2(1,3)*(F2(2,1)*F2(3,2)-F2(2,2)*F2(3,1))
      y=y**(1.0/3.0)
      do 1 i=1,3
      do 11 j=1,3
      F2(i,j)=F2(i,j)/y
  11  continue
   1  continue
      do 3 k=1,3
      do 13 l=1,3
      y=0.0
      do 4 i=1,3
      do 14 j=1,3
      y=y+CIJ(i,j)*F2(i,k)*F2(j,l)
  14  continue
   4  continue
      X(k,l)=y
  13  continue
   3  continue
      do 2 i=1,3
      do 12 j=1,3
      CIJ(i,j)=X(i,j)
  12  continue
   2  continue
      return
      end
      subroutine UPDATF(F,F1)
      implicit double precision (a-h,o-z)
      dimension F(3,3),F1(3,3),X(3,3)
      call MATPROD(X,F1,F,3,3,3)
C
C     y is the determinant of X
C
      y=X(1,1)*(X(2,2)*X(3,3)-X(2,3)*X(3,2))
      y=y-X(1,2)*(X(2,1)*X(3,3)-X(2,3)*X(3,1))
      y=y+X(1,3)*(X(2,1)*X(3,2)-X(2,2)*X(3,1))
      y=y**(1.0/3.0)
      do 1 i=1,3
      do 11 j=1,3
      F(i,j)=X(i,j)/y
  11  continue
   1  continue
      return
      end
      subroutine Ftensor(DG,F1,F2)
      implicit double precision (a-h,o-z)
C
C     DG = approximate displacement gradient (input)
C     DG is in fact the velocity gradient * time increment
C
C     F1 is the "F tensor" (deformation gradient ?)
C     which corresponds to such velocity gradient
C     and such time increment
C
C     F2 is the inverse of F1
C
C
      dimension DG(3,3),F1(3,3),F2(3,3),X(3,3),Y(3,3),A(3,3),B(3,3)
      data n/10/
      do 1 i=1,3
      do 2 j=1,3
      X(i,j)=DG(i,j)/n
      Y(i,j)=-DG(i,j)/n
      F1(i,j)=X(i,j)
      F2(i,j)=Y(i,j)
   2  continue
      X(i,i)=X(i,i)+1.0
      Y(i,i)=Y(i,i)+1.0
      F1(i,i)=X(i,i)
      F2(i,i)=Y(i,i)
   1  continue
      do 23 k=2,n
      call MATprod(A,X,F1,3,3,3)
      call MATprod(B,Y,F2,3,3,3)
      do 3 i=1,3
      do 13 j=1,3
      F1(i,j)=A(i,j)
      F2(i,j)=B(i,j)
  13  continue
   3  continue
  23  continue
      return
      end
  




      Subroutine STR33(A,V)
      implicit double precision (a-h,o-z)
C     To make a 3 x 3 tensor from a 5-dim. vector
C     dev. stress space or strain space
      dimension A(3,3),V(5)
      logical eerste
      SAVE
      data eerste/.true./
C      data sq22/0.7071068/,const3/0.7886751/,const4/0.2113249/
      if (eerste) then
            eerste=.false.
            sq22=sqrt(0.5d0)
            const3=(SQRT(3.0d0)+3.0d0)/6.0d0
            const4=(3.0d0-SQRT(3.0d0))/6.0d0
      endif
      A(2,2)=CONST3*V(1)-CONST4*V(2)
      A(3,3)=-CONST4*V(1)+CONST3*V(2)
      A(1,1)=-A(2,2)-A(3,3)
      X1=SQ22*V(3)
      A(2,3)=X1
      A(3,2)=X1
      X1=SQ22*V(4)
      A(3,1)=X1
      A(1,3)=X1
      X1=SQ22*V(5)
      A(1,2)=X1
      A(2,1)=X1
      return
      end
      Subroutine STR5(D5,VGRAD)
      IMPLICIT double precision (A-H,O-Z)
      dimension D5(5),VGRAD(3,3)
      SAVE
C      data c1/1.3660254/,c2/0.3660254/,c3/0.707107/
      logical spring
      data spring /.false./
      if (spring) goto 1
      spring=.true.
      c1=0.5d00*(sqrt(3.0d00)+1)
      c2=C1-1.0d00
      C3=sqrt(0.5d00)
   1  D5(1)=c1*VGRAD(2,2)+c2*VGRAD(3,3)
      D5(2)=c2*VGRAD(2,2)+c1*VGRAD(3,3)
      D5(3)=c3*(VGRAD(2,3)+VGRAD(3,2))
      D5(4)=c3*(VGRAD(3,1)+VGRAD(1,3))
      D5(5)=c3*(VGRAD(1,2)+VGRAD(2,1))
      return
      end
      Subroutine Tmatrix(T,fi1,PHI,fi2)
      IMPLICIT double precision (A-H,O-Z)
C     To calculate T-matrix from Euler angles
      dimension T(3,3)
      C1=COS(fi1)
      C= COS(PHI)
      C2=COS(fi2)
      S1=SIN(fi1)
      S= SIN(PHI)
      S2=SIN(fi2)
      T(1,1)=C1*C2-S1*S2*C
      T(1,2)=S1*C2+C1*S2*C
      T(1,3)=S2*S
      T(2,1)=-C1*S2-S1*C2*C
      T(2,2)=-S1*S2+C1*C2*C
      T(2,3)=C2*S
      T(3,1)=S1*S
      T(3,2)=-C1*S
      T(3,3)=C
      return
      end
      Subroutine Transf(Gaxes,Aprime,T)
      IMPLICIT double precision (A-H,O-Z)
C
C     to calculate the CIJ matrix of an ellipsoid with half axes
C     stored in Gaxes. T defines the orientation of the axes.
C
C     This version assumes that A is a diagonal matrix
C
      dimension Gaxes(3),A(3),Aprime(3,3),T(3,3),X(3,3)
      do 2 k=1,3
      A(k)=1.0/Gaxes(k)**2
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
      end
      Subroutine GETANG(CIJ,prval,GEULR,TMAT)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      IMPLICIT double precision (A-H,O-Z)
C
C     find half-lengths of ellipsoid axes from CIJ matrix
C     store them in prval
C     find Euler angles of these axes, store in GEULR
C
      dimension CIJ(3,3),TMAT(3,3),GEULR(3)
      Dimension prval(3),prdir(3,3),e(3,3)
      logical axisym
      CIJTR=(CIJ(1,1)+CIJ(2,2)+CIJ(3,3))/3.0
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
      prval(i)=1.0/sqrt(prval(i))
      do 37 j=1,3
      TMAT(i,j)=prdir(j,i)
   37 continue
      call EULER1(TMAT,GEULR(1),GEULR(2),GEULR(3))
      return
      end
      Subroutine eigenv(e,prval,prdir,enrm,axisym)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      IMPLICIT double precision (A-H,O-Z)
C     Principal values of symmetric tensor with zero trace
C     The eigenvectors are normalized.
C     prval contains the principal values
C
      dimension e(3,3),x(3),prval(3), y(3,3),prdir(3,3)
      logical axisym,eerste
      SAVE
      data eerste/.true./
      if (eerste) then
         pi=4.0d0*atan(1.0d0)
      endif
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
      stop
#else
      RCM_RAISE(1,'eigenv','The input tensor is not symmetric',RCM_RTN)
#endif
      
    7 a=a+e(i,j)**2
    6 continue
    5 continue
      enrm=sqrt(a)
      if (enrm.lt.0.5e-5) goto 33
      a=a*0.5
      b=e(1,1)*e(2,3)**2+e(2,2)*e(3,1)**2+e(3,3)*e(1,2)**2-
     1 2.0*e(1,2)*e(2,3)*e(3,1)-e(1,1)*e(2,2)*e(3,3)
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
      eta=(theta+(imax-1)*2*pi)/3.0
      if (eta.gt.pi) eta=eta-2.0*pi
C
C     Correction on the order of the eigenvalues
C
      if (abs(prval(2)).lt.abs(prval(1))) goto 11
      xx=prval(2)
      prval(2)=prval(1)
      prval(1)=xx
   11 axisym=abs(prval(2)-prval(1)).lt.0.5e-5
      if (.not.axisym) goto 9
      prval(1)=0.5*(prval(1)+prval(2))
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
C     the minor with the max. value has been identified
      i1=imax+1
      if (i1.gt.3) i1=1
      i2=i1+1
      if (i2.gt.3) i2=1
      j1=jmax+1
      if (j1.gt.3) j1=1
      j2=j1+1
      if (j2.gt.3) j2=1
      prdir(jmax,ipr)=1.0
      pmax=-(y(i1,jmax)*y(i2,j2)-y(i2,jmax)*y(i1,j2))
      prdir(j1,ipr)=pmax/pp
      pmax=-(y(i1,j1)*y(i2,jmax)-y(i2,j1)*y(i1,jmax))
      prdir(j2,ipr)=pmax/pp
      call normaliz(prdir(1,ipr),xx)
      if (axisym) goto 17
   12 continue
      goto 19
C
C     Vectorial product between prdir(,3) and x3 axis
C
   17 prdir(1,2)=-prdir(2,3)
      prdir(2,2)=prdir(1,3)
      prdir(3,2)=0.0
      call normaliz(prdir(1,2),xx)
      if (xx.gt.0.7) goto 19
C
C     Vectorial product between prdir(,3) and x2 axis
C
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
      prdir(i,i)=1.0
   34 continue
      goto 21
      end
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
      end
      subroutine canoni(a,b,X,theta,pi)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      IMPLICIT double precision (A-H,O-Z)
c
c     should find the roots of an equation
c
c     x**3 - A x + B = 0
c
c     The roots are suppposed to be real.
c
      dimension x(3)
      if (a.lt.0.5e-11) goto 2
      roota=sqrt(a**3/27.0d0)
      delta=0.5*b/roota
      if (abs(delta).lt.(1.0d0+1.0d-6)) goto 1
    2 continue
#ifndef ALTAY_SUBROUTINE
      write (*,100)
  100 format(' Subroutine CANONI - 2 Roots seem to be complex')
      stop
#else
      RCM_RAISE(1,'CANONI','Two roots seem to be complex',RCM_RTN)
#endif      
    1 if (delta.gt.1.0) delta=1.0
      if (delta.lt.-1.0) delta=-1.0
      theta=acos(delta)
      delta=-2.0*sqrt(a/3.0)
      x(1)=delta*cos(theta/3.0)
      x(2)=delta*cos((theta+2.0*pi)/3.0)
      x(3)=delta*cos((theta+4.0*pi)/3.0)
      return
      end
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
      end
      Subroutine EULER1(T,fi1,PHI,fi2)
      IMPLICIT double precision (A-H,O-Z)
C     To calculate Euler angles from T-matrix
      Dimension T(3,3)
      x=0.0
      do 1 i=1,3
      x=x+T(i,3)**2
   1  continue
      x=T(3,3)/SQRT(x)
      PHI=ACOS(x)
      if (x.eq.1.0.or.x.eq.-1.0) goto 2
      fi1=ATAN2(T(3,1),-T(3,2))
      fi2=ATAN2(T(1,3),T(2,3))
      goto 3
   2  fi1=ATAN2(-T(2,1)/X,T(2,2)/X)
      fi2=0.0
   3  return
      end
      SUBROUTINE MINV(A,N,D,L,M,NXXX)                                   
      DIMENSION A(NXXX),L(N),M(N)                                       
C                                                                       
C        ...............................................................
C                                                                       
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      
C        STATEMENT WHICH FOLLOWS.                                       
C                                                                       
      DOUBLE PRECISION A,D,BIGA,HOLD
C                                                                       
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS    
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS      
C        ROUTINE.                                                       
C                                                                       
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO      
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  ABS IN STATEMENT  
C        10 MUST BE CHANGED TO DABS.                                    
C                                                                       
C        ...............................................................
C                                                                       
C        SEARCH FOR LARGEST ELEMENT                                     
C                                                                       
      D=1.0                                                             
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
C                                                                       
C        INTERCHANGE ROWS                                               
C                                                                       
      J=L(K)                                                            
      IF(J-K) 35,35,25                                                  
   25 KI=K-N                                                            
      DO 30 I=1,N                                                       
      KI=KI+N                                                           
      HOLD=-A(KI)                                                       
      JI=KI-K+J                                                         
      A(KI)=A(JI)                                                       
   30 A(JI) =HOLD                                                       
C                                                                       
C        INTERCHANGE COLUMNS                                            
C                                                                       
   35 I=M(K)                                                            
      IF(I-K) 45,45,38                                                  
   38 JP=N*(I-1)                                                        
      DO 40 J=1,N                                                       
      JK=NK+J                                                           
      JI=JP+J                                                           
      HOLD=-A(JK)                                                       
      A(JK)=A(JI)                                                       
   40 A(JI) =HOLD                                                       
C                                                                       
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS        
C        CONTAINED IN BIGA)                                             
C                                                                       
   45 IF(BIGA) 48,46,48                                                 
   46 D=0.0                                                             
      RETURN                                                            
   48 DO 55 I=1,N                                                       
      IF(I-K) 50,55,50                                                  
   50 IK=NK+I                                                           
      A(IK)=A(IK)/(-BIGA)                                               
   55 CONTINUE                                                          
C                                                                       
C        REDUCE MATRIX                                                  
C                                                                       
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
C                                                                       
C        DIVIDE ROW BY PIVOT                                            
C                                                                       
      KJ=K-N                                                            
      DO 75 J=1,N                                                       
      KJ=KJ+N                                                           
      IF(J-K) 70,75,70                                                  
   70 A(KJ)=A(KJ)/BIGA                                                  
   75 CONTINUE                                                          
C                                                                       
C        PRODUCT OF PIVOTS                                              
C                                                                       
      D=D*BIGA                                                          
C                                                                       
C        REPLACE PIVOT BY RECIPROCAL                                    
C                                                                       
      A(KK)=1.0/BIGA                                                    
   80 CONTINUE                                                          
C                                                                       
C        FINAL ROW AND COLUMN INTERCHANGE                               
C                                                                       
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
      END
