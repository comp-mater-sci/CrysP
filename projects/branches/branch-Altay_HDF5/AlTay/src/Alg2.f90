#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayMacroKinematic
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

      
      !> function that returns “the ratio of the parallel strain rates”
      pure double precision function ratlon(MacroDefRate,relaxationrate_sam)
      implicit none
      type(DeformationRate), intent(in)               :: MacroDefRate
      double precision,dimension(3,3),intent(in)      :: relaxationrate_sam
      !
      ! MacroDefRate%StrainMode & relaxationrate_sam: expressed in same (sample) reference frame
      ratlon= sum( (MacroDefRate%StrainMode + relaxationrate_sam/MacroDefRate%NormStrainRate) *  &
                    MacroDefRate%StrainMode                            )
      !
      end function
      

      
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
      
