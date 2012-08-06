      Subroutine Kleinkwa(N1,N2,M1,M2,A,B,AA,BA,VAL,XV,YV,TOL,RES)
C     N1=number of equations
C     N2=number of unknowns
C     A=coefficient matrix
C     B=right hand sides
C     BA=solution on output
C     AA,VAL,XV,YV=work space
C     RES=residu (sum of squares)
C     M1,M2=dimensions
C
C     We make it a set with a symmetrical matrix, because
C     we want to use STELSEL to solve it.
C
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
      end






      Subroutine STELSEL(N,M,A,R,TOL,VAL,XV,YV)
      IMPLICIT double precision (A-H,O-Z)
      dimension A(M,M),R(M),VAL(M),XV(M),YV(M)
C
C     to solve the system of equations A * X = R using
C     eigenvalues and eigenvectors
C
C     This method is known as "Singular Value Decomposition Method".
C
C     A s a symmetrical matrix (NxN). Modified during the process.
C
C     The output X is actually stored in R, which is hence modified.
C
C     On return, A (NxN) contains the eigenvectors
C     VAL (N) contains eigenvalues
C     XV, YV : workspace
C
C      write (IMP,203) TOL
C 203  format (' TOL',d20.10)
C      do 50 i=1,N
C      write (IMP,201) r(i),(A(i,j),j=1,N)
C 201  format (d12.4,5x,5d12.4)
C  50  continue
      call tred2(a,N,M,VAL,XV)
      call tqli(VAL,XV,N,M,a)
      do 1 j=1,N
      y=0.0d00
      do 2 i=1,N
      y=y+A(i,j)*R(i)
   2  continue
      z=VAL(j)
C      write (IMP,202) j,z
C 202  format (' VAL(j)',i5,d20.10)
      if (abs(z).lt.tol) then
           z=0.0d0
      else
           z=y/z
      endif
C      write (IMP,202) j,z
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
      end
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
C     Omit following line if finding only eigenvalues
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
C     Omit following line if finding only eigenvalues.
      d(1)=0.
      e(1)=0.
      do 24 i=1,n
C     Delete lines from here ...
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
C     ... to here when finding only eigenvalues.
        d(i)=a(i,i)
C     Also delete lines from here ...
        a(i,i)=1.
        do 23 j=1,l
          a(i,j)=0.
          a(j,i)=0.
23      continue
C     ... to here when finding only eigenvalues.
24    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      SUBROUTINE tqli(d,e,n,np,z)
      implicit double precision (a-h,o-z)
      INTEGER n,np
      double precision d(np),e(np),z(np,np)
CU    USES pythag
      INTEGER i,iter,k,l,m
      double precision b,c,dd,f,g,p,r,s,pythag
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
          if(iter.eq.100)pause 'too many iterations in tqli'
          iter=iter+1
          g=(d(l+1)-d(l))/(2.*e(l))
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
            r=(d(i)-g)*s+2.*c*b
            p=s*r
            d(i+1)=g+p
            g=c*r-b
C     Omit lines from here ...
            do 13 k=1,n
              f=z(k,i+1)
              z(k,i+1)=s*z(k,i)+c*f
              z(k,i)=c*z(k,i)-s*f
13          continue
C     ... to here when finding only eigenvalues.
14        continue
          d(l)=d(l)-p
          e(l)=g
          e(m)=0.
          goto 1
        endif
15    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      FUNCTION pythag(a,b)
      implicit double precision (a-h,o-z)
      double precision a,b,pythag
      double precision absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1.+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag=0.
        else
          pythag=absb*sqrt(1.+(absa/absb)**2)
        endif
      endif
      return
      END
