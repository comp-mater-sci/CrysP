!
! $Id$
!

#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif

      module altaySliprate
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayPancake
      use altayDeformationMechanismData_preconfigured
      use altayDeformationMechanism
      use altayCRSSTypes

      type SlipratSolution
          !> Shear rates over all deformation systems (slip and twinning systems)
          !> for given grain 
          type(ShearRateData) :: shearrate
          !> Total (sum of absolute values of) shear rate over all deformation 
          !> systems (slip and twinning systems)
          double precision    :: totalshearrate = 0.0D0
          !> Taylor factor for given grain
          double precision    :: taylorfactor = 0.0D0
          !> Work rate for given grain
          double precision    :: workrate = 0.0D0
          !> von Mises equivalent stress for given grain
          double precision    :: vMeqstress = 0.0D0
      end type
      
      !> Maximum number of potentially active slip systems that can be 
      !> processed by this module.
      integer, parameter :: n_active_max = 8
      
      contains
      
      Subroutine SLIPRAT(solution,MacroDefRate,Pancak2_input,crss_data,DM_data)
      use altayIOConfig
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      use altayMacroKinematic
      implicit none
      !
      type(SlipratSolution),intent(out)          :: solution
      type(DeformationRate),intent(in)           :: MacroDefRate      
      type(Pancak2Solution),intent(in)           :: Pancak2_input
      type(CRSSData),intent(in)                  :: crss_data
      type(DeformationMechanismData), intent(in) :: DM_data
!     September 2000
!     To find the slip rates assuming that
!     - the stress, strain rate and the active slip systems are known,
!       previously obtained by PANCAK2;
!     - (under the above resrtrictions) the sum of the squares of the slip
!       rates must be minimal. 
!
!     Modified in Aug 2010
!
      !> The sign of slip (1.d0 or -1.d0) as found by Pancak2
      double precision, dimension(DM_max_systems) :: sgnn = 0.d0
      !> Alternative A1-matrix: the columns corresponding to negative slip 
      !> as found by Pancak2, have reversed sign. Consequently, all slip systems
      !> associated to A1_sgnn are supposed to have positive slip.
      double precision, dimension(5,DM_max_systems) :: A1_sgnn = 0.d0
      integer :: i, i1, i2, i3, j, j1, j2
      !> number of valid solutions found by minsqu
      integer :: nopl
      integer :: nn
      integer :: info
      !
      integer :: IND(n_active_max), ISTOR(0:n_active_max)
      double precision :: SLSTOR(0:n_active_max)
      !
      NOPL = 0
      !
      !Allocate the (allocatable components of) solution
      call ShearRateData_init(solution%shearrate,DM_data%n_systems,info)   
      !
      !Set sgnn & A1_sgnn
      do concurrent (i=1:Pancak2_input%nactiv)
          j = Pancak2_input%indact(i)
          if (Pancak2_input%taurlp(i) < 0.0D0) then
              sgnn(j) = -1.D0
          else
              sgnn(j) = 1.D0
          end if
          A1_sgnn(:,j) = sgnn(j) * DM_data%A1(:,j)
      end do

      !
      !check whether solution is totally zero
      if ( sum(abs(Pancak2_input%sliplp(1:Pancak2_input%nactiv))) < &
           Pancak2_tolerance ) goto 6
      !
      IND(1:Pancak2_input%nactiv)=Pancak2_input%indact(1:Pancak2_input%nactiv)
      call minsqu(Pancak2_input%nactiv,IND,A1_sgnn,Pancak2_input%BB8,ISTOR,SLSTOR,NOPL)
      !
      if (Pancak2_input%nactiv <= 5 .AND. NOPL > 0) goto 2
      !
!
!     Let us take all combinations of NN out of Pancak2_input%nactiv
!
!     "Levels" in the combination search:
!     (first level:  if Pancak2_input%nactiv=8, find all combinations of 7 sl. syst.
!      second level: find all combinations of 6 - etc.)
!
!     First level
!
      if (Pancak2_input%nactiv < 6) goto 6
      do I1=1,Pancak2_input%nactiv
          call minsqu(Pancak2_input%nactiv - 1,IND,A1_sgnn,Pancak2_input%BB8,ISTOR,SLSTOR,NOPL)
          J=Pancak2_input%nactiv-I1
          if (J > 0) IND(J)=Pancak2_input%indact(J+1)
      enddo
!
!     Level 2
!
      if (Pancak2_input%nactiv < 7) goto 2
      do I1=2,Pancak2_input%nactiv
          J1=I1-1
          do I2=1,J1
              j=1
              do i=1,Pancak2_input%nactiv
                  if (.not. (i==I1 .or. i==I2)) then
                      IND(j)=Pancak2_input%indact(i)
                      j=j+1
                  end if
              end do
              call minsqu(Pancak2_input%nactiv - 2,IND,A1_sgnn,Pancak2_input%BB8,ISTOR,SLSTOR,NOPL)
          enddo
      enddo
!
!     Level 3
!
      if (Pancak2_input%nactiv < 8) goto 2
      do I1=3,Pancak2_input%nactiv
          J1=I1-1
          do I2=2,J1
              J2=I2-1
              do I3=1,J2
                  j=1
                  do i=1,Pancak2_input%nactiv
                      if (.not. (i==I1 .or. i==I2 .or. i==I3)) then
                          IND(j)=Pancak2_input%indact(i)
                          j=j+1
                      end if
                  end do
                  call minsqu(Pancak2_input%nactiv - 3,IND,A1_sgnn,Pancak2_input%BB8,ISTOR,SLSTOR,NOPL)
              enddo
          enddo
      enddo
      !
      !---LABEL 2---
2     continue
      !
      if (NOPL==0) goto 6
      !
      NN=ISTOR(0)
      IND(1:NN)=ISTOR(1:NN)
      solution%shearrate%shearrate(IND(1:NN)) = sgnn(IND(1:NN))*SLSTOR(1:NN)*MacroDefRate%vMeqStrainRate
      !
      if (IPR == 2 .and. NLIST == 1) then
          write (IMP,100)
          write (IMP,104) Pancak2_input%nactiv,NN,NOPL
          write (IMP,106) (IND(i),i=1,NN)
          do i=1,NN
              write (IMP,101) i,IND(i),SLSTOR(i)*sgnn(IND(i))
          end do
      end if
100   format (' Results SLIPRAT')
104   format (' Reduction of NACTIV from',I5,'   to',i5,' NOPL=',i5)
106   format (8i5)
101   format (2i5,5x,d15.6)
      !
      goto 789
      !
      !---LABEL 6---
  6   NN = Pancak2_input%nactiv
      IND(1:NN)=Pancak2_input%indact(1:NN)
      solution%shearrate%shearrate(IND(1:NN)) = Pancak2_input%sliplp(1:NN)*MacroDefRate%vMeqStrainRate
      !
      if (IPR==2 .and. NLIST==1) then
          write (IMP,100)
          write (IMP,108) NN
          do i=1,NN
              write (IMP,101) i,IND(i),Pancak2_input%sliplp(i)
          end do
      end if
108   format (' Linear programming solution retained  NN=',i5)
      !
      !---LABEL 789---
789   continue   
      !
      !Set all remaining components of the solution
      solution%totalshearrate = sum(abs(solution%shearrate%shearrate))
      !
      solution%taylorfactor = solution%totalshearrate / MacroDefRate%vMeqStrainRate
      !
      call altayCRSSTypes_CalcWorkRate(solution%workrate, &
          crss_data, solution%shearrate, info)
      !
      solution%vMeqStress = solution%workrate / MacroDefRate%vMeqStrainRate
      !
      return
      
      contains
      
      pure subroutine minsqu(NN,IND,A1_sgnn,BB8,ISTOR,SLSTOR,NOPL)
      use altayIOConfig
      implicit none
      integer, intent(in)                          :: NN
      integer, dimension(n_active_max), intent(in) :: IND
      double precision, dimension(:,:), intent(in) :: A1_sgnn
      double precision, dimension(5), intent(in)   :: BB8
      integer, intent(inout)                       :: ISTOR(0:n_active_max)
      double precision, intent(inout)              :: SLSTOR(0:n_active_max)
      integer, intent(inout)                       :: NOPL
      !
      integer :: N1, N2, i, j, ineg
      double precision, dimension(n_active_max)  :: SLPR
      double precision :: A(13,13), AA(13,13), B(13), BA(13), RES, sumsq, AA_LU(13,13)
      double precision, parameter :: tol = 1.0d-10
!     December 2000
!     The  normalisation by DELTAT (now: MacroDefRate%vMeqStrainRate) of the september 2000 version has been
!     removed here. Is now done in PANCAK2.
!
!     Modified Aug 2010
!
      !
      !     Set up system of equations
      if (NN <= 5) then
          N1=5
          N2=NN
          do i=1,N2
             A(1:5,i) = A1_sgnn(1:5,IND(i))
          enddo
          B(1:5)=BB8(1:5)
          !> Do matrix multiplication for both A and B with transpose(A) on 
          !> the left-side. This has two beneficial effects:
          !> -The system of linear equations now has symmetric coefficient matrix AA
          !> -For NN<5, the original overdetermined system (5 equations for NN
          !>  variables) is replaced with a system of NN equations.
          AA(1:N2,1:N2) = matmul(transpose(A(1:N1,1:N2)),A(1:N1,1:N2))
          BA(1:N2) = matmul(transpose(A(1:N1,1:N2)),B(1:N1))
      else
          !> Solve a quadratic minimization problem (i.e. minimal sum-of-squares
          !> of unknown deformation rates) with 5 linear constraints (i.e. the solution
          !> realizes a strain rate equal to imposed local strain rate (5D-vectors)),
          !> by solving system of linear equations with Lagrange Multipliers.
          N1=NN+5
          N2=N1
          A = 0.D0
          do i=1,NN
             A(i,i)=1.D0
             B(i)=0.0
             do j=1,5
                A(i,NN+j) = A1_sgnn(j,IND(i))
                A(NN+j,i) = A1_sgnn(j,IND(i))
             enddo
          enddo
          do j=1,5
             B(NN+j)=BB8(j)
          enddo
          !> the system of equations to be solved further on (AA*X=BB)
          !> is identical to the 'original' one (A*X=B). Note that the 
          !> coefficient matrix AA=A is symmetric.
          AA = A
          BA = B
      endif
      !
      call STELSEL(N2,N2,AA(1:N2,1:N2),BA(1:N2),TOL)
      !
      RES=0.0
      do i=1,N1
          RES = RES + (sum(A(i,1:N2)*BA(1:N2))-B(i))**2
      end do
      !
      SLPR(1:NN) = BA(1:NN)
      !
      sumsq = sum(SLPR(1:NN)**2)
      !
      !Set ineg
      if (minval(SLPR(1:NN)) > 0.0d0) then
          ineg = 0
      else
          ineg = 1
      end if
      if (RES > (10000.0*TOL)) ineg = -1
      !
      if (ineg == 0) then !A valid solution
          if(NOPL == 0 .OR. sumsq < SLSTOR(0)) then
              ISTOR(0) = NN
              ISTOR(1:NN) = IND(1:NN)
              if (NN<n_active_max) ISTOR(NN+1:n_active_max) = 0
              !
              SLSTOR(0) = sumsq
              SLSTOR(1:NN) = SLPR(1:NN)
              if (NN<n_active_max) SLSTOR(NN+1:n_active_max) = 0.d0
          end if
          NOPL = NOPL + 1
      end if
          
      return
      end subroutine
      !
      end subroutine
      
      
      !External procedures:
      !
      pure subroutine Kleinkwa(N1,N2,M1,M2,A,B,BA,TOL,RES)
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
      implicit none
      integer,intent(in)                :: N1,N2,M1,M2
      double precision,intent(in)    :: A(M1,M2)
      double precision,intent(in)    :: B(M2)
      double precision,intent(out)    :: BA(M2)
      double precision,intent(in)       :: TOL
      double precision,intent(out)      :: RES
      !
      double precision    :: AA(M2,M2)
      integer :: kk, i, j
      !
      do kk=1,N2
          BA(kk) = sum(A(1:N1,kk)*B(1:N1))
          do j=1,N2
              AA(kk,j) = sum(A(1:N1,kk)*A(1:N1,j))
          end do
      end do
      !
      call STELSEL(N2,M2,AA,BA,TOL)
      !
      RES=0.0
      do i=1,N1
          RES = RES + (sum(A(i,1:N2)*BA(1:N2))-B(i))**2
      end do
      !
      return
      end subroutine
      !
      pure subroutine STELSEL(N,M,A,R,TOL)
      implicit none
      integer,intent(in)                :: N,M
      double precision,intent(inout)    :: A(M,M)
      double precision,intent(inout)    :: R(M)
      double precision,intent(in)       :: TOL
      !
      double precision    :: VAL(M)
      double precision    :: XV(M)
      double precision    :: YV(M)
      integer :: i,j
      double precision :: y, z
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
      pure subroutine tred2(a,n,np,d,e)
      implicit none
      integer,intent(in)                :: n,np
      double precision,intent(inout)    :: a(np,np)
      double precision,intent(out)      :: d(np)
      double precision,intent(inout)    :: e(np)
      INTEGER :: i,j,k,l
      double precision :: f,g,h,hh,scale
      !
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
      end subroutine
      !
!  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      pure subroutine tqli(d,e,n,np,z)
      implicit none
      double precision,intent(inout) :: d(np)
      double precision,intent(inout) :: e(np)
      double precision,intent(inout) :: z(np,np)
      integer,intent(in)             :: n,np
      !
      integer :: i,iter,k,l,m
      double precision :: b,c,dd,f,g,p,r,s
      !
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
          ! if(iter.eq.100) write(*,*) 'too many iterations in tqli'
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
      end subroutine
      !
!  (C) Copr. 1986-92 Numerical Recipes Software D04-4-+5Z5{..
      pure double precision function pythag(a,b)
      implicit none
      double precision,intent(in) :: a,b
      !
      double precision :: absa,absb
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
      