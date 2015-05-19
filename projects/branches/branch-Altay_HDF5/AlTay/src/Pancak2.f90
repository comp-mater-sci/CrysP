#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayPancake
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use criMathUtils
      use altayHardTypes
      use altayCRSSTypes
      use altayDeformationMechanism
      
      integer, parameter, public ::          Pancak2_max_activesystems = 8
      integer, parameter, public ::          Pancak2_max_grains = 2
      double precision, parameter, public :: Pancak2_tolerance = 5.0d-6
      
      type Pancak2Solution
          !> local stress expressed in the sample reference frame
          double precision, dimension(3,3)                       :: stress_sam
          !> symmetric part of (non-normalized) relaxation tensor, in sample reference system
          double precision, dimension(3,3)                       :: relaxationrate_sam
          !> anti-symmetric part of (non-normalized) relaxation tensor, in sample reference system
          double precision, dimension(3,3)                       :: relaxationspin_sam
          integer                                                :: nactiv = 0
          integer,dimension(Pancak2_max_activesystems)           :: indact = 0
          double precision, dimension(Pancak2_max_activesystems) :: sliplp = 0.0d0
          !> CRSS of active deformation systems
          double precision, dimension(Pancak2_max_activesystems) :: taurlp = 0.0d0
          double precision, dimension(5)                         :: BB8
      end type Pancak2Solution
      
      contains
      
! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!
      Subroutine Pancak2(laml, ngr, Tprinc, eulerb, CRSSb, DM_data,  &
                         MacroDefRate, MacroDefState, solution)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif      
      use altayMesostructure
      use altayIOConfig
      use altayHard
      use altayTBH
      use altayAlgorithms
      use altayMacroKinematic
#ifdef PEBP_ENABLED
      use AltayDSHstate
#endif
      implicit double precision (a-h,o-z)
      integer,intent(in)                            :: laml
      integer,intent(in)                            :: ngr
      !> transformation matrix associated to the cluster (grain boundary)
      double precision,dimension(3,3),intent(in)    :: Tprinc
      type (CRSSData),dimension(Pancak2_max_grains),intent(in)       :: CRSSb
      type(EulerAngles),dimension(Pancak2_max_grains),intent(in)     :: eulerb      
      type(DeformationMechanismData), intent(in)    :: DM_data
      type(DeformationRate),intent(in)              :: MacroDefRate
      type(DeformationState),intent(in)             :: MacroDefState
      type(Pancak2Solution),intent(out)             :: solution
      !
      !> Number of active systems founds so far by the search algorithm
      integer nactiv_sofar
      !> Local stress, in crystal reference system
      double precision, dimension(3,3):: S33=0.0d0
      !> Symmetric and anti-symmetric parts of normalized relaxation tensor, in crystal reference system
      double precision, dimension(3,3):: RHOS33, RHOA33
      !> Symmetric and anti-symmetric parts of normalized relaxation tensor, in sample reference system
      double precision, dimension(3,3):: RHOSsa, RHOAsa
      double precision,dimension(5):: RHOS, RHOA 
      double precision,dimension(194) :: XX  
      double precision,dimension(3,3,Pancak2_max_grains)  :: TRFb
      dimension C2(3,3),                                                 &
       TDCb(3,3,Pancak2_max_grains),TRCb(3,3,Pancak2_max_grains),                                          &
       relax(3,3,3),DACC(10),                                            &
       rls(3,3,3,2),rla(3,3),rlm(3,3,3),C3(3,3),TRP(10),APRIME(10),      &
       B3(10,3),PLUMIN(2,3),CUst(10),B5(5),UU(10,10)
!     first index op PLUMIN = nr. of grain
!     second index = nr. of relaxation
      dimension spanv(5),STRSS(10),BB(10)
      dimension CCC(Pancak2_max_grains,194),DTAU(194),DTAU1(194),TAUR(194),TAUR1(194)
      logical bas(194),VALID(194)
!     rlm is unit relaxation tensor in macroscopic frame
!     rls and rla in crystal frame (symmetric and anti-sym. part)
      dimension B8(5,Pancak2_max_grains),UBUF(10),UU2(10,10),UU3(10,10),DD(10)
      integer DI(10),DI2(10)
      dimension GAMR(Pancak2_max_grains),TAURL(Pancak2_max_grains)
      double precision, dimension(10,194) :: A2
      data SQR2/0.7071067811865476D+00/,B3/30*0.0D0/
!     Definition of the two relaxations, representing a
!     13-simple shear and a 23-simple shear, respectively:
      data relax /0.0D0, 0.0D0, 0.0D0,                                   &
                  0.0D0, 0.0D0, 0.0D0,                                   &
                  1.0D0, 0.0D0, 0.0D0,                                   &
                  0.0D0, 0.0D0, 0.0D0,                                   &
                  0.0D0, 0.0D0, 0.0D0,                                   &
                  0.0D0, 1.0D0, 0.0D0,                                   &
                  0.0D0, 0.0D0, 0.0D0,                                   &
                  0.0D0, 0.0D0, 0.0D0,                                   &
                  0.0D0, 0.0D0, 0.0D0/
      data PLUMIN/1.0D0,-1.0D0,                                          &
                  1.0D0,-1.0D0,                                          &
                  1.0D0, 1.0D0/
      data NDIM/10/
!     NDIM=dimension A 
      !> Number of relaxations (0 for Taylor and 2 for ALAMEL) 
      integer :: NRL
      data TAURL/2*0.0d0/
      data GETAL/1.0D6/,TOL/1.0d-6/
      integer :: info
      SAVE

      if (laml.ne.1.and.laml.ne.2) then
#ifndef ALTAY_SUBROUTINE
      write(*,*) 'laml=', laml
      call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'Pancak2','Wrong selection of lamels',RCM_RTN)
#endif      
      endif
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      TWOSQ3=sqrt(2.D0/3.D0)
!     N is number of rows of A2;   NU number of rows of UU2
      N=5*NGR
      NU=N
      M2=NGR*DM_data%n_systems
      NRL=(NGR-1)*2
      M12=NGR*DM_data%n_systems+NRL
      if (laml.eq.2) goto 3
      !Construct A2
      A2 = 0.0d0
      A2( 1:5  ,     1:DM_data%n_systems   ) = DM_data%A1
      A2( 6:10 , 1+DM_data%n_systems:Pancak2_max_grains*DM_data%n_systems ) = DM_data%A1
      !
      do 33 i=M2+1,M12
      do 33 jsgn=1,Pancak2_max_grains
  33  CCC(jsgn,i)=0.0
  32  do 31 i=1,NU
      do 31 j=1,NU
      UU(j,i)=0.0
  31  continue
      DO 53 I=1,5
      DI(I)=DM_data%DI(I)
      DI(I+5)=DM_data%DI(I)+DM_data%n_systems
53    CONTINUE    
      !
      do 1 IL=1,NGR
      L1=5*(IL-1)
      TRFb(:,:,IL) = rotmat(eulerb(IL))
      C2 = rotateSRTensorFrom(MacroDefRate%VelGrad,TRFb(:,:,IL))
      if (NRL.eq.0) goto 87
      do 82 IRL=1,NRL
!     Transform relaxation from grain reference frame to macroscopic frame
      RLM(:,:,IRL) = rotateSRTensorTo(RELAX(:,:,IRL),Tprinc)
!     ... and now to crystal frame:
      C3 = rotateSRTensorFrom(RLM(:,:,IRL),TRFb(:,:,IL))
      do 83 j=1,3
      do 83 i=1,3
      RLS(i,j,IRL,IL)=(C3(I,J)+C3(J,I))*0.5D0
      RLA(i,j)=(C3(I,J)-C3(J,I))*0.5D0
83    continue
      B3(L1+1:L1+3,IRL) = PLUMIN(IL,IRL) * AntiSymMat33ToVec3(RLA) / sqr2 
      B5= SymMat33ToVec5(RLS(1:3,1:3,IRL,IL)) ! sym.(3,3) -> (5)
!     Insert the relaxations as columns in A2-matrix
      j=M2+IRL
      do 84 i=1,5
      i1=i+L1
      x=B5(i)*PLUMIN(IL,IRL)
      A2(i1,j)=x
  84  continue
  82  continue 
  87  continue
      DO 80 I=1,3
      DO 81 J=1,3                                                       
      TDCb(I,J,IL)=(C2(I,J)+C2(J,I))*0.5D0
  81  TRCb(I,J,IL)=(C2(I,J)-C2(J,I))*0.5D0
  80  CONTINUE                                                          
      B5= SymMat33ToVec5(TDCb(1:3,1:3,IL)) ! sym.(3,3) -> (5)
      do 30 i=1,5
      j=i+L1
      BB(j)=B5(i)
  30  continue
!
!     Calculation of time increment by dividing von Mises equivalent
!     strain by von Mises equivalent strain rate
!
      do 44 j=1,5
      B5(j)=B5(j)/MacroDefRate%vMeqStrainRate
      B8(j,IL)=B5(j)
  44  continue
      K1=DM_data%n_systems*(IL-1)
      !
      ! Assign crss_cluster to proper section of CCC
      CCC(:,1+K1:DM_data%n_systems+K1)=CRSSb(IL)%crss(:,1:DM_data%n_systems)  
      !
      ! Set CCC for antitwinning direction equal to
      ! GETAL times CCC for twinning direction       
      do I=DM_data%n_slip_systems+1,DM_data%n_systems ! this do-loop will only be executed for DM_data%n_twinning_systems > 0
          CCC(2,I+K1)=CCC(1,I+K1)*GETAL !notePE20150428
      end do
      !
!   92 write (IMP,914) i,j,CCC(1,j),CCC(2,j)
 914  format (' i,j',2i5, ' CCC ',2d16.4)
      DO 15 J=1,5
      DO 15 I=1,5
      UU(I+L1,J+L1)=DM_data%B(I,J)
  15  continue
   1  continue 
      DO 54 I=1,N 
!     Conversion of strain to normalized strain rate
      BB(I)=BB(I)/MacroDefRate%vMeqStrainRate
  54  CONTINUE
      if (NRL.eq.0) goto 88
      do 85 j=M2+1,M12             
!    The coefficient of the relaxations is set to a very large number
!    in order to suppress the relaxations in a first call of the TBH program     
      CCC(1,j)=GETAL  
      CCC(2,j)=GETAL 
  85  continue
!     Full constraints calculation
!
!     UITVOEREN VAN DE SIMPLEX-SUBROUTINE
  88  IF (IPR.EQ.2) then
      if(NLIST.eq.1) then 
      WRITE (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
      end if
      end if
 218  FORMAT(/' COST FUNCTION',/,(2x,12F10.4))
      IF (IPR.EQ.2) then
      if (NLIST.eq.1) then
      WRITE (IMP,219) (BB(I),I=1,N)
      end if
      end if
 219  format (' right hand side',/,(2x,10F10.4),/)
!     First call of Simplex (full constraints)
      Call TBH(NDIM,N,M2,A2,BB,                                      &
       CCC,UU,UU2,DI,DI2,Dacc,XX,UBUF,FakM,                              &
       Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID) 
! CCC (input): critical resolved shear stresses (Tauc)
! UU (input): initial inverse of "basis" = columns of A2
!      corresponding to thoses slip systems which are active
!      according to first guess 
! UU2 (output): inverse of final "basis" (active slip systems)
! DI (input): indices of basis corresponding to UU
! DI2 (output): indices of basis corresponding to UU2
! Dacc (output): slip rates in basis DI
! XX (output): slip rates (numbered from 1 to M12)
! UBUF (output): stresses, in crystal frames
!                  (2 sets of stresses, one for each crystal)
! Fakm: rate of plastic work of the 2 crsytals together
! Taur (output) resolved shear stress (can be + or -)        
! DTAU (output)=abs(Taur)-Tauc 
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
!
  345 if (IPR.lt.4) goto 220
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) then
      write (IMP,221) IPR
      end if
      write (*,221) IPR
 221  format (' Pancak2 ',                                               &
       ' IPR    =',4I5)
      if (IPR.ge.4) call terminate(stopcode_runtimeerror) 
#else
      RCM_RAISE(1,'Pancak2','IPR must be < 4',RCM_RTN)
#endif
  220    DTAU1=DTAU 
         TAUR1=TAUR  

      if (NRL.eq.0) then
                      UU=UU2
                      DI=DI2
                      STRSS=UBUF
                      goto 89
                    endif
        do 86 IRL=1,NRL
        j=M2+IRL  
        CCC(1,j)=TAURL(IRL)
        CCC(2,j)=TAURL(IRL)
  86    continue 
      IF (IPR.EQ.2) then
      if(NLIST.eq.1) then 
      WRITE (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
      end if
      end if
!     Second call of Simplex (relaxed constraints)
      if (IPR.eq.2) then
      if (NLIST.eq.1) then
      write (IMP,401)
      end if
      end if
 401  format (' Second call of TBH')
      Call TBH(N,N,M12,A2,BB,                                        &
       CCC,UU2,UU,DI2,DI,Dacc,XX,STRSS,FakM,                             &
       Taur,bas,Trp,Aprime,CUst,UU3,DD,DTAU,VALID)
! CCC (input): critical resolved shear stresses (Tauc)
! UU2 (input): initial inverse of "basis" = columns of A2
!      corresponding to thoses slip systems which are active
!      according to first guess 
! UU (output): inverse of final "basis" (active slip systems)
! DI2 (input): indices of basis corresponding to UU2
! DI (output): indices of basis corresponding to UU
! Dacc (output): slip rates in basis DI
! XX (output): slip rates (numbered from 1 to M12)
! STRSS (output): stresses, in crystal frames
!                  (2 sets of stresses, one for each crystal)
! Fakm: rate of plastic work of the 2 crsytals together
! Taur (output) resolved shear stress (can be + or -)        
! DTAU (output)=abs(Taur)-Tauc 
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
!


      if (IPR.ge.4) then
         if(NLIST.eq.1) then
         write (IMP,222) IPR
         end if
         write (*,222) IPR
 222     format (' Pancak2 222 - Problem with TBH',/,                    &
         ' IPR    =',4I5)
#ifndef ALTAY_SUBROUTINE
          call terminate(stopcode_runtimeerror)  
#else
          RCM_RAISE(1,'Pancak2','Problem with TBH',RCM_RTN)

#endif         
      endif

!     GAMR will contain the relaxed shears:
 204  if (NRL.gt.0) then
                       do IRL=1,NRL
                         gamr(IRL)=XX(M2+IRL)
                       enddo
                    endif
!     Check whether 1 grain does not deform at all.
  89  j=0
      do 40 IG=1,NGR
      XXTOT=0.0
      do i=1,DM_data%n_systems
       j=j+1  
       XXTOT=XXTOT+ABS(xx(j))
      enddo
      if (XXTOT.lt.Pancak2_tolerance) goto 213
   40 continue
!     If all grains have a non-zero slip, do the following:
  99  DTAU1=DTAU
      TAUR1=TAUR
      UBUF=STRSS
 213  if(NLIST.eq.1) then 
        write (IMP,780) gamr
      end if
 780  format (' RELAXATIONS:                   ',2d12.4)      
   2  continue
!
!     From here on, output is produced for grain number "laml"
!
   3  continue
      jj=DM_data%n_systems*(laml-1)
      !
      ii=5*(laml-1)
      do 201 i=1,5
!     If one grain does not deform, note that stress UBUF has come
!      from the fullconstraints solution.
      spanv(i)=UBUF(i+ii)
      B5(i)=B8(i,laml)
!      write (IMP,776) laml,B5(i),spanv(i),i+ii
! 776  format (' B5  ',i5,e15.8,   'spanv  ',d15.8,' i+ii',i5)
      x8=0.0
      y8=0.0
      if (NRL.gt.0) then
                     do IRL=1,NRL
                       x8=x8+A2(i+ii,M2+IRL)*gamr(IRL)
                       y8=y8+B3(i+ii,IRL)*gamr(IRL)
                     enddo
                    endif
      solution%BB8(i)=B8(i,laml)-x8
      RHOS(i)=-x8
      RHOA(i)=-y8
 201  continue
      S33 = Vec5ToSymMat33(spanv) 
      !Report S33 to LST-file
      if(NLIST.eq.1) then
          write (IMP,100)
          do i=1,3
              write (IMP,101) (S33(i,j),j=1,3)
          end do
      end if
 100  format(' Bishop-Hill stress (crystal system):')
101   format(3d20.7)
      !
      !Transform stress from local frame (S33) to sample frame (solution%stress_sam) 
      solution%stress_sam = rotateSRTensorTo(S33,TRFb(1:3,1:3,laml))          
      !
      RHOS33 = Vec5ToSymMat33(RHOS)   
      RHOA33 = Vec3ToAntiSymMat33(RHOA(1:3)) * sqr2
      !
      !Transform relaxation strain rate tensor from local frame (RHOS33) 
      !                                         to sample frame (RHOSsa)
      RHOSsa = rotateSRTensorTo(RHOS33,TRFb(1:3,1:3,laml))
      !non-normalization
      solution%relaxationrate_sam = RHOSsa * MacroDefRate%vMeqStrainRate
      !
      !Transform relaxation spin tensor from local frame (RHOA33) to sample frame (RHOAsa)
      RHOAsa = rotateSRTensorTo(RHOA33,TRFb(1:3,1:3,laml))
      !non-normalization
      solution%relaxationspin_sam = RHOAsa * MacroDefRate%vMeqStrainRate
      !
      !Report RHOSsa and RHOAsa to LST-file 
 1701 format(/,' RHOSsa')
 1706 format(/,' RHOAsa')
      if(NLIST.eq.1) then
          write (IMP,1701)
          do i=1,3 
              write (IMP,101) (RHOSsa(i,j),j=1,3)
          end do 
          write (IMP,1706)
          do i=1,3
              write (IMP,101) (RHOAsa(i,j),j=1,3)
          end do          
      end if
!
      if (IPR.EQ.2 .AND. NLIST.eq.1) then 
        WR=0.0
        do i=1,5
            WR=WR+spanv(i)*BB(i+ii)
        end do             
        write (IMP,777) WR
      end if
  777 format (' spanv . BB          :',d10.4)
!     (Modification June 2001: note that if one of the grains does
!      not deform at all, the stress and the active slip systems
!       of the full constraintssolution are used.)
!
!      do i=1,DM_data%n_systems
!        j=i+jj
!        write (IMP,308) i,DTAU1(j),XX(j)
!      enddo
! 308  format ('PANCAK2  i,DTAU1, XX',i5,2d12.4)
      nactiv_sofar=0
      do 305 i=1,DM_data%n_systems
      j=i+jj
!     If one grain does not deform, then DTAU1 comes from the full
!     constraints solution.
      if (ABS(DTAU1(j)).gt.TOL) goto 305
      nactiv_sofar=nactiv_sofar+1
      if ( nactiv_sofar .le. Pancak2_max_activesystems) then
                           solution%indact(nactiv_sofar)=i
                        ELSE
#ifndef ALTAY_SUBROUTINE
                           if(NLIST.eq.1) then
                                     write (IMP,306)
                           end if
                           write (*,306)
                           call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'Pancak2','Too many active slip systems',RCM_RTN)
#endif                        
                        endif
 306  format (' PANCAK2 - 306 - TOO MANY ACTIVE SLIP SYSTEMS')
305   continue
      !
      solution%nactiv = nactiv_sofar
      !
      if (solution%nactiv.eq.0) then
#ifndef ALTAY_SUBROUTINE      
                           if(NLIST.eq.1) then
                                     write (IMP,307)
                           end if
                           write (*,307)
                           call terminate(stopcode_runtimeerror)
#else
      RCM_RAISE(1,'Pancak2','No active slip systems found',RCM_RTN)
#endif
                       endif
 307  format (' PANCAK2 - 307 - No active slip systems found')
      !
      do i=1,solution%nactiv
          j = solution%indact(i) + jj
          solution%sliplp(i) = XX(j)
          solution%taurlp(i) = TAUR1(j) 
      end do
      !
      RETURN
      END SUBROUTINE        

    
      
    !> Make a matrix assembly [[A 0] [0 A]]
    !>
    subroutine assemblyMatrix_A00A(A, B, info)
    implicit none
    double precision,dimension(:,:),intent(in)  :: A
    double precision,dimension(:,:),intent(out) :: B
    integer,intent(out)                         :: info
    !
    integer,dimension(2) :: dims
        ! check preconditions
        info = criErr_BadArgs
        dims = shape(A)
        if (all(2*dims == shape(B))) then
            ! Zeros:
            B(dims(1)+1:,:dims(2)) = 0.D0
            B(:dims(1),dims(2)+1:) = 0.D0
            ! Set A matrix in the upper-right and lower-left corners of B
            B(:dims(1),:dims(2)) = A
            B(dims(1)+1:,dims(2)+1:) = A
            info = criSuccess
        endif
    !
    end subroutine


      end module