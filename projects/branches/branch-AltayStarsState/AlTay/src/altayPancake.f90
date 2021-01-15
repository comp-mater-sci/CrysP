!
! $Id$
!
#include "criMacros.fpp"

#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
    module altayPancake
    use altayMiscutils, only: terminate, stopcode_runtimeerror
    use criMathUtils
    use altayHardConstants
    use altayCRSSTypes
    use altayDeformationMechanism
    use altayDeformationMechanismConstants, only: DM_dev_dims
    use altaySDVTypes
    use altayMacroKinematic, only: DeformationRate

    integer, parameter, public ::          Pancak2_max_grains = 2
    double precision, parameter, public :: Pancak2_tolerance = 5.0d-6    ! total shear rate theshold below which grain is considered non-deforming



    contains


    Subroutine LinProg_solver(this, cluster_state, grain_states, MacroDefRate, info)
    implicit none
    type(GrainClusterSolution), intent(inout)    :: this
    type(ClusterState), intent(in)               :: cluster_state
    type(GrainStateCollection), intent(in)       :: grain_states
    type(DeformationRate), intent(in)            :: MacroDefRate
    integer, intent(out)                         :: info    
    !
    integer :: i
    integer :: n_grains
    integer :: n_interfaces
    double precision, dimension(3,3)                 :: T_cluster ! grain boundary orientation matrix in macro frame (3rd column is grain boundary normal)
    type(EulerAngles), dimension(Pancak2_max_grains) :: grain_euler
    type(CRSSdata), dimension(Pancak2_max_grains)    :: grain_CRSS
    type(DeformationMechanismData),dimension(Pancak2_max_grains) :: grain_DM_data
        !
        !
        ALLOCATED_SIZE(n_grains,cluster_state%idx)
        !
        !Set T_cluster
        select case (n_grains)
        case (1) !Taylor
            T_cluster = 0.D0 ! Irrelevant
        case (2) !Alamel
            ALLOCATED_SIZE(n_interfaces,cluster_state%interfaces)
            RETURN_IF_WITH(n_interfaces /= 1, info = criErr_BadDims)
            i = lbound(cluster_state%interfaces(:),dim=1)
            T_cluster = cluster_state%interfaces(i)%ptr%trafo%matrix
        case default
            info = criErr_BadDims
            return
        end select
        !
        !Initializations
        do i = 1,n_grains
            associate (grain => grain_states%grains(cluster_state%idx(i)))
                !initialize grain_euler
                grain_euler(i) = grain%orientation%euler
                !initialize grain_CRSS
                grain_CRSS(i) = this%components(i)%crss
                !initialize grain_DM_data
                grain_DM_data(i) = grain%phase%deformationmechanism
            end associate
        end do
        !
        do i = 1,n_grains
            !
            call Pancak2(i, n_grains, &
                T_cluster, &
                grain_euler, grain_CRSS, grain_DM_data,   &
                MacroDefRate, &
                this%components(i)%solution)
        end do
        !
        info = criSuccess    
       
    end subroutine
    
    
! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!
      Subroutine Pancak2(laml, ngr, Tprinc, eulerb, CRSSb, DM_datab,  &
                         MacroDefRate, solution)
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
      integer,intent(in)                            :: laml  ! grain index inside cluster
      integer,intent(in)                            :: ngr   ! number of grains in cluster
      !> transformation matrix associated to the cluster grain boundary (3rd column is boundary normal vector)
      double precision,dimension(3,3),intent(in)    :: Tprinc
      type (CRSSData),dimension(Pancak2_max_grains),intent(in)       :: CRSSb
      type(EulerAngles),dimension(Pancak2_max_grains),intent(in)     :: eulerb      
      type(DeformationMechanismData),dimension(Pancak2_max_grains), intent(in)    :: DM_datab
      type(DeformationRate),intent(in)              :: MacroDefRate
      type(LinearProgrammingSDV),intent(out)        :: solution
      !
      !> Number of active systems founds so far by the search algorithm
      integer nactiv_sofar
      !> Local stress (TBH output), in crystal reference system
      double precision, dimension(3,3):: S33=0.0d0
      !> Symmetric and anti-symmetric parts of normalized relaxation tensor, in crystal reference system
      double precision, dimension(3,3):: RHOS33, RHOA33
      !> Symmetric and anti-symmetric parts of normalized relaxation tensor, in sample reference system
      double precision, dimension(3,3):: RHOSsa, RHOAsa
      double precision,dimension(5):: RHOS,                             & ! full relaxation strain rate
                                        RHOA                              ! full relaxation spin rate
      double precision,dimension(194) :: XX                               ! shear rates [grain 1, grain 2, relaxations]
      double precision,dimension(3,3,Pancak2_max_grains)  :: TRFb         ! rotation matrices representing crystal orientations
      dimension C2(3,3),                                                & ! macro velocity gradient in crystal frame
       TDCb(3,3,Pancak2_max_grains),TRCb(3,3,Pancak2_max_grains),       & ! symmetric and anti-symmetric parts of macro velocity gradient in crystal grame
       relax(3,3,3),                                                    & ! 3 basic relaxations in 3x3 notation, 3rd index is relaxtion number
       DACC(10),                                                        &
       rls(3,3,3,2),rla(3,3),                                           & ! symmetric and anti-sym. parts of relaxation tensor in crystal frame
       rlm(3,3,3),                                                      & ! unit relaxation tensor (3x3) in macro frame, 3rd index is for 1st 2nd and 3rd relaxation mode
       C3(3,3),                                                         & ! relaxation mode in crystal frame
       TRP(10),APRIME(10),                                              &
       B3(10,3),                                                        & ! spin rate of relaxation tensor for 3 relaxations
       PLUMIN(2,3),                                                     & ! first index = nr. of grain, second index = nr. of relaxation
       CUst(10),                                                        & 
       B5(5),                                                           & ! container for 5-component representations of symm. 3x3 matrices
       UU(10,10)                                                          ! inverse of A2 without relaxations
      dimension spanv(5),                                               & ! stress output from TBH for single grain
                STRSS(10),                                              & ! stress output from TBH for both grains
                BB(10)                                                  & ! 5-component representations of macro strain rates in crystal frames for all grains in cluster (=right-hand side of Taylor equation)
      dimension CCC(Pancak2_max_grains,194),                            & ! array of CRSSs for linear programming: 1st row: positive direction; 2nd row: negative direction
                DTAU(194),DTAU1(194),                                   & ! DTAU... abs(TAUR) - CRSS
                TAUR(194),TAUR1(194)                                      ! TAUR... RSS (resolved shear stress)
      logical bas(194),VALID(194)
      dimension B8(5,Pancak2_max_grains),                               & ! imposed strain rate (5-dim strain space) for each grain
                UBUF(10),UU2(10,10),UU3(10,10),                         & ! containers for TBH out/input: stress (UBUF), inverse of A2 without relaxations (UU2,UU3)
                DD(10)
      integer DI(10),                                                   & ! initial set of independent deformation systems
                DI2(10)                                                   ! final set of independent deformation systems
      dimension GAMR(Pancak2_max_grains),TAURL(Pancak2_max_grains)        ! relaxed shears;  To do: Should the length not be NRL instead of Pancak2_max_grains?
      double precision, dimension(10,194) :: A2                           ! left-hand side coefficient matrix of Taylor equation
      data SQR2/0.7071067811865476D+00/,B3/30*0.0D0/
      !> Definition of the two relaxations, representing a
      !> 13-simple shear and a 23-simple shear, respectively. 3rd relaxation currently set to 0:
      !> \remark Other relaxations might be considered in the future, e.g.
      !> Manik and Holmedal, Materials Science&Engineering A580(2013)349–354
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
      data NDIM/10/ ! NDIM=dimension A 
      !> Number of relaxations (0 for Taylor and 2 for ALAMEL) 
      integer :: NRL
      data TAURL/2*0.0d0/ ! CRSS for relaxations
      data GETAL/1.0D6/,                    & ! ratio of CRSS in anti-twin direction to CRSS in twin direction, also used to suppress relaxations
            TOL/1.0d-6/                       ! tolerance on CRSS: if (abs(RSS)-CRSS) > TOL deformation system considered inactive 
      integer :: info
      SAVE

      if (laml.ne.1.and.laml.ne.2) then ! only 1 or 2 grains can be handled
#ifndef ALTAY_SUBROUTINE
          write(*,*) 'laml=', laml
          call terminate(stopcode_runtimeerror)
#else
          RCM_RAISE(1,'Pancak2','Wrong selection of lamels',RCM_RTN)
#endif      
      endif
      !
      TWOSQ3=sqrt(2.D0/3.D0)
      !
      N=5*NGR           ! N is number of rows of A2
      NU=N              ! NU number of rows of UU2
      select case (ngr) ! determine total number of deformation systems
      case (1)
          M2 = DM_datab(1)%n_systems
      case (2)
          M2 = DM_datab(1)%n_systems + DM_datab(2)%n_systems
      end select
      NRL=(NGR-1)*2 ! 2 relaxations per grain boundary
      M12 = M2 + NRL ! determine total number of def. systems + relaxations
      !      
      !============================================================
      !     Solve (relaxed) Taylor equation
      !============================================================     
      !     
      if (laml.eq.2) goto 3 ! call solver only for first grain in cluster (this call already solves the entire cluster, all other calls only operate on the output)
          ! Set up the system of equations for the linear programming
          ! (see Van Houtte, Textures and Microstructures, 1988, 8 & 9, pp. 313-350, section 5):
          !
          ! A2 = | A1_g1   0       RLX+ |
          !      | 0       A1_g2   RLX- |
          ! CCC  = | CRSS_g1 CRSS_g2  CRSS_RLX |
          ! shape(A2,dim=2) == shape(CCC,dim=2)
          !
          ! Construct A2
          A2 = 0.0d0
          A2(1:5,1:DM_datab(1)%n_systems) = DM_datab(1)%A1
          if (ngr == 2) A2(6:10,1+DM_datab(1)%n_systems:M2) = DM_datab(2)%A1
          ! initialize relaxation components of CCC (shape: [Pancak2_max_grains,..])
          do i=M2+1,M12
              do jsgn=1,Pancak2_max_grains
                  CCC(jsgn,i)=0.0
              end do
          end do
          ! initialize UU (shape: [NU,NU])
          do i=1,NU
              do j=1,NU
                  UU(j,i)=0.0
              end do
          end do
          ! initialize initial set of independent deformation systems
          do I=1,5
              DI(I)=DM_datab(1)%set0(I)
              if (ngr == 2) DI(I+5)=DM_datab(2)%set0(I)+DM_datab(1)%n_systems
          end do    
          !
          do IL=1,NGR
              L1=5*(IL-1)
              TRFb(:,:,IL) = rotmat(eulerb(IL))
              C2 = rotateSRTensorFrom(MacroDefRate%VelGrad,TRFb(:,:,IL)) ! transform macro velocity gradient into crystal frame
              if (NRL.eq.0) goto 87 ! do only if relaxations exist
              do IRL=1,NRL
                  ! Transform relaxation from grain boundary frame to macroscopic frame
                  RLM(:,:,IRL) = rotateSRTensorTo(RELAX(:,:,IRL),Tprinc)
                  ! ... and now to crystal frame:
                  C3 = rotateSRTensorFrom(RLM(:,:,IRL),TRFb(:,:,IL))
                  do j=1,3
                      do i=1,3
                          RLS(i,j,IRL,IL)=(C3(I,J)+C3(J,I))*0.5D0 ! symmetric part of relaxation tensor
                          RLA(i,j)=(C3(I,J)-C3(J,I))*0.5D0  ! anti-symmetric part of relaxation tensor
                      end do
                  end do
                  B3(L1+1:L1+3,IRL) = PLUMIN(IL,IRL) * AntiSymMat33ToVec3(RLA) / sqr2 
                  B5= SymMat33ToVec5(RLS(1:3,1:3,IRL,IL)) ! sym.(3,3) -> (5)
                  ! Insert the relaxations as columns in A2-matrix
                  j=M2+IRL
                  do i=1,5
                      i1=i+L1
                      x=B5(i)*PLUMIN(IL,IRL)
                      A2(i1,j)=x
                  end do
              end do 
          87  continue
              do I=1,3
                  do J=1,3                                                       
                      TDCb(I,J,IL)=(C2(I,J)+C2(J,I))*0.5D0 ! symmetric part of macro velocity gradient (strain rate) in crystal grame
                      TRCb(I,J,IL)=(C2(I,J)-C2(J,I))*0.5D0 ! anti-symmetric part of macro velocity gradient in crystal grame
                  end do
              end do                                                          
              !
              ! build right-hand side BB of Taylor equation from strain rate, BB has shape [NGR*5,1]
              B5= SymMat33ToVec5(TDCb(1:3,1:3,IL)) ! sym.(3,3) -> (5)
              do i=1,5
                  j=i+L1 ! same strain rate imposed on each grain
                  BB(j)=B5(i)
              end do
              !
              ! Calculation of time increment by dividing von Mises equivalent
              ! strain by von Mises equivalent strain rate
              do j=1,5
                  B5(j)=B5(j)/MacroDefRate%vMeqStrainRate
                  B8(j,IL)=B5(j)
              end do
              !> \fixme Generalize CCC for multi-phase input (grain 1 and 2 may belong
              !>        to different phases)
              K1=DM_datab(1)%n_systems*(IL-1)
              !
              ! Assign crss_cluster to proper section of CCC
              CCC(:,1+K1:DM_datab(IL)%n_systems+K1)=CRSSb(IL)%crss(:,1:DM_datab(IL)%n_systems)  
              !
              ! Set CCC for antitwinning direction equal to
              ! GETAL times CCC for twinning direction       
              do I=DM_datab(IL)%n_slip_systems+1,DM_datab(IL)%n_systems ! this do-loop will only be executed for DM_data%n_twinning_systems > 0
                  CCC(2,I+K1)=CCC(1,I+K1)*GETAL !notePE20150428
              end do
              !
              ! write (IMP,914) i,j,CCC(1,j),CCC(2,j)
         914  format (' i,j',2i5, ' CCC ',2d16.4)
              do J=1,5
                  do I=1,5
                      UU(I+L1,J+L1)=DM_datab(IL)%B(I,J)
                  end do
              end do
          end do 
          !
          ! Conversion of strain to normalized strain rate
          do I=1,N 
              BB(I)=BB(I)/MacroDefRate%vMeqStrainRate
          end do
          !
          if (NRL.eq.0) goto 88 ! do only if relaxations exist
          do j=M2+1,M12             
          ! The CRSS of the relaxations is set to a very large number
          ! in order to suppress the relaxations in a first call of the TBH program     
              CCC(1,j)=GETAL  
              CCC(2,j)=GETAL 
          end do
          !
          ! Full constraints calculation
          ! Carry out the Simplex subroutine
      88  if (IPR.EQ.2) then
              if(NLIST.eq.1) then 
                  write (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
              end if
          end if
     218  format(/' COST FUNCTION',/,(2x,12F10.4))
          if (IPR.EQ.2) then
              if (NLIST.eq.1) then
                  write (IMP,219) (BB(I),I=1,N)
              end if
          end if
     219  format (' right hand side',/,(2x,10F10.4),/)
          ! First call of Simplex (full constraints)
          Call TBH(NDIM,N,M2,A2,BB,                                      &
           CCC,UU,UU2,DI,DI2,Dacc,XX,UBUF,FakM,                          &
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
  220     DTAU1=DTAU 
          TAUR1=TAUR  
          !
          if (NRL.eq.0) then ! if no relaxations
              UU=UU2
              DI=DI2
              STRSS=UBUF
              goto 89
          endif
          !
          do IRL=1,NRL ! set CRSS for relaxations to finite value to allow them to become active
            j=M2+IRL  
            CCC(1,j)=TAURL(IRL)
            CCC(2,j)=TAURL(IRL)
          end do 
          !
          if (IPR.EQ.2) then
              if(NLIST.eq.1) then 
                  write (IMP,218) ((CCC(J,I),I=1,M12),J=1,2)
              end if
          end if
          ! Second call of Simplex (relaxed constraints)
          if (IPR.eq.2) then
              if (NLIST.eq.1) then
                  write (IMP,401)
              end if
          end if
     401  format (' Second call of TBH')
          Call TBH(N,N,M12,A2,BB,                                            &
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
          !
          ! GAMR will contain the relaxed shears:
     204  if (NRL.gt.0) then
               do IRL=1,NRL
                     gamr(IRL)=XX(M2+IRL)
               enddo
          endif
          ! Check whether 1 grain does not deform at all. Calculate total shear rate for each grain.
      89  j=0
          do 40 IG=1,NGR
              XXTOT=0.0
              do i=1,DM_datab(IG)%n_systems
                   j=j+1  
                   XXTOT=XXTOT+ABS(xx(j))
              enddo
              if (XXTOT.lt.Pancak2_tolerance) goto 213
       40 continue
          ! If all grains have a non-zero slip, do the following:
      99  DTAU1=DTAU
          TAUR1=TAUR
          UBUF=STRSS
     213  if(NLIST.eq.1) then 
              write (IMP,780) gamr
          end if
     780  format (' RELAXATIONS:                   ',2d12.4)      
       2  continue
      !
      !==================================================================
      !     Preparing output
      !     From here on, output is produced for grain number "laml"
      !==================================================================
      !
   3  continue
      !
      ii=5*(laml-1)
      do i=1,5
          ! If one grain does not deform, note that stress UBUF has come
          ! from the fullconstraints solution.
          spanv(i)=UBUF(i+ii)
          B5(i)=B8(i,laml) ! purpose?? B5 already contains B8 content
!         write (IMP,776) laml,B5(i),spanv(i),i+ii
!    776  format (' B5  ',i5,e15.8,   'spanv  ',d15.8,' i+ii',i5)
          x8=0.0
          y8=0.0
          if (NRL.gt.0) then
             do IRL=1,NRL
               x8=x8+A2(i+ii,M2+IRL)*gamr(IRL) ! full relaxation shear rate, component i
               y8=y8+B3(i+ii,IRL)*gamr(IRL)    ! full relaxation spin rate of, component i
             enddo
          endif
          solution%localstrainrate(i)=B8(i,laml)-x8 ! local strain rate: imposed macro strain rate - relaxation, component i
          RHOS(i)=-x8
          RHOA(i)=-y8
      end do
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
              WR=WR+spanv(i)*BB(i+ii) ! work rate for grain laml, component i
          end do             
          write (IMP,777) WR
      end if
  777 format (' spanv . BB          :',d10.4)
      ! (Modification June 2001: note that if one of the grains does
      !  not deform at all, the stress and the active slip systems
      !  of the full constraintssolution are used.)
      !
      ! do i=1,DM_data%n_systems
      !    j=i+jj
      !    write (IMP,308) i,DTAU1(j),XX(j)
      ! enddo
! 308  format ('PANCAK2  i,DTAU1, XX',i5,2d12.4)
      nactiv_sofar=0
      jj=DM_datab(1)%n_systems*(laml-1)
      do 305 i=1,DM_datab(laml)%n_systems
          j=i+jj
          ! If one grain does not deform, then DTAU1 comes from the full
          ! constraints solution.
          if (ABS(DTAU1(j)).gt.TOL) goto 305 ! do only if DTAU1 <= TOL: then system considered active
          nactiv_sofar=nactiv_sofar+1
          if ( nactiv_sofar .le. Pancak2_max_activesystems) then
               solution%indact(nactiv_sofar)=i
          else
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
      return
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