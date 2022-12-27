#include "altayRCM.fpp"
module altaySimul
    use altay_definitions, only: dp
    use altayMiscutils, only: terminate, stopcode_runtimeerror
    use altayHardTypes
    use altayMacroKinematic
    use altayHardLaw_DSH
    use altayDSHState

      ! Initial rations of CRSS, set in MAINA1.
      ! It is used only by the stand-alone AlTay
      type(CRSS),save :: crss_ratiosIN

      contains

! ALAMEL V3
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!  All comments about modifications of the source have been removed
! for clarity
! See "annotated source codes" if you need these
!
!
      SUBROUTINE SIMUL(IW,NFILE0,MacroDefRate)

!     TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES
!     USING THE ALAMEL MODEL
      use altayCurAccess
      use altayDYNFIL
      use altayHard
      use altayTaylor
      use altayAlgorithms
      use altayConfig
      use altayRCM
      use altayIOConfig
      use altayMiscutils
      !
      implicit real(dp) (a-h,o-z)
      ! optional argument for IW=1 or 2:
      type(DeformationRate),intent(in),optional :: MacroDefRate !inout
!
!     IW=2 is meant for outputting the final texture.
!
      common /IGLIJS/ CC(2,96), M11
      common /DOUBLE/ XM(5,96),XEPS(5),RHO(5),B5(5)
      common /TEXTUR/ TRF(3,3),C1(3,3),C2(3,3),NO,                       &
                      ITW,GEWF
      common /SYMP/ TEN(3,3),TOTGEW,INV,ISP,LOM,KSYM,KTYP
      common /EULERA/ fi1,PHI,fi2
      common /GENRLX/ YY(5,5),SHsam(3,3),Ssam(3,3),RHOSsa(3,3),          &
                      SWRLX(3)

      real(dp) :: fi10b(2),phi0b(2),fi20b(2),gewfb(2),Fb(3,3,2),GAXESb(3,2),GEULRb(3,2), &
                  CIJb(3,3,2),TGb(3,3,2),RHOSSb(3,3,2),fi1b(2),phib(2),fi2b(2)
      common /LAMEL/ TRFb(3,3,2),GMMAb(2),ENTA,NGR,NRL,ITFMAS, laml                                 ! NRL= number of relaxations, NGR= number of grains
      common /CEIGEN/ IOR,ISTP,NBLOC
      common /PE/ Fmicro !Temporary!!!
      dimension GAXES(3),                                  &    ! half axes a,b,c, of the grain shape ellipsoid
           GEULR(3),TG(3,3),                               &
           CIJ(3,3),STOT(3,3),                             &
           RHOST(3,3),RHOSm(3,3),FMicro(3,3)
      dimension FS(3,3)
      character(len=40) :: TITEL
      logical SWRLX
      integer :: NPOINT
      integer :: info
      type(DeformationState) :: MacroDefState
      ! HGAM: homogenized slip per step
      ! HGAMCALL: homogenized slip per call
      ! HGAMTOT: homogenized slip accumulated over calls
      real(dp) :: HGAM=0.D0,HGAMCALL=0.D0,HGAMTOT=0.D0
      ! Macroscopically imposed vM equivalent strain per call.
      real(dp) :: MEPSCALL=0.D0
      real(dp) :: GMMdot !Total slip rate in current grain
      real(dp) :: Mgrain !Taylor factor of the current grain
      real(dp) :: Mavg   !Volume-averaged Taylor factor
      real(dp) :: srh !Strain Rate Heterogeneity in polycrystal
      real(dp) :: SeqGrain=0.D0 ! Equivalent stress in crystal, defined as..
                                    !  plastic work rate in crystal normalized by..
                                    !  (macro) von Mises equivalent strain rate
      real(dp) :: WorkRate ! Rate of plastic work per unit
                                   ! volume in the crystal
      real(dp) :: Wtot ! Total plastic work per unit volume in crystal
#ifdef PEBP_ENABLED
      type(StateDerivedVars) :: pebpSDV, pebpSDVavg
#endif
      integer :: seedsize
      integer, allocatable :: seed(:)

      real(dp), parameter :: convf=0.5729577951308232e+02_dp
      data FS/9*1.0D0/
      save
      !
      NPOINT = size(DFIL)
      !
      ! Top branches depending on first argument IW of SIMUL
      if (IW) 32,33,30         ! 32: return if IW<0; 33: IW=0; 30: continue if IW>0
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !-------- IW=0 --------
      ! initialization call
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
  33  if(.not. allocated(seed)) then
        call random_seed(size=seedsize)
        allocate(seed(seedsize),source=20191102) ! low entropy, but at least deterministic
        call random_seed(put=seed)
      endif
      NGR    = acnf%simul_init%NGR
      ENTA   = acnf%simul_init%ENTA
      KOST   = acnf%hardening%HardLawID
      !
      NLIST  = acnf%output_config%NLIST   ! control "output listing"
      NFILE1 = acnf%output_config%NFILE   ! control "CUR"
      NFILTW = acnf%output_config%NFILTW  ! control "TWN"
      IPR    = acnf%output_config%IPR     ! control printing level
      NRES   = acnf%output_config%NRES    ! control "RES" and "RPT"
      NPEBP  = acnf%output_config%NPEBP   ! control "BEP"
      NMSS   = acnf%output_config%NMSS    ! control "MSS"
      HGAMTOT=0.D0
      ! NGR == 3: enable MAS-AL
      if(NGR.eq.3) then
            ITFMAS=1
            NGR=2
      else
            ITFMAS=0
      endif
      !
      if (NGR.lt.1.or.NGR.gt.2) then
            RCM_RAISE(1,'SIMUL','Incorrect value of NGR',RCM_RTN)
      endif
 140  format (' NGR can only take the values 1 or 2 but was',I5)
      ! Check if number of crystals is right for the model
      if (modulo(NPOINT, NGR) /= 0) then
            RCM_RAISE(1,'SIMUL','The number of grains must be an even number',RCM_RTN)
      endif
!     Number of relaxations: 0 for Taylor and 2 for ALAMEL:
      NRL=(NGR-1)*2
      !
      FMicro = acnf%simul_init%FMicro
      !
      TITEL  = acnf%jobtitle
      if(NLIST.eq.1) write(IMP,97) TITEL
      if (NRES.gt.0) write (IMP2,98) TITEL
  97  format (' Title of the new simulation: ',A)
      ! Only if CUR file is requested
      if (NFILE1.eq.1) call CURwriteTitle(IMP1,TITEL,info)
  98  format (A)
!     read the parameters of the work hardening model
      call TAYLOR(1,KOST) ! read slip system file
      RCM_GUARD
      !
      ! Various IO/initialization calls
      !
      return

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !-------- IW>0 --------
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
  30  continue
      ! Per-call selection of the model: NGR & NRL must be set
      NGR = acnf%simul_init%NGR
      if(NGR.eq.3) then
            ITFMAS=1
            NGR=2
      else
            ITFMAS=0
      endif
      ! Number of relaxations: 0 for Taylor and 2 for ALAMEL:
      NRL=(NGR-1)*2
  36  NFILE=NFILE0*NFILE1
      NPEBPx=NFILE0*NPEBP   ! control "BEP" (effective value)
      NMSSx= NFILE0*NMSS    ! control "MSS" (effective value)

      NSTP     = astate%simulCalls(astate%this)%input%nsteps
      swrlx(1) = astate%simulCalls(astate%this)%input%rlx1
      swrlx(2) = astate%simulCalls(astate%this)%input%rlx2
      swrlx(3) =.false.
      call TAYLOR(2,KOST,MacroDefRate)
      RCM_GUARD
      ! Output the current texture
      if (NFILE.eq.1) call CURwriteBlock(IMP1,info)
#if !defined(INTERMEDIATEBPM_DISABLED)
      select case(KOST)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
          if (NPEBPx.eq.1) info = KS_writeState(IMP4)
      endselect
#endif
      HGAMCALL = 0.D0
      MEPSCALL = 0.D0
!
!     Main Loop over the Steps
!
      steploop: DO 8 ISTP=1,NSTP
          !
          TOTGEW=0.0
          STOT = 0.D0
          RHOST = 0.D0
          SeqAvg=0.
          Mavg=0.
          srh=0.
          HGAM=0.D0
          pebpSDVavg = StateDerivedVars()

          call dynfil2(nrstep,MacroDefState%TotalDefGrad,GAXES,GEULR,CIJ,TG)
#ifndef NO_STDOUT
          write (*,96) ISTP,GAXES
#endif
          if(NLIST.eq.1) then
              write (IMP,96) ISTP,GAXES
          end if
      96  format(' Step nr.',i5,5X,3f12.5)
          if (IW.gt.1) goto 70
          if(NLIST.eq.1) then
              write (IMP,3456) MacroDefRate%VelGrad
          end if
 3456     format ('DG=',3(T10,3d12.3,/))
!
!     Get the 5x5 transformation matrix MACRO to morfol. GRAIN AXES
!
          if(NLIST.eq.1) then
              write (IMP,3458) TG
          end if

 3458     format (' TG=',3(T10,3d12.3,/))
  70      if (nfile.eq.0.or.ISTP.gt.1) goto 44
          if (NLIST.EQ.1) write (IMP,112) ISTP
 112      format (//' DEFORMATION STEP ',I5,//)
          if (NRES.gt.0) write (IMP2,404) nrstep+1,NPOINT
 404      format (' Def. Step ',i5,'  Number of orientations',i5,/,T3,'ior'  &
           ,T7,'EquivStress',T23,'WorkRate',T37,'tau_ref',T56,'M',T64,       &
           'ratlon',T109,'RHO-SYMMETRIC',T172,'RHO-ROTATIONAL',T239,'STRESS' &
           ,/,1x,278('*'))
          !
  44      continue
          if (NLIST.eq.1) then
              do i=1,3
                 write (IMP,407) (MacroDefState%TotalDefGrad(j,i),j=1,3)
              enddo
          end if
 407      format (' F ',3d15.7)
          !
          nrstep=nrstep+1
          !
          call Update_DeformationState(MacroDefRate,MacroDefState,info)
          !
          call UPDATC(CIJ,MacroDefState%IncrDefGrad_inverse)
          call GETANG(CIJ,GAXES,GEULR,TG)
          RCM_GUARD
          ! We can choose not to update the texture data
          if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                call DYNFIL3(nrstep,MacroDefState%TotalDefGrad,GAXES,GEULR,  &
                             CIJ,TG)
          endif
!
!         Added for lamel model:
!         Organisation reading temporary texture file,
!         in such way that the program TAYLOR can process the crystals
!         by sets of 2.
!         Taylor must therefore have "advance knowledge" of the
!         orientation to come at the moment that it starts such
!         computation.
!         See also the comment before the calling of subroutine TAYLOR.
!
  10      laml=1
          laml1=NGR
          ifil4=0
          if (NFILTW.eq.1) write (IMP3,399)
 399      format(1x)
          !
          ! Begin the loop over grains/clusters
          !
          clusterloop: DO 23 IOR=1,NPOINT
              Mgrain=0.0
              GMMdot=0.0
              WorkRate = 0.D0
              SeqGrain = 0.D0
              Wtot = 0.0
              !
 2626         do L=laml,laml1
                  if (ifil4.eq.NPOINT) exit
                  ifil4=ifil4+1
                  call DYNFIL4(ifil4,fi10b(L),PHI0b(L),fi20b(L),                     &
                   TRFb(1,1,L),GEWFb(L),GMMAb(L),Fb(1,1,L),GAXESb(1,L),              &
                   GEULRb(1,L),CIJb(1,1,L),TGb(1,1,L),RHOSSb(1,1,L))
            !
                  fi1b(L)=fi10b(L)*convf
                  PHIb(L)=PHI0b(L)*convf
                  fi2b(L)=fi20b(L)*convf
              end do
              laml1=laml1+1
              if (laml1 > NGR) laml1=1
              laml=laml1
              GMM0=GMMAb(laml)
              call getTau(GMM0,TAU,info)
              fi1=fi1b(laml)
              PHI=PHIb(laml)
              fi2=fi2b(laml)
              !
              TRF = TRFb(:,:,laml)
              TG = TGb(:,:,laml)
              RHOSSa = RHOSSb(:,:,laml)
              if(laml.eq.1) then
                  qgx=GEWFb(laml)
                  GEWF=qgx
              else
                  GEWF=qgx
              end if
              if (NFILE.eq.0.or.ISTP.gt.1) goto 999
!
!     In case of NGR=2:
!        LAML=1: TAYLOR
!                - has the present and the next orientation available
!                - must perform the computation of a set of 2 crystals
!                - has to output the result for the first crystal
!        LAML=2: TAYLOR
!                - should not perform any computation
!                - has to output the result of the second crystal found
!                  during the previous computation.
 999          if (IW.le.1) then
                    call  TAYLOR(3,KOST,MacroDefRate,MacroDefState)
                    RCM_GUARD
              endif
! this modification is to suit for the output of stress
              if(laml.eq.1) then
                  ssqgx=GEWF
              else
                  GEWF=ssqgx
              end if
              TOTGEW=TOTGEW+GEWF
              !
              ! Skip the rest of the loop if IW > 1
  41          if (IW.gt.1) cycle
              !
              if (astate%simulCalls(astate%this)%input%full_model) then
                    call TAYLR1(ISTP,IOR,NRES,TAU,GMMdot,SeqGrain,WorkRate,      &
                                MacroDefRate)
                    RCM_GUARD
              endif
   49         if (NFILTW.eq.1) write (IMP3,398) ITW
 398          format (I3)
              !
              STOT = STOT + Ssam*GEWF
              RHOST = RHOST + RHOSsa*GEWF
              !
  63          SeqAvg = SeqAvg + SeqGrain*GEWF
              Mgrain = GMMdot /  MacroDefRate%vMeqStrainRate
              Mavg = Mavg + Mgrain*GEWF
              ! norm2(RHOSsa)=||RHOSsa||=(||d-D||)/MacroDefRate%vMeqStrainRate
              srh = srh + norm2(RHOSsa)*GEWF
              HGAM = HGAM + GMMdot*GEWF !Step time here implicitly assumed to be 1.0s
              GMM1 = GMM0 + GMMdot !Step time here implicitly assumed to be 1.0s
              Wtot = Wtot + WorkRate !Step time here implicitly assumed to be 1.0s
              select case(KOST)
              case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
                   call KS_getSDV(IOR,pebpSDV,info)
                   pebpSDVavg = pebpSDVavg + pebpSDV * GEWF
              endselect
              ! We can choose not to update the texture state
              if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
                    call DYNFIL5(IOR,fi1,PHI,fi2,C2,GEWF,GMM1,                   &
                                 MacroDefState%TotalDefGrad,GAXES,GEULR,CIJ,TG,  &
                                 RHOSsa)
              endif
              !
              ! End of the loop over crystals
              !
  23      enddo clusterloop
          !
          ! Finish processing if IW > 1
          if (IW.gt.1) exit
          !
          SHsam = STOT / TOTGEW
          RHOSm = RHOST / TOTGEW
          !
  66      do i=1,2
              do j=i+1,3
                  SHsam(i,j)=SHsam(i,j)*FS(i,j)
                  SHsam(j,i)=SHsam(i,j)
                  RHOSm(i,j)=RHOSm(i,j)*FS(i,j)
                  RHOSm(j,i)=RHOSm(i,j)
              end do
          end do
          Mavg=Mavg/TOTGEW
          ! DEFINITION: srh = (||d-D||) / ||D||
          srh=sqrt(2.D0/3.D0)*srh/TOTGEW
          SeqAvg=SeqAvg/TOTGEW
          !
          MEPSCALL= MacroDefState%IncrvMeqStrain * (ISTP-1)
          !
          if (NMSSx /= 0) then
                call writeMSSRecord(IMP5,MEPSCALL,                           &
                              MacroDefState%AccumvMeqStrain_ToStartOfInc,    &
                              HGAMCALL,HGAMTOT,SHsam,Mavg,srh,info)
          endif
          select case(KOST)
          case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
               pebpSDVavg = pebpSDVavg * (1.D0/TOTGEW)
          endselect
          ! Get the homogenized quantities:
          associate (callout => astate%simulCalls(astate%this)%output)
                callout%stress_tensor= SHsam
                callout%taylor_factor= Mavg
                callout%strain_rate_heterogeneity = srh
                callout%equivalent_stress= SeqAvg
                callout%effective_stress = sqrt(3.D0/2.D0)*norm2(SHsam)
                callout%homogenised_slip = HGAMCALL
                callout%homogenised_slip_tot = HGAMTOT
                callout%effective_macro_strain = MEPSCALL
                callout%effective_macro_strain_tot =                         &
                    MacroDefState%AccumvMeqStrain_ToStartOfInc
                callout%effective_macro_strain_tot_end =                     &
                    MacroDefState%AccumvMeqStrain_ToEndOfInc
          end associate
          !
          HGAM = HGAM / TOTGEW
          HGAMCALL = HGAMCALL + HGAM
          ! We can choose not to update the internal state
          if (.not.astate%simulCalls(astate%this)%input%keep_state) then
                HGAMTOT = HGAMTOT + HGAM
          endif
          if(NLIST.eq.1) then
          write (IMP,105) ISTP,SeqAvg,Mavg,MacroDefState%IncrvMeqStrain
          end if
 105      format (' FOR STEP',I5,'  AVERAGE STRESS=',F15.5,'   AVERAGE M-VALUE=',F10.5, &
              '  EFF. STRAIN EPS USED=',F10.5)
          !
          ! End of the loop over steps
          !
   8  enddo steploop
      !
  22  return
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !-------- IW<0 --------
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
  32  return
      !
      END SUBROUTINE

      end module
