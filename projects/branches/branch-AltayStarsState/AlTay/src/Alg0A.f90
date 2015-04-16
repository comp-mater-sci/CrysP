#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altaySimul
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayHardTypes
      use altayMacroKinematic
      use altayState
      use altayMaterial
      use altayMesostructure

      contains
    
    
! ALAMEL V3
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY
!  All comments about modifications of the source have been removed
! for clarity 
! See "annotated source codes" if you need these
!
!
      SUBROUTINE SIMUL(config,state,material,IW,NFILE0,MacroDefRate)

!     TO ORGANIZE SIMULATIONS OF DEFORMATION TEXTURES
!     USING THE ALAMEL MODEL
      use altayCurAccess
      use altayHard
      use altayTaylor
      use altayAlgorithms
#ifdef PEBP_ENABLED
      use AltayDSHstate
      use altayHardLaw_DSH
#endif
#ifdef ALTAY_SUBROUTINE
      use altayConfig
      use altayRCM
#endif
      use altayIOConfig
      use altayMiscutils
      !
      use altayDynfilStitch
      !
      implicit double precision (a-h,o-z)
      type(altayConfigData),intent(in)          :: config
      type(altayStateData),target,intent(inout) :: state
      type(altayMaterialData),intent(in)        :: material
      integer,intent(in)                        :: IW
      integer,intent(in)                        :: NFILE0
      ! optional argument for IW=1 or 2:
      type(DeformationRate),intent(in),optional :: MacroDefRate !inout
!
!     IW=2 is meant for outputting the final texture.
!
      COMMON /TEXTUR/ TRF(3,3),C2(3,3)
      COMMON /SIMUL_TAYLOR/ SHsam(3,3), SWRLX(3), IOR,laml,ngr,nrl,TRFb(3,3,2),GMMAb(2)
      COMMON /EULERA/ fi1,PHI,fi2
      COMMON /GENRLX/ Ssam(3,3),relaxationrate_sam(3,3)
      dimension fi10b(2),phi0b(2),fi20b(2)
      dimension Fb(3,3,2)
      dimension fi1b(2),phib(2),fi2b(2)
      common /CEIGEN/ NBLOC
      DIMENSION                                &
       STOT(3,3),                                               &
       RHOST(3,3),RHOSm(3,3),gewfb(2)
      dimension FS(3,3)
      character(len=40) :: TITEL
      logical SWRLX
      integer :: NPOINT
      integer :: info
      !> sequence number of the cluster
      integer :: i_cluster
      double precision :: GEWF = 1.0D0
      type(DeformationState) :: MacroDefState
      ! HGAM: homogenized slip per step
      ! HGAMCALL: homogenized slip per call
      ! HGAMTOT: homogenized slip accumulated over calls
      double precision :: HGAM=0.D0,HGAMCALL=0.D0,HGAMTOT=0.D0
      ! Macroscopically imposed vM equivalent strain per call.
      double precision :: MEPSCALL=0.D0 
      double precision :: GMMdot !Total slip rate in current grain      
      double precision :: Mgrain !Taylor factor of the current grain
      double precision :: Mavg   !Volume-averaged Taylor factor
      double precision :: srh !Strain Rate Heterogeneity in polycrystal
      double precision :: SeqGrain=0.D0 ! Equivalent stress in crystal, defined as..
                                    !  plastic work rate in crystal normalized by..
                                    !  (macro) von Mises equivalent strain rate
      double precision :: WorkRate ! Rate of plastic work per unit 
                                   ! volume in the crystal
      double precision :: Wtot ! Total plastic work per unit volume in crystal
#ifdef PEBP_ENABLED
      type(StateDerivedVars) :: pebpSDV, pebpSDVavg
#endif
      !
      type(TextureAssembly) :: assembly
      !
      data convf/0.5729577951308232D+02/
      data FS/9*1.0D0/ 
      SAVE
      !
      NPOINT = altayStateData_size(state)
      assembly = TextureAssembly(state%old%texture, state%old%frame)
      !
      IF (IW) 32,33,30
  33  call  random_seed
      !
      NGR    = config%simul_init%NGR
      ! Number of relaxations: 0 for Taylor and 2 for ALAMEL: 
      NRL=(NGR-1)*2
      !
      KOST   = config%hardening%hardLawID
      ! Note:  integers NLIST,IPR,NRES,NPEBP,NMSS are module variables of altayIOConfig
      NLIST  = config%output_config%NLIST   ! control "listing"
      NFILE1 = config%output_config%NFILE   ! control "CUR"
      NFILTW = config%output_config%NFILTW  ! control "TWN"
      IPR    = config%output_config%IPR     ! control printing level
      NRES   = config%output_config%NRES    ! control "RES" and "RPT"
      NPEBP  = config%output_config%NPEBP   ! control "BEP"
      NMSS   = config%output_config%NMSS    ! control "MSS"
      !
      HGAMTOT=0.D0
      !
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) then
            WRITE (IMP,101) NGR,NLIST,NFILE1,NFILTW,KOST,IPR
      end if
#ifndef NO_STDOUT   
      WRITE (*,101) NGR,NLIST,NFILE1,NFILTW,KOST,IPR
#endif
 101  FORMAT (' SIMUL - PARAMETERS:',/                                   &
      'NGR=   ',I5,/,'NLIST= ',I5,/,'NFILE1=',I5,/,'NFILTW=',i5,/,       &
      'KOST=  ',I5,/,'IPR=   ',I5) 
#endif
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE 
      if (NGR.lt.1.or.NGR.gt.2) then
#ifndef ALTAY_SUBROUTINE
            write (*,140) NGR
            if(NLIST.eq.1) then
            write (IMP,140) NGR
            end if
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'SIMUL','Incorrect value of NGR',RCM_RTN)
#endif
      endif
 140  format (' NGR can only take the values 1 or 2 but was',I5)   
      !
      TITEL  = config%jobtitle
      !
      if(NLIST.eq.1) then
      write (IMP,97) TITEL
      end if
      if (NRES.gt.0) write (IMP2,98) TITEL
  97  format (' Title of the new simulation: ',A)
      ! Only if CUR file is requested
      if (NFILE1.eq.1) then
          assembly%texture%title = TITEL ! FIXME: texture title should be set in a different way
          call CURwriteTitle(assembly, IMP1,info)
      endif
  98  format (A)      
  99  FORMAT (2I5)
      !
      CALL TAYLOR(state%old, material%hardening, 1)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
#ifndef ALTAY_SUBROUTINE
      if (NFILTW.eq.1) then
          write (IMP3,98) TITEL
          write (IMP3,99) NPOINT
      endif
#endif
      !
      ! Various IO/initialization calls
      !
#ifndef ALTAY_SUBROUTINE
#ifdef PEBP_ENABLED
      if (NPEBP /= 0) info = writeSDV(IPEBPSDV,header=.true.)
#endif
#endif
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  30  continue
      i=NPOINT/NGR
      if (NGR*i.ne.npoint) then
#ifndef ALTAY_SUBROUTINE      
            write (6,405) NPOINT
            write (*,405) NPOINT
 405  format (' Subroutine SIMUL',/' The LAMEL version works only if',   &
      ' the number of orientations NPOINT=',I5,/,                        &
      ' is an even number')
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'SIMUL',                                         &
            'The number of grains must be an even number',RCM_RTN)
#endif
      endif
  36  NFILE=NFILE0*NFILE1
      NPEBPx=NFILE0*NPEBP   ! control "BEP" (effective value)
      NMSSx= NFILE0*NMSS    ! control "MSS" (effective value)

#ifdef ALTAY_SUBROUTINE
      NSTP     = astate%simulCalls(astate%this)%input%nsteps
      swrlx(1) = astate%simulCalls(astate%this)%input%rlx1
      swrlx(2) = astate%simulCalls(astate%this)%input%rlx2
      swrlx(3) =.false.
#else      
      read (KLEC,99) NSTP
      if(NLIST.eq.1) then
      write (IMP,115) NSTP
      end if
 115  format (//,' S I M U L         NR. STEPS=',I5,//)
      read (KLEC,99) ICRAT1,ICRAT2
      if(NLIST.eq.1) then
      write (IMP,104) ICRAT1,ICRAT2
      end if
 104  format (' ICRAT:',2I5)

      swrlx(1)=(ICRAT1.eq.1)
      swrlx(2)=(ICRAT2.eq.1)
      swrlx(3)=.false.
      if (IPR.gt.0.and.NLIST.eq.1) write (IMP,*)'Relaxations:',swrlx(1)
#endif
      !
      CALL TAYLOR(state%old, material%hardening, 2,MacroDefRate)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
      ! Output the current texture
      if (NFILE.eq.1) call CURwriteBlock(assembly,IMP1,info)
#ifndef ALTAY_SUBROUTINE
      if ((NRES >= 1).and.(IW <= 1)) call writeReportHeader(IMP6,info)
#endif      
#if defined(PEBP_ENABLED) && .not. defined(INTERMEDIATEBPM_DISABLED)
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
#ifdef PEBP_ENABLED
      pebpSDVavg = StateDerivedVars()
#endif      
      !
      assembly = TextureAssembly(state%old%texture, state%old%frame)
      call dynfil2(state%old,nrstep,MacroDefState%TotalDefGrad)
#ifndef NO_STDOUT       
      write (*,96) ISTP
#endif
      if(NLIST.eq.1) then
      write (IMP,96) ISTP
      end if
  96  format(' Step nr.',i5,5X,3f12.5)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
      if (IW.gt.1) goto 70
      if(NLIST.eq.1) then
          write (IMP,3456) MacroDefRate%VelGrad
      end if
 3456 format ('DG=',3(T10,3d12.3,/))
!
!     Get the 5x5 transformation matrix MACRO to morfol. GRAIN AXES
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011
!      if (IGLIJ.eq.1) then
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE

  70  if (nfile.eq.0.or.ISTP.gt.1) goto 44
      IF (NLIST.EQ.1) WRITE (IMP,112) ISTP
 112  FORMAT (//' DEFORMATION STEP ',I5,//)
      if (NRES.gt.0) write (IMP2,404) nrstep+1,NPOINT
 404  format (' Def. Step ',i5,'  Number of orientations',i5,/,T3,'ior'  &
       ,T7,'EquivStress',T23,'WorkRate',T37,'tau_ref',T56,'M',T64,       &
       'ratlon',T109,'RHO-SYMMETRIC',T172,'RHO-ROTATIONAL',T239,'STRESS' &
       ,/,1x,278('*'))
      !
  44  continue
      if (NLIST.eq.1) then
          do i=1,3 
             write (IMP,407) (MacroDefState%TotalDefGrad(j,i),j=1,3)
          enddo
 407  format (' F ',3d15.7)
      end if
      !
      nrstep=nrstep+1
      !
      call Update_DeformationState(MacroDefRate,MacroDefState,info)
      !
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
      ! We can choose not to update the texture data
      if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
            call DYNFIL3(state%new,nrstep,MacroDefState%TotalDefGrad)
      endif
#else          
      call DYNFIL3(state%new,nrstep,MacroDefState%TotalDefGrad)
#endif
!
!       Added for lamel model:
!     Organisation reading temporary texture file,
!     in such way that the program TAYLOR can process the crystals
!     by sets of 2.
!     Taylor must therefore have "advance knowledge" of the
!     orientation to come at the moment that it starts such
!     computation.
!     See also the comment before the calling of subroutine TAYLOR.
!
  10  laml=1
      laml1=NGR
      ifil4=0
      if (NFILTW.eq.1) write (IMP3,399)
 399  format(1x)
      !
      ! Begin the loop over grains/clusters
      !
      clusterloop: DO 23 IOR=1,NPOINT
      !
      i_cluster = floor(IOR/2.0D0 + 0.6D0)
      !
      Mgrain=0.0
      GMMdot=0.0
      WorkRate = 0.D0
      SeqGrain = 0.D0
      Wtot = 0.0
      !
 2626 do 80 L=laml,laml1
      if (ifil4.eq.NPOINT) goto 80
      ifil4=ifil4+1
      call DYNFIL4(state%old,ifil4,fi10b(L),PHI0b(L),fi20b(L),                     &
       TRFb(1,1,L),GEWFb(L),GMMAb(L),Fb(1,1,L))
!
      fi1b(L)=fi10b(L)*convf
      PHIb(L)=PHI0b(L)*convf
      fi2b(L)=fi20b(L)*convf
  80  continue
      laml1=laml1+1
      if (laml1.gt.NGR) laml1=1
      laml=laml1
      GMM0=GMMAb(laml)
      call altayHard_getTau(material%hardening, GMM0,TAU,info)
      fi1=fi1b(laml)
      PHI=PHIb(laml)
      fi2=fi2b(laml)
      !
      TRF = TRFb(:,:,laml)
      IF (NFILE.eq.0.or.ISTP.gt.1) goto 999
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
!
!      write (*,3210)
! 3210 format (' Just before Taylor')
 999  if (IW.le.1) then
            CALL  TAYLOR(state%old, material%hardening,3,MacroDefRate,MacroDefState)
#ifdef ALTAY_SUBROUTINE
            RCM_GUARD
#endif            
      endif
      !
      call mesostr_clusterweightfactor(NGR,i_cluster,MacroDefState,GEWF,info)
      !
      TOTGEW=TOTGEW+GEWF
      !
      ! Skip the rest of the loop if IF > 1
  41  if (IW.gt.1) cycle
      !
#ifdef ALTAY_SUBROUTINE
      if (astate%simulCalls(astate%this)%input%full_model) then
            CALL TAYLR1(ISTP,IOR,NRES,TAU,GMMdot,SeqGrain,WorkRate,      &
                        MacroDefRate)
            RCM_GUARD
      endif
#else
      CALL TAYLR1(ISTP,IOR,NFILE,TAU,GMMdot,SeqGrain,WorkRate,           &
                  MacroDefRate)
#endif      
!   49 if (NFILTW.eq.1) write (IMP3,398) ITW
 398  format (I3)
      !
      STOT = STOT + Ssam*GEWF
      RHOST = RHOST + relaxationrate_sam*GEWF
      !      
  63  SeqAvg = SeqAvg + SeqGrain*GEWF
      Mgrain = GMMdot /  MacroDefRate%vMeqStrainRate
      Mavg = Mavg + Mgrain*GEWF
      ! Normalized relaxation: ||relaxationrate_sam||/MacroDefRate%vMeqStrainRate = (||d-D||)/MacroDefRate%vMeqStrainRate 
      srh = srh + norm2(relaxationrate_sam)/MacroDefRate%vMeqStrainRate*GEWF
      HGAM = HGAM + GMMdot*GEWF !Step time here implicitly assumed to be 1.0s      
      GMM1 = GMM0 + GMMdot !Step time here implicitly assumed to be 1.0s
      Wtot = Wtot + WorkRate !Step time here implicitly assumed to be 1.0s
#ifdef PEBP_ENABLED
      select case(KOST)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
           call KS_getSDV(IOR,pebpSDV,info)
           pebpSDVavg = pebpSDVavg + pebpSDV * GEWF
      endselect
#endif
#ifdef ALTAY_SUBROUTINE
      ! We can choose not to update the texture state
      if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
            call DYNFIL5(state%new,IOR,fi1,PHI,fi2,C2,GEWF,GMM1,                   &
                         MacroDefState%TotalDefGrad) 
      endif
#else
      call DYNFIL5(state%new,IOR,fi1,PHI,fi2,C2,GEWF,GMM1,                         &
                   MacroDefState%TotalDefGrad)
#endif
      ! 
#ifndef ALTAY_SUBROUTINE
      ! Output the plastic work of the grain (in its initial configuration)
      if (NRES >= 1) call writeReportRecord(IMP6,fi1b(laml),PHIb(laml),  &
                                            fi2b(laml),Wtot,info)
#endif      
      !
      ! End of the loop over crystals
      !
  23  enddo clusterloop
      !
      ! Finish processing if IW > 1
      if (IW.gt.1) exit
      !
      SHsam = STOT / TOTGEW
      RHOSm = RHOST / TOTGEW
      !
  66  do 65 i=1,2
      do 65 j=i+1,3
      SHsam(i,j)=SHsam(i,j)*FS(i,j)
      SHsam(j,i)=SHsam(i,j)
      RHOSm(i,j)=RHOSm(i,j)*FS(i,j)
      RHOSm(j,i)=RHOSm(i,j)
65    continue

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
#ifdef PEBP_ENABLED
      select case(KOST)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
           pebpSDVavg = pebpSDVavg * (1.D0/TOTGEW)
      endselect
#ifndef ALTAY_SUBROUTINE
      if (NPEBPx /= 0) info = writeSDV(IPEBPSDV,pebpSDVavg)
#endif
#endif      
#ifdef ALTAY_SUBROUTINE
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
      end associate
#endif
      !
      HGAM = HGAM / TOTGEW
      HGAMCALL = HGAMCALL + HGAM
#ifdef ALTAY_SUBROUTINE
      ! We can choose not to update the internal state
      if (.not.astate%simulCalls(astate%this)%input%keep_state) then  
            HGAMTOT = HGAMTOT + HGAM 
      endif
#else
      HGAMTOT = HGAMTOT + HGAM
#endif
      if(NLIST.eq.1) then
      WRITE (IMP,105) ISTP,SeqAvg,Mavg,MacroDefState%IncrvMeqStrain
      end if
105   FORMAT (' FOR STEP',I5,'  AVERAGE STRESS=',F15.5,'   AVERAGE M-VALUE=',F10.5, &
              '  EFF. STRAIN EPS USED=',F10.5) 
      !
      ! Advance state pointers
      info = altayStateData_advance(state)
#ifdef TESTING_ENABLED
      !!! TESTING -->>
      call altayStateData_printStatus(state)
      !!! <<-- TESTING
#endif
      !
      ! End of the loop over steps
      !
   8  enddo steploop
      !
  22  RETURN
  32  return
      END SUBROUTINE
    
      end module
