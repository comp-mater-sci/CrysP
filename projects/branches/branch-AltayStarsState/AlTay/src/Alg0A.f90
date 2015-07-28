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
      use altayDeformationMechanism
      use altayPancake
      use altaySliprate
      use altayHard
      !
      type(Pancak2Solution) :: Pancak2_solution
      type(SlipratSolution) :: Sliprat_solution
      !
      type(InterfaceDataset),save :: interface_dataset !SAVE attribute due to multiple calls to SIMUL
      type(MesostructureState),save :: mesostructure_state !save attribute required to keep the state in subsequent simul
                                                           ! calls (continuation of deformation along new strain path)      
      !
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
      common /CEIGEN/ NBLOC
      DIMENSION                                &
       STOT(3,3),                                               &
       RHOST(3,3),RHOSm(3,3),gewfb(2) ,GMMAb(2),SHSAM(3,3),ROT(3)
      type (CRSSData),dimension(2) :: CRSSb
      !> \fixme: infoarr variable is added just to conform with cluster shape in elemental call to CRSSData_init
      integer :: infoarr(2)
      dimension FS(3,3)
      character(len=40) :: TITEL
      integer :: NPOINT
      integer :: info
      double precision :: ddt
      !> sequence number of the cluster
      integer :: i_cluster
      !> index of cluster interface
      integer :: i_interface
      double precision, dimension(3,3) :: T_cluster
      double precision :: GEWF = 1.0D0
      type(DeformationState) :: MacroDefState
      ! HGAM: homogenized slip per step
      ! HGAMCALL: homogenized slip per call
      ! HGAMTOT: homogenized slip accumulated over calls
      double precision :: HGAM=0.D0,HGAMCALL=0.D0,HGAMTOT=0.D0
      ! Macroscopically imposed vM equivalent strain per call.
      double precision :: MEPSCALL=0.D0 
      double precision :: Mavg   !Volume-averaged Taylor factor
      double precision :: srh !Strain Rate Heterogeneity in polycrystal
      double precision :: Wtot ! Total plastic work per unit volume in crystal
      !> work-around to relocate large part of TAYLR1 to within SIMUL, without repetition
      !   (cf. ifdef ALTAY_SUBROUTINE)
      logical :: calling_TAYLR1 = .false.
      !
      type(EulerAngles), dimension(2) :: eulerb_1_rad, eulerb_0_deg, eulerb_0_rad ! _0_: start of inc; _1_: end of inc
#ifdef PEBP_ENABLED
      type(StateDerivedVars) :: pebpSDV, pebpSDVavg
#endif
      !
      data convf/0.5729577951308232D+02/
      data FS/9*1.0D0/ 
      SAVE
      !
      NPOINT = altayStateData_size(state)
      !
      IF (IW) 32,33,30
  33  call  random_seed
      !
      NGR    = config%simul_init%NGR
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
  98  format (A)      
  99  FORMAT (2I5)
      !
      if(NLIST.eq.1) then
          WRITE (IMP,216)
      end if
216   FORMAT (/,' SUBROUTINE SIMUL  - READS ITS CRYSTAL DATA',//)
      !
      if (material%phase(1)%deformationmechanism%n_systems.gt.DM_max_systems)then
#ifndef ALTAY_SUBROUTINE
            write (*,5001) material%phase(1)%deformationmechanism%n_systems,DM_max_systems
            if(NLIST.eq.1) then
                  write (IMP,5001) material%phase(1)%deformationmechanism%n_systems,DM_max_systems
            end if
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'SIMUL ','Too large slip system set',RCM_RTN)
#endif
      endif   
5001  format(' SIMUL  - n_systems=',I5,' LARGER THAN  DM_max_systems=',I5)       
      !
      if(NLIST.eq.1) then !!echo to LST
      write (IMP,221) material%phase(1)%deformationmechanism%description
  221 format (/,' Slip system set:',A,/)
      WRITE (IMP,211) 0,material%phase(1)%deformationmechanism%n_slip_systems,material%phase(1)%deformationmechanism%n_twinning_systems,material%phase(1)%deformationmechanism%set0
 211  FORMAT (1X,I4,10X,2I5,10X,5I5)
      DO 500 I1=1,material%phase(1)%deformationmechanism%n_systems                                                     
      WRITE (IMP,213) I1,(material%phase(1)%deformationmechanism%A1(J,I1),J=1,5),(material%phase(1)%deformationmechanism%B1(L,I1),L=1,3)
 213   FORMAT (I3,' A ',5F10.7,' B ',3F10.7)
 500  CONTINUE
      DO 501 I=1,5                                                      
      WRITE (IMP,215) I1-1+I,(material%phase(1)%deformationmechanism%B(I,L),L=1,5)
  215  FORMAT (1X,I4,10X,5D15.8)
 501  CONTINUE
      IF (material%phase(1)%deformationmechanism%n_twinning_systems.EQ.0) GOTO 504                                            
      DO 505 I=1,material%phase(1)%deformationmechanism%n_twinning_systems                                                    
      WRITE (IMP,218) I1+4+I,(material%phase(1)%deformationmechanism%B2(L,I),L=1,6),material%phase(1)%deformationmechanism%G(I)
 218  format (i4,' B2',6f10.7,' G',f10.7)
 505  CONTINUE
504   CONTINUE
      end if !!end of echo to LST
      !
      call CRSSData_init(CRSSb, material%phase(1)%deformationmechanism%n_systems, infoarr)
      !
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
      !
#ifndef ALTAY_SUBROUTINE
      ! Initializing mesostructure      
      ! 
      !Reading of InterfaceDataset from SMT-file
      call InterfaceDataset_readfromSMTfile( interface_dataset, config%micros_fname, info)
      if (info.ne.0) then
          write(*,415)
          call exit(stopcode_ioerror)
 415      format('Error condition is returned by InterfaceDataset_readfromSMTfile')
      endif   
      !
      !Initialisation of mesostructure_state with FMicro
      call mesostructure_state%update(config%simul_init%FMicro, info)
#endif
      !
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

#endif
      !
      if(NLIST.eq.1) then                                         
          WRITE (IMP,203)
      end if     
      do I=1,3                                                       
          if(NLIST.eq.1) then                                   
              WRITE (IMP,204) (MacroDefRate%VelGrad(I,J),J=1,3),         &
                              (MacroDefRate%StrainRate(I,J),J=1,3),      &
                              (MacroDefRate%Spin(I,J),J=1,3) 
          end if
      end do
 203  FORMAT (' SIMUL  - DISPLACEMENT GRADIENT WHICH WILL BE USED FOR THE SIMULATION', &
              //T9,'GLOBAL TENSOR',T47,'SYMMETRICAL PART',T85,     &
              'ANTISYMMETRICAL PART',/)
 204  FORMAT (1X,3(3F10.5,10X))
      if (MacroDefRate%NormStrainRate.lt.1.0D-10) then
#ifndef ALTAY_SUBROUTINE
         write (*,205) MacroDefRate%NormStrainRate
         if(NLIST.eq.1) then
              write (IMP,205) MacroDefRate%NormStrainRate
         end if
         call terminate(stopcode_runtimeerror)
#else
         RCM_RAISE(1,'SIMUL ',                                           &
        'Symmetric part of the strain step is too small',RCM_RTN)
#endif
      endif
 205  format (' SIMUL  - symmetric part of strain step is too small'     &
       ,d20.8)
!      
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
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
      call dynfil2(state%old,MacroDefState%TotalDefGrad)
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
      if (NRES.gt.0) write (IMP2,404) NPOINT
 404  format (16x,'  Number of orientations',i5,/,T3,'ior'  &
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
      !"nrstep=nrstep+1"
      !
      call Update_DeformationState(MacroDefRate,MacroDefState,info)
      !
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
      ! We can choose not to update the texture data
      if (.not.astate%simulCalls(astate%this)%input%keep_texture) then
            call DYNFIL3(state%new,MacroDefState%TotalDefGrad)
      endif
#else          
      call DYNFIL3(state%new,MacroDefState%TotalDefGrad)
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
      !Update mesostructure_state - For current increment, the mesostructure_state is updated (to end of inc...)
      ! BEFORE cluster trafo (subr. mesostr_clustertrafo) and cluster weight factor (subr. mesostr_clusterweightfactor) are calculated !!!
      call mesostructure_state%update(MacroDefState%IncrDefGrad, info)
      !
      ! Begin the loop over grains/clusters
      !
      clusterloop: DO 23 IOR=1,NPOINT
      !
      ! Cluster number and interface index (relevant only in Alamel scheme)
      i_cluster = (IOR+1) / 2
      i_interface = modulo(i_cluster - 1, interface_dataset%n_interfaces) + 1
      !
      Wtot = 0.0
      !
 2626 do 80 L=laml,laml1
      if (ifil4.eq.NPOINT) goto 80
      ifil4=ifil4+1
      call DYNFIL4(state%old,ifil4,eulerb_0_rad(L), GEWFb(L),GMMAb(L))
      !
      eulerb_0_deg(L) = rad2deg(eulerb_0_rad(L))
      !
      ! Retrieve the CRSSb for ifil4  = sequence number of current grain
      call altayHard_getCRSS(material%phase(1)%hardening, state%old, ifil4,CRSSb(L),info)
      !
  80  continue
      laml1=laml1+1
      if (laml1.gt.NGR) laml1=1
      laml=laml1
      GMM0=GMMAb(laml)
      call altayHard_getTau(material%phase(1)%hardening, GMM0,TAU,info) !note: usefullness of obtained TAU is limited to outputting to RES-file.
      !
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
999 if (IW.le.1) then
            call mesostr_clustertrafo(ngr,interface_dataset%trafo(i_interface),MacroDefRate,mesostructure_state%deformationgradient,T_cluster,info)
            CALL Pancak2(laml,ngr,T_cluster,eulerb_0_rad, CRSSb,material%phase(1)%deformationmechanism,   &
                         MacroDefRate,MacroDefState,Pancak2_solution)
#ifdef ALTAY_SUBROUTINE
            RCM_GUARD
#endif            
      endif
      !
      call mesostr_clusterweightfactor(NGR,interface_dataset%trafo(i_interface),mesostructure_state%deformationgradient,GEWF,info)
      !
      TOTGEW=TOTGEW+GEWF
      !
      ! Skip the rest of the loop if IF > 1
  41  if (IW.gt.1) cycle
      !
      calling_TAYLR1 = .false.
#ifdef ALTAY_SUBROUTINE
      if (astate%simulCalls(astate%this)%input%full_model) then
            calling_TAYLR1 = .true.
      endif
#else
      calling_TAYLR1 = .true.
#endif      
      if (calling_TAYLR1) then
      !
      !-> -> -> content from TAYLR1
      !
      call SLIPRAT(sliprat_solution,MacroDefRate,Pancak2_solution,CRSSb(laml),material%phase(1)%deformationmechanism)
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif  
#ifdef PEBP_ENABLED
      select case(KOST)
      case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
            ! Here we explicitly set time increment to the value
            ! that is implicitly assumed in Pancak2.
            ddt = 1.D0
#ifdef ALTAY_SUBROUTINE
            if (.not. astate%simulCalls(astate%this)%input%keep_state)   &
            !> The explicit interface expects as 2nd argument an array of size equal to 24.
            !> \todo: propagate the ShearRateData type to the interface. 
            call KS_updateState(IOR,sliprat_solution%shearrate%shearrate(1:24),ddt,info) 
#else
            !The explicit interface expects as 2nd argument an array of size equal to 24.
            !> \todo: propagate the ShearRateData type to the interface. 
            call KS_updateState(IOR,sliprat_solution%shearrate%shearrate(1:24),ddt,info)
#endif
      endselect
#endif
      !
      if(NLIST.eq.1) then
          write (IMP,103) ISTP,IOR,eulerb_0_deg(laml)%fi1,eulerb_0_deg(laml)%PHI,eulerb_0_deg(laml)%fi2
      end if
103   format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)  
      !
      if(NLIST.eq.1) then
          WRITE (IMP,301) sliprat_solution%workrate
 301  FORMAT (//,1X,'SYSTEM - SLIPS    VIRTUAL WORK=',D17.8,//)
          WRITE (IMP,109) MacroDefRate%vMeqStrainRate,sliprat_solution%vMeqstress,                   &
                      (sliprat_solution%shearrate%shearrate(I)/MacroDefRate%vMeqStrainRate,I=1,material%phase(1)%deformationmechanism%n_systems)
109                   FORMAT ('vMeqStrainRate=',D17.8,' RATE OF VIRTUAL WORK=',D17.8,/,  &
       '  SLIP RATES',/,(T2,10F10.5))
      end if  
      !
      ROT = matmul(material%phase(1)%deformationmechanism%B1,sliprat_solution%shearrate%shearrate)
      if(NLIST.eq.1) then
          WRITE (IMP,305) ROT
      end if
305   FORMAT (' ROTATIONS',3F12.6)      
      !
      CALL Grain_EulerAngles_update(eulerb_1_rad(laml),eulerb_0_rad(laml), &
              sliprat_solution,Pancak2_solution,material%phase(1)%deformationmechanism,MacroDefRate,info=info)
      !
      call Grain_accumulatedshear_update(GMM1,GMM0,sliprat_solution,1.0D0,info)
      !
      if (nfile.ne.0.and.istp.eq.1) then
          !
          call writeRESRecord(IMP2,ior,sliprat_solution%vMeqstress, &
              sliprat_solution%workrate, tau, &
              sliprat_solution%taylorfactor, &
              ratlon(MacroDefRate,Pancak2_solution%relaxationrate_sam), &
              Pancak2_solution%relaxationrate_sam, &
              Pancak2_solution%relaxationspin_sam, &
              Pancak2_solution%stress_sam, info)
      end if
      !
      !<- <- <- content from TAYLR1
      !
      endif      
#ifdef ALTAY_SUBROUTINE
      if (calling_TAYLR1) then
            RCM_GUARD
      endif
#endif     
      !
 398  format (I3)
      !
      STOT = STOT + Pancak2_solution%stress_sam*GEWF
      RHOST = RHOST + Pancak2_solution%relaxationrate_sam*GEWF
      !      
  63  SeqAvg = SeqAvg + sliprat_solution%vMeqstress*GEWF
      Mavg = Mavg + sliprat_solution%taylorfactor*GEWF
      ! Normalized relaxation: ||relaxationrate_sam||/MacroDefRate%vMeqStrainRate = (||d-D||)/MacroDefRate%vMeqStrainRate 
      srh = srh + norm2(Pancak2_solution%relaxationrate_sam)/MacroDefRate%vMeqStrainRate*GEWF
      HGAM = HGAM + sliprat_solution%totalshearrate*GEWF !Step time here implicitly assumed to be 1.0s      
      Wtot = Wtot + sliprat_solution%workrate !Step time here implicitly assumed to be 1.0s
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
            call DYNFIL5(state%new,IOR,eulerb_1_rad(laml),GEWF,GMM1)
      endif
#else
      call DYNFIL5(state%new,IOR,eulerb_1_rad(laml), GEWF,GMM1)
#endif
      ! 
#ifndef ALTAY_SUBROUTINE
      ! Output the plastic work of the grain (in its initial configuration)
      if (NRES >= 1) call writeReportRecord(IMP6,eulerb_0_deg(laml),Wtot,info)
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
