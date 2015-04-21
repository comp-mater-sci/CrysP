#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayTaylor
      use altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror, &
                                writeRESRecord
      use altayMacroKinematic
      use criMathUtils
      use altayPancake, only: Pancak2Solution
      use altayDeformationMechanism
 
      integer,parameter,private :: N = 5, N1 = N + 1 
      
      
      type(Pancak2Solution),private, save :: Pancak2_solution !cf note#1.
      type(DeformationMechanismData), private, save ::DM_data !cf note#1.
      !> note#1: the save attribute is required to save these objects 
      !> in-between calls to Taylor and taylr1 from simul.
            
      contains
      
! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY 
!
      SUBROUTINE TAYLOR (state, hardparams, IRICHT, MacroDefRate, MacroDefState)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      use altayState
      use altayHard
      use altayIOConfig
      use altayPancake
      implicit double precision (a-h,o-z)
      type(altayStateVariables),intent(in)          :: state
      type(HardeningModels),intent(in)              :: hardparams
      integer,intent(in)                            :: IRICHT
      !> optional argument - required for IRICHT=2 or 3:
      type(DeformationRate), intent(in),optional :: MacroDefRate
      !> optional argument - required for IRICHT=3:      
      type(DeformationState),intent(in),optional :: MacroDefState
      !
      COMMON /TEXTUR/ TRF(3,3),C2(3,3)
      !     SHsam:    macroscopic stress in sample reference system
      COMMON /SIMUL_TAYLOR/ SHsam(3,3), IOR,laml,ngr,nrl,TRFb(3,3,2),GMMAb(2)
      COMMON /GENRLX/ Ssam(3,3),relaxationrate_sam(3,3)
!
!     DVM = von Mises equivalent strain rate
!

      SAVE
      GOTO (1000,2000,3000),IRICHT
 1000 if(NLIST.eq.1) then
      WRITE (IMP,216)
      end if
216   FORMAT (/,' SUBROUTINE TAYLOR - READS ITS CRYSTAL DATA',//)
      !
      call DeformationMechanismData_readPre(DM_data, LEC, info)
      !
      if (DM_data%n_systems.gt.DM_max_systems)then
#ifndef ALTAY_SUBROUTINE
            write (*,5001) DM_data%n_systems,DM_max_systems
            if(NLIST.eq.1) then
                  write (IMP,5001) DM_data%n_systems,DM_max_systems
            end if
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'TAYLOR','Too large slip system set',RCM_RTN)
#endif
      endif   
5001  format(' TAYLOR - n_systems=',I5,' LARGER THAN  DM_max_systems=',I5)       
      !
      if(NLIST.eq.1) then !!echo to LST
      write (IMP,221) DM_data%description
  221 format (/,' Slip system set:',A,/)
      WRITE (IMP,211) 0,DM_data%n_slip_systems,DM_data%n_twinning_systems,DM_data%DI
 211  FORMAT (1X,I4,10X,2I5,10X,5I5)
      DO 500 I1=1,DM_data%n_systems                                                     
      WRITE (IMP,213) I1,(DM_data%A1(J,I1),J=1,5),(DM_data%B1(L,I1),L=1,3)
 213   FORMAT (I3,' A ',5F10.7,' B ',3F10.7)
 500  CONTINUE
      DO 501 I=1,5                                                      
      WRITE (IMP,215) I1-1+I,(DM_data%B(I,L),L=1,5)
  215  FORMAT (1X,I4,10X,5D15.8)
 501  CONTINUE
      IF (DM_data%n_twinning_systems.EQ.0) GOTO 504                                            
      DO 505 I=1,DM_data%n_twinning_systems                                                    
      WRITE (IMP,218) I1+4+I,(DM_data%B2(L,I),L=1,6),DM_data%G(I)
 218  format (i4,' B2',6f10.7,' G',f10.7)
 505  CONTINUE
504   CONTINUE
      end if !!end of echo to LST
      !
      RETURN
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ QGX 4/11/2011 
! 2000 IF (IGLIJ.EQ.0) GOTO 70  
 2000 continue
!EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
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
 203  FORMAT (' TAYLOR - DISPLACEMENT GRADIENT WHICH WILL BE USED FOR THE SIMULATION', &
              //T9,'GLOBAL TENSOR',T47,'SYMMETRICAL PART',T85,     &
              'ANTISYMMETRICAL PART',/)
 204  FORMAT (1X,3(3F10.5,10X))
   70  continue
      if (MacroDefRate%NormStrainRate.lt.1.0D-10) then
#ifndef ALTAY_SUBROUTINE
         write (*,205) MacroDefRate%NormStrainRate
         if(NLIST.eq.1) then
              write (IMP,205) MacroDefRate%NormStrainRate
         end if
         call terminate(stopcode_runtimeerror)
#else
         RCM_RAISE(1,'TAYLOR',                                           &
        'Symmetric part of the strain step is too small',RCM_RTN)
#endif
      endif
 205  format (' Taylor - symmetric part of strain step is too small'     &
       ,d20.8)
!      
      RETURN
!!     OMREKENING DISPLACEMENT GRADIENT.
 3000 continue
!      write (*,1234)
! 1234 format (' Just before Pancak2')
       CALL Pancak2(Pancak2_solution,state, hardparams,IOR,laml,ngr,nrl,TRFb,GMMAb, &
       IPR,MacroDefRate,MacroDefState,DM_data)
       !work-around to bring following 2 variables in scope of simul, via /GENRLX/
       Ssam =               Pancak2_solution%stress_sam         
       relaxationrate_sam = Pancak2_solution%relaxationrate_sam
       !
#ifdef ALTAY_SUBROUTINE
      RCM_GUARD
#endif
!      write (*,1235)
! 1235 format (' Just after Pancak2')
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE TAYLR1(ISTP,IOR,NFILE,TAU,TOTGAMdot,Seq,WorkRate,       &
                        MacroDefRate)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
      use altayConfig, only: astate
#endif
#ifdef PEBP_ENABLED      
      use AltayDSHstate, KOST => iKOST
      use altayHardTypes, only: hard_BP, hard_PEBPscrew, hard_PEBPloop
#endif
      use altayIOConfig
      use altaySliprate
      use altayCRSSTypes
      implicit double precision (a-h,o-z)
      type(DeformationRate),intent(in) :: MacroDefRate
      COMMON /TEXTUR/ TRF(3,3),TRF_new !-> input, resp., output
      COMMON /EULERA/ fi1,PHI,fi2 !-> output
      !> Equivalent stress in crystal, defined as plastic work rate in crystal
      !> normalized by (macro) von Mises equivalent strain rate
      double precision, intent(out):: Seq
      !> Transformation matrix from sample frame to crystal frame at the 
      !> end of increment. Each of its 3 rows contains a reference axis of the 
      !> crystal reference frame (at end of increment), as expressed in the 
      !> sample reference frame. 
      !> Note: intent(out) attribute in further developments foreseen.
      double precision, dimension(3,3) :: TRF_new
      !> Rate of plastic work per unit volume in the crystal
      double precision, intent(out) :: WorkRate
      double precision :: Mgrain
      type(EulerAngles):: Euler
      !
      DIMENSION ROT(3)
      dimension GAMdot(DM_max_systems)
#ifdef PEBP_ENABLED      
      integer :: info
      double precision :: ddt
#endif
      double precision, intent(OUT) :: TOTGAMdot
      !
      SAVE
      !
      call SLIPRAT(GAMdot,MacroDefRate,Pancak2_solution,DM_data)
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
            call KS_updateState(IOR,GAMdot,ddt,info)
#else
            call KS_updateState(IOR,GAMdot,ddt,info)
#endif
      endselect
#endif
      !
      TOTGAMdot=sum(abs(GAMdot(1:DM_data%n_systems)))
      !
      if(NLIST.eq.1) then
          write (IMP,103) ISTP,IOR,fi1,PHI,fi2
      end if
 103  format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)
      !
      call CRSSData_CalcWorkRate(WorkRate, pancak2_solution%allcrss, GAMdot, info)
      Seq=WorkRate / MacroDefRate%vMeqStrainRate
      !
      if(NLIST.eq.1) then
          WRITE (IMP,301) WorkRate
      end if
 301  FORMAT (//,1X,'SYSTEM - SLIPS    VIRTUAL WORK=',D17.8,//)
      !
      if(NLIST.eq.1) then
          WRITE (IMP,109) MacroDefRate%vMeqStrainRate,Seq,                   &
                      (GAMdot(I)/MacroDefRate%vMeqStrainRate,I=1,DM_data%n_systems)
      end if
 109  FORMAT ('vMeqStrainRate=',D17.8,' RATE OF VIRTUAL WORK=',D17.8,/,  &
       '  SLIP RATES',/,(T2,10F10.5))
      !                                                          
      ROT = matmul(DM_data%B1,GAMdot)
      if(NLIST.eq.1) then
          WRITE (IMP,305) ROT
      end if
305   FORMAT (' ROTATIONS',3F12.6)
      !
      ddt = 1.0 !A time increment of 1s is assumed.
      call update_crystal_trafo_fromSlip(TRF_new,TRF,GAMdot,DM_data,MacroDefRate,Pancak2_solution%relaxationspin_sam,ddt,info)
      !
      Euler= EuleranglesType(TRF_new)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/
      !
      IF (DM_data%n_twinning_systems.EQ.0) GOTO 31                                             
      call update_crystal_trafo_fromTwin(TRF_new,DM_data,NLIST,IMP,GAMdot)
      !
      Euler= EuleranglesType(TRF_new)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/      
      !
31    if (nfile.eq.0.or.istp.gt.1) goto 61
      !      
      ! Taylor Factor of the grain:
      Mgrain = TOTGAMdot / MacroDefRate%vMeqStrainRate
      !
      call writeRESRecord(IMP2,ior,Seq,WorkRate,tau,Mgrain,ratlon(MacroDefRate,Pancak2_solution%relaxationrate_sam), &
                                Pancak2_solution%relaxationrate_sam,Pancak2_solution%relaxationspin_sam,Pancak2_solution%stress_sam,info)
      !
   61 RETURN
      !Below lines with identifiers 26 and 52 are apparently never called.
  26  WRITE (IMP,106)
 106  FORMAT (1X,'TAYLOR - NO UPPER LIMIT FOR LINEAR PROGRAMMING PROBLEM')
#ifndef ALTAY_SUBROUTINE
  52  call terminate(stopcode_runtimeerror)
#else
  52  RCM_RAISE(1,'TAYLR1',                                              &
      'No upper limit for linear programming problem',RCM_RTN)
#endif
      END SUBROUTINE
      

      subroutine update_crystal_trafo_fromSlip(this,previous, &
                         sliprates,DM_data,MacroDefRate,rho_a_sam,dt,info)
      use criErrcodes
      implicit none
      !
      !> Transformation matrix from sample frame to crystal frame at the 
      !> end of increment. Each of its 3 rows contains a reference axis of the 
      !> crystal reference frame (at end of increment), as expressed in the 
      !> sample reference frame. 
      double precision, intent(out), dimension(3,3) :: this
      !> Transformation matrix from sample frame to crystal frame at the 
      !> start of increment. Each of its 3 rows contains a reference axis of the 
      !> crystal reference frame (at start of increment), as expressed in the 
      !> sample reference frame.       
      double precision, intent(in),  dimension(3,3) :: previous
      double precision, intent(in),  dimension(DM_max_systems)  :: sliprates
      type(DeformationMechanismData), intent(in)    :: DM_data
      type(DeformationRate), intent(in)             :: MacroDefRate
      !> The relaxation spin expressed in the sample frame
      double precision, intent(in),  dimension(3,3) :: rho_a_sam    
      !> Time increment
      double precision, intent(in)                  :: dt
      integer, intent(out)                          :: info
      !
      double precision, dimension(3) :: plasticspin_crys_vector
      !
      !> The plastic spin expressed in the crystal lattice frame
      double precision, dimension(3,3) :: plasticspin_crys
      !> The macroscopic (i.e. imposed) rigid body spin expressed in the crystal frame
      double precision, dimension(3,3) :: macrospin_crys
      !> The relaxation spin expressed in the crystal frame
      double precision, dimension(3,3) :: rho_a_crys
      !> The crystal lattice spin expressed in the crystal frame
      double precision, dimension(3,3) :: latticespin_crys
      !> Deformation gradient of the lattice rotation from beginning to end of
      !> increment, expressed in the crystal frame 
      double precision, dimension(3,3) :: F_omega_crys
      !> Transformation matrix from crystal frame at the start of increment to 
      !> crystal frame at the end of increment. Each of its 3 rows contains a
      !> reference axis of the crystal reference frame at end of increment, as 
      !> expressed in the crystal reference frame at start of increment
      double precision, dimension(3,3) :: trafo_cold_cnew(3,3)
      !> Set of Euler angles corresponding to transformation matrix 'this'
      type(EulerAngles) :: Euler
      
      
      plasticspin_crys_vector = matmul(DM_data%B1,sliprates)
      !
      plasticspin_crys = Vec3ToAntiSymMat33(PlasticSpin_crys_vector)
      !
      macrospin_crys = rotateSRTensorFrom(MacroDefRate%Spin,previous)
      !
      rho_a_crys = rotateSRTensorFrom(rho_a_sam,previous)
      !
      latticespin_crys = macrospin_crys - plasticspin_crys + rho_a_crys
      !   Note: in ALAMEL-paper (IJP '05), one term has opposite sign: 
      !   LatticeSpin_crys = MacroSpin_crys - PlasticSpin_crys - "RelaxationSpin_crys"
      !
      F_omega_crys = unit_sr_matrix + latticespin_crys * dt
      !   Notes: 
      !    - Explicit time integration. 
      !    - Due to approximate time integration, orthogonality of Fomega_crys 
      !      is not exactly satisfied in general.
      !
      trafo_cold_cnew = transpose(F_omega_crys)
      !
      !trafo[sample->crystal_new] = trafo[crystal_old->crystal_new] * trafo[sample->crystal_old]                       
      this = matmul(trafo_cold_cnew,previous)
      !
      !Ensure orthogonality
      Euler = EuleranglesType(this)
      this = rotmat(Euler)
      !
      info = criSuccess
      !
      end subroutine


      
      subroutine update_crystal_trafo_fromTwin(TRF, DM_data,NLIST,IMP,GAMdot)
      !use criErrcodes
      implicit none
      double precision, intent(inout), dimension(3,3) :: TRF
      type(DeformationMechanismData), intent(in)    :: DM_data
      integer, intent(in) :: NLIST, IMP
      double precision, intent(in),  dimension(DM_max_systems)  :: GAMdot
      !
      double precision :: X, RNDM
      integer :: I, J, K
      double precision, dimension(DM_max_systems) :: VOLFR
      double precision, dimension(3,3) :: RCC, TDC
      !
      X=0.                                                              
      DO 84 I=1,DM_data%n_twinning_systems                                                     
      J=I+DM_data%n_slip_systems                                                           
      X=X+GAMdot(J)/DM_data%G(I)                                                
      VOLFR(I)=X                                                        
  84  CONTINUE                                                          
       IF (X.LE.1.) GOTO 85  
#ifndef ALTAY_SUBROUTINE
      if(NLIST.eq.1) then                                           
      WRITE (IMP,107) X   
      end if                                              
 107  FORMAT (' SUM OF VOLUME FRACTIONS OF TWINS IS',D15.8,              &
      '   SHOULD BE LESS THAN 1')                                       
       call terminate(stopcode_runtimeerror)
#else       
      RCM_RAISE(1,'TAYLR1',                                              &
      'Total volume fraction of twins exceeds unity',RCM_RTN)
#endif
  85  CALL RANDOM_NUMBER(RNDM)
      DO 86 I=1,DM_data%n_twinning_systems                                                     
      IF (RNDM.LT.VOLFR(I)) GOTO 87                                     
  86  CONTINUE                                                          
      GOTO 31                                                           
  87  DO 88 K=1,3                                                       
      DO 89 J=1,3                                                       
  89  RCC(K,J)=TRF(K,J)                                                  
  88  CONTINUE                                                          
      TDC(1,1)=DM_data%B2(1,I)                                                  
      X=DM_data%B2(2,I)                                                         
      TDC(2,1)=X                                                        
      TDC(1,2)=X                                                        
      X=DM_data%B2(3,I)                                                         
      TDC(3,1)=X                                                        
      TDC(1,3)=X                                                        
      TDC(2,2)=DM_data%B2(4,I)                                                  
      X=DM_data%B2(5,I)                                                         
      TDC(3,2)=X                                                        
      TDC(2,3)=X                                                        
      TDC(3,3)=DM_data%B2(6,I)                                                  
      TRF = matmul(TDC,RCC) 
31    CONTINUE
      !
      end subroutine

      end module
