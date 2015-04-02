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
 
      integer,parameter,private :: N = 5, N1 = N + 1 
      
      integer,private           :: M,NGL,NTW
      double precision,private  :: B1(3,96),B(5,5),B2(6,96),G(96)
      integer,private           :: DI1(5)
      
      type(Pancak2Solution),public,save :: Pancak2_solution 
      !> note: the save attribute is required to save the solution in-between calls
      !> to Taylor and taylr1 from simul.
      
      contains
      
! MODIFICATIONS AUG 2010
! THE OLD HARWELL-LINEAR PROGRAMMING SUBROUTINE IS REPLACED BY ONE
! WRITTEN IN TERMS OF THE TAYLOR BISHOP-HILL THEORY 
!
      SUBROUTINE TAYLOR (IRICHT, KOST, MacroDefRate, MacroDefState)
#ifdef ALTAY_SUBROUTINE
      use altayRCM
#endif
      use altayIOConfig
      use altayPancake
      implicit double precision (a-h,o-z)
      ! optional argument - required for IRICHT=2 or 3:
      type(DeformationRate), intent(in),optional :: MacroDefRate
      ! optional argument - required for IRICHT=3:      
      type(DeformationState),intent(in),optional :: MacroDefState      
      COMMON /TEXTUR/ TRF(3,3),C2(3,3)
      !     SHsam:    macroscopic stress in sample reference system
      !     Ssam:        local stress in sample reference system
      COMMON /SIMUL_TAYLOR/ SHsam(3,3), SWRLX(3)
      COMMON /IGLIJS/ M11,CC(2,96)
      COMMON/TLR2/ RHOAsa
      COMMON /DOUBLE/ A1(5,96),BB8(5)
      COMMON /GENRLX/ Ssam(3,3),RHOSsa(3,3)
      double precision, dimension(3,3):: RHOScrys(3,3)
      double precision, dimension(3,3):: RHOAcrys(3,3), RHOAsa(3,3) 
      character(len=72) :: TITGLIJ
!
!     Extra arrays nodig voor lineare programmatie op 2 korrels tegelijk
!
      common /extra/ A2(10,194)
      dimension XXLP(194)
      logical SWRLX
      INTEGER R 
      DATA MMAX/96/ ! dimension of A1 and other arrays 
!
!     DVM = von Mises equivalent strain rate
!
      !Local stress in crystal reference system
      double precision, dimension(3,3):: Scrys=0.0d0 
      SAVE
      GOTO (1000,2000,3000),IRICHT
 1000 if(NLIST.eq.1) then
      WRITE (IMP,216)
      end if
 216  FORMAT (/,' SUBROUTINE TAYLOR - READS ITS CRYSTAL DATA',//)
!
      R=LEC
!
  507 read (R,217) TITglij
  217 format(A)
      if(NLIST.eq.1) then
      write (IMP,221) titglij
      end if
  221 format (/,' Slip system set:',A,/)
      READ (R,210) I,NGL,NTW,DI1,X,Y
 210  FORMAT (8I4,4X,2F10.0)
      if(NLIST.eq.1) then
      WRITE (IMP,211) I,NGL,NTW,DI1
      end if
 211  FORMAT (1X,I4,10X,2I5,10X,5I5)
#ifndef ALTAY_SUBROUTINE
      IF (I.NE.0) call terminate(stopcode_runtimeerror)
#else
      if (I.NE.0) then
      RCM_RAISE(1,'TAYLOR','Improper slip system set',RCM_RTN)
      endif
#endif
      M=NGL+NTW
      M11=M
      if (M11.gt.MMAX)then
#ifndef ALTAY_SUBROUTINE
            write (*,5001) M11,MMAX
            if(NLIST.eq.1) then
                  write (IMP,5001) M11,MMAX
            end if
            call terminate(stopcode_runtimeerror)
#else
            RCM_RAISE(1,'TAYLOR','Too large slip system set',RCM_RTN)
#endif
      endif
 5001 format(' TAYLOR - NGL+NTW=',I5,' LARGER THAN  MMAX=',I5) 
      DO 500 I1=1,M11                                                     
      READ (R,212) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
 212  FORMAT (I4,8F20.16)
      if(NLIST.eq.1) then
      WRITE (IMP,213) I,(A1(J,I1),J=1,5),(B1(L,I1),L=1,3)
      end if
 213   FORMAT (I3,' A ',5F10.7,' B ',3F10.7)
 500  CONTINUE
      DO 501 I=1,5                                                      
      READ (R,214) J,(B(I,L),L=1,5)
 214  FORMAT (I4,5D23.16)
      if(NLIST.eq.1) then
      WRITE (IMP,215) J,(B(I,L),L=1,5)
      end if
 215  FORMAT (1X,I4,10X,5D15.8)
 501  CONTINUE
      IF (NTW.EQ.0) GOTO 504                                            
      DO 505 I=1,NTW                                                    
      READ (R,212) J,(B2(L,I),L=1,6),G(I)
      if(NLIST.eq.1) then
      WRITE (IMP,218) J,(B2(L,I),L=1,6),G(I)
      end if
 218  format (i4,' B2',6f10.7,' G',f10.7)
 505  CONTINUE
 504  CONTINUE
 502  do 30 j=1,194
      do 30 i=1,10
      A2(i,j)=0.0
  30  continue
      do 31 j=1,M11
      do 31 i=1,5
      x8=A1(i,j)
      A2(i,j)=x8
      A2(i+5,j+M11)=x8
  31  continue
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
       CALL Pancak2(Pancak2_solution,KOST,NGL,B,DI1,Scrys,RHOScrys,RHOAcrys, &
       SWRLX,XXLP,IPR,MacroDefRate,MacroDefState)
      !Report Scrys to LST-file
 100  format(' Bishop-Hill stress (crystal system):')
 101  format(3d20.7)       
      if(NLIST.eq.1) then
          write (IMP,100)
          do i=1,3
              write (IMP,101) (Scrys(i,j),j=1,3)
          end do
      end if
      !Transform stress from local frame (Scrys) to sample frame (Ssam) 
      Ssam = rotateSRTensorTo(Scrys,TRF)      
      !Transform relaxation strain rate tensor from local frame (RHOScrys) 
      !                                         to sample frame (RHOSsa)
      RHOSsa = rotateSRTensorTo(RHOScrys,TRF)      
      !Transform relaxation spin tensor from local frame (RHOAcrys) to sample frame (RHOAsa)
      RHOAsa = rotateSRTensorTo(RHOAcrys,TRF)
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
#endif
      use altayIOConfig
      use altaySliprate
      use altayHard, only: hard_BP, hard_PEBPscrew, hard_PEBPloop
      implicit double precision (a-h,o-z)
      type(DeformationRate),intent(in) :: MacroDefRate
      COMMON /TEXTUR/ TRF(3,3),TRF_new !-> input, resp., output
      COMMON /IGLIJS/ M11,CC(2,96) !-> input
      COMMON/TLR2/ RHOAsa !-> input
      COMMON /EULERA/ fi1,PHI,fi2 !-> output
      COMMON /GENRLX/ Ssam(3,3),RHOSsa(3,3) !-> input      
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
      !> Symmetric part of the relaxation rate tensor in sample frame; non-normalized
      double precision, dimension(3,3) :: RHOSsaNN(3,3)
      !> Anti-symmetric part of the relaxation rate tensor in sample frame; non-normalized
      double precision, dimension(3,3) :: RHOAsaNN(3,3)
      !
      DIMENSION ROT(3)
      dimension RHOAsa(3,3),RHOAcrys(3,3),GAMdot(96)
#ifdef PEBP_ENABLED      
      integer :: info
      double precision :: ddt
#endif
      double precision, intent(OUT) :: TOTGAMdot
      !
      SAVE
      !
      call SLIPRAT(M11,96,GAMdot,ior,IPR,MacroDefRate,Pancak2_solution)
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
      TOTGAMdot=sum(abs(GAMdot(1:M11)))
      !
      if(NLIST.eq.1) then
          write (IMP,103) ISTP,IOR,fi1,PHI,fi2
      end if
 103  format (' ISTP,IOR',2I5,' phi1, PHI, phi2:',3F15.6)
      !
      WorkRate=0.0
      do i=1,M11 
          if (GAMdot(i).GT.0.0) then
              !positive slip rate
              WorkRate= WorkRate + CC(1,i)*GAMdot(i)
          else
              !negative or 0 slip rate
              WorkRate= WorkRate - CC(2,i)*GAMdot(i)
          endif
      end do
      Seq=WorkRate / MacroDefRate%vMeqStrainRate
      !
      if(NLIST.eq.1) then
          WRITE (IMP,301) WorkRate
      end if
 301  FORMAT (//,1X,'SYSTEM - SLIPS    VIRTUAL WORK=',D17.8,//)
      !
      if(NLIST.eq.1) then
          WRITE (IMP,109) MacroDefRate%vMeqStrainRate,Seq,                   &
                      (GAMdot(I)/MacroDefRate%vMeqStrainRate,I=1,M)
      end if
 109  FORMAT ('vMeqStrainRate=',D17.8,' RATE OF VIRTUAL WORK=',D17.8,/,  &
       '  SLIP RATES',/,(T2,10F10.5))
      !                                                          
      ROT = matmul(B1,GAMdot)
      if(NLIST.eq.1) then
          WRITE (IMP,305) ROT
      end if
305   FORMAT (' ROTATIONS',3F12.6)
      !
      ddt = 1.0 !A time increment of 1s is assumed.
      call update_crystal_trafo_fromSlip(TRF_new,TRF,GAMdot,B1,MacroDefRate,RHOAsa,ddt,info)
      !
      Euler= EuleranglesType(TRF_new)
      fi1=Euler%fi1 !
      PHI=Euler%PHI !use of EulerAngles2Arr impeded
      fi2=Euler%fi2 !   by common block /EULERA/
      !
      IF (NTW.EQ.0) GOTO 31                                             
      call update_crystal_trafo_fromTwin(TRF_new,NTW,NGL,NLIST,IMP,GAMdot,G)
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
      ! Non-normalize the RHOSsa and RHOAsa
      RHOSsaNN = RHOSsa * MacroDefRate%vMeqStrainRate
      RHOAsaNN = RHOAsa * MacroDefRate%vMeqStrainRate
      !
      call writeRESRecord(IMP2,ior,Seq,WorkRate,tau,Mgrain,ratlon(MacroDefRate,rhossa), &
                                rhossaNN,rhoasaNN,ssam,info)
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
                         sliprates,B1,MacroDefRate,rho_a_sam,dt,info)
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
      double precision, intent(in),  dimension(96)  :: sliprates
      double precision, intent(in),  dimension(3,96):: B1
      type(DeformationRate), intent(in)             :: MacroDefRate
      !> The normalized relaxation spin expressed in the sample frame
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
      !> The normalized relaxation spin expressed in the crystal frame
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
      
      
      plasticspin_crys_vector = matmul(B1,sliprates)
      !
      plasticspin_crys = Vec3ToAntiSymMat33(PlasticSpin_crys_vector)
      !
      macrospin_crys = rotateSRTensorFrom(MacroDefRate%Spin,previous)
      !
      rho_a_crys = rotateSRTensorFrom(rho_a_sam,previous)
      !
      latticespin_crys = macrospin_crys - plasticspin_crys + rho_a_crys * MacroDefRate%vMeqStrainRate 
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


      
      subroutine update_crystal_trafo_fromTwin(TRF, &
                         NTW,NGL,NLIST,IMP,GAMdot,G)
      !use criErrcodes
      implicit none
      double precision, intent(inout), dimension(3,3) :: TRF
      integer, intent(in) :: NTW, NGL, NLIST, IMP
      double precision, intent(in),  dimension(96)  :: GAMdot, G
      !
      double precision :: X, RNDM
      integer :: I, J, K
      double precision, dimension(96) :: VOLFR
      double precision, dimension(3,3) :: RCC, TDC
      !
      X=0.                                                              
      DO 84 I=1,NTW                                                     
      J=I+NGL                                                           
      X=X+GAMdot(J)/G(I)                                                
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
      DO 86 I=1,NTW                                                     
      IF (RNDM.LT.VOLFR(I)) GOTO 87                                     
  86  CONTINUE                                                          
      GOTO 31                                                           
  87  DO 88 K=1,3                                                       
      DO 89 J=1,3                                                       
  89  RCC(K,J)=TRF(K,J)                                                  
  88  CONTINUE                                                          
      TDC(1,1)=B2(1,I)                                                  
      X=B2(2,I)                                                         
      TDC(2,1)=X                                                        
      TDC(1,2)=X                                                        
      X=B2(3,I)                                                         
      TDC(3,1)=X                                                        
      TDC(1,3)=X                                                        
      TDC(2,2)=B2(4,I)                                                  
      X=B2(5,I)                                                         
      TDC(3,2)=X                                                        
      TDC(2,3)=X                                                        
      TDC(3,3)=B2(6,I)                                                  
      TRF = matmul(TDC,RCC) 
31    CONTINUE
      !
      end subroutine

      end module
