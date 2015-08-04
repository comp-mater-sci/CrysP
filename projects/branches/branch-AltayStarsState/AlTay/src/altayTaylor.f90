!
! $Id$
!

#ifdef ALTAY_SUBROUTINE
#include "altayRCM.fpp"
#endif
      module altayTaylor
      use altayAlgorithms
      use altayMiscutils, only: terminate, stopcode_runtimeerror
      use altayMacroKinematic
      use criMathUtils
      use criErrcodes
      use altayPancake, only: Pancak2Solution
      use altaySliprate, only: SlipratSolution
      use altayDeformationMechanismData_preconfigured
      use altayDeformationMechanism
#ifdef ALTAY_SUBROUTINE
      use altayConfig
      use altayRCM
#endif 
            
      contains
      
      !> Update the Euler angles of an orientation due to increment of
      !> plastic deformation (slip and/or twinning).
      !> The implemented algorithm is essentialy the one found in TAYLR1
      !> subroutine of altay-v4, yet largely rewritten.
      subroutine Grain_EulerAngles_update(this,previous,sliprat_solution,Pancak2_solution,DM_data,MacroDefRate,deltat,info)
      implicit none
      !
      type(EulerAngles),intent(out)                           :: this
      type(EulerAngles),intent(in)                            :: previous
      type(SlipratSolution), intent(in)                       :: sliprat_solution
      type(Pancak2Solution), intent(in)                       :: Pancak2_solution
      type(DeformationMechanismData), intent(in)              :: DM_data
      type(DeformationRate),intent(in)                        :: MacroDefRate
      !> size of time increment [s]. If not provided, an increment size of 1.0s is used.
      double precision,intent(in), optional :: deltat
      integer, intent(out) :: info
      !
      !> Transformation matrix from sample frame to crystal frame at the 
      !> START of increment. Each of its 3 rows contains a reference axis of the 
      !> crystal reference frame (at START of increment), as expressed in the 
      !> sample reference frame. 
      double precision, dimension(3,3) :: trafo_previous
      !
      !> Transformation matrix from sample frame to crystal frame at the 
      !> END of increment. Each of its 3 rows contains a reference axis of the 
      !> crystal reference frame (at END of increment), as expressed in the 
      !> sample reference frame. 
      double precision, dimension(3,3) :: trafo_this
      !
      !> local variable for size of time increment.
      double precision :: dt
      !
      !
      if (present(deltat)) then
          dt = deltat 
      else
          dt = 1.0D0
      end if
      !
      trafo_previous = rotmat(previous)
      !
      call update_crystal_trafo_fromSlip(trafo_this,trafo_previous,dt,info)
      if (info .ne. criSuccess) return
      !
      if (DM_data%n_twinning_systems>0) then                                             
          call update_crystal_trafo_fromTwin(trafo_this,info)
          if (info .ne. criSuccess) return
      end if
      !
      this = EuleranglesType(trafo_this)
      !
      return

      
      
      contains

      
      !> Dislocation slip contribution to evolution of crystal transformation matrix       
      subroutine update_crystal_trafo_fromSlip(this,previous,dt,info)
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
      !> Time increment
      double precision, intent(in)                  :: dt
      integer, intent(out)                          :: info
      !
      !
      double precision, dimension(3) :: plasticspin_crys_vector
      !
      !> The plastic spin expressed in the crystal lattice frame
      double precision, dimension(3,3) :: plasticspin_crys
      !> The macroscopic (i.e. imposed) rigid body spin expressed in the crystal frame
      double precision, dimension(3,3) :: macrospin_crys
      !> The relaxation spin expressed in the crystal frame
      double precision, dimension(3,3) :: relaxationspin_crys
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
      !
      !
      info = criError
      !      
      plasticspin_crys_vector = matmul(DM_data%B1,sliprat_solution%shearrate%shearrate)
      !
      plasticspin_crys = Vec3ToAntiSymMat33(PlasticSpin_crys_vector)
      !
      macrospin_crys = rotateSRTensorFrom(MacroDefRate%Spin,previous)
      !
      relaxationspin_crys = rotateSRTensorFrom(Pancak2_solution%relaxationspin_sam,previous)
      !
      latticespin_crys = macrospin_crys - plasticspin_crys + relaxationspin_crys
      !   Note: in ALAMEL-paper (IJP '05), one term has opposite sign: 
      !   LatticeSpin_crys = MacroSpin_crys - PlasticSpin_crys - RelaxationSpin_crys
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

      
      !> Twinning contribution to evolution of crystal transformation matrix 
      subroutine update_crystal_trafo_fromTwin(TRF,info)
      implicit none
      double precision, intent(inout), dimension(3,3)           :: TRF
      integer, intent(out)                                      :: info
      !
      double precision :: X, RNDM
      integer :: I, J, K
      double precision, dimension(DM_max_systems) :: VOLFR
      double precision, dimension(3,3) :: RCC, TDC
      !
      !
      info = criError
      !
      X=0.                                                              
      DO 84 I=1,DM_data%n_twinning_systems                                                     
      J=I+DM_data%n_slip_systems                                                           
      X=X+sliprat_solution%shearrate%shearrate(J)/DM_data%G(I)                                                
      VOLFR(I)=X                                                        
  84  CONTINUE                                                          
       IF (X.LE.1.) GOTO 85  
#ifndef ALTAY_SUBROUTINE
!      if(NLIST.eq.1) then                                           
!      WRITE (IMP,107) X   
!      end if                                              
! 107  FORMAT (' SUM OF VOLUME FRACTIONS OF TWINS IS',D15.8,              &
!      '   SHOULD BE LESS THAN 1')                                       
       call terminate(stopcode_runtimeerror)
#else       
      RCM_RAISE(1,'TAYLR1',                                              &
      'Total volume fraction of twins exceeds unity',RCM_RTN)
#endif
      return
      !
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
      info = criSuccess
      !
      end subroutine

     
      end subroutine

      
      !> Update the accumulated shear deformation (including both slip and twinning).
      subroutine Grain_accumulatedshear_update(this,previous,sliprat_solution,deltat,info)
      implicit none
      !
      double precision,intent(out)       :: this
      double precision,intent(in)        :: previous
      type(SlipratSolution), intent(in)  :: sliprat_solution
      double precision,intent(in)        :: deltat
      integer, intent(out)               :: info
      !
      info = CriError
      !
      this = previous + sliprat_solution%totalshearrate * deltat
      !
      info = CriSuccess
      !
      end subroutine
      
      
      end module
