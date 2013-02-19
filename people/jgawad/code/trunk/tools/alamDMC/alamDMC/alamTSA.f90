!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-05-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!> alamTSA (ALAMel Tensile Stress Analysis)  allows one to track anisotropic properties  
!> along deformation due to uniaxial tensile stress.
module alamTSA
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use dmcBasicModule
use commonUtils
use qrsTypes
use fngLog
implicit none

      integer,parameter                         :: scaleFullTensor = 0, scaleTensileComponent = 1

      type,extends(BasicModule) :: TSAModule
            double precision  :: angle = 0.D0, NormMax = 0.D0, PNormIter = 0.D0

            integer           :: scalingID = scaleFullTensor
            
            !> Uniaxial stress state. Negative values denote compressive state; non-negative values are used for tensile state.
            double precision  :: stress_state = 1.D0
            
            !> Stress ratio
            double precision  :: rho = 0.D0 

      contains
      
            procedure,pass(this)    :: readConfig => TSAModule_ReadConfig
            
            procedure,pass(this)    :: run => TSAModule_run

            procedure,pass(this)    :: printConfig => TSAModule_printConfig
            
      end type

contains

      integer function TSAModule_ReadConfig(this,cnfunit) result(info)
      implicit none
      integer,intent(in)                        :: cnfunit
      class(TSAModule),intent(inout)            :: this
      !
      integer :: ioerr
      logical :: input_ok = .false.
      !
            info = BasicModule_ReadConfig(this,cnfunit)
            if (info /= 0) return
            input_ok = .false.
            info = -1
            ! Read parameters specific for the TSAModule program
            read(cnfunit,fmt=*,iostat=ioerr)  this%angle
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  this%scalingID, this%NormMax, this%PNormIter
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  this%stress_state
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  this%rho
            if (ioerr /= 0) return
            ! Validate input
            select case(this%scalingID)
                  case(scaleFullTensor,scaleTensileComponent)
                        input_ok = .true.
            end select
            this%stress_state = merge(-1.D0,1.D0,(this%stress_state < 0.0))
            if ((ioerr /= 0) .or. (.not. input_ok)) return
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end function

      
      integer function TSAModule_printConfig(this,outunit) result (info)
      implicit none
      class(TSAModule),intent(in)         :: this
      integer,intent(in)                  :: outunit
      !
      character(len=32)       :: description
      !
            info = BasicModule_printConfig(this,outunit)
            if (info /= 0) return
            info = -1
            ! Introduce youself ;-)
            write(outunit,'(A)') 'TSAModule, $Rev$'
            !
            if (doLogging(fngLogInfo,this%output%verbosity)) then 
                  if (this%stress_state > 0.0) then
                        description = 'uniaxial tensile'      
                  else
                        description = 'uniaxial compression'
                  endif
                  write(outunit,fmt=fmtMsg2Msg) 'Test type:', description
                  write(outunit,fmt=fmtMsg2Other//'F10.4)') 'Orientation of the sample:', this%angle
                  write(outunit,fmt=fmtMsg2Other//'F10.4)') 'Stress ratio:', this%rho
                  !
                  select case(this%scalingID)
                        case(scaleFullTensor)
                              write(outunit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling full tensor'
                        case(scaleTensileComponent)
                              write(outunit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling tensile component'
                        case default
                              write(outunit,*) 'Unknown scaling type, full tensor will be used'
                  end select
            endif
            info = 0
      !
      end function
      

      subroutine TSAModule_Run(this,info)
      implicit none
      class(TSAModule),intent(inout)            :: this
      integer,intent(out)                       :: info      
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: P,D,De,Se,Sm,St, SmIdent, Dt, Pt_accum
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision                          :: plast_pot, scal_s, norm_sona
      type(qrsData)                             :: qrsvalue = qrsData(0.D0, 0.D0, 0.D0), qrsvalue_accum = qrsData(0.D0, 0.D0, 0.D0)
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn, vP, vD, vSe
      double precision                          :: fi1,phi,fi2
      double precision                          :: Pnorm, normP, normD, Tnorm
      double precision                          :: R
      double precision                          :: plastic_work_inc = 0.D0, plastic_work_total = 0.D0
      double precision                          :: taylor_factor
      integer     :: step,j 
      !
      integer,parameter       :: thisunit = 90, ofunit = 91, histunit = 92
      
      !
      info = 1
      !

      ! Open and initialize result files
      open(unit=ofunit,file=trim(this%output%outputPrefix)//'.tsa',status='replace')
      !      
#define COMMONOUTHEADER 'iter','eps_vM','Pnorm','Tnorm','W','plast_pot','M','||SonA||','q-value','r-value','s-value'      
#define OUTHEADER COMMONOUTHEADER##,'R'
#define FILEOUTHEADER COMMONOUTHEADER##,'q-valueA','r-valueA','R'
      !
      write(ofunit,fmt=705) FILEOUTHEADER ! write header line
      open(unit=histunit,file=trim(this%output%outputPrefix)//'.hts',status='replace')
       
      fi1 = 0.0
      phi = 0.0
      ! Convert angle from degs to rads
      fi2 = deg2rad(this%angle)
   
      Pt_accum = 0.D0
      vP = 0.D0
      Pnorm = 0.D0 ! sum||P||
      normP = 0.D0 ! ||P||
      Tnorm = 0.D0
      plastic_work_inc = 0.D0
      plastic_work_total = 0.D0
      step = 0
      do 
            if (doLogging(fngLogDebug,this%output%verbosity))  write(display_unit,800)
            !! -> Take uniaxial/slightly biaxial tensile stress, rotate it to given direction     
            St = 0.D0
            St(1,1) = this%stress_state * sqrt(3.D0/2.D0)/dsqrt(this%rho**2-this%rho+1)
            St(2,2) = this%rho*St(1,1)
            
            ! Rotate from "tensile" to material coordinate system
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Sm = matmul(transpose(Mrot),matmul(St,Mrot))
            !
            call KMAT2VEC5D(Sm,vS) ! Note: as of this call, Sm is deviatoric
            ! Enforce unit length of vS
            vS = vS / vec_norm2(vS)
            !
            !! -> Calculate corresponding strain rate vA
            call multilevelYLP(vS,vA,vSonA,R,info,.true.,this%ylp,verbose=this%output%verbosity)
            if (info /= 0) then
                  write(display_unit,fmt=960)
                  exit
            endif
            !
            plast_pot = dot_product(vA, vSonA)
            norm_sona = vec_norm2(vSonA)
            scal_s = norm_sona / vec_norm2(vS)
            ! Calculate normalized stess
            vSonAn = vSonA / vec_norm2(vSonA) 
            !
            if (doLogging(fngLogInfo,this%output%verbosity)) call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
            !
            call KVEC5D2MAT(vA,D)
            call KVEC5D2MAT(vSonAn,SmIdent)
            !
            if (doLogging(fngLogDebug,this%output%verbosity)) then
                  write(display_unit,400)
                  do j=1,3
                        write(display_unit,401) Sm(j,:),SmIdent(j,:),D(j,:)
                  enddo
                  write(display_unit,*)
            endif
            
            !! Calculate q and r in tensile reference frame
            ! Rotate back to the "tensile test" coordinate system   
            Dt = matmul(matmul(Mrot,D),transpose(Mrot))
            Pt_accum = Pt_accum + Dt
            ! Calculate output variables
            qrsvalue = calculateQRS(Dt,scal_s)
            qrsvalue_accum = calculateQRS(Pt_accum,scal_s)
            ! 
            taylor_factor = 0.D0
            call getTaylorFactor(1,taylor_factor,info)
            if (info /= 0) then
                  write(display_unit,fmt=980) 
                  exit
            endif
            !! -> Report the results
            write(ofunit,706) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, &
                              taylor_factor, & 
                              norm_sona, qrsvalue, qrsvalue_accum%qvalue, qrsvalue_accum%rvalue, R
            !
            if (doLogging(fngLogInfo,this%output%verbosity)) then
                  write(display_unit,710)
                  write(display_unit,700) OUTHEADER ! write header line
                  write(display_unit,701) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, &
                                          taylor_factor, &
                                          norm_sona, qrsvalue, R
                  write(display_unit,710)
            endif
            !!
            !
            select case(this%scalingID)
            case(scaleFullTensor)
                  ! Check termination condition
                  if (PNorm >= this%NormMax)  exit
                  !
                  !! -> Scale the vA in order to get ||vA|| = PNormIter
                  vD = vA * (this%PNormIter / vec_norm2(vA)) 
            case(scaleTensileComponent)
                  ! Check termination condition: only tensile component
                  if (Tnorm >= this%NormMax) exit
                  !
                  !! -> Scale the vA in order to get ||Dt_11|| equal to PNormIter
                  vD = vA * (this%PNormIter / abs(Dt(1,1)))
            case default
                  write(display_unit,*) 'Unknown scaling type, full tensor will be used'
                  vD = vA                  
            end select
            normD = vec_norm2(vD)
            if (doLogging(fngLogDebug,this%output%verbosity)) write(display_unit,'(A,1X,F12.6)') 'Norm of vD = ', normD 
            ! Calculate strain increment for texture evolution           
            call KVEC5D2MAT(vD,De)
            !
            !! -> Impose De as ALAMEL input, advance the state of texture
            !
            if (doLogging(fngLogInfo,this%output%verbosity)) then
                  write(display_unit,*) 'Strain to be imposed for texture evolution De = '
                  write(display_unit,500) De
                  write(display_unit,*)
            endif
            !
            ! Update the texture
            call makeTextureUpdateStep(De,Se,taylor_factor,this%output%outputRequest,info)
            if (info /= 0) then
                  write(display_unit,fmt=970) 
                  exit
            endif
            call KMAT2VEC5D(Se,vSe)
            !! -> Calculate total strain
            vP = vP + vD
            P = P + De  
            normP = vec_norm2(vP)
            Pnorm = Pnorm + normD
            Tnorm = Tnorm + abs(De(1,1))
            ! Calculate increment of plastic work (strain * deviatoric_stress)
            plastic_work_inc = dot_product(vD,vSe) 
            plastic_work_total = plastic_work_total + plastic_work_inc
            ! Write history of deformations
            write(histunit,800)
            write(histunit,'(A,1X,I4)') 'Step:', step
            write(histunit,601)
            do j=1,3
                  write(histunit,602) De(:,j), Se(:,j)
            enddo
            write(histunit,'(A,T35,F12.8)') 'Increment of plastic work:', plastic_work_inc
            write(histunit,'(A,T35,F12.8)') 'Total of plastic work:', plastic_work_total
            !
            if (doLogging(fngLogErr,this%output%verbosity)) then
                  write(display_unit,'(A)') 'Total strain:'
                  write(display_unit,'(A,1X,F12.6)') '||P|| =', normP
                  write(display_unit,'(A,1X,F12.6)') 'sum||De|| =', Pnorm
                  write(display_unit,*) 'P='
                  write(display_unit,500) P
                  write(display_unit,'(A,1X,E12.5)') 'Wtot =', plastic_work_total
            endif
            !
            step = step + 1 
      enddo            
      !
      close(ofunit)
      close(histunit)

      info = 0
      
      400 format('|Smcoord',T42,'|SmIdent',T86,'|Dmcoord')
      401 format(3(E11.4,1X),T42,3(E11.4,1X),T86,3(E11.4,1X))

      500 format(3(3(E10.3,1X),/))
      501 format(3(E10.3,1X),/,3(E10.3,1X),/,3(E10.3,1X))
      
      601 format('Strain increment',T40,'Deviatoric stress')
      602 format(3(E10.3,1X),T40,3(E10.3,1X))
      
      ! Format for screen output
      700 format(1X,A5,1X,11(A12,1X))
      701 format(1X,I5,1X,11(E12.5,1X))
      ! Format for file output
      705   format(1X,A5,1X,13(A12,1X))
      706 format(1X,I5,1X,13(E12.5,1X))
      
      710 format('|',5('-'),'|',11(12('-'),'|'))

#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS

#ifdef OUTHEADER
#undef OUTHEADER
#endif      
      end subroutine
      
end module