!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-05-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file AlamTSA program allows one to track anisotropic properties  
!>          along deformation due to uniaxial tensile stress.

!> ALAMel Tensile Stress Analysis
!>
module alamTSA
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use commonConfig
use commonUtils
use qrsTypes
implicit none

      integer,parameter                         :: scaleFullTensor = 0, scaleTensileComponent = 1

      type :: TSAConfig
            double precision  :: angle = 0.D0, NormMax = 0.D0, PNormIter = 0.D0

            integer           :: scalingID = scaleFullTensor
            
            !> Uniaxial stress state. Negative values denote compressive state; non-negative values are used for tensile state.
            double precision  :: stress_state = 1.D0
            
            !> Stress ratio
            double precision  :: rho = 0.D0 

      end type

contains

      subroutine AlamTSA_ReadConfig(cnf,cnfunit,info)
      implicit none
      integer,intent(in)                        :: cnfunit
      type(TSAConfig),intent(out)               :: cnf
      integer,intent(out)                       :: info
      !
      integer :: ioerr
      logical :: input_ok = .false.
            input_ok = .false.
            info = -1
            ! Read parameters specific for the AlamTSA program
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%angle
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%scalingID, cnf%NormMax, cnf%PNormIter
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%stress_state
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%rho
            if (ioerr /= 0) return
            ! Validate input
            select case(cnf%scalingID)
                  case(scaleFullTensor,scaleTensileComponent)
                        input_ok = .true.
            end select
            cnf%stress_state = merge(-1.D0,1.D0,(cnf%stress_state < 0.0))
            if ((ioerr /= 0) .or. (.not. input_ok)) return
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamTSA_Run(cnf,info)
      implicit none
      type(TSAConfig),intent(in)                :: cnf
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
      integer,parameter       :: cnfunit = 90, ofunit = 91, histunit = 92
      character(len=32)       :: description
      !
      info = 1
      !
      ! Introduce youself ;-)
      write(*,'(A)') 'AlamTSA, $Rev$'

      if (cnf%stress_state > 0.0) then
            description = 'uniaxial tensile'      
      else
            description = 'uniaxial compression'
      endif
      write(display_unit,fmt=fmtMsg2Msg) 'Test type:', description
      write(display_unit,fmt=fmtMsg2Other//'F10.4)') 'Orientation of the sample:', cnf%angle
      write(display_unit,fmt=fmtMsg2Other//'F10.4)') 'Stress ratio:', cnf%rho
      !
      select case(cnf%scalingID)
            case(scaleFullTensor)
                  write(display_unit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling full tensor'
            case(scaleTensileComponent)
                  write(display_unit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling tensile component'
            case default
                  write(*,*) 'Unknown scaling type, full tensor will be used'
      end select
      ! Open and initialize result files
      open(unit=ofunit,file=trim(outputPrefix)//'.tsa',status='replace')
      !      
#define COMMONOUTHEADER 'iter','eps_vM','Pnorm','Tnorm','W','plast_pot','M','||SonA||','q-value','r-value','s-value'      
#define OUTHEADER COMMONOUTHEADER##,'R'
#define FILEOUTHEADER COMMONOUTHEADER##,'q-valueA','r-valueA','R'
      !
      write(ofunit,fmt=705) FILEOUTHEADER ! write header line
      open(unit=histunit,file=trim(outputPrefix)//'.hts',status='replace')
       
      fi1 = 0.0
      phi = 0.0
      ! Convert angle from degs to rads
      fi2 = cnf%angle * deg2rad
   
      Pt_accum = 0.D0
      vP = 0.D0
      Pnorm = 0.D0 ! sum||P||
      normP = 0.D0 ! ||P||
      Tnorm = 0.D0
      plastic_work_inc = 0.D0
      plastic_work_total = 0.D0
      step = 0
      do 
            write(*,800)
            !! -> Take uniaxial/slightly biaxial tensile stress, rotate it to given direction     
            St = 0.D0
            St(1,1) = cnf%stress_state * sqrt(3.D0/2.D0)/dsqrt(cnf%rho**2-cnf%rho+1)
            St(2,2) = cnf%rho*St(1,1)
            
            ! Rotate from "tensile" to material coordinate system
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Sm = matmul(transpose(Mrot),matmul(St,Mrot))
            !
            call KMAT2VEC5D(Sm,vS) ! Note: as of this call, Sm is deviatoric
            write(*,*) norm2(Sm)
            ! Enforce unit length of vS
            vS = vS / vec_norm2(vS)
            !
            !! -> Calculate corresponding strain rate vA
            call multilevelYLP(vS,vA,vSonA,R,info,.true.,ylpCnf)
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
            call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
            !
            call KVEC5D2MAT(vA,D)
            call KVEC5D2MAT(vSonAn,SmIdent)
            write(*,400)
            do j=1,3
                  write(*,401) Sm(j,:),SmIdent(j,:),D(j,:)
            enddo
            write(*,*)
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
                  write(*,fmt=980) 
                  exit
            endif
            !! -> Report the results
            write(ofunit,706) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, &
                              taylor_factor, & 
                              norm_sona, qrsvalue, qrsvalue_accum%qvalue, qrsvalue_accum%rvalue, R
            !
            write(display_unit,710)
            write(display_unit,700) OUTHEADER ! write header line
            write(display_unit,701) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, &
                                    taylor_factor, &
                                    norm_sona, qrsvalue, R
            write(display_unit,710)
            !!
            !
            select case(cnf%scalingID)
            case(scaleFullTensor)
                  ! Check termination condition
                  if (PNorm >= cnf%NormMax)  exit
                  !
                  !! -> Scale the vA in order to get ||vA|| = PNormIter
                  vD = vA * (cnf%PNormIter / vec_norm2(vA)) 
            case(scaleTensileComponent)
                  ! Check termination condition: only tensile component
                  if (Tnorm >= cnf%NormMax) exit
                  !
                  !! -> Scale the vA in order to get ||Dt_11|| equal to PNormIter
                  vD = vA * (cnf%PNormIter / abs(Dt(1,1)))
            case default
                  write(*,*) 'Unknown scaling type, full tensor will be used'
                  vD = vA                  
            end select
            normD = vec_norm2(vD)
            write(*,'(A,1X,F12.6)') 'Norm of vD = ', normD 
            ! Calculate strain increment for texture evolution           
            call KVEC5D2MAT(vD,De)
            !
            !! -> Impose De as ALAMEL input, advance the state of texture
            !
            write(*,*) 'Strain to be imposed for texture evolution De = '
            write(*,500) De
            write(*,*)
            !
            ! Update the texture
            call makeTextureUpdateStep(De,Se,taylor_factor,outputRequest,info)
            if (info /= 0) then
                  write(*,fmt=970) 
                  exit
            endif
            call KMAT2VEC5D(Se,vSe)
            !! -> Calculate total strain
            vP = vP + vD
            P = P + De  
            normP = vec_norm2(vP)
            Pnorm = Pnorm + normD
            Tnorm = Tnorm + cnf%PNormIter
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
            write(*,'(A)') 'Total strain:'
            write(*,'(A,1X,F12.6)') '||P|| =', normP
            write(*,'(A,1X,F12.6)') 'sum||De|| =', Pnorm
            write(*,*) 'P='
            write(*,500) P
            write(*,'(A,1X,F12.6)') 'Wtot =', plastic_work_total
            !
            step = step + 1 
      enddo            
      !
      close(ofunit)
      close(histunit)

      info = 0
      
      400 format('| Smcoord',T40,'| SmIdent',T80,'|Dmcoord')
      401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))

      500 format(3(3(F10.6,1X),/))
      501 format(3(F10.6,1X),/,3(F10.6,1X),/,3(F10.6,1X))
      
      601 format('Strain increment',T40,'Deviatoric stress')
      602 format(3(F10.6,1X),T40,3(F10.6,1X))
      
      ! Format for screen output
      700 format(1X,A5,1X,11(A12,1X))
      701 format(1X,I5,1X,11(F12.6,1X))
      ! Format for file output
      705   format(1X,A5,1X,13(A12,1X))
      706 format(1X,I5,1X,13(F12.6,1X))
      
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