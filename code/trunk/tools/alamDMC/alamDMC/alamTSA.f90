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
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use commonConfig
use qrsTypes
implicit none

      double precision                          :: angle, NormMax, PNormIter

      integer                                   :: scalingID
      integer,parameter                         :: scaleFullTensor = 0, scaleTensileComponent = 1


contains

      subroutine AlamTSA_ReadConfig(cnfunit,info)
      implicit none
      integer,intent(in)                        :: cnfunit
      integer,intent(out)                       :: info
      !
      integer :: ioerr
      logical :: input_ok = .false.
            input_ok = .false.
            info = -1
            ! Read parameters specific for the AlamTSA program
            read(cnfunit,fmt=*,iostat=ioerr)  angle 
            read(cnfunit,fmt=*,iostat=ioerr)  scalingID, NormMax, PNormIter
            ! Validate input
            select case(scalingID)
                  case(scaleFullTensor,scaleTensileComponent)
                        input_ok = .true.
            end select
            if ((ioerr /= 0) .or. (.not. input_ok)) then
                  write(*,fmt=902) 'AlamTSA'
                  return
            endif
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamTSA_Run(info)
      implicit none
      integer,intent(out)                       :: info      
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: P,D,De,Se,Sm,St, SmIdent, Dt
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision                          :: plast_pot, scal_s, norm_sona
      type(qrsData)                             :: qrsvalue = qrsData(0.D0, 0.D0, 0.D0)
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn, vP, vD, vSe
      double precision                          :: fi1,phi,fi2
      double precision                          :: Pnorm, normP, normD, Tnorm
      double precision                          :: R
      double precision                          :: plastic_work_inc = 0.D0, plastic_work_total = 0.D0
      integer     :: step,j 
      !
      integer,parameter       :: cnfunit = 90, ofunit = 91, histunit = 92
      !
      info = 1
      !
      ! Introduce youself ;-)
      write(*,'(A)') 'AlamTSA, $Rev$'

      write(display_unit,fmt=fmtMsg2Other//'F10.4)') 'Orientation of the sample:', angle
      !
      select case(scalingID)
            case(scaleFullTensor)
                  write(display_unit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling full tensor'
            case(scaleTensileComponent)
                  write(display_unit,fmt=fmtMsg2Msg) 'Strain calculation:', 'scaling tensile component'
            case default
                  write(*,*) 'Unknown scaling type, full tensor will be used'
            end select
      ! Open and initialize result files
      open(unit=ofunit,file=trim(outputPrefix)//'.tsa',status='replace')
#define OUTHEADER 'iter','eps_vM','Pnorm','Tnorm','W','plast_pot','M','||SonA||','q-value','r-value','s-value','R'
      write(ofunit,700) OUTHEADER ! write header line
      open(unit=histunit,file=trim(outputPrefix)//'.hts',status='replace')
       
      fi1 = 0.0
      phi = 0.0
      ! Convert angle from degs to rads
      fi2 = angle * deg2rad
      
      vP = 0.D0
      Pnorm = 0.D0 ! sum||P||
      normP = 0.D0 ! ||P||
      Tnorm = 0.D0
      plastic_work_inc = 0.D0
      plastic_work_total = 0.D0
      step = 0
      do 
            write(*,900)
            !! -> Take uniaxial tensile stress, rotate it to given direction     
            St = 0.D0
            St(1,1) = dsqrt(3.D0/2.D0)
            ! Rotate from "tensile" to material coordinate system
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Sm = matmul(transpose(Mrot),matmul(St,Mrot))
            !            
            call KMAT2VEC5D(Sm,vS) 
            ! Enforce unit length of vS
            vS = vS / vec_norm2(vS)
            !
            !! -> Calculate corresponding strain rate vA
            call multilevelYLP(vS,vA,vSonA,R,info,.true.,ylpCnf)
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
            ! Calculate output variables
            if ( abs(Dt(3,3)) >= epsilon(0.D0) ) then
                  qrsvalue%rvalue = Dt(2,2) / Dt(3,3)
                  qrsvalue%qvalue = qrsvalue%rvalue / (1.D0 + qrsvalue%rvalue)
                  qrsvalue%svalue = scal_s
            else
                  qrsvalue = qrsData(0.D0, 0.D0, 0.D0)
                  info = 4
            endif
            ! 
            !! -> Report the results
            write(ofunit,701) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, ares%taylor_factors(1), norm_sona, qrsvalue, R
            !
            write(display_unit,710)
            write(display_unit,700) OUTHEADER ! write header line
            write(display_unit,701) step, root23*normP, Pnorm, TNorm, plastic_work_total, plast_pot, ares%taylor_factors(1), norm_sona, qrsvalue, R
            write(display_unit,710)
            !!
            !
            select case(scalingID)
            case(scaleFullTensor)
                  ! Check termination condition
                  if (PNorm >= NormMax)  exit
                  !
                  !! -> Scale the vA in order to get ||vA|| = PNormIter
                  vD = vA * (PNormIter / vec_norm2(vA)) 
            case(scaleTensileComponent)
                  ! Check termination condition: only tensile component
                  if (Tnorm >= NormMax) exit
                  !
                  !! -> Scale the vA in order to get Dt_11 equal to PNormIter
                  vD = vA * (PNormIter / Dt(1,1))
            case default
                  write(*,*) 'Unknown scalin type, full tensor will be used'
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
            ! Set input data for ALAMEL
            acnf%simulCalls(1)%dgf = De
            acnf%simulCalls(1)%keep_texture = .false.
            acnf%simulCalls(1)%do_output = outputRequest
            acnf%nSimulCalls = 1
            call ALAMEL(3)       
            !
            ! Get the result
            Se = ares%stress_tensors(:,:,1)
            call KMAT2VEC5D(Se,vSe)
            !! -> Calculate total strain
            vP = vP + vD
            P = P + De  
            normP = vec_norm2(vP)
            Pnorm = Pnorm + normD
            Tnorm = Tnorm + PNormIter
            ! Calculate increment of plastic work (strain * deviatoric_stress)
            plastic_work_inc = dot_product(vD,vSe) 
            plastic_work_total = plastic_work_total + plastic_work_inc
            ! Write history of deformations
            write(histunit,900)
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
      

      
      700 format(1X,A5,1X,11(A12,1X))
      701 format(1X,I5,1X,11(F12.6,1X))
      710 format('|',5('-'),'|',11(12('-'),'|'))
      
      900 format(112('='))

#ifdef OUTHEADER
#undef OUTHEADER
#endif      
      end subroutine
      
end module