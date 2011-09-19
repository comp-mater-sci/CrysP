!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-07-18
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> ALAMel Arbitrary Stress Response
!>
module alamASR
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use commonConfig
implicit none

      double precision                          :: fi1 = 0.D0, phi = 0.D0 , fi2 = 0.D0
      character(len=512)                        :: data_fname
      logical                                   :: update_texture = .false.
      integer                                   :: scalingID = 0
      double precision                          :: NormMax, PNormIter

      integer,parameter :: scaleFullTensor = 0, scaleTensileComponent = 1

contains

      subroutine AlamASR_ReadConfig(cnfunit,info)
      implicit none
      integer,intent(in)                        :: cnfunit
      integer,intent(out)                       :: info
      !
      integer :: ioerr
            info = -1
            ! Read parameters specific for the alamASR program
            read(cnfunit,fmt=*,iostat=ioerr)  fi1, phi, fi2
            read(cnfunit,fmt='(A)',iostat=ioerr) data_fname
            call stripComment(data_fname)
            read(cnfunit,fmt='(L2)',iostat=ioerr) update_texture 
            read(cnfunit,fmt=*,iostat=ioerr)  scalingID, NormMax, PNormIter 
            if (ioerr /= 0) then
                  write(*,fmt=902) 'AlamASR'
                  return
            endif
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamASR_Run(info)
      implicit none
      integer,intent(out)                       :: info      
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: D, De, Sm, Smn, SonA, SmIdent, Pressure
      double precision,dimension(3,3)           :: St, Stdev, StonA, StIdent, Dt
      double precision,dimension(3,3)           :: Mrot = 0.0, MI = 0.0
      !
      double precision                          :: plast_pot, scal_s, norm_sona, vS_norm
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn, vP, vTotalP, vDe

      double precision                          :: Pnorm, normP, normDe, totalPnorm
      double precision                          :: R
      integer     :: i,j,point = 0, npoints = 0, increment = 0
      !
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91, histunit = 92, dtaunit = 93
      integer,dimension(2),parameter :: teeunits = [display_unit,histunit]
      !
      info = 1
      !
      do i=1,3
            MI(i,i) = 1.D0
      enddo
      !
      ! Introduce youself ;-)
      write(*,'(A)') 'AlamASR, $Rev$'
      !
      ! Open input file
      open(unit=dtaunit,file=trim(data_fname),status='old',form='formatted',iostat=ioerr)
      if ( ioerr /= 0) then
            write(*,*) 'Cannot open data file: ', trim(data_fname)
            stop
      endif
      read(dtaunit,fmt='(I5)',iostat=ioerr) npoints
      if ((ioerr /= 0) .or. (npoints <= 0)) then
            write(*,*) 'Wrong header of data file'
            stop
      endif
      !
      ! Open and initialize result files
      open(unit=ofunit,file=trim(outputPrefix)//'.asr',status='replace')
      !
#define OUTHEADER 'point','iter','eps_vM','Pnorm','totalP_vM','R','plast_pot','M','scal_s','||SonA||'
      !
      write(ofunit,700) OUTHEADER ! write header line
      open(unit=histunit,file=trim(outputPrefix)//'.hsr',status='replace')
      !      
      ! Convert angle from degs to rads
      fi1 = fi1 * deg2rad
      phi = phi * deg2rad
      fi2 = fi2 * deg2rad
      ! Calculate rotation matrix
      call KROTMAT(fi1,phi,fi2,Mrot)
      !
      vTotalP = 0.D0
      totalPnorm = 0.D0
      do  point = 1, npoints
            write(*,800)
            vP = 0.D0
            Pnorm = 0.D0 ! sum||P||
            normP = 0.D0 ! ||P||
            !! Acquire full stress tensor St
            St = 0.D0
            read(dtaunit,*) ! Read separator line
            do j=1,3
                  read(dtaunit,*) St(j,:)
            enddo
            ! Make sure the tensor is symmetrical
            St = 0.5*(St + transpose(St)) 
            Pressure = ((St(1,1) + St(2,2) + St(3,3))/3.0) * MI
            Stdev = St - Pressure
            !!! Print the input data:
            write(*,'(A)') 'Input stress tensor, original reference frame'
            write(*,400) 'Total stress', 'Deviatoric', 'Pressure'
            do j=1,3
                  write(*,411) St(j,:),Stdev(j,:),Pressure(j,:)
            enddo
            ! Rotate from original reference frame to superimposed coordinate system
            !
            Sm = matmul(transpose(Mrot),matmul(Stdev,Mrot))
            !            
            call KMAT2VEC5D(Sm,vS) 
            ! Enforce unit length of vS
            vS_norm = vec_norm2(vS)
            if (abs(vS_norm) < epsilon(0.D0)) then
                  write(*,*) 'Norm of the stress cannot be zero, skipping'
                  cycle
            endif
            vS = vS / vS_norm
            call KVEC5D2MAT(vS,Smn)
            ! Loop for evolution of texture            
            increment  = 1
            do 
                  call outputSeparator(teeunits)
                  !! -> Calculate corresponding strain rate vA
                  call multilevelYLP(vS,vA,vSonA,R,info,.true.,ylpCnf)
                  !
                  plast_pot = dot_product(vA, vSonA)
                  ! Calculate normalized stess
                  norm_sona = vec_norm2(vSonA)
                  scal_s = norm_sona / vS_norm
                  vSonAn = vSonA / vec_norm2(vSonA) 

                  ! Convert stresses and strain rates to [3x3] tensors
                  call KVEC5D2MAT(vA,D)
                  call KVEC5D2MAT(vSonA,SonA)
                  call KVEC5D2MAT(vSonAn,SmIdent)
                  !
                  ! Print vector form
                  call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
                  !
                  StonA = matmul(matmul(Mrot,SonA),transpose(Mrot))
                  StIdent = matmul(matmul(Mrot,SmIdent),transpose(Mrot))
                  Dt = matmul(matmul(Mrot,D),transpose(Mrot))
                  !! -> report the results to history file and to the screen
                  call outputIdentResults(teeunits)                  
                  !! -> Report the results to output file
                  write(ofunit,701) point, increment , root23*normP, Pnorm, root23*totalPnorm, R, plast_pot, ares%taylor_factors(1), scal_s, norm_sona
                  !
                  write(display_unit,710)
                  write(display_unit,700) OUTHEADER ! write header line
                  write(display_unit,701) point, increment , root23*normP, Pnorm, root23*totalPnorm, R, plast_pot, ares%taylor_factors(1), scal_s, norm_sona
                  write(display_unit,710)
                  !
                  ! Check termination condition: 
                  ! skip the rest if no texure update is requested, or if requested strain is exceeded.
                  if ((.not. update_texture) .or. (PNorm >= NormMax)) exit
                  !
                  !! -> Scale the vA in order to get ||vA|| = PNormIter
                  vDe = vA * (PNormIter / vec_norm2(vA))
                  normDe = vec_norm2(vDe)
                  write(*,'(A,1X,F12.6)') 'Norm of vDe = ', normDe 
                  ! Calculate strain increment for texture evolution           
                  call KVEC5D2MAT(vDe,De)
                  !
                  !! -> Impose De as ALAMEL input, advance the state of texture
                  !
                  ! Write history of deformations
                  call outputImposedStrain(teeunits)
                  !
                  ! Set input data for ALAMEL
                  acnf%simulCalls(1)%dgf = De
                  acnf%simulCalls(1)%keep_texture = .false.
                  acnf%simulCalls(1)%do_output = outputRequest
                  acnf%nSimulCalls = 1
                  call ALAMEL(3)       
                  !       
                  !! -> Calculate total strain
                  vP = vP + vDe
                  vTotalP = vTotalP + vDe
                  !
                  normP = vec_norm2(vP)
                  Pnorm = Pnorm + normDe
                  totalPnorm = vec_norm2(vTotalP)
                  increment = increment  + 1 
                  !
                  call outputStrainProgress(teeunits)
                  !
            enddo
                  
       enddo            

      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'

      close(ofunit)
      close(histunit)
      
      info = 0
      
      400 format(A,T40,A,T80,A)
      
      410 format('| SmScaled',T40,'| SmIdent',T80,'|SonA')
      411 format(3(F10.6,1X),T40,'|',3(F10.6,1X),'|',T80,3(F10.6,1X))

      420 format('| Dm')
      421 format(3(F10.6,1X))

      500 format(3(3(F10.6,1X),/))
      501 format(3(F10.6,1X),/,3(F10.6,1X),/,3(F10.6,1X))
      ! Formats for output file
      700 format(2(1X,A5),8(A12,1X))
      701 format(2(1X,I5),8(F12.6,1X))
      710 format(2('|',5('-')),'|',8(12('-'),'|'))
      !
      
      800 format(112('='))
      801 format(112('-'))
      
      ! Internal subroutines
      contains
      
            subroutine outputIdentResults(teeunits)
            implicit none
            integer,dimension(:),intent(in) :: teeunits
            !
            integer :: j, n
                  do j = 1,size(teeunits)
                        n = teeunits(j)
                        write(n,'(A,1X,I3,1X,A,1X,I3)') 'Point:',point,'Increment:',increment 
                        write(n,'(A)') 'In rotated reference frame:' 
                        call printIdentResultsT(n,Sm,SmIdent*vS_norm,SonA,D,info)
                        !
                        write(n,'(A)') 'In original reference frame:' 
                        call printIdentResultsT(n,Stdev,StIdent*vS_norm,StonA,Dt,info)
                  enddo
            end subroutine
            
            subroutine outputImposedStrain(teeunits)
            implicit none
            integer,dimension(:),intent(in) :: teeunits
            !
            integer :: j, n
                  do j = 1,size(teeunits)
                        n = teeunits(j)
                        write(n,'(A)') 'Strain to be imposed for texture evolution De = '
                        write(n,500) De
                        write(n,*)
                  enddo
            500 format(3(3(F10.6,1X),/))

            end subroutine
            
            subroutine outputStrainProgress(teeunits)
            implicit none
            integer,dimension(:),intent(in) :: teeunits
            double precision,dimension(3,3)           :: tmpP
            integer :: j, n
                  do j = 1,size(teeunits)
                        n = teeunits(j)
                        !
                        write(n,'(A)') 'Increment strain:'
                        write(n,'(A,1X,F12.6)') '||P|| =', normP
                        write(n,'(A,1X,F12.6)') 'sum||De|| =', Pnorm
                        call KVEC5D2MAT(vP,tmpP)
                        write(n,'(A)') 'P='
                        write(n,500) tmpP
                        write(n,'(A)') 'Total strain:'
                        write(n,'(A,1X,F12.6)') '||Ptot|| =', totalPnorm
                        call KVEC5D2MAT(vTotalP,tmpP)
                        write(n,'(A)') 'Ptot='
                        write(n,500) tmpP
                  enddo
            500 format(3(3(F10.6,1X),/))

                  
            end subroutine
            
            
            subroutine outputSeparator(teeunits)
            implicit none
            integer,dimension(:),intent(in) :: teeunits
            integer :: j, n
                  do j = 1,size(teeunits)
                        n = teeunits(j)
                        write(n,801)
                  enddo
                  801 format(112('-'))
            end subroutine
      end subroutine
      
end module
