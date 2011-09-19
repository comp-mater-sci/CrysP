!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2010-11-03
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file Alamq calculates plastic anisotropic properties, expressed in terms of q-values,
!>          directly from texture data, presented in form of SMT, CUR or CUB files.
!>
!
module alamQ
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use qrsTypes
use alamUtils
use commonConfig
implicit none

      double precision                          :: fi2min = 0.D0
      double precision                          :: fi2max =  acos(-1.D0)
      integer                                   :: nfis = 2
      double precision                          :: rho 

      logical                                   :: reuse_previous = .false., resuse_stainrate = .false.
      


contains

      subroutine AlamQ_ReadConfig(cnfunit,info)
      implicit none
      integer,intent(in)                        :: cnfunit
      integer,intent(out)                       :: info
      !
      integer :: ioerr
            info = -1
            ! Read parameters specific for the AlamQ module
            ! Read alamq-specific parameters
            read(cnfunit,fmt=*,iostat=ioerr)  fi2min, fi2max,  nfis 
            read(cnfunit,fmt=*,iostat=ioerr)  rho
            read(cnfunit,fmt='(2L2)',iostat=ioerr)  reuse_previous, resuse_stainrate
            if (ioerr /= 0) then
                  write(*,fmt=902) 'alamq'
                  return
            endif
            !
            ! Validate config values
            if (nfis < 1) then
                  write(*,*) 'Number of intervals cannot be smaller than 1' 
                  return
            endif
            !
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamQ_Run(info)
      implicit none
      integer,intent(out)                       :: info

      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: Dmcoord, Dtcoord, Smcoord,Stcoord, SmIdent, Xmcoord_resume, Xtcoord_resume
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
      double precision                          :: fi1,phi,fi2
      double precision                          :: SonA_len
      double precision                          :: R
      integer     :: i,j, npoints
      logical     :: useVMGuess
      double precision :: delta_fi2
      double precision,dimension(:),allocatable       :: residuals
      type(qrsData),dimension(:),allocatable          :: qrsvalues
      !
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91
      !
      info = 1
      !
      ! Print banner
      write(*,'(A)') 'Alamq: $Id$'
      !
      !
      npoints = nfis + 1
      ! 
      ! Print-out summary of the configuration 
      write(display_unit,fmt=fmtMsg2Other//'2(F8.3,1X))',iostat=ioerr) 'Angular range:', fi2min, fi2max
      write(display_unit,fmt=fmtMsg2Int,iostat=ioerr)   'Number of points:', npoints 
      write(display_unit,fmt=fmtMsg2Float,iostat=ioerr) 'Stress ratio', rho 
      !
      write(display_unit,fmt='(A,\)') 'Info:'
      if (reuse_previous) then
            if (resuse_stainrate) then
                  write(display_unit,'(1X,A,\)') 'Strain rate'
            else
                  write(display_unit,'(1X,A,\)') 'Stress'
            endif
            write(display_unit,'(1X,A)') 'from the previous solution will be re-used.'
      else
            write(display_unit,'(1X,A)') 'von Mises guess will be used.'
      endif
      
      ! Convert fi2min, fi2max from degs to rads
      fi2min = fi2min * deg2rad      
      fi2max = fi2max * deg2rad      
      
      open(unit=ofunit,file=trim(outputPrefix)//'.xqrs',buffered='no',iostat=ioerr)
      if (ioerr /= 0) then
            write(display_unit,fmt=952)
            return 
      endif
      write(ofunit,fmt=500) trim(acnf%texture%input_fname)
      write(ofunit,fmt=501)
       
      fi1 = 0.D0
      phi = 0.D0
      fi2 = 0.D0
      ! Make space for the results      
      allocate(qrsvalues(npoints), residuals(npoints))
      residuals = 0.D0
      
      delta_fi2 = (fi2max  - fi2min) / dble(nfis) 
      ! use von Mises guess as a default
      useVMGuess = .true.



      do i = 1,npoints
            write(*,900)
            write(*,'(/,A,1X,I4,1X,A,1X,F8.3,A,/)')'Point:',i,'fi2 =',fi2 * rad2deg, ' degs'
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Stcoord = 0.D0
            Stcoord(1,1) = dsqrt(3.D0/2.D0)*1.D0/dsqrt(rho**2-rho+1)
            Stcoord(2,2) = rho*Stcoord(1,1)

            ! Rotate from "tensile" to material coordinate system
            Smcoord = matmul(transpose(Mrot),matmul(Stcoord,Mrot))
            !            
            call KMAT2VEC5D(Smcoord,vS) 
            if ((reuse_previous) .AND. (i > 1))  then
                  ! Reuse previously stored result in new coordinate system
                  ! Type of result (strain rate or stress) is decided in line mared with (***)
                  ! Rotate Xtcoord_resume to new coordinate system
                  Xmcoord_resume = matmul(transpose(Mrot),matmul(Xtcoord_resume,Mrot))
                  ! Set starting point
                  call KMAT2VEC5D(Xmcoord_resume,vA)
                  vA = vA / sqrt(dot_product(vA,vA))
                  ! Disable Von Mises guess in multilevelYLP: vA will be used as a starting point
                  useVMGuess = .false.
            endif
            !
            call multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,ylpCnf)
            residuals(i) = R
            ! Calculate normalized stess
            SonA_len = sqrt(dot_product(vSonA,vSonA)) 
            vSonAn = vSonA / SonA_len
            write(*,('(/)'))
            write(*,'(A,T30,5F10.6)') 'Requested stress:', vS
            write(*,'(A,T30,5F10.6)') 'Identified stress:',vSonAn       
            write(*,('(/)'))
            write(*,*) 'Identified stress (before normalization):'
            write(*,'(5F10.6)') vSonA
            
            ! Convert AONSET vector to tensor form
            call KVEC5D2MAT(vA,Dmcoord)

            call KVEC5D2MAT(vSonAn,SmIdent)
            
            write(*,400)
            do j=1,3
                  write(*,401) Smcoord(j,:),SmIdent(j,:),Dmcoord(j,:)
            enddo
            
            ! Rotate back to the "tensile test" coordinate system   
            Dtcoord = matmul(matmul(Mrot,Dmcoord),transpose(Mrot))
            !
            !(***) Prepare next iteration if re-using is requested.
            if (reuse_previous) then
                  ! Re-used data are always in "tensile" coordinate system (initial coordinate system) 
                  if (resuse_stainrate) then
                        Xtcoord_resume = Dtcoord
                  else
                        ! Rotate stresses to "tensile" coordinate system 
                        Xtcoord_resume = matmul(matmul(Mrot,SmIdent),transpose(Mrot))
                  endif
            endif
            !            
            ! Calculate output variables
            if ( abs(Dtcoord(3,3)) >= epsilon(0.D0) ) then
                  qrsvalues(i)%rvalue =  Dtcoord(2,2) / Dtcoord(3,3)
                  !qv = -Dtcoord(2,2) / Dtcoord(1,1)
                  qrsvalues(i)%qvalue = qrsvalues(i)%rvalue / (1.D0 + qrsvalues(i)%rvalue)
                  qrsvalues(i)%svalue = SonA_len
            else
                  info = 4
            endif
            
            write(*,402) 'qvalue=', qrsvalues(i)%qvalue
            write(*,402) 'rvalue=', qrsvalues(i)%rvalue
            write(*,402) '||SonA||=',qrsvalues(i)%svalue
            !
            write(ofunit,fmt=450) rad2deg * fi2, rho, qrsvalues(i), residuals(i)

            !
            ! Next step
            fi2 = fi2 + delta_fi2
      enddo            

      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'

      fi2 = 0
      do i=1,npoints
            write(*,fmt=450) rad2deg * fi2, rho, qrsvalues(i), residuals(i)
            fi2 = fi2 + delta_fi2
      enddo

      info = 0
      
      400 format('| Smcoord',T40,'| SmIdent',T80,'|Dmcoord')
      401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))
      402 format(A,T10,F10.6)
      450 format(F10.6,1X,F6.3,1X,3(F12.8,1X),F12.8) ! phi2, rho, (q,r,s,), residual
      ! Format for header file
      500 format('#Material:',1X,A,/,'#Generated by Alamq $Revision$')
      ! Format for title line
      501 format('#Angle',T11,'rho',T24,'q-value',T37,'r-value',T50,'s-value',T63,'residual')
      ! Format for screen separator
      900 format(112('='))
      
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
       

      
      end subroutine


end module
