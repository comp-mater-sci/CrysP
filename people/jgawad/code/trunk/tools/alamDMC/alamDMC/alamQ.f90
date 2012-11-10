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
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use qrsTypes
use alamUtils
use commonConfig
use commonUtils
use fngMathUtils 


implicit none

      type :: QConfig
            class(range_type),pointer                 :: ptr_range
            
            double precision                          :: rho = 0.D0
            
            logical                                   :: calculate_Mfactor = .false.

            logical                                   :: reuse_previous = .false.
            logical                                   :: resuse_stainrate = .false.
            logical                                   :: fold_symmetry = .false.
      end type


contains

      subroutine AlamQ_ReadConfig(cnf,cnfunit,info)
      implicit none
      class(QConfig),intent(inout)              :: cnf
      integer,intent(in)                        :: cnfunit
      integer,intent(out)                       :: info
      !
      integer :: ioerr
      
            info = -1
            ! Read parameters specific for the AlamQ module
            ! Read alamq-specific parameters
            cnf%ptr_range => rangeFromConfig(cnfunit,info)
            if ( (info /= 0) .or. (.not. associated(cnf%ptr_range)) ) return
            info = -1
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%rho
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%calculate_MFactor
            if (ioerr /= 0) return
            read(cnfunit,fmt='(2L2)',iostat=ioerr)  cnf%reuse_previous, cnf%resuse_stainrate
            if (ioerr /= 0) return
            read(cnfunit,fmt='(L2)',iostat=ioerr)  cnf%fold_symmetry
            if (ioerr /= 0) return
            !
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamQ_Run(cnf,info)
      implicit none
      class(QConfig),intent(inout)              :: cnf
      integer,intent(out)                       :: info

      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: Dmcoord, Dtcoord, Smcoord,Stcoord, SmIdent, Xmcoord_resume, Xtcoord_resume
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
      double precision                          :: fi1,phi,fi2
      double precision                          :: SonA_len, scal_s
      double precision                          :: R
      integer     :: i,j, k, npoints
      logical     :: useVMGuess
      double precision,dimension(:),allocatable       :: residuals,mfactors,phis
      type(qrsData),dimension(:),allocatable          :: qrsvalues
      !
      integer                 :: left, right, stride
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91
      !
      info = 1
      !
      ! Print banner
      write(display_unit,'(A)') 'Alamq: $Rev$'
      !
      !
      npoints = cnf%ptr_range%size()
      ! 
      ! Print-out summary of the configuration 
!      write(display_unit,fmt=fmtMsg2Other//'2(F8.3,1X))',iostat=ioerr) 'Angular range:', cnf%fi2min, cnf%fi2max
      write(display_unit,fmt=fmtMsg2Int,iostat=ioerr)   'Number of points:', npoints 
      write(display_unit,fmt=fmtMsg2Float,iostat=ioerr) 'Stress ratio', cnf%rho 
      !
      write(display_unit,fmt='(A,\)') 'Info:'
      if (cnf%reuse_previous) then
            if (cnf%resuse_stainrate) then
                  write(display_unit,'(1X,A,\)') 'Strain rate'
            else
                  write(display_unit,'(1X,A,\)') 'Stress'
            endif
            write(display_unit,'(1X,A)') 'from the previous solution will be re-used.'
      else
            write(display_unit,'(1X,A)') 'von Mises guess will be used.'
      endif
     
      open(unit=ofunit,file=trim(outputPrefix)//'.xqrs',iostat=ioerr)
      if (ioerr /= 0) then
            write(display_unit,fmt=952)
            return 
      endif
      ! write(ofunit,fmt=500) trim(acnf%texture%input_fname)
      write(ofunit,fmt=700)
      !
      ! Apply correction to the configuration of the search procedure:
      ! there will be no need to use the full model in the last call unless 
      ! the average Taylor factor is requested.
      ylpCnf%evaluate_full_model  = cnf%calculate_MFactor
      

      ! Make space for the results      
      allocate(qrsvalues(npoints), residuals(npoints),mfactors(npoints),phis(npoints))
      residuals = 0.D0
      mfactors = 0.D0
      
      fi1 = 0.D0
      phi = 0.D0
      
      ! use von Mises guess as a default
      useVMGuess = .true.
      i = 0
      do while (cnf%ptr_range%next(fi2))
            i = i + 1            
            write(*,800)
            write(*,'(/,A,1X,I4,1X,A,1X,F8.3,A,/)')'Point:',i,'fi2 =',fi2, ' degs'
            !
            phis(i) = fi2
            !
            fi2 = deg2rad(fi2)
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            ! Set Stcoord in such way that deviatoric part is of unit length
            Stcoord = 0.D0
            Stcoord(1,1) = dsqrt(3.D0/2.D0)*1.D0/dsqrt(cnf%rho**2-cnf%rho+1)
            Stcoord(2,2) = cnf%rho*Stcoord(1,1)

            ! Rotate from "tensile" to material coordinate system
            Smcoord = rotateSRTensorTo(Stcoord, Mrot)
            
            !            
            call KMAT2VEC5D(Smcoord,vS) 
            if ((cnf%reuse_previous) .AND. (i > 1))  then
                  ! Reuse previously stored result in new coordinate system
                  ! Type of result (strain rate or stress) is decided in line mared with (***)
                  ! Rotate Xtcoord_resume to new coordinate system
                  Xmcoord_resume = rotateSRTensorTo(Xtcoord_resume,Mrot)
                  ! Set starting point
                  call KMAT2VEC5D(Xmcoord_resume,vA)
                  vA = vA / vec_norm2(vA)
                  ! Disable Von Mises guess in multilevelYLP: vA will be used as a starting point
                  useVMGuess = .false.
            endif
            !
            call multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,ylpCnf)
            if (info /= 0) then
                  write(display_unit,fmt=960)
                  exit
            endif
            
            residuals(i) = R
            ! 
            if (cnf%calculate_MFactor) then
                  call getTaylorFactor(1,mfactors(i),info)
                  if (info /= 0) then
                        write(*,980)
                        exit
                  endif
            endif
            ! Calculate normalized stess
            SonA_len = vec_norm2(vSonA)
            scal_s = SonA_len / vec_norm2(vS)
            vSonAn = vSonA / SonA_len
            !
            call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
            ! Convert AONSET vector to tensor form
            call KVEC5D2MAT(vA,Dmcoord)

            call KVEC5D2MAT(vSonAn,SmIdent)
            
            write(*,400)
            do j=1,3
                  ! would be just:  write(*,401) Smcoord(j,:),SmIdent(j,:),Dmcoord(j,:)
                  write(*,401) (Smcoord(j,k),k=1,3), (SmIdent(j,k),k=1,3), (Dmcoord(j,k), k=1,3)
            enddo
            
            ! Rotate back to the "tensile test" coordinate system  
            Dtcoord = rotateSRTensorFrom(Dmcoord, Mrot)
            !
            !(***) Prepare next iteration if re-using is requested.
            if (cnf%reuse_previous) then
                  ! Re-used data are always in "tensile" coordinate system (initial coordinate system) 
                  if (cnf%resuse_stainrate) then
                        Xtcoord_resume = Dtcoord
                  else
                        ! Rotate stresses to "tensile" coordinate system 
                        Xtcoord_resume = rotateSRTensorFrom(SmIdent, Mrot)
                  endif
            endif
            !            
            ! Calculate output variables
            qrsvalues(i) = calculateQRS(Dtcoord,scal_s)
            !            
            write(display_unit,fmt=701)
            write(display_unit,fmt=700)
            write(display_unit,fmt=710) phis(i), cnf%rho, qrsvalues(i), mfactors(i),residuals(i)
            write(display_unit,fmt=701)
            !
            info = 0
      enddo
      ! 
      ! End of the main loop, check what's the status of the last operation
      if (info /= 0) return
      !
      ! Write complete output to the terminal
      write(*,800)
      write(display_unit,fmt=700)
      do i=1,npoints
            write(*,fmt=710) phis(i), cnf%rho, qrsvalues(i), mfactors(i), residuals(i)
      enddo
      ! Write output file
      if (cnf%fold_symmetry) then
            ! Average over symmetric positions
            left = 1
            right = npoints
            do 
                  if (left > right) exit
                  stride = right - left
                  if (stride == 0) stride = 1
                  write(ofunit,fmt=710) phis(left), cnf%rho,                      &
                                        avgQRS(qrsvalues(left:right:stride)),     &
                                        average(mfactors(left:right:stride) ),    &
                                        average(residuals(left:right:stride) )
                  left = left + 1
                  right = right -1
            enddo
      else
            ! Output complete set of points
            do i=1,npoints
                  write(ofunit,fmt=710) phis(i), cnf%rho, qrsvalues(i), mfactors(i),residuals(i)
            enddo
      endif
      !
      deallocate(qrsvalues, residuals,mfactors,phis)
      close(ofunit)
      !
      info = 0
      !
      400 format('| Smcoord',T40,'| SmIdent',T80,'|Dmcoord')
      401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))
      ! Format for header file
      500 format('#Material:',1X,A,/,'#Generated by Alamq $Revision$')
      ! Format for title line
      700 format('#Angle',T15,'rho',T24,'q-value',T37,'r-value',T50,'s-value',T62,'M-factor',T75,'residual')
      701 format('|',9('-'),'|',6('-'),'|',4(12('-'),'|'),12('-'),'|')
      710 format(F10.4, 1X ,F6.3  ,1X, 4(F12.8,1X), F12.8) ! phi2, rho, (q,r,s,M), residual

#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS
       

      
      end subroutine


end module
