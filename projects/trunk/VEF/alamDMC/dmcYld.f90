!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2012-06-06 (under the name alamYld)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> Yield locus calculations
module dmcYld
use alamYLP
use dmcUtils
use commonConfig
use dmcBasicModule
use criAlgorithm
use criErrcodes
use criRange
use criLog
implicit none

private

      integer,parameter                               :: nbase = 3
      type,extends(BasicModule) :: YldModule
            
            class(range_type),pointer                 :: ptr_theta_range
            
            class(range_type),pointer                 :: ptr_w_range
            
            double precision,dimension(sr_symm_voigt_dim,nbase) :: base_vectors = reshape( &
                                                [1., 0., 0., 0., 0., 0., & ! First base vector
                                                 0., 1., 0., 0., 0., 0., & ! second base vector
                                                 0., 0., 0., 0., 0., 0.], & ! offset vector (zeros)
                                                [sr_symm_voigt_dim,nbase])
      
            logical                                   :: do_scaling = .true.

            double precision,dimension(sr_symm_voigt_dim) :: scaling_vector = &
                                                [1., 0., 0., 0., 0., 0.]
      
            logical                                   :: normalizeSm = .false.
            
      contains
      
            procedure,pass(this)    :: readConfig => YldModule_ReadConfig
            procedure,pass(this)    :: printConfig => YldModule_printConfig
            procedure,pass(this)    :: run => YldModule_run
            
      end type
      
      public YldModule
      
      
      type :: yldResult
            double precision :: theta = 0.D0 
            double precision :: w = 0.D0
            double precision :: scal_s = 0.D0
            double precision :: scal_s_rel = 0.D0
            double precision :: norm_sona = 0.D0
            double precision :: dotWonA = 0.D0
            type(pair_double) :: scal_s_rel_cart = pair_double(0.D0,0.D0)
            type(pair_double) :: normal_cart = pair_double(0.D0,0.D0)
            double precision :: beta = 0.D0
            double precision :: residual = 0.D0
      end type
      
contains

      integer function YldModule_ReadConfig(this,cnfunit) result(info)
      implicit none
      class(YldModule),intent(inout)            :: this
      integer,intent(in)                        :: cnfunit
      !
      integer :: i
      double precision :: norm
      logical :: normalize, use_default_settings
      !
            info = BasicModule_ReadConfig(this,cnfunit)
            if (info /= criSuccess) return
            ! Read parameters specific for the dmcYld program
            this%ptr_theta_range => rangeFromConfig(cnfunit,info)
            if ( (info /= criSuccess) .or. (.not. associated(this%ptr_theta_range)) ) return
            if (.not. readValue(cnfunit, use_default_settings)) return
            if (use_default_settings) then
                  ! use the defaults:
                  allocate(uniformRange :: this%ptr_w_range)
            else
                  info = criErr_BadArgs
                  this%base_vectors = 0.D0
                  if (.not. readValue(cnfunit, normalize)) return
                  do i=1,nbase
                        if (.not. readValue(cnfunit, this%base_vectors(:,i))) return
                        if (normalize) then
                              norm = norm2(this%base_vectors(:,i))
                              if (norm > 0.D0) this%base_vectors(:,i)  = this%base_vectors(:,i) / norm
                        endif
                  enddo
                  !
                  if (.not. readValue(cnfunit, this%normalizeSm)) return
                  this%ptr_w_range => rangeFromConfig(cnfunit,info)
                  if ( (info /= 0) .or. (.not. associated(this%ptr_w_range)) ) return
                  if (.not. readValue(cnfunit, this%do_scaling)) return
                  if (this%do_scaling) then
                        if (.not. readValue(cnfunit, this%scaling_vector)) return
                  endif
            endif
            !
            ! Override the requests for outputs: 
            this%altay%output_config%nfile = 0   ! texture
            this%altay%output_config%npebp = 0   ! KOST1x state
            this%output%outputRequest = .false.       ! idem.
            !
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end function


      integer function YldModule_printConfig(this, outunit) result (info)
      implicit none
      class(YldModule),intent(in)         :: this
      integer,intent(in)                  :: outunit
      !
      character(len=6),dimension(nbase)    :: veclabels = [ character(len=6) :: 'base','base','offset' ]
      integer :: i
      !
            info = BasicModule_printConfig(this, outunit)
            if (info /= 0) return
            info = criErr_BadArgs
            ! Introduce youself ;-)
            write(outunit,'(A)') 'UDSA, $Rev$'
            !
            if (doLogging(criLogInfo,this%output%verbosity)) then
                  !
                  ! Introduce youself ;-)
                  write(display_unit,'(A)') 'dmcYld, $Rev$'
                  do i=1,nbase
                        write(display_unit,'(A,1x,A,6(F6.2,1X))') veclabels(i),'vector:',this%base_vectors(:,i)
                  enddo
                  !
                  write(display_unit,'(A,1X,L1)') 'Normalization of the full Sm tensor:',this%normalizeSm
                  if (this%do_scaling) write(display_unit,'(A,1X,6(F6.2,1X))') 'Scaling by yield stress for:', this%scaling_vector
            endif
            info = criSuccess
      !
      end function
            
            
      subroutine YldModule_Run(this,info)
      implicit none
      class(YldModule),intent(inout)            :: this
      integer,intent(out)                       :: info
      
      ! Base tensors
      double precision,dimension(3,3)           :: Sm
      double precision                          :: theta, w
      double precision                          :: iunilen ! Inverse of the length of the deviatoric part of uniaxial tensile stress

      !
      double precision                          :: dotWonA, scal_s, norm_sona, vS_norm, scal_s_rel,Sm_norm
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
      double precision                          :: R
      type(yldResult),dimension(:),allocatable  :: yldRes
      double precision,dimension(sr_symm_voigt_dim) :: sigma_vector
      class(range_type),allocatable             :: theta_range
      !
      integer                 :: ioerr,i,npoints
      integer,parameter       :: cnfunit = 90, ofunit = 91
      !
      integer :: posA, posB
      logical :: first_run
      double precision,parameter :: beta = 0.D0
      !
            info = criErr_BadArgs
            if (.not. (associated(this%ptr_theta_range) .and. associated(this%ptr_w_range)))  return
            !
            npoints = this%ptr_theta_range%size()
            if (npoints <= 0) then
                  write(display_unit,fmt='(A)') 'Cannot run using empty range of theta angles.'
                  return 
            endif
            !
            ! Open the main output file
            open(unit=ofunit,file=trim(this%output%outputPrefix)//'.xyld',iostat=ioerr)
            if (ioerr /= 0) then
                  write(display_unit,fmt=952)
                  return 
            endif
            !
            ! Fix the configuration: no need for anything except for the stresses.
            this%ylp%evaluate_full_model = .false.
            !
            iunilen = 1.D0
            if (this%do_scaling) then
                  Sm =  Vec6ToMat33(this%scaling_vector)
                  if (norm2(Sm) < epsilon(0.D0)) then
                        write(display_unit,fmt=900) 'Norm of the input stress for scaling cannot be zero'
                        return
                  endif
                  ! Run the identification
                  if (findSolution() /= 0) then
                        write(display_unit,fmt=900) 'Cannot find solution for the scaling stress'
                        return
                  endif
                  
                  if (abs(scal_s) < epsilon(0.D0)) then
                        write(display_unit,fmt=900) 'Identification results in zero-length stress tensor.'
                        return
                  endif
                  iunilen = 1.D0 / scal_s
            endif
            !
            allocate(yldRes(npoints))
            !
            first_run = .true.
            do while (this%ptr_w_range%next(w))
                  ! Clone theta range
                  allocate(theta_range, source=this%ptr_theta_range)
                  !
                  ! Loop over the range of theta angles
                  i = 1
                  do while (theta_range%next(theta))
                        
                        if (doLogging(criLogDebug,this%output%verbosity)) then
                              write(display_unit,800)
                              !
                              write(display_unit,fmt=200)
                              write(display_unit,fmt=201) theta
                              write(display_unit,fmt=200)
                        endif
                        !
                        theta = deg2rad(theta) 
                        ! Combine the base vectors
                        ! Note: explicit temporary sigma_vector prevents runtime warning about
                        !       a temporary created in a call to Vec6ToMat33
                        sigma_vector = this%base_vectors(:,1)*cos(theta) + this%base_vectors(:,2)*sin(theta) & 
                                       + w*this%base_vectors(:,3)
                        Sm = Vec6ToMat33(sigma_vector)
                        !                  
                        if (findSolution() /= 0) cycle
                        !
                        if (doLogging(criLogInfo,this%output%verbosity)) then
                              write(display_unit,fmt=510)
                              write(display_unit,fmt=500) 'theta', 'S', 'S_rel', 'dotW(A)' 
                              write(display_unit,fmt=501) rad2deg(theta), scal_s, scal_s_rel, dotWonA
                              write(display_unit,fmt=510)
                        endif
                        yldRes(i) = yldResult(rad2deg(theta), w, scal_s, scal_s_rel, norm_sona, dotWonA, &
                                              pair_double(scal_s_rel * cos(theta), scal_s_rel * sin(theta)),&
                                              pair_double(0.D0,0.D0), beta, R)
                        
                        i = i + 1
                  enddo
                  deallocate(theta_range)
                  !
                  ! Post-process the results
                  npoints = size(yldRes)
                  do i = 1, npoints
                        ! Get the positions of the bracketing points:
                        posA = merge(npoints-1,i - 1,i == 1)
                        posB = merge(2,i + 1, i == npoints)
                        ! write(display_unit,*) posA,i,posB
                        call getNormalVector2D(yldRes(posA)%scal_s_rel_cart, yldRes(posB)%scal_s_rel_cart, &
                                               1.D0, yldRes(i)%normal_cart, yldRes(i)%beta)
                        yldRes(i)%beta = rad2deg(yldRes(i)%beta)
                  enddo
                  !
                  call writeYldResults(ofunit,yldRes,info,write_header=first_run)
                  first_run = .false.
            enddo
            !
            close(ofunit)
            info = criSuccess
      !
      200 format(28('-'))
      201 format('Theta angle =',T20,F8.3) 
      400 format(A,T40,A,T80,A)
      500 format(1X, A10,    '|',3(A12,'|'))
      501 format(1X, F10.3,  1X, 3(E12.5,1X))
      510 format('|',10('-'),'|',3(12('-'),'|'))
      
      !
#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS

      contains 
      
            integer function findSolution() result(info)
            implicit none
            
                  info = -1
                  Sm_norm = norm2(Sm)
                  if (Sm_norm < epsilon(0.D0)) then
                        write(display_unit,fmt=900) 'Norm of the stress cannot be zero, skipping'
                        return
                  endif      
                  if (this%normalizeSm) Sm = Sm / Sm_norm
            
                  ! Convert to 5D space
                  vS = tens2vec5D(Sm)
                  ! Enforce unit length of vS
                  vS_norm = vec_norm2(vS)
                  vS = vS / vS_norm

                  !! -> Calculate corresponding strain rate vA
                  call multilevelYLP(vS,vA,vSonA,R,info,.true.,this%ylp,verbose=this%output%verbosity)
                  !
                  dotWonA = dot_product(vA, vSonA)
                  ! Calculate normalized stess
                  norm_sona = vec_norm2(vSonA)
                  scal_s = norm_sona / vS_norm
                  vSonAn = vSonA / vec_norm2(vSonA) 
                  ! Print vector form
                  if (this%output%verbosity > 1) call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)
                  scal_s_rel = scal_s * iunilen
                  info = 0
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
            end function


      end subroutine
      
      
      subroutine writeYldResults(ounit,res,info,write_header)
      implicit none
      integer,intent(in)                        :: ounit
      type(yldResult),dimension(:),intent(in)   :: res
      integer,intent(out)                       :: info
      logical,intent(in),optional               :: write_header
      !
      integer :: i,ierr
      integer,parameter :: column_width = 18, ncolumns = 12
      character(len=column_width),dimension(ncolumns),parameter  :: column_labels = [ character(len=column_width) :: &
            'theta', 'w', 'sigma', 'sigma_scaled', 'S','dotW', 'sigma_x', 'sigma_y', 'dsigma_x', 'dsigma_y', 'beta', 'residual']
      !
            info = criErr_IOWrite
            ! Write the header
            if (optionalDefault(write_header,.false.)) then
                  write(ounit,fmt=700,iostat=ierr) (centered(i,column_width),  i = 1, ncolumns)
                  if (ierr /= 0) return
                  write(ounit,fmt=701,iostat=ierr) (centered(column_labels(i)),i = 1, ncolumns)
                  if (ierr /= 0) return
            endif
            !
            do i = 1, size(res)
                 write(ounit,fmt=710,iostat=ierr) res(i) 
                 if (ierr /= 0) exit
            enddo
            write(ounit,fmt=720)
            if (ierr == 0) info = criSuccess
            !            
            ! Formats for output file
            700 format('#',12(A15,1X)) 
            701 format(1X, 12(A15,1X)) 
            710 format(1X, 12(E15.8,1X),4(F15.8,1X))
            720 format(/) ! Double empty line
      !
      end subroutine
      
end module
