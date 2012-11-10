!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2012-06-06
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> ALAMel Yield
!>
module alamYld
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamUtils
use commonConfig
implicit none

private

      integer,parameter                               :: nbase = 3
      type :: YldConfig
            
            class(range_type),pointer                 :: ptr_theta_range
            
            class(range_type),pointer                 :: ptr_w_range
            
            double precision,dimension(nSymTensComps,nbase)       :: base_vectors = 0.D0
      
            logical                                   :: do_scaling = .false.

            double precision,dimension(nSymTensComps) :: scaling_vector = 0.D0
      
            logical                                   :: normalizeSm = .false.
      end type
      
      public YldConfig,AlamYld_ReadConfig, AlamYld_Run
contains

      subroutine AlamYld_ReadConfig(cnf,cnfunit,info)
      implicit none
      class(YldConfig),intent(inout)            :: cnf
      integer,intent(in)                        :: cnfunit
      integer,intent(out)                       :: info
      !
      integer :: ioerr,i
      double precision :: norm
      logical :: normalize
            info = -1
            ! Read parameters specific for the alamASR program
            cnf%ptr_theta_range => rangeFromConfig(cnfunit,info)
            if ( (info /= 0) .or. (.not. associated(cnf%ptr_theta_range)) ) return
            cnf%base_vectors = 0.D0
            do i=1,nbase
                  normalize = .false.
                  read(cnfunit,fmt=*,iostat=ioerr) normalize, cnf%base_vectors(:,i)
                  if (ioerr /= 0)  exit
                  if (normalize) then 
                        norm = norm2(cnf%base_vectors(:,i))
                        if (norm > 0.D0) cnf%base_vectors(:,i)  = cnf%base_vectors(:,i) / norm
                  endif
            enddo
            if (ioerr /= 0) return
            read(cnfunit,fmt='(L)',iostat=ioerr) cnf%normalizeSm
            if (ioerr /= 0) return
            cnf%ptr_w_range => rangeFromConfig(cnfunit,info)
            if ( (info /= 0) .or. (.not. associated(cnf%ptr_w_range)) ) return
            read(cnfunit,fmt=*,iostat=ioerr) cnf%do_scaling, cnf%scaling_vector
            if (ioerr /= 0) return
            !
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamYld_Run(cnf,info)
      implicit none
      class(YldConfig),intent(inout)            :: cnf
      integer,intent(out)                       :: info      
      
      ! Base tensors
      double precision,dimension(3,3)           :: Sm
      double precision                          :: theta, w
      double precision                          :: iunilen ! Inverse of the length of the deviatoric part of uniaxial tensile stress

      !
      double precision                          :: plast_pot, scal_s, norm_sona, vS_norm, scal_s_rel,Sm_norm
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
      double precision                          :: R
      !
      integer                 :: ioerr,i
      integer,parameter       :: cnfunit = 90, ofunit = 91
      character(len=6),dimension(nbase)    :: veclabels = [ character(len=6) :: 'base','base','offset' ]
      !
            info = 1
            if (.not. (associated(cnf%ptr_theta_range) .and. associated(cnf%ptr_w_range)))  return
            !
            ! Introduce youself ;-)
            write(display_unit,'(A)') 'AlamYld, $Rev$'
            do i=1,nbase
                  write(display_unit,'(A,1x,A,6(F6.2,1X))') veclabels(i),'vector:',cnf%base_vectors(:,i)
            enddo
            write(display_unit,'(A,1X,L1)') 'Normalization of the full Sm tensor:',cnf%normalizeSm
            if (cnf%do_scaling)   write(display_unit,'(A,1X,6(F6.2,1X))') 'Scaling by yield stress for:', cnf%scaling_vector
            ! Open the main output file
            open(unit=ofunit,file=trim(outputPrefix)//'.xyld',iostat=ioerr)
            if (ioerr /= 0) then
                  write(display_unit,fmt=952)
                  return 
            endif
            write(ofunit,fmt=700) '#Theta', 'scal_S', 'scal_S_rel', '||SonA||', 'W', 'S1_rel', 'S2_rel', 'w'
            !
            ! Fix the configuration: no need for anything except for the stresses.
            ylpCnf%evaluate_full_model = .false.
            !
            iunilen = 1.D0
            if (cnf%do_scaling) then
                  Sm =  Vec6ToMat33(cnf%scaling_vector)
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
            do while (cnf%ptr_w_range%next(w))
                  !
                  ! Loop over the range of theta angles
                  do while (cnf%ptr_theta_range%next(theta))
                        write(*,800)
                        !
                        write(display_unit,fmt=200)
                        write(display_unit,fmt=201) theta
                        write(display_unit,fmt=200)
                        !
                        theta = deg2rad(theta) 
                        ! Combine the base vectors
                        Sm = Vec6ToMat33(cnf%base_vectors(:,1)*cos(theta) + cnf%base_vectors(:,2)*sin(theta) & 
                                         + w*cnf%base_vectors(:,3))
                        !                  
                        if (findSolution() /= 0) cycle
                        !
                        write(display_unit,fmt=510)
                        write(display_unit,fmt=500) 'theta', 'S', 'S_rel', 'W' 
                        write(display_unit,fmt=501) rad2deg(theta), scal_s, scal_s_rel, plast_pot
                        write(display_unit,fmt=510)

                        ! write output & advance theta
                        write(ofunit,fmt=701)   theta, scal_s, scal_s_rel, norm_sona, plast_pot, &
                                                scal_s_rel * cos(theta), scal_s_rel * sin(theta), w
                        
                  enddo
                  write(ofunit,'(A)') ''
            enddo
            !
            close(ofunit)
      
            info = 0
      !
      200 format(28('-'))
      201 format('Theta angle =',T20,F8.3) 
      400 format(A,T40,A,T80,A)
      500 format(1X, A10,    '|',3(A12,'|'))
      501 format(1X, F10.3,  1X, 3(E12.5,1X))
      510 format('|',10('-'),'|',3(12('-'),'|'))
      ! Formats for output file
      700 format(8(A12,1X)) 
      701 format(8(F12.6,1X))
      
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
                  if (cnf%normalizeSm) Sm = Sm / Sm_norm
            
                  ! Convert to 5D space, note that Sm becomes deviatoric after this call: 
                  call KMAT2VEC5D(Sm,vS) 
                  ! Enforce unit length of vS
                  vS_norm = vec_norm2(vS)
                  vS = vS / vS_norm

                  !! -> Calculate corresponding strain rate vA
                  call multilevelYLP(vS,vA,vSonA,R,info,.true.,ylpCnf)
                  !
                  plast_pot = dot_product(vA, vSonA)
                  ! Calculate normalized stess
                  norm_sona = vec_norm2(vSonA)
                  scal_s = norm_sona / vS_norm
                  vSonAn = vSonA / vec_norm2(vSonA) 

                  ! Print vector form
                  call printIdentResults(display_unit,vS,vA,vSonA,vSonAn,R,info)

                  scal_s_rel = scal_s * iunilen
                  info = 0
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
            end function


      end subroutine
      
end module
