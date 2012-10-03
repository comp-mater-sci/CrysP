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

      integer,parameter                         :: nbase = 3
      type :: YldConfig
            
            double precision                          :: theta_min = 0.D0, theta_max = 360.D0
            double precision                          :: dtheta = 10.D0      
      
            double precision                          :: w_min = 1.D0, w_max = 1.D0, dw = 0.D0
            
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
            read(cnfunit,fmt=*,iostat=ioerr)  cnf%theta_min, cnf%theta_max, cnf%dtheta
            if (ioerr /= 0) return
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
            read(cnfunit,fmt=*,iostat=ioerr) cnf%w_min, cnf%w_max, cnf%dw
            if (ioerr /= 0) return
            read(cnfunit,fmt=*,iostat=ioerr) cnf%do_scaling, cnf%scaling_vector
            if (ioerr /= 0) return
            !
            ! Check if the requested range description
            if (cnf%theta_min + cnf%dtheta < cnf%theta_min) return
            !
            ! Convert the angles into radians
            cnf%theta_min = cnf%theta_min * deg2rad
            cnf%theta_max = cnf%theta_max * deg2rad
            cnf%dtheta = cnf%dtheta * deg2rad
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
            w = cnf%w_min
            do while (w < cnf%w_max)
            !
                  ! Loop over the range of theta angles
                  theta = cnf%theta_min
                  do while (theta <= cnf%theta_max)
                        write(*,800)
                        !
                        write(display_unit,fmt=200)
                        write(display_unit,fmt=201) (theta * rad2deg)
                        write(display_unit,fmt=200)
            
                        ! Combine the base vectors
                        Sm = Vec6ToMat33(cnf%base_vectors(:,1)*cos(theta) + cnf%base_vectors(:,2)*sin(theta) & 
                                         + w*cnf%base_vectors(:,3))
                        !                  
                        if (findSolution() /= 0) cycle
                        !
                        write(display_unit,fmt=510)
                        write(display_unit,fmt=500) 'theta', 'S', 'S_rel', 'W' 
                        write(display_unit,fmt=501) theta*rad2deg, scal_s, scal_s_rel, plast_pot
                        write(display_unit,fmt=510)

                        ! write output & advance theta
                        write(ofunit,fmt=701)   theta, scal_s, scal_s_rel, norm_sona, plast_pot, &
                                                scal_s_rel * cos(theta), scal_s_rel * sin(theta), w
                        theta = theta + cnf%dtheta
                  enddo
                  write(ofunit,'(A)') ''
                  w = w + cnf%dw
            enddo
            !
            close(ofunit)
      
            info = 0
      !
      200 format(28('-'))
      201 format('Theta angle =',T20,F8.3) 
      400 format(A,T40,A,T80,A)
      500 format(1X,4(A10,'|'))
      501 format(F10.3,1X,3(E12.5,1X))
      510 format('|',4(10('-'),'|'))
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
