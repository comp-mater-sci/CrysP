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
use fngAlgorithm
use fngErrcodes
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
      
      
      type :: yldResult
            double precision :: theta = 0.D0 
            double precision :: w = 0.D0
            double precision :: scal_s = 0.D0
            double precision :: scal_s_rel = 0.D0
            double precision :: norm_sona = 0.D0
            double precision :: plast_pot = 0.D0
            type(pair_double) :: scal_s_rel_cart = pair_double(0.D0,0.D0)
            type(pair_double) :: normal_cart = pair_double(0.D0,0.D0)
            double precision :: beta = 0.D0
            double precision :: R = 0.D0
      end type
      
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
      type(yldResult),dimension(:),allocatable  :: yldRes
      !
      integer                 :: ioerr,i,npoints
      integer,parameter       :: cnfunit = 90, ofunit = 91
      character(len=6),dimension(nbase)    :: veclabels = [ character(len=6) :: 'base','base','offset' ]
      !
      integer :: posA, posB
      double precision,parameter :: beta = 0.D0
      !
            info = 1
            if (.not. (associated(cnf%ptr_theta_range) .and. associated(cnf%ptr_w_range)))  return
            !
            ! Introduce youself ;-)
            write(display_unit,'(A)') 'AlamYld, $Rev$'
            do i=1,nbase
                  write(display_unit,'(A,1x,A,6(F6.2,1X))') veclabels(i),'vector:',cnf%base_vectors(:,i)
            enddo
            !
            npoints = cnf%ptr_theta_range%size()
            if (npoints <= 0) then
                  write(display_unit,fmt='(A)') 'Cannot run using empty range.'
                  return 
            endif
            !
            write(display_unit,'(A,1X,L1)') 'Normalization of the full Sm tensor:',cnf%normalizeSm
            if (cnf%do_scaling)   write(display_unit,'(A,1X,6(F6.2,1X))') 'Scaling by yield stress for:', cnf%scaling_vector
            ! Open the main output file
            open(unit=ofunit,file=trim(outputPrefix)//'.xyld',iostat=ioerr)
            if (ioerr /= 0) then
                  write(display_unit,fmt=952)
                  return 
            endif
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
            allocate(yldRes(npoints))
            !
            do while (cnf%ptr_w_range%next(w))
                  !
                  ! Loop over the range of theta angles
                  i = 1
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

                        yldRes(i) = yldResult(theta, w, scal_s, scal_s_rel, norm_sona, plast_pot, &
                                              pair_double(scal_s_rel * cos(theta), scal_s_rel * sin(theta)),&
                                              pair_double(0.D0,0.D0),beta, R)
                        
                        i = i + 1
                  enddo

                  ! Post-process the results
                  npoints = size(yldRes)
                  do i = 1, npoints
                        ! Get the positions of the bracketing points:
                        posA = merge(npoints-1,i - 1,i == 1)
                        posB = merge(2,i + 1, i == npoints)
                        ! write(*,*) posA,i,posB                        
                        call getArrow(yldRes(posA)%scal_s_rel_cart, yldRes(posB)%scal_s_rel_cart, &
                                      yldRes(i)%scal_s_rel_cart, & 
                                      1.D0,yldRes(i)%normal_cart,yldRes(i)%beta)
                  enddo
            enddo
            !
            call writeYldResults(ofunit,yldRes,info)
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
      
      
      subroutine getArrow(A,B,C,arrlen,u,beta)
      implicit none
      type(pair_double), intent(in) :: A,B,C !< Positions of the points: C is bracketed by A and B
      double precision,intent(in)   :: arrlen  !< Length of the arrow
      type(pair_double),intent(out) :: u  !< vector
      double precision,intent(out)  :: beta !< 
      !
      double precision :: xdeltaAB, ydeltaAB, DCnorm
      double precision :: pa, pb, pc, pd, s
      type(pair_double) :: D ! Intersection of the lines
            xdeltaAB = B%x  - A%x
            ydeltaAB = B%y  - A%y
            !
            ! tan(beta) = - (xdelta)/(ydelta)
            !             
            if ( (abs(ydeltaAB) > epsilon(0.D0)) .and. (abs(xdeltaAB) > epsilon(0.D0))  ) then
                  ! The most typical case
                  beta = atan(-xdeltaAB/ydeltaAB)
                  !
                  pa = ydeltaAB / xdeltaAB
                  pb = A%y - pa * A%x
                  pc = -1.D0 / pa
                  pd = C%y - pc * C%x  ! Cy + (-1/a)*Cx
                  D%x = (pd - pb) / (pa - pc)
                  D%y = pa * D%x + pb
            else
                  if (abs(xdeltaAB) <= epsilon(0.D0)) then
                        ! the normal is a horizontal line
                        beta = 0.D0
                        beta = merge(0.D0,pi, ydeltaAB > 0) ! the sign matters
                        D%x = A%x ! or B%x as well
                        D%y = C%y 
                  else
                        ! the normal is a vertical line
                        beta = merge(pi2,3.0*pi2, xdeltaAB > 0) ! the sign matters
                        D%x = C%x
                        D%y = A%y ! or B%y as well
                  endif
            endif
            DCnorm = sqrt((D%x - C%x)**2 + (D%y - C%y)**2)
            
            if (abs(DCnorm) > epsilon(0.D0)) then
                  s = arrlen /  DCnorm
                  u%x = s * (C%x - D%x)
                  u%y = s * (C%y - D%y)
            else
                  ! ouups, the points D and C overlap!
                  u = pair_double(0.D0,0.D0)
            endif
      !      
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
            'theta', 'w', 'S', 'S/S_0', '||S(A)||','Phi', 'S_x', 'S_y', 'delta_x', 'delta_y', 'beta', 'R']
      !
            info = fngErr_IOWrite
            ! Write the header
            if (present(write_header)) then
                  if (write_header) then
                        write(ounit,fmt=700,iostat=ierr) (centered(i,column_width),  i = 1, ncolumns)
                        if (ierr /= 0) return
                        write(ounit,fmt=701,iostat=ierr) (centered(column_labels(i)),i = 1, ncolumns)
                  endif
            endif
            !
            if (ierr /= 0) return
            do i = 1, size(res)
                 write(ounit,fmt=710) res(i) 
                 if (ierr /= 0) exit
            enddo
            if (ierr == 0) info = fngSuccess
            !            
            ! Formats for output file
            700 format('#',12(A15,1X)) 
            701 format(1X, 12(A15,1X)) 
            710 format(1X, 12(E15.8,1X),4(F15.8,1X))

      !
      end subroutine
      
end module
