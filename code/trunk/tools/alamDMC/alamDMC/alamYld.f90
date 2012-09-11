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
!> ALAMel Arbitrary Stress Response
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
      double precision                          :: theta_min = 0.D0, theta_max = 360.D0
      double precision                          :: dtheta = 10.D0      
      
      logical                                   :: scaleByFirstBase = .true.
      
      integer,parameter                         :: nbase = 3
      double precision,dimension(6,nbase)       :: base_vectors = 0.D0
      
      logical                                   :: normalizeSm
      
      public AlamYld_ReadConfig, AlamYld_Run
contains

      subroutine AlamYld_ReadConfig(cnfunit,info)
      implicit none
      integer,intent(in)                        :: cnfunit
      integer,intent(out)                       :: info
      !
      integer :: ioerr,i
      double precision :: norm
      logical :: normalize
            info = -1
            ! Read parameters specific for the alamASR program
            read(cnfunit,fmt=*,iostat=ioerr)  theta_min, theta_max, dtheta
            if (ioerr /= 0) return
            read(cnfunit,fmt='(L)',iostat=ioerr) scaleByFirstBase
            if (ioerr /= 0) return
            base_vectors = 0.D0
            do i=1,nbase
                  normalize = .false.
                  read(cnfunit,fmt=*,iostat=ioerr) normalize, base_vectors(:,i)
                  if (ioerr /= 0)  exit
                  if (normalize) then 
                        norm = norm2(base_vectors(:,i))
                        if (norm > 0.D0) base_vectors(:,i)  = base_vectors(:,i) / norm
                  endif
            enddo
            if (ioerr /= 0) return
            read(cnfunit,fmt='(L)',iostat=ioerr) normalizeSm
            !
            ! Check if the requested range description
            if (theta_min + dtheta < theta_min) return
            !
            ! Convert the angles into radians
            theta_min = theta_min * deg2rad
            theta_max = theta_max * deg2rad
            dtheta = dtheta * deg2rad
            !
            info = 0
            
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine


      subroutine AlamYld_Run(info)
      implicit none
      integer,intent(out)                       :: info      
      
      ! Base tensors
      double precision,dimension(3,3)           :: Sm
      double precision                          :: theta
      double precision                          :: iunilen ! Inverse of the length of uniaxial tensile stress
      logical                                   :: doEvalBase1
      !
      double precision                          :: plast_pot, scal_s, norm_sona, vS_norm, scal_s_rel
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
                  write(display_unit,'(A,1x,A,6(F6.2,1X))') veclabels(i),'vector:',base_vectors(:,i)
            enddo
            write(display_unit,'(A,1X,L1)') 'Normalization of the full Sm tensor:',normalizeSm
            ! Open the main output file
            open(unit=ofunit,file=trim(outputPrefix)//'.xyld',iostat=ioerr)
            if (ioerr /= 0) then
                  write(display_unit,fmt=952)
                  return 
            endif
            write(ofunit,fmt=700) '#Theta', 'scal_S', 'scal_S_rel', '||SonA||', 'W', 'S1_rel', 'S2_rel'
            !
            iunilen = 1.D0
            doEvalBase1 = scaleByFirstBase
            ! Fix the configuration: no need for anything except for the stresses.
            ylpCnf%evaluate_full_model = .false.
            !
            ! Loop over the range of theta angles
            theta = theta_min
            do while (theta <= theta_max)
                  write(*,800)
                  ! Trick: we run evaluation of the uniaxial case as a "fake iteration".
                  ! If the uniaxial case corresponds with the first theta point, it will be reused.
                  if (doEvalBase1) then
                        theta = 0.D0
                  endif
                  !
                  write(display_unit,fmt=200)
                  write(display_unit,fmt=201) (theta * rad2deg)
                  write(display_unit,fmt=200)
            
                  ! Combine the base vectors
                  Sm = Vec6ToMat33(base_vectors(:,1)*cos(theta) + base_vectors(:,2)*sin(theta) + base_vectors(:,3))
                  if (normalizeSm) Sm = Sm / norm2(Sm)
                  ! Convert to 5D space, note that Sm becomes deviatoric after this call: 
                  call KMAT2VEC5D(Sm,vS) 
                  ! Enforce unit length of vS
                  vS_norm = vec_norm2(vS)
                  !if (abs(vS_norm) < epsilon(0.D0)) then
                  !      write(*,*) 'Norm of the stress cannot be zero, skipping'
                  !      cycle
                  !endif
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

                  if (doEvalBase1) then
                        iunilen = 1.D0 / scal_s
                  endif
                  scal_s_rel = scal_s * iunilen
            
                  ! Smn = Sm * scal_s

                  !
                  write(display_unit,fmt=510)
                  write(display_unit,fmt=500) 'theta', 'S', 'S_rel', 'W' 
                  write(display_unit,fmt=501) theta*rad2deg, scal_s, scal_s_rel, plast_pot
                  write(display_unit,fmt=510)
                  ! write output & advance theta
                  if ( (.not. doEvalBase1) .or. (doEvalBase1 .and. (theta == theta_min)) ) then
                        write(ofunit,fmt=701) theta, scal_s, scal_s_rel, norm_sona, plast_pot, &
                                              scal_s_rel * cos(theta), scal_s_rel * sin(theta)
                  endif
                        
                  if (doEvalBase1) then
                        doEvalBase1 = .false.  ! No more "false iterations"
                        ! If the uniaxial case corresponds to theta_min, there is no need to repeat the calculations
                        if (theta /= theta_min) theta = theta_min - dtheta
                  endif
                  theta = theta + dtheta
            enddo            
            !
            close(ofunit)
      
            info = 0
      !
      200 format(28('-'))
      201 format('Theta angle =',T20,F8.3) 
      400 format(A,T40,A,T80,A)
      500 format(1X,4(A10,'|'))
      501 format(F10.3,1X,3(F10.6,1X))
      510 format('|',4(10('-'),'|'))
      ! Formats for output file
      700 format(7(A12,1X)) 
      701 format(7(F12.6,1X))
      
      !
#define MSG_GROUP_RULERS     
#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
#undef MSG_GROUP_RULERS

      end subroutine
      
end module
