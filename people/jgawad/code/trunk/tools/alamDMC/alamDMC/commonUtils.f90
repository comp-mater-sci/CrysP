!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2012-08-16
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!> Shared subroutines that offer (safer) access to the results of the multilevel
!> model.
!>
!> This design emphasizes separation of the algorithms in the driver modules
!> from the actual implementation of the underlying multilevel model.
module commonUtils
implicit none

contains

      !> Perform basic checks if the state variables in altayConfig are consistent.
      logical function isStateOK(stepid)
      use altayConfig
      implicit none
      integer,intent(in)      :: stepid
      !
            isStateOK = .false.
            if (allocated(astate%simulCalls)) then
                  isStateOK = (size(astate%simulCalls) <= stepid) .and. (astate%this >= stepid)
            endif
      !
      end function

      subroutine  getTaylorFactor(stepid,M,info)
      use altayConfig
      implicit none
      integer,intent(in)            :: stepid
      double precision,intent(out)  :: M
      integer,intent(out)           :: info
      !
            info = -1
            M = 0.D0
            if (isStateOK(stepid)) then
                  M = astate%simulCalls(stepid)%output%taylor_factor
                  info = 0
            endif
      !
      end subroutine

      subroutine makeTextureUpdateStep(D,S,M,output_flag,info)
      use altaySub
      use altayConfig
      implicit none
      double precision,dimension(3,3),intent(in)      :: D
      double precision,dimension(3,3),intent(out)     :: S
      double precision,intent(out)                    :: M
      logical,intent(in)                              :: output_flag
      integer,intent(out)                             :: info
      !
      integer,parameter :: istp = 1
            info = -1
            !
            call initStepData(istp,astate,info)
            if (info /= 0) return
            ! Set input data for AlTay
            associate (input => astate%simulCalls(istp)%input)
                  input%dgf = D
                  input%keep_texture = .false.
                  input%full_model = .true.
                  input%do_output_init = .false.
                  input%do_output_final = output_flag
                  call setStepType(input,acnf%model_id,info)
            end associate
            call runSteps(astate,info)
            if (info /= 0) return
            !
            ! Get the result
            S = astate%simulCalls(istp)%output%stress_tensor(:,:)
            M = astate%simulCalls(istp)%output%taylor_factor

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine
      
      subroutine outputTexture(info)
      use altaySub
      implicit none
      integer,intent(out)     :: info
      !
            call outputCurrentTexture(info)
      !
      end subroutine
      
end module