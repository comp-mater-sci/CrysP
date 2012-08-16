!
! $Id$
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
            associate (cnf => astate%simulCalls(istp)%input)
                  cnf%dgf = D
                  cnf%keep_texture = .false.
                  cnf%full_model = .true.
                  cnf%do_output = output_flag
            end associate
            call runSteps(astate,info)
            if (info /= 0) return
            !
            ! Get the result
            S = astate%simulCalls(1)%output%stress_tensor(:,:)
            M = astate%simulCalls(1)%output%taylor_factor

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine
      
      
end module