!> Shared subroutines that offer (safer) access to the results of the multilevel
!> model.
!>
!> This design emphasizes separation of the algorithms in the driver modules
!> from the actual implementation of the underlying multilevel model.
module commonUtils
    use base_defs

    implicit none

    !> Test the presence of optional value, and return a default if the optional
    !> is not present.
    !>
    !> The function provides a simplified access pattern to optional parameters.
    !> The first formal argument is declared as optional parameter, butit must always
    !> appear as the actual parameter in a context where the actual parameter is
    !> declared itself as "optional".
    interface optionalDefault
        module procedure optionalDefault_logical, optionalDefault_integer
    end interface

contains

      subroutine  getTaylorFactor(stepid, M, info)
      use altayConfig
      integer, intent(in)            :: stepid
      real(DP), intent(out)  :: M
      integer, intent(out)           :: info
      !
            info = -1
            M = 0.D0
            if (isStateOK(stepid)) then
                  M = astate%simulCalls(stepid)%output%taylor_factor
                  info = 0
            endif
      !
      contains
      !> Perform basic checks if the state variables in altayConfig are consistent.
      logical function isStateOK(stepid)
      use altayConfig
      integer, intent(in)      :: stepid
      !
            isStateOK = .false.
            if (allocated(astate%simulCalls)) then
                  isStateOK = (size(astate%simulCalls) <= stepid) .and. (astate%this >= stepid)
            endif
      !
      end function

      end subroutine

      subroutine makeTextureUpdateStep(D, S, M, output_flag, info)
      use altay
      use altayConfig
      real(DP), dimension(3, 3), intent(in)      :: D
      real(DP), dimension(3, 3), intent(out)     :: S
      real(DP), intent(out)                    :: M
      logical, intent(in)                              :: output_flag
      integer, intent(out)                             :: info
      !
      integer, parameter:: istp = 1
            info = -1

            !
            call initStepData(istp, astate, info)
            if (info /= 0) return
            ! Set input data for AlTay
            associate (input => astate%simulCalls(istp)%input)
                  input%dgf = D
                  input%keep_texture = .false.
                  input%keep_state = .false.
                  input%full_model = .true.
                  input%do_output_init = .false.
                  input%do_output_final = output_flag
            end associate
            call runSteps(astate, info)
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
      use altay
      integer, intent(out)     :: info
      !
            call outputCurrentState(info)
      !
      end subroutine

      !> Test the presence of optional logical value, and return a default if the optional
      !> is not present.
      pure logical function optionalDefault_logical(value, default) result(res)
          logical, intent(in), optional   :: value !< The parameter to be tested for presence. The actual parameter MUST have optional attribute.
          logical, intent(in)            :: default  !< Default value

          if(present(value))then; res = value; else; res = default; endif
      end function

    !> Test the presence of optional integer value, and return a default if the optional
    !> is not present.
    pure integer function optionalDefault_integer(value, default) result(res)
          integer, intent(in), optional   :: value !< The parameter to be tested for presence. The actual parameter MUST have optional attribute.
          integer, intent(in)            :: default  !< Default value
          if(present(value))then; res = value; else; res = default; endif

    end function

    pure function toString(num) result(str)
        class(*), intent(in)    :: num
        character(12):: str

        select type(num)
            type is (integer)
                write (str, '(I0)') num
            type is (real(DP))
                write (str, '(G10.4)') num
        end select

        str = trim(str)
    end function
end module
